package fnl.collection

/* lexikos/fnl.collection [2013-08-14T13:08]
 * (C) Florian Leitner 2013. All rights reserved. */

import scala.annotation.tailrec
import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.math.Ordering.Implicits.seqDerivedOrdering

/** A Minimal Acyclic DFA (MADFA) [1] data structure for sets of sequences ("words")
  * containing elements ("symbols").
  *
  * Note that the chosen implementation ("Algorithm 1" in paper [1]) requires that the Lexicon
  * is build from the sequences ("words") in their natural order.
  *
  * All sequences except the '''empty word''' are valid words.
  *
  * [1] [[http://www.mitpressjournals.org/doi/abs/10.1162/089120100561601
  * Daciuk et al., Comp Ling 2000]] */
class Lexicon[T <% Ordered[T]](
	private val digraph: IndexedSeq[Map[T, Int]] = IndexedSeq.empty,
	private val words: IndexedSeq[Int] = IndexedSeq.empty)
	extends SortedSet[Seq[T]] with SortedSetLike[Seq[T], Lexicon[T]] {

	/* A Lexicon is represented as a MADFA, where a single state in the DFA is associated with
	 * labeled transitions to child states, stored as a vector map of transitions.
	 * Recursive functions are implemented as tail recursions or as while loops to avoid BOs when
	 * working with a bigger Lexicon or long words. */

	/** Pre-calculate the start state's transitions in [natural] order. */
	lazy val startTransitions = if (digraph.length == 0) Nil else digraph(0).toList.sorted

	/** Return a Graphviz DOT representation of the digraph and words (counts). */
	private def graphvizDot(): String = {
		val buf = new StringBuilder()
		var state = 0
		digraph.foreach { transition =>
			buf ++= "  %d [label=%s]\n".format(state, if (state == 0) "S" else words(state))
			buf ++= transition.map { case (symbol, child) =>
				"    %d -> %d [label=\" %s \"]\n".format(state, child, symbol)
			}.mkString
			state += 1
		}
		buf.toString()
	}

	/** Return a iterator over all suffixes at or after `state` in the Lexicon using [natural]
	  * order.
	  *
	  * Note that this iterator may not be used on an empty Lexicon. */
	private def iterator(state: Int): Iterator[Seq[T]] =
		new TransitionIterator(List(sortedTransitions(state))) {
			private val path = new mutable.ArrayBuffer[T]()
			private var word: Option[Seq[T]] = nextWord()

			/** A depth-first trail walk that produces all paths to all words. */
			private def nextWord(): Option[Seq[T]] = {
				while (!stack.isEmpty) {
					val transitions = stack.head
					if (!transitions.isEmpty) {
						val (symbol, child) = transitions.next()
						path.append(symbol)
						stack = sortedTransitions(child) :: stack
						if (words(child) != 0) return Some(path.toVector)
					} else {
						stack = stack.tail
						if (!path.isEmpty) path.remove(path.length - 1)
					}
				}
				None
			}

			def next(): Seq[T] = word match {
				case Some(seq) => {word = nextWord(); seq}
				case None => Iterator.empty.next()
			}

			def hasNext: Boolean = word.isDefined
		}

	/** Get the state at the end of `path` if that path exists. */
	@tailrec private[collection] final def traverse(path: Seq[T], state: Int):
	Option[Int] =
		if (path.isEmpty) Some(state)
		else {
			val transitions = digraph(state)
			if (!transitions.contains(path.head)) None
			else traverse(path.tail, transitions(path.head))
		}

	/** Return an iterator over the symbol transitions at this state in [natural] order. */
	private def sortedTransitions(state: Int): Iterator[(T, Int)] =
		if (state == 0) startTransitions.iterator
		else {
			val transitions = digraph(state)
			if (transitions.size < 2) transitions.iterator else transitions.toList.sorted.iterator
		}

	// Public Set API ==============================================================================

	/** Return `true` if the `word` is in the Lexicon. */
	def contains(word: Seq[T]): Boolean = words(traverse(word, 0).getOrElse(0)) > 0

	/** Fetch a fresh, empty Lexicon. */
	override def empty: Lexicon[T] = Lexicon.empty[T]

	/** Apply a `function` to each word of this Lexicon in their [natural] order. */
	override def foreach[U](function: Seq[T] => U): Unit = {
		if (length > 0) {
			val path = mutable.ArrayBuffer[T]()
			var stack = List(startTransitions.iterator)
			// implements a depth-first trail walk
			while (!stack.isEmpty) {
				val transitions = stack.head
				if (!transitions.isEmpty) {
					val (symbol, child) = transitions.next()
					path.append(symbol)
					if (words(child) != 0) function(path.toVector)
					if (digraph(child).size == 0) path.remove(path.length - 1)
					else stack = sortedTransitions(child) :: stack
				} else {
					stack = stack.tail
					if (!path.isEmpty) path.remove(path.length - 1)
				}
			}
		}
	}

	/** Return an iterator over all words in the Lexicon using their [natural] order. */
	def iterator: Iterator[Seq[T]] = if (digraph.length > 0) iterator(0) else Nil.iterator

	/** Provide a new Lexicon Builder for the same element type. */
	override def newBuilder: mutable.Builder[Seq[T], Lexicon[T]] = new LexiconBuilder[T]()

	/** Words (`Seq[T]`) are [implicitly] ordered by their [natural] order. */
	implicit def ordering: Ordering[Seq[T]] = Lexicon.ordering[T]

	/** Create a ranged projection from the words in their [natural] order.
	  *
	  * @param from  The lower-bound (inclusive) of the ranged projection.
	  *              `None` if there is no lower bound.
	  * @param until The upper-bound (exclusive) of the ranged projection.
	  *              `None` if there is no upper bound. */
	def rangeImpl(from: Option[Seq[T]], until: Option[Seq[T]]): Lexicon[T] =
		filter(from match {
			case None => until match {
				case None => word => true
				case Some(end) => ordering.gt(end, _)
			}
			case Some(start) => until match {
				case None => ordering.lteq(start, _)
				case Some(end) => word => ordering.gt(end, word) && ordering.lteq(start, word)
			}
		})

	/** Calculate the number of words encoded by this Lexicon. */
	override def size: Int = words.fold(0)(_ + _)

	/** Create a new set with an additional `word` inserted, unless that word is already present. */
	def +(word: Seq[T]): Lexicon[T] = if (!contains(word)) {
		val (before, after) = iterator.partition(ordering.lt(_, word))
		val b = newBuilder
		b.sizeHint(length + word.length)
		before.foreach(b.+=)
		b += word
		after.foreach(b.+=)
		b.result()
	} else this

	/** Create a new set with the given `word` removed, unless that word is already absent. */
	def -(word: Seq[T]): Lexicon[T] = if (contains(word)) {
		val b = newBuilder
		b.sizeHint(length)
		for (w <- this if w != word) b += w
		b.result()
	} else this

	// Public Lexicon API ==========================================================================

	/** Generate a digraph representation of the underlying MADFA in Graphviz DOT format.
	  *
	  * @param id name (graph 'ID' in DOT notation) to use for the digraph */
	def dot(id: String = "MADFA"): String =
		"digraph %s {\n  node [shape=circle]\n%s}".format(id, graphvizDot())

	/** Find the end index of a word in the lexicon that is the longest common prefix in input at
	  * offset `start`.
	  *
	  * @param input string to check
	  * @param start offset at which to begin matching (default: 0)
	  * @return the matched word's end offset if any */
	def indexOf(input: IndexedSeq[T], start: Int = 0): Option[Int] = {
		{
			var node: Option[Int] = Some(0)
			var end: Option[Int] = None
			for (idx <- start to input.length) {
				node match {
					case Some(state) => {
						if (0 != words(state)) end = Some(idx)
						node = if (idx != input.length) digraph(state).get(input(idx)) else None
					}
					case _ => return end
				}
			}
			end
		}
	}

	/** Get an iterator for all words in the lexicon that start with `prefix`. */
	def iterator(prefix: Seq[T]): Iterator[Seq[T]] = traverse(prefix, 0) match {
		case None => Nil.iterator
		case Some(state) => words(state) match {
			case 0 => iterator(state).map(prefix ++ _)
			case _ => Iterator.single[Seq[T]](prefix) ++ iterator(state).map(prefix ++ _)
		}
	}

	/** Get the number of states in the underlying MADFA. */
	def length: Int = digraph.length

	/** Get the word in the lexicon that is the longest common prefix of `input` at offset `start`.
	  *
	  * This is equal to calling `input.substring(start, lexicon.find(input, start).get)` if such a
	  * word and end offset exist.
	  *
	  * @param input string to check
	  * @param start offset at which to begin matching (default: 0)
	  * @return the matched word if any */
	def lookup(input: IndexedSeq[T], start: Int = 0): Option[Seq[T]] = indexOf(input, start) match {
		case Some(end) => Some(input.slice(start, end))
		case None => None
	}
}

object Lexicon extends {
	/** Build a new Lexicon from a collection of `words`. */
	def apply[T <% Ordered[T]](words: Seq[T]*): Lexicon[T] = fromSeq(words)

	/** Implicitly construct a new Lexicon while traversing an existing Lexicon. */
	implicit def canBuildFrom[T <% Ordered[T]]: CanBuildFrom[Lexicon[T], Seq[T], Lexicon[T]] =
		new CanBuildFrom[Lexicon[T], Seq[T], Lexicon[T]] {
			def apply(from: Lexicon[T]): mutable.Builder[Seq[T], Lexicon[T]] = from.newBuilder
			def apply(): mutable.Builder[Seq[T], Lexicon[T]] = newBuilder
		}

	/** Fetch a new, empty Lexicon. */
	def empty[T <% Ordered[T]] = new Lexicon[T]

	/** Build a new Lexicon from a sequence of `words`.
	  *
	  * This method ensures word order and removes duplicates. */
	def fromSeq[T <% Ordered[T]](words: Seq[Seq[T]]): Lexicon[T] = {
		val b = newBuilder[T]
		b.sizeHint(words.length)
		var last = Seq.empty[T]
		for (w <- words.sorted(ordering[T]) if w != last) {
			last = w.toSeq; b += last
		}
		b.result()
	}

	/** Build a new Lexicon from an iterator over ordered, unique `words`.
	  *
	  * The caller needs to ensure uniqueness and [natural] order of the words.
	  *
	  * @throws AssertionError if the words are not in order or unique */
	def fromIterator[T <% Ordered[T]](words: Iterator[Seq[T]]): Lexicon[T] = {
		val b = newBuilder[T]
		words.foreach(b.+=)
		b.result()
	}

	/** Create a new Lexicon from [naturally] ordered, unique words. */
	def newBuilder[T <% Ordered[T]]: mutable.Builder[Seq[T], Lexicon[T]] = new LexiconBuilder[T]()

	/** Natural, "sequence derived" ordering of words. */
	def ordering[T <% Ordered[T]]: Ordering[Seq[T]] = seqDerivedOrdering[Seq, T]
}

private abstract class TransitionIterator[T](
	/** A `stack` of transition `symbols: T` and child `states: Int` as '''ordered''' iterators. */
	private[collection] var stack: List[Iterator[(T, Int)]])
	extends Iterator[Seq[T]]