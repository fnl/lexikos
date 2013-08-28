package fnl.collection

/* lexikos/fnl.collection [2013-08-21T11:39]
 * (C) Florian Leitner 2013. All rights reserved. */

import scala.annotation.tailrec
import scala.collection._

/** A Builder for a Lexicon.
  *
  * This Builder implements "Algorithm 1" from Daciuk et al. [1] and therefore requires that a
  * Lexicon is build from a set of sequences ("words") in natural order.
  *
  * [1] [[http://www.mitpressjournals.org/doi/abs/10.1162/089120100561601
  * Daciuk et al., Comp Ling 2000]] */
private class LexiconBuilder[T <% Ordered[T]] extends mutable.Builder[Seq[T], Lexicon[T]] {
	val elems = new mutable.ArrayBuffer[Map[T, Int]]()
	val words = new mutable.ArrayBuffer[Int]()
	val registry = mutable.Map[Vector[Int], Int]()
	var lastWord = Seq.empty[T]

	/** Append the suffix `word` to `state` (see [1], Algorithm 1, func add_suffix). */
	@tailrec private def addSuffix(word: Seq[T], state: Int): Unit =
		if (word.isEmpty) words(state) += 1
		else {
			val next = words.length
			elems(state) = elems(state) + ( word.head -> next )
			words.append(0)
			elems.append(Map.empty[T, Int])
			addSuffix(word.tail, next)
		}

	/** Get the suffix and state for `word` at `state` that is at the longest common prefix of `word`
	  * in the Lexicon (see [1], Algorithm 1, func common_prefix). */
	@tailrec private def commonPrefix(word: Seq[T], state: Int): (Int, Seq[T]) = {
		if (word.isEmpty) (state, word)
		else {
			val transitions = elems(state)
			if (!transitions.contains(word.head)) (state, word)
			else commonPrefix(word.tail, transitions(word.head))
		}
	}

	/** Register new states or expand transitions from existing states to add the latest unregistered
	  * suffix - defined as path.substring(offset) - to the digraph.
	  *
	  * This method implements the "simple" version of the build Algorithm (Algorithm 1 vs. 2 in
	  * [1]), so all words have to be added to the `Lexicon.inLexicalOrder`. It represents the core
	  * approach of the build process.
	  *
	  * This procedure is described in [1] (see Algorithm 1, "func replace_or_register(State)").
	  *
	  * @return the start state */
	private def replaceOrRegister(lastState: Int) = {
		// implemented as a while loop instead of the head recursion used in the paper
		var transitions = elems(lastState)
		var state = lastState
		var stack = List[(Int, Map[T, Int])]()
		// build up a "stack" of states as long as the current "state" has children
		while (!transitions.isEmpty) {
			stack = (state, transitions) +: stack
			state = if (transitions.size == 1) transitions.values.head
			else
				transitions(transitions.keys.toSeq.sorted.last)
			transitions = elems(state)
		}
		// while the stack is not empty, keep popping it,
		// effectively working on the deepest states first
		while (!stack.isEmpty) {
			state = stack.head._1
			transitions = stack.head._2
			stack = stack.tail
			val symbol = if (transitions.size == 1) transitions.keys.head
			else
				transitions.keys.toSeq.sorted.last
			val child = transitions(symbol)
			val code = rightLanguage(child)
			registry.get(code) match {
				case Some(existing) => {
					// if so, replace the child with the registered state while ...
					assert(existing != child, existing)
					val parent = rightLanguage(state)
					if (registry.contains(parent)) {
						// [special case: update registry entry with new right language]
						registry.remove(parent)
						words(existing) += words(child)
						elems(state) = elems(state) + ( symbol -> existing )
						registry.put(rightLanguage(state), state)
					} else {
						words(existing) += words(child)
						elems(state) = elems(state) + ( symbol -> existing )
					}
					// ... removing the replaced child entries, or ...
					// [replaceable child states always should be the latest element]
					assert(elems.length - 1 == child,
						"child %d is not the last element (%d)\n%s".format(child, elems.length, elems))
					elems.remove(child)
					words.remove(child)
				}
				case None => {
					// ... if not, only register the child's right language
					registry.put(code, child)
				}
			}
		}
	}

	/** Return the right Language for an existing state. */
	private def rightLanguage(state: Int) =
		LexiconBuilder.rightLanguage(elems(state), words(state) != 0)

	// Public Builder API ===========================================================================

	/** Insert a new word into the Lexicon (has to be in [natural] order);
	  * see [1], Algorithm 1, do loop). */
	override def +=(word: Seq[T]): this.type = {
		assert(Lexicon.ordering[T].lt(lastWord, word),
			"%s < %s order violation".format(lastWord, word))
		if (elems.length == 0) {
			elems.append(Map.empty[T, Int]) // Start transitions
			words.append(0) // Start node word count (always zero)
			addSuffix(word, 0)
		} else {
			val (lastState, suffix) = commonPrefix(word, 0)
			if (!suffix.isEmpty) replaceOrRegister(lastState)
			addSuffix(suffix, lastState)
		}
		lastWord = word
		this
	}

	/** Clear the builder for re-use. */
	override def clear() {
		elems clear()
		registry clear()
		words clear()
		lastWord = Seq.empty[T]
	}

	/** Provide the underlying data structures with a size hint that should be about the length of
	  * the Lexicon being built. */
	override def sizeHint(length: Int) = {
		elems.sizeHint(length)
		words.sizeHint(length)
		registry.sizeHint(length)
	}

	/** Run the replacement routine on the start state and return a fresh Lexicon. */
	override def result(): Lexicon[T] = {
		if (elems.length != 0) replaceOrRegister(0)
		new Lexicon(elems.toVector, words.toVector)
	}
}

private[collection] object LexiconBuilder {
	/** Return a unique value wrt. a state's right language (`L^{->}(q)`). */
	private def rightLanguage[T <% Ordered[T]](
		transitions: Map[T, Int], isFinal: Boolean = false): Vector[Int] = {
		val code = Array.fill(1 + 2 * transitions.size) {0}
		if (isFinal) code(0) = 1
		var idx = 1
		transitions.keys.toSeq.sorted.foreach({ case symbol => {
			code(idx) = symbol.hashCode()
			code(idx + 1) = transitions(symbol)
			idx += 2
		}
		})
		code.toVector
	}
}