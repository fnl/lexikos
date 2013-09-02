package fnl.collection

/* lexikos/fnl.collection [2013-08-21T11:39]
 * (C) Florian Leitner 2013. All rights reserved. */

import scala.annotation.tailrec
import scala.collection.mutable

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

	/** Replace [and re-register] or register (update) the `parent -symbol-> child` transition. */
	private def replaceOrUpdate(parent: Int, symbol: T, child: Int) {
		val existing = registry.getOrElseUpdate(rightLanguage(child), child)
		if (existing != child) { // replace
			val entry = registry remove rightLanguage(parent)
			words(existing) += words(child)
			elems(parent) = elems(parent) + ( symbol -> existing )
			assert(elems.length - 1 == child,
				"child %d is not the last element added (%d)\n%s" format (child, elems.length, elems))
			elems remove child
			words remove child
			if (entry.isDefined) registry put (rightLanguage(parent), parent)
		}
	}

	/** Build a linear stack of all `parent -symbol-> child` transitions starting at `parent` by
	  * following only the last ("max") transitions. */
	@tailrec private def latestTransitions(parent: Int, stack: List[(Int, T, Int)]):
	List[(Int, T, Int)] = {
		val transitions = elems(parent)
		if (transitions.isEmpty) stack
		else {
			val symbol = transitions.keys.max
			val child = transitions(symbol)
			latestTransitions(child, ( parent, symbol, child ) :: stack)
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
	private def replaceOrRegister(lastState: Int) {
		var stack = latestTransitions(lastState, Nil)
		while (!stack.isEmpty) {
			replaceOrUpdate(stack.head._1, stack.head._2, stack.head._3)
			stack = stack.tail
		}
	}

	/** Return the right Language for an existing state. */
	private def rightLanguage(state: Int) =
		LexiconBuilder rightLanguage (elems(state), words(state) != 0)

	// Public Builder API ===========================================================================

	/** Insert a new word into the Lexicon (has to be in [natural] order);
	  * see [1], Algorithm 1, do loop). */
	override def +=(word: Seq[T]): this.type = {
		assert(Lexicon.ordering[T] lt (lastWord, word),
			"%s < %s order violation" format (lastWord, word))
		if (elems.length == 0) {
			elems append Map.empty[T, Int]  // Start transitions
			words append 0 // Start node word count (always zero)
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
		var idx = 1
		if (isFinal) code(0) = 1
		transitions.keys.toSeq.sorted foreach { symbol => {
			code(idx) = symbol.hashCode()
			code(idx + 1) = transitions(symbol)
			idx += 2
		}}
		code.toVector
	}
}