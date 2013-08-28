Lexicos
=======

Synopsis
--------

This library provides a set-like data structure for the [Scala language][], a *Lexicon*.

Behaviour
---------

A Lexicon is a specialized, immutable, sorted set of sequences containing elements of a generic, but ordered type (`Lexicon[T <% Ordered[T]] <: SortedSet[Seq[T]]`) that can be used to scan indexed input sequences for the presence of any sequence contained in the set at a given offset in the input sequence.

Implementation
--------------

To achieve this behaviour, a Lexicon creates a minimal acyclic deterministic finite state automaton (MADFA) representation of all sequences it contains. To build that MADFA, it uses the linear ("_O(n)_") MADFA construction algorithm (see "Algorithm 1") described in [Daciuk et al.][], Comp Ling 2000. Matching the entire set against an input sequence is also approximately linear wrt. the length _m_ of the input sequence ("_O(m)_").

API
---

In addition to the default [SortedSet API][], a Lexicon provides the following methods:

```scala
/** A Minimal Acyclic DFA (MADFA) data structure for sets of sequences ("words")
  * containing elements ("symbols").
  * A Lexicon has to be built from the sequences ("words") in their natural order.
  * All sequences except the '''empty word''' are valid words. */
class Lexicon[T <% Ordered[T]]
    extends SortedSet[Seq[T]]
    with SortedSetLike[Seq[T], Lexicon[T]] {

  /** Generate a digraph representation of the underlying MADFA in Graphviz DOT format.
    * @param id name (graph 'ID' in DOT notation) to use for the digraph */
  def dot(id: String = "MADFA"): String

  /** Find the end index of a word in the lexicon that is the longest common prefix in input at
    * offset `start`.
    * @param input string to check
    * @param start offset at which to begin matching (default: 0)
    * @return the matched word's end offset if any */
  def indexOf(input: IndexedSeq[T], start: Int = 0): Option[Int] 

  /** Get an iterator for all words in the lexicon that start with `prefix`. */
  def iterator(prefix: Seq[T]): Iterator[Seq[T]]

  /** Get the number of states in the underlying MADFA. */
  def length: Int

  /** Get the word in the lexicon that is the longest common prefix of `input` at offset `start`.
    * This is equal to calling `input.slice(start, lexicon.indexOf(input, start).get)` if such a
    * word and end offset exist.
    * @param input string to check
    * @param start offset at which to begin matching (default: 0)
    * @return the matched word if any */
  def lookup(input: IndexedSeq[T], start: Int = 0): Option[Seq[T]]

}

object Lexicon extends {

  /** Build a new Lexicon from a collection of `words`. */
  def apply[T <% Ordered[T]](words: Seq[T]*): Lexicon[T]

  /** Fetch a new, empty Lexicon. */
  def empty[T <% Ordered[T]] = new Lexicon[T]

  /** Build a new Lexicon from a sequence of `words`.
    * This method ensures word order and removes duplicates. */
  def fromSeq[T <% Ordered[T]](words: Seq[Seq[T]]): Lexicon[T]

  /** Build a new Lexicon from an iterator over ordered, unique `words`.
    * The caller needs to ensure uniqueness and [natural] order of the words.
    * @throws AssertionError if the words are not in order or unique */
  def fromIterator[T <% Ordered[T]](words: Iterator[Seq[T]]): Lexicon[T]

}
```

Copyright
---------

(C) Florian Leitner 2013. All rights reserved.

License
-------

This library is made availabe under the terms of the:

__Apache License v2__

  [Scala language] http://www.scala-lang.org/
  [Daciuk et al.]: http://www.mitpressjournals.org/doi/abs/10.1162/089120100561601
  [SortedSet API]: http://www.scala-lang.org/api/2.10.0/index.html#scala.collection.SortedSet
