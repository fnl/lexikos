package fnl.collection

/* lexikos/fnl.collection [2013-08-21T14:53]
 * (C) Florian Leitner 2013. All rights reserved. */

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LexiconSpec extends FlatSpec with ShouldMatchers {

	def time[R](block: => R): (R, Long) = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		(result, t1 - t0)
	}

	"An empty Lexicon" should "not have any words" in {
		Lexicon.empty[Char].toList should be === List()
	}
	it should "have size zero" in { Lexicon.empty[Char].size should be === 0 }
	it should "have length zero" in { Lexicon.empty[Char].length should be === 0 }
	it should "respond to contains" in { assert(!Lexicon.empty[Char].contains("a")) }
	it should "not traverse anything" in { Lexicon.empty[Char].foreach(w => assert(w==false)) }
	it should "have an empty iterator" in { Lexicon.empty[Char].iterator.length should be === 0 }
	it should "have an empty prefix iterator" in {
		Lexicon.empty[Char].iterator("a").length should be === 0
	}
	it should "only create empty range projections" in {
		assert(Lexicon.empty[Char].range("a", "b") === Lexicon.empty[Char])
	}
	it should "be able to add any other word" in {
		assert((Lexicon.empty[Char] + "a") === Lexicon("a"))
	}
	it should "remain empty after removing any word" in {
		assert((Lexicon.empty[Char] - "a") === Lexicon.empty[Char])
	}
	it should "have a 'pseudo-empty' .dot string" in {
		Lexicon.empty[Char].dot() should be === "digraph MADFA {\n  node [shape=circle]\n}"
	}
	it should "have an indexOf value of None" in {
		Lexicon.empty[Char].indexOf("a") should be === None
	}
	it should "have a look-up value of None" in {
		Lexicon.empty[Char].lookup("a") should be === None
	}

	"A Lexicon with one single-letter word" should "contain that word" in {
		Lexicon("a").toList should be === List("a".toSeq)
	}
	it should "have size one" in {Lexicon("a").size should be === 1}
	it should "have a length of that word plus one" in {
		assert(Lexicon("abc").length === 4, Lexicon("abc").dot())
	}

	"A Lexicon with three single-letter words" should "contain those words" in {
		Lexicon("a", "b", "c").toList should be === List("a".toSeq, "b".toSeq, "c".toSeq)
	}
	it should "have size three" in {Lexicon("a", "b", "c").size should be === 3}
	it should "have length two" in {
		val lex = Lexicon("a", "b", "c")
		assert(lex.length === 2, lex.dot())
	}

	"A Lexicon with three equal affix words" should "contain those words" in {
		val lex = Lexicon("aaa", "aba", "aca")
		assert(lex.toList === List("aaa".toSeq, "aba".toSeq, "aca".toSeq), lex.dot())
	}
	it should "have size three" in {Lexicon("aaa", "aba", "aca").size should be === 3}
	it should "have length four" in {
		val lex = Lexicon("aaa", "aba", "aca")
		assert(lex.length === 4, lex.dot())
	}


	"A Lexicon with (a, aaa)" should "contain those words" in {
		assert(Lexicon("a", "aaa").toList === List("a".toSeq, "aaa".toSeq), Lexicon("a", "aaa").dot())
	}
	it should "have size two" in {Lexicon("a", "aaa").size should be === 2}
	it should "have length four" in {
		assert(Lexicon("a", "aaa").length === 4, Lexicon("a", "aaa").dot())
	}
	it should "not contain aa" in {
		assert(!Lexicon("a", /*"aa",*/ "aaa").contains("aa"))
	}

	"A Lexicon with (a, aa, aaa)" should "contain those words" in {
		Lexicon("a", "aa", "aaa").toList should be === List("a".toSeq, "aa".toSeq, "aaa".toSeq)
	}
	it should "have size three" in {Lexicon("a", "aa", "aaa").size should be === 3}
	it should "have length four" in {Lexicon("a", "aa", "aaa").length should be === 4}
	it should "contain aa" in {
		assert(Lexicon("a", "aa", "aaa").contains("aa"))
	}

	"Mutating the Lexicon" should "return a new Lexicon if a new word is added" in {
		val a = Lexicon("a", "b")
		val b = a + "ab"
		a.size should be === 2
		b.size should be === 3
		assert(a != b, (a, b))
	}
	it should "return the same Lexicon if no new word is added" in {
		val a = Lexicon("a", "b")
		val b = a + "a"
		a.size should be === 2
		b.size should be === 2
		assert(a === b)
	}
	it should "return a new Lexicon if an existing word is removed" in {
		val a = Lexicon("a", "ab", "b")
		val b = a - "ab"
		a.size should be === 3
		b.size should be === 2
		assert(a != b)
	}
	it should "return the same Lexicon if no existing word is removed" in {
		val a = Lexicon("a", "b")
		val b = a - "ab"
		a.size should be === 2
		b.size should be === 2
		assert(a === b)
	}

	"A Lexicon" should "be convertible to Graphiz DOT language" in {
		val dot = Lexicon("a").dot("test")
		assert(dot.matches( """digraph test \{
 {2}node \[shape=circle\]
 {2}0 \[label=S\]
 {4}0 -> 1 \[label=" a "\]
 {2}1 \[label=1\]
\}"""), dot)
	}
	it should "throw an IllegalArgumentException if initialized with unequal length vectors" in {
		intercept[IllegalArgumentException] {
			new Lexicon[Int](Vector(), Vector(1))
		}
	}
	it should "produce a range wrt. lexical order" in {
		val range = Lexicon("a", "aa", "aaa", "aab", "ab", "b").range("aa", "ab")
		assert(range.toList === List("aa".toSeq, "aaa".toSeq, "aab".toSeq), range.dot())
	}
	it should "find the longest match offset in a string" in {
		assert(Lexicon("a", "aa", "b").indexOf("a").get === 1)
		assert(Lexicon("a", "aaa", "ab").indexOf("aaaaa").get === 3)
	}
	it should "not find non-existing match offsets in a string" in {
		assert(Lexicon("a").indexOf("") === None)
		assert(Lexicon("a").indexOf("b") === None)
		assert(Lexicon("aa", "ab").indexOf("a") === None)
		assert(Lexicon("aaa", "ab").indexOf("aab") === None)
	}
	it should "find the longest match offset at an offset in a string" in {
		assert(Lexicon("a", "aa", "aaa").indexOf("baab", 1).get === 3)
	}
	it should "not find non-existing match offsets at an offset in a string" in {
		assert(Lexicon("a", "aa", "ab").indexOf("bbb", 1) === None)
	}
	it should "list all words for a given prefix" in {
		val lex = Lexicon("a", "aa", "aab", "aaa", "abb")
		assert(lex.iterator("aa").toList === List("aa".toSeq, "aaa".toSeq, "aab".toSeq), lex.dot())
	}
	it should "iterate over an empty prefix as if a regular iterator" in {
		val lex = Lexicon("a", "aa", "aab", "aaa", "abb")
		assert(lex.iterator.toList === lex.iterator("").toList)
	}
	it should "retrieve the longest match in a string" in {
		assert(Lexicon("a", "aa", "b").lookup("a").get === "a".toSeq)
		assert(Lexicon("a", "aaa", "ab").lookup("aaaaa").get === "aaa".toSeq)
	}
	it should "not retrieve non-existing matches in a string" in {
		assert(Lexicon("a").lookup("") === None)
		assert(Lexicon("a").lookup("b") === None)
		assert(Lexicon("aa", "ab").lookup("a") === None)
		assert(Lexicon("aaa", "ab").lookup("aab") === None)
	}
	it should "retrieve the longest match at an offset in a string" in {
		assert(Lexicon("a", "aa", "aaa").lookup("baab", 1).get === "aa".toSeq)
	}
	it should "not retrieve non-existing matches at an offset in a string" in {
		assert(Lexicon("a", "aa", "ab").lookup("bbb", 1) === None)
	}
	it should "be correctly built even from a non-unique sequence" in {
		assert(Lexicon("a", "a", "a").toList === List("a".toSeq))
	}
	it should "be correctly de/serialized" in {
		import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
		val obj = File.createTempFile("serialization", ".bin")
		obj.deleteOnExit()
		val output = new ObjectOutputStream(new FileOutputStream(obj))
		output.writeObject(Lexicon("a"))
		output.close()
		val input = new ObjectInputStream(new FileInputStream(obj))
		val result = input.readObject.asInstanceOf[Lexicon[Char]]
		input.close()
		assert(result.toList === List("a".toSeq))
	}
}
