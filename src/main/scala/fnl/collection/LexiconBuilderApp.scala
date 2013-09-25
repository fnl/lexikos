package fnl.collection

import scala.io.Source
import java.io.ObjectOutputStream

/* lexikos/fnl.collection [2013-09-23T14:52]
 * (C) Florian Leitner 2013. All rights reserved. */

object LexiconBuilderApp extends App {
  if (args.size > 0) println("usage: LexiconBuilderApp < WORDS > LEXICON")
  else try {
    val out = new ObjectOutputStream(System.out)
    out.writeObject(Lexicon.fromIterator(
      Source.stdin.getLines().map(_.trim).filter(!_.isEmpty).map(_.toList)))
  } catch {
    case e: Exception => System.err.println(e.getMessage)
  }
}
