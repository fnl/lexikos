package fnl.collection

import java.io.ObjectInputStream

object LexiconPrinterApp extends App {
  if (args.size > 0) println("usage: LexiconPrinterApp < LEXICON > DOTFILE")
  else try {
    val in = new ObjectInputStream(System.in)
    println(in.readObject.asInstanceOf[Lexicon[Char]].dot()) 
  } catch {
    case e: Exception => System.err.println(e.getMessage)
  }
}