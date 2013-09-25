package fnl.collection

import java.io.File
import java.io.FileInputStream
import java.io.ObjectInputStream
import scala.io.Source

object LexiconTaggerApp extends App {
	if (args.size > 1 || args(0).startsWith("-")) println("usage: LexiconTaggerApp LEXICON < INFILE")
	else try {
		val file = new File(args(0))
		if (file.canRead) {
			val in = new ObjectInputStream(new FileInputStream(file))
			val lexicon = in.readObject.asInstanceOf[Lexicon[Char]]
			in.close()

			def grep(line: String): Iterator[(Int, Int)] = {
				for (i <- Range(0, line.length).iterator; j = lexicon.indexOf(line, i); if j.isDefined)
				yield (i, j.get)
			}

			for (line <- Source.stdin.getLines(); (start, end) <- grep(line))
				println(line.substring(start, end))
		} else {
			System.err.println("cannot read lexicon file '" + args(0) + "'")
		}
	} catch {
		case e: Exception => System.err.println(e.getMessage)
	}

}