package fnl.collection

/* lexikos/fnl.collection [2013-08-28T15:44]
 * (C) Florian Leitner 2013. All rights reserved. */

import org.scalameter.api._
import java.util.UUID

object LexiconPerf extends PerformanceTest {
	lazy val executor = SeparateJvmsExecutor(
		new Executor.Warmer.Default,
		Aggregator.min,
		new Measurer.Default
	)
	lazy val reporter = new LoggingReporter
	lazy val persistor = Persistor.None

	val sizes = Gen.exponential("Lexicon.size")(10, 1000, 10)
	val words = for {
		size <- sizes
	} yield Array.fill[Seq[Char]](size) {UUID.randomUUID.toString.toSeq :+ 'a'}
			.sorted(Lexicon.ordering[Char])

	performance of "Lexicon" in {
		measure method "fromIterator" in {
			using(words) in {
				w => Lexicon.fromIterator(w.iterator)
			}
		}
	}
}
