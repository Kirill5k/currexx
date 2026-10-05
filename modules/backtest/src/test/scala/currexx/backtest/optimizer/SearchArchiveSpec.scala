package currexx.backtest.optimizer

import cats.effect.IO
import cats.syntax.all.*
import currexx.algorithms.Fitness
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation}
import kirill5k.common.cats.test.IOWordSpec

class SearchArchiveSpec extends IOWordSpec {

  private def indicator(period: Int): Indicator =
    Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.SMA(period))

  "SearchArchive" should {

    "retain the strongest distinct candidates under concurrent insertion within its capacity" in {
      val observations = (1 to 30).toVector.flatMap { period =>
        Vector(indicator(period) -> Fitness(period.toDouble), indicator(period) -> Fitness(period.toDouble / 2))
      }
      val result = for
        archive <- SearchArchive.make[IO](3)
        sizes   <- observations.parTraverse { case (candidate, fitness) =>
          archive.record(candidate, fitness) >> archive.candidates.map(_.size)
        }
        retained <- archive.candidates
      yield (sizes, retained)

      result.asserting { case (sizes, retained) =>
        sizes.foreach(_ must be <= 3)
        retained mustBe Vector(30, 29, 28).map(period => indicator(period) -> Fitness(period.toDouble))
      }
    }

    "resolve tied fitness deterministically by canonical indicator text regardless of insertion order" in {
      val candidates = Vector(3, 10, 20, 2).map(indicator)
      val result     = for
        forward <- SearchArchive.make[IO](2)
        reverse <- SearchArchive.make[IO](2)
        _       <- candidates.traverse_(forward.record(_, Fitness(1.0)))
        _       <- candidates.reverse.traverse_(reverse.record(_, Fitness(1.0)))
        first   <- forward.candidates
        second  <- reverse.candidates
      yield (first, second)

      result.asserting { case (first, second) =>
        first mustBe candidates.sortBy(_.toString).take(2).map(_ -> Fitness(1.0))
        second mustBe first
      }
    }

    "keep a candidate's best score without duplicating it and allocate independent archives" in {
      val candidate = indicator(10)
      val result    = for
        first  <- SearchArchive.make[IO](2)
        _      <- List(0.2, 0.8, 0.5, 0.8).traverse_(score => first.record(candidate, Fitness(score)))
        second <- SearchArchive.make[IO](2)
        kept   <- first.candidates
        fresh  <- second.candidates
      yield (kept, fresh)

      result.asserting { case (kept, fresh) =>
        kept mustBe Vector(candidate -> Fitness(0.8))
        fresh mustBe Vector.empty
      }
    }

    "reject weaker and unchanged observations without replacing the retained scores" in {
      val initial = Vector(indicator(10) -> Fitness(0.8), indicator(20) -> Fitness(0.7))
      val result  = for
        archive <- SearchArchive.make[IO](2)
        _       <- initial.traverse_ { case (candidate, fitness) => archive.record(candidate, fitness) }
        _       <- archive.record(indicator(30), Fitness(0.6))
        _       <- archive.record(indicator(10), Fitness(0.6))
        _       <- archive.record(indicator(20), Fitness(0.6))
        _       <- archive.record(indicator(10), Fitness(0.8))
        _       <- archive.record(indicator(20), Fitness(0.7))
        kept    <- archive.candidates
      yield kept

      result.asserting(_ mustBe initial)
    }

    "reorder improved retained candidates and evict only the weakest when a new candidate qualifies" in {
      val result = for
        archive <- SearchArchive.make[IO](2)
        _       <- archive.record(indicator(10), Fitness(0.8))
        _       <- archive.record(indicator(20), Fitness(0.7))
        _       <- archive.record(indicator(20), Fitness(0.9))
        updated <- archive.candidates
        _       <- archive.record(indicator(30), Fitness(0.85))
        _       <- archive.record(indicator(20), Fitness(0.7))
        kept    <- archive.candidates
      yield (updated, kept)

      result.asserting { case (updated, kept) =>
        updated mustBe Vector(indicator(20) -> Fitness(0.9), indicator(10) -> Fitness(0.8))
        kept mustBe Vector(indicator(20) -> Fitness(0.9), indicator(30) -> Fitness(0.85))
      }
    }

    "admit a tied cutoff candidate only when its indicator text precedes the retained cutoff" in {
      val tied   = Vector(10, 20, 30).map(indicator).sortBy(_.toString)
      val best   = indicator(40) -> Fitness(2.0)
      val result = for
        archive  <- SearchArchive.make[IO](2)
        _        <- archive.record(best._1, best._2)
        _        <- archive.record(tied(1), Fitness(1.0))
        _        <- archive.record(tied(2), Fitness(1.0))
        rejected <- archive.candidates
        _        <- archive.record(tied(0), Fitness(1.0))
        admitted <- archive.candidates
      yield (rejected, admitted)

      result.asserting { case (rejected, admitted) =>
        rejected mustBe Vector(best, tied(1) -> Fitness(1.0))
        admitted mustBe Vector(best, tied(0) -> Fitness(1.0))
      }
    }

    "reject nonpositive capacity as a failed effect" in
      List(0, -1).traverse(capacity => SearchArchive.make[IO](capacity).attempt).asserting { results =>
        results.foreach {
          case Left(error: IllegalArgumentException) => error.getMessage mustBe "Search archive capacity must be positive"
          case other                                 => fail(s"Expected invalid capacity failure, got $other")
        }
        succeed
      }
  }
}
