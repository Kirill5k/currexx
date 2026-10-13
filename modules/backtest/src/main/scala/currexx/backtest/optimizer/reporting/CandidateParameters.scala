package currexx.backtest.optimizer.reporting

import currexx.backtest.TestStrategy
import currexx.domain.json.given
import io.circe.Json
import io.circe.syntax.*

import scala.concurrent.duration.FiniteDuration

/** Report changed leaf values without coupling the renderer to condition or indicator types. */
object CandidateParameters:
  def changes(base: TestStrategy, candidate: TestStrategy): List[String] =
    val before = leaves(base.asJson, "")
    val after  = leaves(candidate.asJson, "")
    (before.keySet ++ after.keySet).toList.sorted.flatMap { path =>
      val previous     = before.get(path)
      val current      = after.get(path)
      val sameDuration = for
        previousDuration <- previous.flatMap(_.as[FiniteDuration].toOption)
        currentDuration  <- current.flatMap(_.as[FiniteDuration].toOption)
      yield previousDuration == currentDuration
      Option.when(previous != current && !sameDuration.contains(true))(
        s"$path: ${previous.fold("absent")(_.noSpaces)} -> ${current.fold("absent")(_.noSpaces)}"
      )
    }

  private def leaves(value: Json, path: String): Map[String, Json] =
    value.asObject match
      case Some(fields) =>
        fields.toList.flatMap { case (name, child) =>
          leaves(child, if (path.isEmpty) name else s"$path.$name")
        }.toMap
      case None =>
        value.asArray match
          case Some(children) => children.zipWithIndex.flatMap { case (child, index) => leaves(child, s"$path[$index]") }.toMap
          case None           => Map(path -> value)
