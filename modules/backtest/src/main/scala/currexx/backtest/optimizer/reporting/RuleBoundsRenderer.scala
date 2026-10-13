package currexx.backtest.optimizer.reporting

import currexx.backtest.optimizer.strategy.{RuleParameterBounds, RuleParameterPath, RulePath, RuleTarget}
import io.circe.Json
import io.circe.syntax.*

/** Save exact configured bounds in a stable order so runs can be compared. */
object RuleBoundsRenderer:
  def json(bounds: RuleParameterBounds): Json = Json.obj(
    "numeric" -> bounds.numeric.toList
      .sortBy(_._1.toString)
      .map { case (target, range) =>
        Json.obj("target" -> targetJson(target), "range" -> numericRange(range))
      }
      .asJson,
    "durations" -> bounds.durations.toList
      .sortBy(_._1.toString)
      .map { case (target, range) =>
        Json.obj(
          "target" -> targetJson(target),
          "min"    -> range.min.toString.asJson,
          "max"    -> range.max.toString.asJson,
          "step"   -> range.step.toString.asJson
        )
      }
      .asJson
  )

  private def targetJson(target: RuleTarget): Json = target match
    case RuleTarget.AtrStop            => Json.obj("kind" -> "AtrStop".asJson)
    case RuleTarget.TrendAge           => Json.obj("kind" -> "TrendAge".asJson)
    case RuleTarget.HoldingTime        => Json.obj("kind" -> "HoldingTime".asJson)
    case RuleTarget.At(path, template) =>
      Json.obj("kind" -> "At".asJson, "path" -> parameterPath(path), "template" -> template.asJson)

  private def numericRange(range: RuleParameterBounds.DoubleRange): Json = Json.obj(
    "min"  -> range.min.asJson,
    "max"  -> range.max.asJson,
    "step" -> range.step.asJson
  )

  private def parameterPath(path: RuleParameterPath): Json =
    rulePath(path.rule).deepMerge(Json.obj("parameter" -> path.parameter.toString.asJson))

  private def rulePath(path: RulePath): Json = Json.obj(
    "ruleList"   -> path.list.toString.asJson,
    "ruleIndex"  -> path.ruleIndex.asJson,
    "conditions" -> path.conditions.asJson
  )
