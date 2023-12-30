package currexx.domain.monitor

import cats.syntax.either.*
import com.cronutils.model.{Cron as JCron, CronType}
import com.cronutils.model.definition.CronDefinitionBuilder
import com.cronutils.model.time.ExecutionTime
import com.cronutils.parser.CronParser
import io.circe.{CursorOp, Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import currexx.domain.json.given

import java.time.{Instant, ZoneOffset}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

sealed trait Schedule(val kind: String):
  def nextExecutionTime(lastExecutionTime: Instant): Instant

object Schedule {
  final case class Periodic(period: FiniteDuration) extends Schedule("periodic"):
    override def nextExecutionTime(lastExecutionTime: Instant): Instant =
      lastExecutionTime.plusSeconds(period.toSeconds)

  final case class Cron(cron: JCron) extends Schedule("cron"):
    override def nextExecutionTime(lastExecutionTime: Instant): Instant =
      ExecutionTime.forCron(cron).nextExecution(lastExecutionTime.atZone(ZoneOffset.UTC)).orElseThrow().toInstant

  object Cron {
    private val definition = CronDefinitionBuilder.instanceDefinitionFor(CronType.UNIX)
    private val parser     = new CronParser(definition)

    def apply(expression: String): Either[Throwable, Cron] =
      Try(parser.parse(expression)).map(Cron.apply).toEither

    def unsafe(expression: String): Cron =
      Cron(parser.parse(expression))
  }

  private val discriminatorField: String = "kind"

  inline given Decoder[Schedule] = Decoder.instance { c =>
    c.downField(discriminatorField).as[String].flatMap {
      case "cron" =>
        c.downField("cron")
          .as[String]
          .flatMap(Cron.apply)
          .leftMap(e => DecodingFailure(e.getMessage, List(CursorOp.Field("cron"))))
      case "periodic" => c.downField("period").as[FiniteDuration].map(Periodic.apply)
      case kind       => Left(DecodingFailure(s"Unexpected schedule kind $kind", List(CursorOp.Field(discriminatorField))))
    }
  }

  inline given Encoder[Schedule] = Encoder.instance {
    case cron: Cron         => Json.obj(discriminatorField -> Json.fromString(cron.kind), "cron" -> Json.fromString(cron.cron.asString()))
    case periodic: Periodic => Json.obj(discriminatorField -> Json.fromString(periodic.kind), "period" -> periodic.period.asJson)
  }
}
