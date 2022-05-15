package currexx.domain.signal

import com.cronutils.model.{CronType, Cron as JCron}
import com.cronutils.model.definition.CronDefinitionBuilder
import com.cronutils.model.time.ExecutionTime
import com.cronutils.parser.CronParser

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
  }
}
