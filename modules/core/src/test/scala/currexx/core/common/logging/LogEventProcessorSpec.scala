package currexx.core.common.logging

import cats.effect.{IO, Ref}
import currexx.clients.messenger.MessengerClient
import currexx.core.common.logging.db.LogEventRepository
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant

class LogEventProcessorSpec extends IOWordSpec {

  private val time = Instant.parse("2026-09-30T12:00:00Z")

  "A LogEventProcessor" should {
    "save accepted events before sending their notifications in order" in {
      val repository           = mock[LogEventRepository[IO]]
      val messenger            = mock[MessengerClient[IO]]
      given logger: Logger[IO] = mock[Logger[IO]]
      val events               = LogLevel.values.toList.map(level => LogEvent(level, time, s"$level message"))
      when(logger.events).thenReturn(Stream.emits(events))

      val accepted = events.filterNot(_.level == LogLevel.Info)
      val result   = for
        operations <- Ref.of[IO, List[String]](Nil)
        _          <- IO {
          accepted.foreach { event =>
            when(repository.save(event)).thenReturn(operations.update(_ :+ s"save:${event.level}"))
            when(messenger.send(s"Currexx [${event.level.toString.toUpperCase}]", event.message))
              .thenReturn(operations.update(_ :+ s"send:${event.level}"))
          }
        }
        _      <- LiveLogEventProcessor[IO](repository, messenger).run.compile.drain
        actual <- operations.get
      yield actual

      result.asserting { operations =>
        accepted.foreach { event =>
          verify(repository).save(event)
          verify(messenger).send(s"Currexx [${event.level.toString.toUpperCase}]", event.message)
        }
        verify(logger).events
        verifyNoMoreInteractions(repository, messenger, logger)
        operations mustBe accepted.flatMap(event => List(s"save:${event.level}", s"send:${event.level}"))
      }
    }

    "ignore Info events" in {
      val repository           = mock[LogEventRepository[IO]]
      val messenger            = mock[MessengerClient[IO]]
      given logger: Logger[IO] = mock[Logger[IO]]
      when(logger.events).thenReturn(Stream.emit(LogEvent(LogLevel.Info, time, "info message")))

      LiveLogEventProcessor[IO](repository, messenger).run.compile.drain.asserting { result =>
        verifyNoInteractions(repository, messenger)
        result mustBe ()
      }
    }

    "continue saving and sending after a notification failure without logging into the event queue" in {
      val repository           = mock[LogEventRepository[IO]]
      val messenger            = mock[MessengerClient[IO]]
      given logger: Logger[IO] = mock[Logger[IO]]
      val first                = LogEvent(LogLevel.Error, time, "first error")
      val second               = LogEvent(LogLevel.Warn, time, "next warning")
      when(logger.events).thenReturn(Stream.emits(List(first, second)))
      when(repository.save(first)).thenReturn(IO.unit)
      when(repository.save(second)).thenReturn(IO.unit)
      when(messenger.send("Currexx [ERROR]", first.message)).thenReturn(IO.raiseError(new RuntimeException("messenger unavailable")))
      when(messenger.send("Currexx [WARN]", second.message)).thenReturn(IO.unit)

      LiveLogEventProcessor[IO](repository, messenger).run.compile.drain.asserting { result =>
        verify(repository).save(first)
        verify(repository).save(second)
        verify(messenger).send("Currexx [ERROR]", first.message)
        verify(messenger).send("Currexx [WARN]", second.message)
        verify(logger).events
        verifyNoMoreInteractions(repository, messenger, logger)
        result mustBe ()
      }
    }

    "propagate database failures without sending the unsaved event or processing subsequent events" in {
      val repository           = mock[LogEventRepository[IO]]
      val messenger            = mock[MessengerClient[IO]]
      given logger: Logger[IO] = mock[Logger[IO]]
      val first                = LogEvent(LogLevel.Error, time, "unsaved error")
      val second               = LogEvent(LogLevel.Warn, time, "next warning")
      val failure              = new RuntimeException("database unavailable")
      when(logger.events).thenReturn(Stream.emits(List(first, second)))
      when(repository.save(first)).thenReturn(IO.raiseError(failure))

      LiveLogEventProcessor[IO](repository, messenger).run.compile.drain.attempt.asserting { result =>
        verify(repository).save(first)
        verify(logger).events
        verifyNoMoreInteractions(repository, logger)
        verifyNoInteractions(messenger)
        result mustBe Left(failure)
      }
    }
  }
}
