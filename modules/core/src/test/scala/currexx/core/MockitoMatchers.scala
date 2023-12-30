package currexx.core

import cats.effect.IO
import org.mockito.ArgumentMatchers
import org.mockito.Mockito
import org.mockito.stubbing.Answer
import org.mockito.stubbing.OngoingStubbing
import org.mockito.stubbing.Stubber
import org.mockito.verification.VerificationMode
import org.scalatestplus.mockito.MockitoSugar

trait MockitoMatchers extends MockitoSugar:
  def any[A]: A                                      = ArgumentMatchers.any[A]()
  def anyList[A]: List[A]                            = ArgumentMatchers.any[List[A]]()
  def anyOpt[A]: Option[A]                           = ArgumentMatchers.any[Option[A]]()
  def eqTo[A](value: A): A                           = ArgumentMatchers.eq[A](value)
  def doAnswer[A](answer: Answer[A]): Stubber        = Mockito.doAnswer(answer)
  def doThrow[A](error: Throwable): Stubber          = Mockito.doThrow(error)
  def when[A](mock: A): OngoingStubbing[A]           = Mockito.when(mock)
  def verify[A](mock: A, mode: VerificationMode): A  = Mockito.verify(mock, mode)
  def verify[A](mock: A): A                          = verify(mock, Mockito.times(1))
  def verifyNoInteractions(mocks: AnyRef*): Unit     = Mockito.verifyNoInteractions(mocks*)
  def verifyNoMoreInteractions(mocks: AnyRef*): Unit = Mockito.verifyNoMoreInteractions(mocks*)
  def never: VerificationMode                        = Mockito.never()

  extension [A](stub: OngoingStubbing[IO[A]])
    def thenReturnIO(value: A): OngoingStubbing[IO[A]]            = stub.thenReturn(IO.pure(value))
    def thenReturnError(error: Throwable): OngoingStubbing[IO[A]] = stub.thenReturn(IO.raiseError(error))

  extension (stub: OngoingStubbing[IO[Unit]]) def thenReturnUnit: OngoingStubbing[IO[Unit]] = stub.thenReturn(IO.unit)

  extension [A](stub: OngoingStubbing[IO[Option[A]]])
    def thenReturnNone: OngoingStubbing[IO[Option[A]]]           = stub.thenReturn(IO.none[A])
    def thenReturnSome(value: A): OngoingStubbing[IO[Option[A]]] = stub.thenReturn(IO.some(value))
