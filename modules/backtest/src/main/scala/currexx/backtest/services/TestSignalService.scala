package currexx.backtest.services

import cats.Monad
import currexx.core.common.http.SearchParams
import currexx.core.signal.db.SignalRepository
import currexx.core.signal.Signal
import currexx.domain.user.UserId

final private class TestSignalRepository[F[_]](using F: Monad[F]) extends SignalRepository[F]:
  override def saveAll(signals: List[Signal]): F[Unit]                   = F.unit
  override def getAll(userId: UserId, sp: SearchParams): F[List[Signal]] = F.pure(Nil)
