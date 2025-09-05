package currexx.clients.broker.ig

import currexx.clients.Fs2HttpClient

private[clients] trait IgClient[F[_]] extends Fs2HttpClient[F] {}
