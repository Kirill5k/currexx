package currexx.core.market

import currexx.domain.user.UserId

final case class MarketSettings (
    userId: UserId,
    broker: BrokerParameters
                                )
