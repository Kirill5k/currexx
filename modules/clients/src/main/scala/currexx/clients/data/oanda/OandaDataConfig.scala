package currexx.clients.data.oanda

final case class OandaDataConfig(
    baseUri: String,
    apiKey: String,
    fetchCandleCount: Int = 150,
    signalCandleCount: Int = 100
):
  require(signalCandleCount > 0, "signalCandleCount must be positive")
  require(fetchCandleCount > signalCandleCount, "fetchCandleCount must exceed signalCandleCount to allow for an incomplete candle")
