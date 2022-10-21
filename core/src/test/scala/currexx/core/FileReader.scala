package currexx.core

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import currexx.domain.market.PriceRange
import currexx.core.common.time.*
import io.circe.{Decoder, JsonObject}
import io.circe.parser.decode

import java.time.LocalDate
import scala.collection.immutable.ListMap
import scala.io.Source

object FileReader {
  def fromResources(path: String): String =
    val source = Source.fromResource(path)
    try source.getLines().toList.mkString
    finally source.close()

  def parseFromResources[A: Decoder](path: String): A =
    decode[A](fromResources(path)).fold(e => throw new RuntimeException(e.getMessage), identity)

  def pricesFromResources(path: String): NonEmptyList[PriceRange] = {
    val json = FileReader.parseFromResources[JsonObject](path)
    for
      prices    <- json("Time Series FX (Daily)").toRight(new RuntimeException("missing prices"))
      priceList <- prices.as[ListMap[String, JsonObject]]
      priceRange <- priceList.toList.traverse { (date, ohlc) =>
        for
          open  <- ohlc("1. open").flatMap(_.asString).map(_.toDouble).toRight(new RuntimeException("missing open"))
          high  <- ohlc("2. high").flatMap(_.asString).map(_.toDouble).toRight(new RuntimeException("missing high"))
          low   <- ohlc("3. low").flatMap(_.asString).map(_.toDouble).toRight(new RuntimeException("missing low"))
          close <- ohlc("4. close").flatMap(_.asString).map(_.toDouble).toRight(new RuntimeException("missing close"))
        yield PriceRange(open, high, low, close, 0D, LocalDate.parse(date).toInstantAtStartOfDay)
      }
      priceValues <- NonEmptyList.fromList(priceRange).toRight(new RuntimeException("empty price range list"))
    yield priceValues
  }.fold(e => throw e, identity)
}
