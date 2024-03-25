package currexx.core

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import currexx.domain.market.PriceRange
import kirill5k.common.syntax.time.*
import kirill5k.common.test.FileReader.fromResources
import io.circe.{Decoder, JsonObject}
import io.circe.parser.decode

import java.time.LocalDate
import scala.collection.immutable.ListMap

object FileReader {

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
        yield PriceRange(open, high, low, close, 0d, LocalDate.parse(date).toInstantAtStartOfDay)
      }
      priceValues <- NonEmptyList.fromList(priceRange).toRight(new RuntimeException("empty price range list"))
    yield priceValues
  }.fold(e => throw e, identity)
}
