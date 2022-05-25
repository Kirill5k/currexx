package currexx.core

import io.circe.Decoder
import io.circe.parser.decode
import scala.io.Source

object FileReader {
  def fromResources(path: String): String =
    val source = Source.fromResource(path)
    try source.getLines().toList.mkString
    finally source.close()

  def parseFromResources[A: Decoder](path: String): A =
    decode[A](fromResources(path)).fold(e => throw new RuntimeException(e.getMessage), identity)

}
