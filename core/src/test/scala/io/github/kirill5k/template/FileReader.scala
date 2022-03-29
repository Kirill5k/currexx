package io.github.kirill5k.template

import scala.io.Source

object FileReader:

  def fromResources(path: String): String =
    val source = Source.fromResource(path)
    try source.getLines().toList.mkString
    finally source.close()
