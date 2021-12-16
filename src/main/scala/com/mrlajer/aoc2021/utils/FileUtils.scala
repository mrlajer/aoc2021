package com.mrlajer.aoc2021.utils

import java.io.{BufferedReader, FileReader, InputStreamReader}
import scala.util.Using

object FileUtils {

  def ReadFile(path: String): Seq[String] =
    Using.resource(new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(s"/$path")))) { reader =>
      Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
    }
}
