package io

import scalaz.effect.IO

import scala.io.Source
import IO._
import scalaz.Scalaz

object Reader extends App{
  def read(filepath: String) = {
    val io = IO {
      val source = Source.fromFile(filepath)
      source.getLines.toStream
    }
    for {
      inputLine <- io.unsafePerformIO.toList
    } yield {
      inputLine.split(",")
    }
  }
}

object Writer {
  def write(data: String) = {
    for {
      _ <- putStrLn(data)
    } yield ()
  }
}

