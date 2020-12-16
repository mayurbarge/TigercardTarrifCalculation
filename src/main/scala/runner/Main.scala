package runner

import datetime.TravelTime
import domain.services.CardStateCalculationService
import io.{Reader, Writer}
import scalaz.Scalaz._
import scalaz._
import validations.Validator.Result

object Main extends App {
  val validatedTravelTimes = {
    for {
      inputLines <- Reader.read("resources/input3.txt")
      input <- inputLines.toList
    } yield {
      TravelTime(input.split(" ").toList)
    }
  }.sequence[Result, TravelTime]

  val result = for {
    travelTimes <- validatedTravelTimes
  } yield {
    CardStateCalculationService.run(travelTimes)
  }

  result match {
    case Success(output) => Writer.write(output.toString).unsafePerformIO()
    case Failure(message) => Writer.write(message.toList.mkString("\n")).unsafePerformIO()
  }

}