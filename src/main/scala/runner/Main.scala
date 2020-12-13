package runner

import datetime.TravelTime
import domain.{CardStateCalculationService, WeeklyTravels}
import io.{Reader, Writer}
import scalaz.Scalaz._
import scalaz._
import validations.Validator.Result

object Main extends App {
  val validatedTravelTimes = {
    for {
      inputLines <- Reader.read("resources/input2.txt")
      input <- inputLines.toList
    } yield {
      TravelTime(input.split(" ").toList)
    }
  }.sequence[Result, TravelTime]

  val result = for {
    travelTimes <- validatedTravelTimes
  } yield {
    (WeeklyTravels.splitByWeek andThen CardStateCalculationService.dailyStates andThen CardStateCalculationService.weeklyStates andThen CardStateCalculationService.totalFare)(travelTimes)
  }

  result match {
    case Success(output) => Writer.write(output.toString).unsafePerformIO()
    case Failure(message) => Writer.write(message.toList.mkString("\n")).unsafePerformIO()
  }

}