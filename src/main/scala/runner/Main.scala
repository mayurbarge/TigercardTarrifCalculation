package runner

import datetime.TravelTime
import domain.CardState
import farecalculation.FareCalculator
import io.{Reader, Writer}
import scalaz._
import scalaz.Scalaz._
import validations.Validator
import validations.Validator.Result

import scala.util.Try

object Main extends App {
  val validatedTravelTimes = {
    for {
      inputLines <- Reader.read("resources/input2.txt")
      input <- inputLines.toList
    } yield {
      TravelTime(input.split(" ").toList)
    }
  }.sequence[Result, TravelTime]


  val totalRides = (travelTimes: List[TravelTime]) => {
    travelTimes.foldLeft(List(List.empty[TravelTime]))((acc, trip) => {
      if (Try {
        acc.head.head.dayWeight
      }.toOption.getOrElse(0) > trip.dayWeight) {
        List(trip) :: acc
      } else {
        (trip :: acc.head) :: acc.tail
      }
    })
  }

  val dailyStates = (totalRides: List[List[TravelTime]]) => {
    for {
      allRidesInAWeek <- totalRides
      dailyRides = allRidesInAWeek.groupBy(_.day).values.toList
    } yield {
      dailyRides.map(CardState.changeDailyState).map(_.run(CardState.monoid.zero)._1)
    }
  }

  val weeklyStates = (dailyStates: List[List[CardState]]) => dailyStates.map(CardState.changeWeeklyState)
  val totalFare = (weeklyStates: List[State[CardState, CardState]]) => weeklyStates.map(_.run(CardState.monoid.zero)._1).map(_.fareMap.values.sum).sum


  val result = for {
    travelTimes <- validatedTravelTimes
  } yield {
    (totalRides andThen dailyStates andThen weeklyStates andThen totalFare)(travelTimes)
  }

  result match {
    case Success(output) => Writer.write(output.toString).unsafePerformIO()
    case Failure(message) => Writer.write(message.toList.mkString("\n")).unsafePerformIO()
  }

}