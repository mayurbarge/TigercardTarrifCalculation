package runner

import datetime.TravelTime
import domain.CardState
import farecalculation.FareCalculator
import io.Reader
import scalaz.State

import scala.util.Try

object Main extends App {
  val travelTimes = {
    for {
      inputLines <- Reader.read("resources/input2.txt")
      input <- inputLines.toList
    } yield {
      TravelTime(input.split(" ").toList).get
    }
  }

  val totalRides: List[List[TravelTime]] = {
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

  val dailyStates = {
    for {
      allRidesInAWeek <- totalRides
      dailyRides = allRidesInAWeek.groupBy(_.day).values.toList
    } yield {
      dailyRides.map(CardState.changeDailyState).map(_.run(CardState.monoid.zero)._1)
    }
  }

  val weeklyStates = dailyStates.map(CardState.changeWeeklyState)
  val result = weeklyStates.map(_.run(CardState.monoid.zero)._1).map(_.fareMap.values.sum).sum
  println(result)

}