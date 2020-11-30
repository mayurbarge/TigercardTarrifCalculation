package runner

import java.time.DayOfWeek
import datetime.TravelTime
import domain.{CardState, FareCalculator}
import io.Reader
import scalaz.State

import scala.util.Try

object Main extends App {
  val travelTimes =
  for {
    inputLines <- Reader.read("resources/input.txt")
    input <- inputLines.toList
  } yield {
    TravelTime(input.split(" ").toList).get
  }

  val splitted = travelTimes.foldLeft(List(List.empty[TravelTime]))((acc, trip) => {
    if(Try{acc.head.head.dayWeight}.toOption.getOrElse(0) > trip.dayWeight) {
      List(trip) :: acc
    } else {
      (trip::acc.head)::acc.tail
    }
  })

  val dailyFares = for {
        trip <- travelTimes
      } yield FareCalculator.calculateDailyFare(trip)
  val z: State[CardState, CardState] = CardState.changeDailyState(dailyFares, travelTimes)

  println(z.run(CardState.monoid.zero)._1)

  val dailyStates =
  for {
    weeklyTravels <- splitted
  } yield {
    val fares =
    for {
      trip <- weeklyTravels
    } yield {
      FareCalculator.calculateDailyFare(trip)
    }
    CardState.changeDailyState(fares, weeklyTravels)
  }

  val zz =
  for {
    dailyState <- dailyStates
  } yield {
    val (state, _) = dailyState.run(CardState.monoid.zero)
    state
  }

  CardState.changeWeeklyState(zz)

}