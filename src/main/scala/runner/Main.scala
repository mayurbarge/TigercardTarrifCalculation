package runner

import java.time.DayOfWeek

import datetime.TravelTime
import domain.CardState
import farecalculation.FareCalculator
import io.Reader
import runner.Main.weeklyTravels
import scalaz.State

import scala.util.Try

object Main extends App {
  val travelTimes = readTravels
  val weeklyTravels = splitByWeeks
  val dailyFares = calculateDailyFares
  val dailyStates = buildDailyStates
  val weeklyStates = buildWeeklyStates
  val result = CardState.changeWeeklyState(weeklyStates)

  println(result.run(CardState.monoid.zero)._1.fareMap.values.sum)

  private def readTravels = {
    for {
      inputLines <- Reader.read("resources/input2.txt")
      input <- inputLines.toList
    } yield {
      TravelTime(input.split(" ").toList).get
    }
  }

  private def splitByWeeks = {
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

  private def calculateDailyFares = {
    for {
      trip <- travelTimes
    } yield FareCalculator.calculateDailyFare(trip)
  }

  private def buildDailyStates = {
    for {
      weeklyTravels <- weeklyTravels
    } yield {
      val fares =
        for {
          trip <- weeklyTravels
        } yield {
          FareCalculator.calculateDailyFare(trip)
        }
      CardState.changeDailyState(fares, weeklyTravels)
    }
  }

  private def buildWeeklyStates = {
    for {
      dailyState <- dailyStates
    } yield {
      val (state, _) = dailyState.run(CardState.monoid.zero)
      state
    }
  }

}