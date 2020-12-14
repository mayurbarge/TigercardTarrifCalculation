package domain.services

import datetime.TravelTime
import domain.model.{CardState, WeeklyTravels}
import scalaz.State

object CardStateCalculationService {
  val dailyStates = (allTravels: List[WeeklyTravels]) => {
    for {
      weeklyTravels <- allTravels
      dailyRides = weeklyTravels.travels.groupBy(_.day).values.toList
    } yield {
      dailyRides.map(CardState.changeDailyState).map(_.run(CardState.monoid.zero)._1)
    }
  }

  val weeklyStates = (dailyStates: List[List[CardState]]) => dailyStates.map(CardState.changeWeeklyState)
  val totalFare = (weeklyStates: List[State[CardState, CardState]]) => weeklyStates.map(_.run(CardState.monoid.zero)._1).map(_.fareMap.values.sum).sum

  val run = (allTravels: List[TravelTime]) => (WeeklyTravels.splitByWeek andThen CardStateCalculationService.dailyStates andThen CardStateCalculationService.weeklyStates andThen CardStateCalculationService.totalFare)(allTravels)

}
