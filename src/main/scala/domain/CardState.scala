package domain

import datetime.TravelTime
import scalaz.Scalaz._
import scalaz.syntax._
import scalaz.{Monoid, State}

case class CardState(countMap: Map[TravelZones,  Int], fareMap: Map[TravelZones,  BigDecimal])
object CardState {
  def changeDailyState(fares: List[Double], rides: List[TravelTime]): State[CardState, CardState] = {
    for {
      i <- State.init[CardState]
      _ <- State.modify[CardState](oldCardState => {
        (fares zip rides).foldLeft(oldCardState)((acc, pair) => {
          val (fare, ride) = pair
          //println(fare + " " + ride.travelZones.fromZone+ " "+  ride.travelZones.toZone +  acc.countMap)
          //println(fare + " " + ride.travelZones.fromZone+ " "+  ride.travelZones.toZone +  acc.fareMap)
          //println("------------------------")
          val zone = ride.travelZones
          val cap = ride.travelZones.zoneRates.dailyCap
          if ( (acc.countMap.getOrElse(zone, 0) >= 3) && (acc.fareMap.getOrElse(zone, BigDecimal(0)) + fare) < cap)
            acc |+| CardState(Map(zone -> 1), Map(zone -> (cap - (acc.fareMap.getOrElse(zone, BigDecimal(0)) + fare))))
          else
            acc |+| CardState(Map(zone -> 1), Map(zone ->  BigDecimal(fare)))
        })
      })
      modified <- State.get[CardState]
    } yield modified
  }

  def changeWeeklyState(cardStates: List[CardState]): State[CardState, CardState] = {
    for {
      i <- State.init[CardState]
      _ <- State.modify[CardState](oldCardState => oldCardState |+| cardStates.reduce(_|+|_))
      modified <- State.get[CardState]
    } yield modified
  }

  implicit val monoid: Monoid[CardState] = Monoid.instance(
    (cardState1, cardState2) =>
      CardState(cardState1.countMap |+| cardState2.countMap, cardState1.fareMap |+| cardState2.fareMap),
    CardState(Map.empty[TravelZones, Int], Map.empty[TravelZones, BigDecimal])
  )
}