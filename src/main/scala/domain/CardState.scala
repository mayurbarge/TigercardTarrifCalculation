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
        val cap = rides.map(_.travelZones.zoneRates.dailyCap).max
        (fares zip rides).foldLeft(oldCardState)((acc, pair) => {
          val (fare, ride) = pair
          val zone = ride.travelZones
          if (acc.fareMap.values.sum + fare < cap)
            acc |+| CardState(Map(zone -> 1), Map(zone ->  BigDecimal(fare)))
          else
            acc |+| CardState(Map(zone -> 1), Map(zone -> (cap - acc.fareMap.values.sum)))

        })
      })
      modified <- State.get[CardState]
    } yield modified
  }

  def changeWeeklyState(cardStates: List[CardState]): State[CardState, CardState] = {
    for {
      i <- State.init[CardState]
      _ <- State.modify[CardState](oldCardState => {
        val withoutCaps: CardState = oldCardState |+| cardStates.reduce(_|+|_)

        val cappedFares = withoutCaps.fareMap.foldLeft(Map.empty[TravelZones, BigDecimal])((acc, e)=>{
         val (zone, fare) = e
         if(zone.zoneRates.weeklyCap < fare) {
           acc + (zone -> zone.zoneRates.weeklyCap)
         } else acc + (zone -> fare)
       })
        withoutCaps.copy(fareMap = cappedFares)
      })
      modified <- State.get[CardState]
    } yield modified
  }

  implicit val monoid: Monoid[CardState] = Monoid.instance(
    (cardState1, cardState2) =>
      CardState(cardState1.countMap |+| cardState2.countMap, cardState1.fareMap |+| cardState2.fareMap),
    CardState(Map.empty[TravelZones, Int], Map.empty[TravelZones, BigDecimal])
  )
}