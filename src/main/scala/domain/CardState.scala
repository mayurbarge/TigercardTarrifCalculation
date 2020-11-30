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
          /*println(fare + " " + ride.travelZones.fromZone+ " "+  ride.travelZones.toZone +  acc.countMap)
          println(fare + " " + ride.travelZones.fromZone+ " "+  ride.travelZones.toZone +  acc.fareMap)
          println("------------------------")*/
          val zone = ride.travelZones
          val cap = ride.travelZones.zoneRates.dailyCap
          /*println(cap)
          println((cap - (acc.fareMap.getOrElse(zone, BigDecimal(0)) + fare)))
          println("%%%%%%%%%%%%%%%%%")*/
          if ( (acc.countMap.getOrElse(zone, 0) == 2) && (acc.fareMap.getOrElse(zone, BigDecimal(0)) + fare) < cap)
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