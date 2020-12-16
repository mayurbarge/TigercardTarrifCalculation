package domain.model

import datetime.TravelTime
import domain.services.FareCalculationService
import scalaz.Scalaz._
import scalaz.{Monoid, State}
import sun.jvm.hotspot.debugger.cdbg.basic.BasicCDebugInfoDataBase

case class CardState(countMap: Map[TravelZones,  Int], fareMap: Map[TravelZones,  BigDecimal])


object CardState {
  def changeDailyState(rides: List[TravelTime]): State[CardState, CardState] = {
    for {
      i <- State.init[CardState]
      _ <- State.modify[CardState](oldCardState => {
        val cap = rides.map(_.travelZones.zoneRates.dailyCap).max
        rides.foldLeft(oldCardState)((acc, ride) => {
          val zone = ride.travelZones
          val fare = FareCalculationService.calculateDailyFare(ride)
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
        val cap = cardStates.map(_.countMap.keySet.map(_.zoneRates.weeklyCap).max).max

        val cappedFares = withoutCaps.fareMap.foldLeft(Map.empty[TravelZones, BigDecimal])((acc, e)=>{
         val (zone, fare) = e
         if(acc.values.sum + fare < cap) {
           acc + (zone -> fare)
         } else acc + (zone -> (cap-acc.values.sum))
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