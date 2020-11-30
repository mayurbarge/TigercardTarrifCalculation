package domain

import datetime.TravelTime
import scalaz.Scalaz._
import scalaz.State

trait ZoneID
case object ZoneI extends ZoneID
case object ZoneII extends ZoneID
case object ZoneIII extends ZoneID

object ZoneID {
  def apply(zoneID: Int): ZoneID = {
    zoneID match {
      case 1 => ZoneI
      case 2 => ZoneII
      case 3 => ZoneIII
    }
  }
}

object FareCalculator {
  def calculateDailyFare(travelTime: TravelTime) = {
    val zoneRates = travelTime.travelZones.zoneRates
    travelTime.isPeakHour match {
      case true => zoneRates.peakHour
      case false=> zoneRates.offPeakHour
    }
  }
}