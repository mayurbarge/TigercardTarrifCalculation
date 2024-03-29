package domain.services

import datetime.TravelTime

object FareCalculationService {
  def calculateDailyFare(travelTime: TravelTime) = {
    val zoneRates = travelTime.travelZones.zoneRates
    travelTime.isPeakHour match {
      case true => zoneRates.peakHour
      case false=> zoneRates.offPeakHour
    }
  }
}
