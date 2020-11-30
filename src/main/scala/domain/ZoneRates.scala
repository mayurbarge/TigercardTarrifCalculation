package domain

trait ZoneRates {
  val peakHour: Double
  val offPeakHour: Double
  val dailyCap: Double
  val weeklyCap: Double
}
case object ZoneIToZoneI extends ZoneRates {
  val peakHour: Double = 30
  val offPeakHour: Double = 25
  val dailyCap: Double = 100
  val weeklyCap: Double = 500
}
case object ZoneIIToZoneI extends ZoneRates {
  val peakHour: Double = 35
  val offPeakHour: Double = 30
  val dailyCap: Double = 120
  val weeklyCap: Double = 600
}
case object ZoneIToZoneII extends ZoneRates {
  val peakHour: Double = 35
  val offPeakHour: Double = 30
  val dailyCap: Double = 120
  val weeklyCap: Double = 600
}
case object ZoneIIToZoneII extends ZoneRates {
  val peakHour: Double = 25
  val offPeakHour: Double = 20
  val dailyCap: Double = 80
  val weeklyCap: Double = 400
}
object ZoneRates {
  def apply(from: ZoneID, to: ZoneID): ZoneRates = {
    (from, to) match {
      case (ZoneI, ZoneI) => ZoneIToZoneI
      case (ZoneII, ZoneI) => ZoneIIToZoneI
      case (ZoneI, ZoneII) => ZoneIToZoneII
      case (ZoneII, ZoneII) => ZoneIIToZoneII
    }
  }
}

case class TravelZones(fromZone: ZoneID, toZone: ZoneID, zoneRates: ZoneRates)
object TravelZones {
  def apply(fromZone: ZoneID, toZone: ZoneID): TravelZones = new TravelZones(fromZone, toZone, ZoneRates(fromZone, toZone))
}
