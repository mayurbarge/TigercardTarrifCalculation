package datetime


import java.time.{DayOfWeek, LocalTime}

import domain.{TravelZones, ZoneI, ZoneID, ZoneRates}
case class TravelTime(day: DayOfWeek, time: LocalTime, travelZones: TravelZones) {
  import TravelTime._
  import travelZones._

  val isWeekend = day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY
  val isWeekday = !isWeekend

  def isPeakHour() = {
    val skipOffHours = !((fromZone != ZoneI) && (toZone == ZoneI))
    val weekEndPeakHour = {
      val isWeekendMorningPeak = time.isAfter(nine) && time.isBefore(eleven)
      val isWeekendEveningPeak = skipOffHours && time.isAfter(eighteen) && time.isBefore(twentyTwo)
      isWeekend && (isWeekendMorningPeak || isWeekendEveningPeak)
    }

    val weekDayPeakHour = {
      val isWeekdayMorningPeak = time.isAfter(seven) && time.isBefore(tenThirty)
      val isWeekdayEveningPeak = skipOffHours && time.isAfter(seventeen) && time.isBefore(twenty)
      isWeekday && (isWeekdayMorningPeak || isWeekdayEveningPeak)
    }

    weekDayPeakHour || weekEndPeakHour
  }

  def isOffPeakHour() = !isPeakHour()

  def dayWeight = {
    day match {
      case DayOfWeek.MONDAY => 1
      case DayOfWeek.TUESDAY => 2
      case DayOfWeek.WEDNESDAY => 3
      case DayOfWeek.THURSDAY => 4
      case DayOfWeek.FRIDAY => 5
      case DayOfWeek.SATURDAY => 6
      case DayOfWeek.SUNDAY => 7
    }
  }
}

object TravelTime {
  val seven = LocalTime.of(7,0)
  val nine = LocalTime.of(9,0)
  val tenThirty = LocalTime.of(10,30)
  val eleven = LocalTime.of(11,0)
  val seventeen = LocalTime.of(17,0)
  val eighteen = LocalTime.of(18,0)
  val twenty = LocalTime.of(20,0)
  val twentyTwo = LocalTime.of(22,0)

  def getDay(value: String) = DayOfWeek.valueOf(value.toUpperCase)

  def apply(input: List[String]): Option[TravelTime] = {
    input match {
      case List(day, time, from, to) => {
        val h = time.split(":").map(_.toInt)
        Some(new TravelTime(getDay(day), LocalTime.of(h.head,h.last), TravelZones(ZoneID(from.toInt), ZoneID(to.toInt))))
      }
      case _=> {
       // println("@@@@@@@@@@@@")
        None
      }
    }
  }

}