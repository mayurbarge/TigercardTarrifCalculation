package datetime

import java.time.{DayOfWeek, LocalTime}

import domain.{TravelZones, ZoneI, ZoneII}
import org.scalatest.{FunSpec, FunSuite, Matchers}

class TravelTimeTest extends FunSpec with Matchers {
  describe("TravelTime") {
    describe("Peak Hours") {
      describe("Weekday") {
        val weekDayMorningPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(7, 0), TravelZones(ZoneI, ZoneI))
        val weekDayEveningPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneI, ZoneI))
        it("should check for weekday morning peak hours") {
          weekDayMorningPeak.isPeakHour() shouldBe true
        }
        it("should check for weekday evening peak hours") {
          weekDayEveningPeak.isPeakHour() shouldBe true
        }
      }
      describe("Weekend") {
        val weekEndMorningPeak = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(9, 0), TravelZones(ZoneI, ZoneI))
        val weekEndEveningPeak = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(18, 0), TravelZones(ZoneI, ZoneI))
        it("should check for weekend morning peak hours") {
          weekEndMorningPeak.isPeakHour() shouldBe true
        }
        it("should check for weekend evening peak hours") {
          weekEndEveningPeak.isPeakHour() shouldBe true
        }

      }
    }
    describe("Off-Peak Hours") {
      describe("Weekday") {
        val offHoursWeekDay = TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI))
        val weekDayMorningOffPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(6, 58), TravelZones(ZoneI, ZoneI))
        val weekDayAfternoonOffPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(10, 31), TravelZones(ZoneI, ZoneI))
        val weekDayNightOffPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI))

        it("should check for weekday off-peak hours when user is travelling from outer zones to zoneI") {
          offHoursWeekDay.isOffPeakHour() shouldBe true
        }
        it("should check for weekday off-peak hours in the early morning") {
          weekDayMorningOffPeak.isOffPeakHour() shouldBe true
        }
        it("should check for weekday off-peak hours in the afternoon") {
          weekDayAfternoonOffPeak.isOffPeakHour() shouldBe true
        }
        it("should check for weekday off-peak hours in the night") {
          weekDayNightOffPeak.isOffPeakHour() shouldBe true
        }
      }
      describe("Weekend") {
        val offHoursWeekEnd = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(18, 0), TravelZones(ZoneII, ZoneI))

        val weeEndBefore18OffPeak = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI))
        val weekEndAfter22OffPeak = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(22, 31), TravelZones(ZoneI, ZoneI))

        it("should check for weekend off-peak hours when user is travelling from outer zones to zoneI") {
          offHoursWeekEnd.isOffPeakHour() shouldBe true
        }
        it("should check for weekend off-peak hours when travelling before 18:00 hrs") {
          weeEndBefore18OffPeak.isOffPeakHour() shouldBe true
        }
        it("should check for weekend off-peak hours when travelling after 22:00 hrs") {
          weekEndAfter22OffPeak.isOffPeakHour() shouldBe true
        }
      }
    }
  }

  describe("Day weight") {
    it("should give weight 1 to Monday") {
      TravelTime(DayOfWeek.MONDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 1
    }
    it("should give weight 2 to Tuesday") {
      TravelTime(DayOfWeek.TUESDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 2
    }
    it("should give weight 3 to Wednesday") {
      TravelTime(DayOfWeek.WEDNESDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 3
    }
    it("should give weight 4 to Thursday") {
      TravelTime(DayOfWeek.THURSDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 4
    }
    it("should give weight 5 to Friday") {
      TravelTime(DayOfWeek.FRIDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 5
    }
    it("should give weight 6 to Saturday") {
      TravelTime(DayOfWeek.SATURDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 6
    }
    it("should give weight 7 to Sunday") {
      TravelTime(DayOfWeek.SUNDAY, LocalTime.of(17, 58), TravelZones(ZoneI, ZoneI)).dayWeight shouldBe 7
    }
  }
}
