package datetime

import java.time.{DayOfWeek, LocalTime}

import domain.model.{TravelZones, ZoneI, ZoneII}
import org.scalatest.{FunSpec, Matchers}

class TravelTimeTest extends FunSpec with Matchers {
  describe("TravelTime") {
    describe("Peak Hours") {
      describe("Weekday") {
        val weekDayMorningPeak1 = TravelTime(DayOfWeek.MONDAY, LocalTime.of(7, 0), TravelZones(ZoneI, ZoneI))
        val weekDayMorningPeak2 = TravelTime(DayOfWeek.MONDAY, LocalTime.of(10, 29), TravelZones(ZoneI, ZoneI))
        val weekDayMorningNonPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(10, 30), TravelZones(ZoneI, ZoneI))
        it("should check for weekday morning peak hours") {
          weekDayMorningPeak1.isPeakHour() shouldBe true
          weekDayMorningPeak2.isPeakHour() shouldBe true
          weekDayMorningNonPeak.isPeakHour() shouldBe false
        }
        val weekDayEveningPeak1 = TravelTime(DayOfWeek.MONDAY, LocalTime.of(17, 0), TravelZones(ZoneI, ZoneI))
        val weekDayEveningPeak2 = TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 59), TravelZones(ZoneI, ZoneI))
        val weekDayEveningNonPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(20, 0), TravelZones(ZoneI, ZoneI))
        it("should check for weekday evening peak hours") {
          weekDayEveningPeak1.isPeakHour() shouldBe true
          weekDayEveningPeak2.isPeakHour() shouldBe true
          weekDayEveningNonPeak.isPeakHour() shouldBe false
        }
      }
      describe("Weekend") {
        val weekEndMorningPeak1 = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(9, 0), TravelZones(ZoneI, ZoneI))
        val weekEndMorningPeak2 = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(10, 59), TravelZones(ZoneI, ZoneI))
        val weekEndMorningNonPeak = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(11, 0), TravelZones(ZoneI, ZoneI))
        it("should check for weekend morning peak hours") {
          weekEndMorningPeak1.isPeakHour() shouldBe true
          weekEndMorningPeak2.isPeakHour() shouldBe true
          weekEndMorningNonPeak.isPeakHour() shouldBe false
        }
        val weekEndEveningPeak1 = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(18, 0), TravelZones(ZoneI, ZoneI))
        val weekEndEveningPeak2 = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(21, 59), TravelZones(ZoneI, ZoneI))
        val weekEndEveningPeak3 = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(21, 59), TravelZones(ZoneI, ZoneI))
        val weekEndEveningNonPeak = TravelTime(DayOfWeek.SUNDAY, LocalTime.of(21, 59), TravelZones(ZoneII, ZoneI))

        it("should check for weekend evening peak hours") {
          weekEndEveningPeak1.isPeakHour() shouldBe true
          weekEndEveningPeak2.isPeakHour() shouldBe true
          weekEndEveningPeak3.isPeakHour() shouldBe true
          weekEndEveningNonPeak.isPeakHour() shouldBe false
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

  describe("Validation") {
    it("should validate input for correct parameters") {
      TravelTime.apply(List("sunday","10:30","1","2")).isSuccess shouldBe true
    }
    it("should fail when invalid input is given") {
      TravelTime.apply(List("sunday","10:30","1","2")).isSuccess shouldBe true
    }
  }
}
