package domain.services

import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import domain.model.{TravelZones, ZoneI, ZoneII}
import org.scalatest.{FunSpec, Matchers}

class FareCalculationServiceTest extends FunSpec with Matchers {
  describe("FareCalculator") {
    val weekDayMorningPeak = TravelTime(DayOfWeek.MONDAY, LocalTime.of(7, 0), TravelZones(ZoneI, ZoneI))
    val offHoursWeekEnd = TravelTime(DayOfWeek.SATURDAY, LocalTime.of(18, 0), TravelZones(ZoneII, ZoneI))

    it("should return daily peak hour fare based on travel zone") {
      FareCalculationService.calculateDailyFare(weekDayMorningPeak) shouldBe 30
    }

    it("should return daily off-peak hour fare based on travel zone") {
      FareCalculationService.calculateDailyFare(offHoursWeekEnd) shouldBe 30
    }
  }

}
