package domain.model

import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import org.scalatest.{FunSpec, Matchers}
import scalaz.Scalaz._
import scalaz._


class WeeklyTravelsTest extends FunSpec with Matchers {
  describe("WeeklyTravels") {
    it("should add to two travel lists when first travel list is empty") {
      val result = WeeklyTravels(List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI))))
      WeeklyTravels.weeklyTravelsMonoid.zero |+| WeeklyTravels(List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI)))) shouldBe result
    }

    it("should add to two travel lists") {
      val result = WeeklyTravels(List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI)),
                  TravelTime(DayOfWeek.MONDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI))))
      val expectedResult = WeeklyTravels(List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI)))) |+| WeeklyTravels(List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI))))
      result.travels should contain theSameElementsAs(expectedResult.travels)
    }

    describe("SplitByWeek") {
      it("should combine given travels into list of weekly travels given travel gap between travels is not more than one week") {
        val travels = List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI)),
          TravelTime(DayOfWeek.WEDNESDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI)),
          TravelTime(DayOfWeek.TUESDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI)))

          val result = WeeklyTravels.splitByWeek(travels)
        result.head.travels.map(_.day) should contain theSameElementsAs List(DayOfWeek.TUESDAY)
        result.last.travels.map(_.day) should contain theSameElementsAs List(DayOfWeek.WEDNESDAY,DayOfWeek.MONDAY)
      }

      it("should combine given travels into a week when travels are fir single week") {
        val travels = List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneII, ZoneI)),
          TravelTime(DayOfWeek.TUESDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI)),
          TravelTime(DayOfWeek.SUNDAY, LocalTime.of(21, 0), TravelZones(ZoneI, ZoneI)))

        val result = WeeklyTravels.splitByWeek(travels)
        result.head.travels.map(_.day) should contain theSameElementsAs List(DayOfWeek.SUNDAY, DayOfWeek.TUESDAY,DayOfWeek.MONDAY)
        result.size shouldBe 1
      }
      it("should give empty list when user does not travel") {
        val empty = List.empty[TravelTime]
        WeeklyTravels.splitByWeek(empty) shouldBe List.empty[WeeklyTravels]
      }
    }
  }

}
