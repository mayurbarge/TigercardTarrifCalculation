package domain.model

import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import org.scalatest.{FunSpec, Matchers}

class CardStateTest extends FunSpec with Matchers {
  describe("CardState") {
    describe("monoid") {
      it("should combine two card states") {
        val expectedState = CardState(Map(TravelZones(ZoneI,ZoneII) -> BigDecimal(36)))
        val result = CardState.monoid.append(CardState(Map(TravelZones(ZoneI, ZoneII) -> BigDecimal(12))),
          CardState(Map(TravelZones(ZoneI,ZoneII) -> BigDecimal(24))))

        result shouldBe expectedState
      }
    }

    describe("Daily state") {
      it("should update daily state by combining the fares") {
        val rides = List(TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)),
          TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)))

        val state = CardState.changeDailyState(rides).run(CardState(Map(TravelZones(ZoneI, ZoneII) -> 23)))._1
        state.fareMap shouldBe Map(TravelZones(ZoneI, ZoneII) -> 93)
      }

      it("should cap the fares for daily travels") {
        val rides = List(TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)),
          TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)),
          TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)),
          TravelTime(DayOfWeek.THURSDAY, LocalTime.of(19,0), TravelZones(ZoneI, ZoneII)))

        val state = CardState.changeDailyState(rides).run(CardState(Map(TravelZones(ZoneI, ZoneII) -> 23)))._1
        state.fareMap shouldBe Map(TravelZones(ZoneI, ZoneII) -> 120)
      }
    }

    describe("Weekly state") {
      it("should update weekly state by combining the daily states") {
        val dailyStates = List(
          CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
          CardState(Map(TravelZones(ZoneI, ZoneII) -> 240)))

        val state = CardState.changeWeeklyState(dailyStates).run(CardState( Map(TravelZones(ZoneI, ZoneII) -> 23)))._1
        state.fareMap shouldBe Map(TravelZones(ZoneI, ZoneII) -> 383)
      }

      it("should cap the fares for weekly travels") {
        val dailyStates = List(
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)),
        CardState(Map(TravelZones(ZoneI, ZoneII) -> 120)))

        val state = CardState.changeWeeklyState(dailyStates).run(CardState(Map.empty))._1
        state.fareMap shouldBe Map(TravelZones(ZoneI, ZoneII) -> 600)
      }
    }
  }

}
