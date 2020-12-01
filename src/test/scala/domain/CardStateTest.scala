package domain

import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import org.scalatest.{FunSpec, FunSuite, Matchers}

class CardStateTest extends FunSpec with Matchers {
  describe("CardState") {
    describe("monoid") {
      it("should combine two card states") {
        val expectedState = CardState(Map(TravelZones(ZoneI,ZoneII) -> 2), Map(TravelZones(ZoneI,ZoneII) -> BigDecimal(36)))
        val result = CardState.monoid.append(CardState(Map(TravelZones(ZoneI, ZoneII) -> 1), Map(TravelZones(ZoneI, ZoneII) -> BigDecimal(12))),
          CardState(Map(TravelZones(ZoneI,ZoneII) -> 1), Map(TravelZones(ZoneI,ZoneII) -> BigDecimal(24))))

        result shouldBe expectedState
      }
    }

    describe("Daily state") {
      it("should update daily state by combining the fares") {
        //val rides = List(TravelTime(DayOfWeek.THURSDAY, LocalTime.of(), TravelZones()))

      }

    }
  }

}
