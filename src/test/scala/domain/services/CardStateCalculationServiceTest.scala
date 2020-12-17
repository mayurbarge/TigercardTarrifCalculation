package domain.services

import java.time.DayOfWeek._
import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import domain.model._
import org.scalatest.{FunSpec, Matchers}

class CardStateCalculationServiceTest extends  FunSpec with Matchers {
  describe("Card State Calculation Service") {
    describe("Daily states") {
      it("should cap daily fares for ZoneI to Zone II travels") {
        val weeklyTravels = List(
          WeeklyTravels(List(TravelTime(MONDAY, LocalTime.of(10, 20), TravelZones(ZoneID(2), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(10, 45), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(16, 15), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(18, 15), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(19, 0), TravelZones(ZoneID(1), ZoneID(2)))
          )),
          WeeklyTravels(List(TravelTime(MONDAY, LocalTime.of(10, 20), TravelZones(ZoneID(2), ZoneID(1))),
            TravelTime(FRIDAY, LocalTime.of(6, 0), TravelZones(ZoneID(1), ZoneID(2)))
          ))
        )
        val result = CardStateCalculationService.dailyStates(weeklyTravels)
        val expectedResult = List(
          List(CardState(Map(TravelZones(ZoneI, ZoneI, ZoneIToZoneI) -> 80.0, TravelZones(ZoneII, ZoneI, ZoneIIToZoneI) -> 35.0, TravelZones(ZoneI, ZoneII, ZoneIToZoneII) -> 5.0))),
          List(CardState(Map(TravelZones(ZoneII, ZoneI, ZoneIIToZoneI) -> 35.0)), CardState(Map(TravelZones(ZoneI, ZoneII, ZoneIToZoneII) -> 30.0))))

        expectedResult.head should contain theSameElementsAs (expectedResult.head)
        expectedResult.last should contain theSameElementsAs (expectedResult.last)
      }

      it("should cap daily fares for ZoneI to Zone I travels") {
        val weeklyTravels = List(
          WeeklyTravels(List(TravelTime(MONDAY, LocalTime.of(10, 20), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(10, 45), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(16, 15), TravelZones(ZoneID(1), ZoneID(1))),
            TravelTime(MONDAY, LocalTime.of(18, 15), TravelZones(ZoneID(1), ZoneID(1)))
          )),
          WeeklyTravels(List(TravelTime(MONDAY, LocalTime.of(10, 20), TravelZones(ZoneID(2), ZoneID(1))),
            TravelTime(FRIDAY, LocalTime.of(6, 0), TravelZones(ZoneID(1), ZoneID(2)))
          ))
        )
        val result = CardStateCalculationService.dailyStates(weeklyTravels)
        val expectedResult =
          List(
            List(CardState(Map(TravelZones(ZoneI,ZoneI,ZoneIToZoneI) -> 100.0))),
            List(CardState(Map(TravelZones(ZoneI,ZoneII,ZoneIToZoneII) -> 30.0)), CardState(Map(TravelZones(ZoneII,ZoneI,ZoneIIToZoneI) -> 35.0))))
        expectedResult.head should contain theSameElementsAs (expectedResult.head)
        expectedResult.last should contain theSameElementsAs (expectedResult.last)
      }

      it("should cap daily fares for ZoneII to Zone II travels") {
        val weeklyTravels = List(
          WeeklyTravels(List(TravelTime(MONDAY, LocalTime.of(10, 20), TravelZones(ZoneID(2), ZoneID(2))),
            TravelTime(MONDAY, LocalTime.of(10, 45), TravelZones(ZoneID(2), ZoneID(2))),
            TravelTime(MONDAY, LocalTime.of(16, 15), TravelZones(ZoneID(2), ZoneID(2))),
            TravelTime(MONDAY, LocalTime.of(18, 15), TravelZones(ZoneID(2), ZoneID(2)))
          ))
        )
        val result = CardStateCalculationService.dailyStates(weeklyTravels)
        val expectedResult = List(List(CardState(Map(TravelZones(ZoneII,ZoneII,ZoneIIToZoneII) -> 80.0))))
        expectedResult.head should contain theSameElementsAs (expectedResult.head)
      }
    }
    describe("Weekly States Capping") {
      it("should cap weekly fares for ZoneI to ZoneII travels") {
        val travelZones = TravelZones(ZoneI, ZoneII, ZoneIToZoneII)
        val dailyTravelStates = List(
          List.fill(7)(CardState(Map(travelZones -> 120.0))),
          List(CardState(Map(travelZones -> 100.0)))
        )
        val result  = CardStateCalculationService.weeklyStates(dailyTravelStates).map(_.run(CardState.monoid.zero)._1)
        val expectedStates = List(
          CardState(Map(travelZones -> 600.0)),
          CardState(Map(travelZones -> 100.0))
        )
        result should contain theSameElementsAs expectedStates
      }

      it("should cap weekly fares for ZoneII to ZoneII travels") {
        val travelZones = TravelZones(ZoneII, ZoneII, ZoneIIToZoneII)
        val dailyTravelStates = List(List.fill(6)(CardState(Map(travelZones -> 80.0))))
        val result  = CardStateCalculationService.weeklyStates(dailyTravelStates).map(_.run(CardState.monoid.zero)._1)
        val expectedStates = List(CardState(Map(TravelZones(ZoneII,ZoneII,ZoneIIToZoneII) -> 400.0)))
        result should contain theSameElementsAs expectedStates
      }

      it("should cap weekly fares for ZoneI to ZoneI travels") {
          val travelZones = TravelZones(ZoneI, ZoneI, ZoneIToZoneI)
          val dailyTravelStates = List(List.fill(6)(CardState(Map(travelZones -> 100.0))))
          val result = CardStateCalculationService.weeklyStates(dailyTravelStates).map(_.run(CardState.monoid.zero)._1)
          val expectedStates = List(CardState(Map(TravelZones(ZoneI, ZoneI, ZoneIToZoneI) -> 500.0)))
          result should contain theSameElementsAs expectedStates
      }
    }
  }

  it("should calculate total fare for the journyes") {
    val travelZones = TravelZones(ZoneI, ZoneI, ZoneIToZoneI)
    val dailyTravelStates = List(List.fill(6)(CardState(Map(travelZones -> 100.0))))
    val result  = CardStateCalculationService.weeklyStates(dailyTravelStates)
    CardStateCalculationService.totalFare(result) should be (500.0)
  }

  it("should calculate total fare by running the states in the pipeline") {
    val travels = List(TravelTime(DayOfWeek.MONDAY, LocalTime.of(10, 20), TravelZones(ZoneII, ZoneI)),
      TravelTime(DayOfWeek.MONDAY, LocalTime.of(10, 45), TravelZones(ZoneI, ZoneI)),
      TravelTime(DayOfWeek.MONDAY, LocalTime.of(16, 15), TravelZones(ZoneI, ZoneI)),
      TravelTime(DayOfWeek.MONDAY, LocalTime.of(18, 15), TravelZones(ZoneI, ZoneI)),
      TravelTime(DayOfWeek.MONDAY, LocalTime.of(19, 0), TravelZones(ZoneI, ZoneII))
    )
    CardStateCalculationService.run(travels) should be (120.0)
  }
}
