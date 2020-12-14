package domain.model

import org.scalatest.{FunSpec, Matchers}

class TravelZonesTest extends FunSpec with Matchers {
  describe("TravelZones") {
   it("should give configurations for ZoneI to ZoneI travel") {
     val zoneRates = TravelZones(ZoneI, ZoneI).zoneRates
     zoneRates.peakHour shouldBe 30
     zoneRates.offPeakHour shouldBe 25
     zoneRates.dailyCap shouldBe 100
     zoneRates.weeklyCap shouldBe 500
   }

    it("should give configurations for ZoneI to ZoneII travel") {
      val zoneRates = TravelZones(ZoneI, ZoneII).zoneRates
      zoneRates.peakHour shouldBe 35
      zoneRates.offPeakHour shouldBe 30
      zoneRates.dailyCap shouldBe 120
      zoneRates.weeklyCap shouldBe 600
    }

    it("should give configurations for ZoneII to ZoneI travel") {
      val zoneRates = TravelZones(ZoneII, ZoneI).zoneRates
      zoneRates.peakHour shouldBe 35
      zoneRates.offPeakHour shouldBe 30
      zoneRates.dailyCap shouldBe 120
      zoneRates.weeklyCap shouldBe 600
    }

    it("should give configurations for ZoneII to ZoneII travel") {
      val zoneRates = TravelZones(ZoneII, ZoneII).zoneRates
      zoneRates.peakHour shouldBe 25
      zoneRates.offPeakHour shouldBe 20
      zoneRates.dailyCap shouldBe 80
      zoneRates.weeklyCap shouldBe 400
    }
  }
}
