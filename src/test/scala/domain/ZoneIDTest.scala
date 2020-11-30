package domain

import org.scalatest.{FunSpec, FunSuite, Matchers}

class ZoneIDTest extends FunSpec with Matchers {
  describe("ZoneID") {
    it("should return ZoneI for value 1") {
      ZoneID(1) shouldBe ZoneI
    }

    it("should return ZoneII for value 2") {
      ZoneID(2) shouldBe ZoneII
    }

    it("should return ZoneIII for value 3") {
      ZoneID(3) shouldBe ZoneIII
    }
  }
}
