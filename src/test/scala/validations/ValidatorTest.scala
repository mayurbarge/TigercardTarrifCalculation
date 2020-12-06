package validations

import org.scalatest.{FunSpec, Matchers}

class ValidatorTest extends FunSpec with Matchers {
  describe("Validation") {
    it("should be successful when day format is correct") {
      Validator.validateDay("sunday").isSuccess shouldBe true
    }
    it("should be successful when time format is correct") {
      Validator.validateLocalTime("11:30").isSuccess shouldBe true
    }
    it("should be successful when zoneID is correct") {
      Validator.validateZoneID("1").isSuccess shouldBe true
    }

    it("should fail when day format is incorrect") {
      Validator.validateDay("11").isFailure shouldBe true
      Validator.validateDay("sundayy").isFailure shouldBe true
    }
    it("should fail when time format is correct") {
      Validator.validateLocalTime("11:30PM").isFailure shouldBe true
      Validator.validateLocalTime("1ZZ:3ZZ").isFailure shouldBe true
    }
    it("should fail when zoneID is less or equal to zero") {
      Validator.validateZoneID("0").isFailure shouldBe true
      Validator.validateZoneID("-1").isFailure shouldBe true
    }
    it("should fail when zoneID is greater than 2") {
      Validator.validateZoneID("3").isFailure shouldBe true
    }

    describe("TravelTime") {
      it("should be successful for valid inputs") {
        Validator.validate("sunday", "11:30", "1","2").isSuccess shouldBe true
      }

      it("should fail for invalid inputs") {
        Validator.validate("sday", "1111113:30", "677","2").isFailure shouldBe true
      }
    }
  }

}
