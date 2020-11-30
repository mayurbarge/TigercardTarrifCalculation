package domain
import datetime.TravelTime
import scalaz.Scalaz._
import scalaz.syntax._
import scalaz.Semigroup._
import scalaz.{Foldable, Monoid, Semigroup}


/*
case class Trip(fromZone: Zone, toZone: Zone, travelTime: TravelTime) {
  def isWithinZoneTravel() = fromZone.id == toZone.id
  def isOutsideZoneTravel() = !isWithinZoneTravel()
  def getZoneRates: ZoneRates = ZoneRates(fromZone.id, toZone.id)
}
*/


/*
case class TigerCard(id: Int, balance: Double) {
  def credit(balance: Double) = TigerCard(id, balance + this.balance)
  def debit(balance: Double) = TigerCard(id, this.balance - balance)
}*/

trait FareCalculator {
  /*def calculate(trip: Trip, tigerCard: TigerCard) = {

    120
  }*/
}



