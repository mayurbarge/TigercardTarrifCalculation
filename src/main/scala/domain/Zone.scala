package domain

trait ZoneID
case object ZoneI extends ZoneID
case object ZoneII extends ZoneID
case object ZoneIII extends ZoneID

object ZoneID {
  def apply(zoneID: Int): ZoneID = {
    zoneID match {
      case 1 => ZoneI
      case 2 => ZoneII
      case 3 => ZoneIII
    }
  }
}
