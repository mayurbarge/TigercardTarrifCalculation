package domain.model

import datetime.TravelTime
import scalaz.Monoid
import scalaz.Scalaz._

case class WeeklyTravels(travels: List[TravelTime])
object WeeklyTravels {
  implicit val weeklyTravelsMonoid: Monoid[WeeklyTravels] = {
    Monoid.instance(
      (travel1, travel2) => WeeklyTravels(travel1.travels ++ travel2.travels),
      WeeklyTravels(List.empty[TravelTime])
    )
  }

  val splitByWeek = (travelTimes: List[TravelTime]) => {
    travelTimes.foldLeft(List.empty[WeeklyTravels])((acc, travel) => {
      if (acc.headOption.flatMap(_.travels.headOption.map(_.day.getValue)).getOrElse(0) <= travel.day.getValue)
        (WeeklyTravels(List(travel)) |+| acc.headOption.getOrElse(weeklyTravelsMonoid.zero)) :: acc.tailOption.getOrElse(List.empty[WeeklyTravels])
      else
        (weeklyTravelsMonoid.zero |+| WeeklyTravels(List(travel))) :: acc
    })
  }

}
