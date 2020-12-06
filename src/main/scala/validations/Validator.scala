package validations

import java.time.{DayOfWeek, LocalTime}

import datetime.TravelTime
import domain.{TravelZones, ZoneID}
import scalaz.Scalaz._
import scalaz.{ValidationNel, _}

import scala.util.Try

object Validator {
  type Result[A] = ValidationNel[String, A]

  def validateDay(day: String): Result[DayOfWeek] = {
    Try {DayOfWeek.valueOf(day.toUpperCase)}.toOption match {
      case Some(value) => value.success
      case None => "Day invalid".failureNel
    }
  }

  def validateLocalTime(time: String): Result[LocalTime] = {
    Try {
      val h = time.split(":").map(_.toInt)
      LocalTime.of(h.head,h.last)
    }.toOption match {
      case Some(value) => value.success
      case None => "Day invalid".failureNel
    }
  }

  def validateZoneID(id: String): Result[ZoneID] = {
    Try {id.toInt}.toOption match {
      case Some(value) if value > 0 && value < 3 => ZoneID(value).success
      case  _=> "ZoneID invalid or not supported".failureNel
    }
  }

  def validate(day: String, time:String, from: String, to: String) = {
    val travelZone: Result[TravelZones] = (validateZoneID(from) |@| validateZoneID(to)).apply(TravelZones.apply)
    (validateDay(day) |@| validateLocalTime(time) |@| travelZone).apply(TravelTime.apply)
  }

}