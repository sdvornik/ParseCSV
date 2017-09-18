package com.yahoo.sdvornik

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField
import java.time.{Instant, LocalDate, ZoneId}


object TimeHelper {

  trait TimeHelper {
    implicit def toStringRep(time: Instant): String
    implicit def toInstant(timeStr: String): Instant
  }

  def apply(zone: ZoneId): TimeHelper = new TimeHelperImpl(zone)

  private class TimeHelperImpl(val zone: ZoneId) extends TimeHelper {

    import java.time.ZonedDateTime
    import java.time.format.DateTimeFormatter
    import java.util.Locale

    val formatter: DateTimeFormatter =
      new DateTimeFormatterBuilder()
        .appendPattern("d-MMM-")
        .appendValueReduced(ChronoField.YEAR, 2, 2, 2000)
        .toFormatter(Locale.US)

    implicit def toStringRep(instant: Instant): String = formatter.format(
      ZonedDateTime.ofInstant(
        instant,
        zone
      )
    )

    implicit def toInstant(timeStr: String): Instant = LocalDate
      .parse(timeStr, formatter).atStartOfDay()
      .atZone(zone)
      .toInstant
  }
}

