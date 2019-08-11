package utopia.flow.util

import java.time.chrono.ChronoLocalDate

import scala.language.implicitConversions
import java.time.{DayOfWeek, Duration, Instant, LocalDate, Month, Year, YearMonth, ZoneId}
import java.time.temporal.TemporalAmount

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

import scala.collection.immutable.VectorBuilder

/**
* This object contains some extensions for java's time classes
* @author Mikko Hilpinen
* @since 17.11.2018
**/
object TimeExtensions
{
	implicit class ExtendedInstant(val i: Instant) extends AnyVal
	{
	    /**
	     * The date time value of this instant in the local time zone
	     */
	    def toLocalDateTime = i.atZone(ZoneId.systemDefault()).toLocalDateTime
	    
	    /**
	     * The date time value of this instant in the UTC 'zulu' time zone
	     */
	    def toUTCDateTime = i.atZone(ZoneId.of("Z")).toLocalDateTime
	    
	    /**
	     * An instant after the specified duration has passed from this instant
	     */
	    def +(amount: TemporalAmount) = i.plus(amount)
	    
	    /**
	     * An instant before the specified duration
	     */
	    def -(amount: TemporalAmount) = i.minus(amount)
	    
	    /**
	     * Finds the difference (duration) between the two time instances
	     */
	    def -(time: Instant) = Duration.between(time, i)
	    
	    /**
	     * Checks whether this instant comes before the specified instant
	     */
	    def <(other: Instant) = i.isBefore(other)
	    
	    /**
	     * Checks whether this instant comes after the specified instant
	     */
	    def >(other: Instant) = i.isAfter(other)
	}
	
	implicit class ExtendedDuration(val d: Duration) extends AnyVal
	{
	    /**
	     * This duration as milliseconds, but with double precision
	     */
	    def toPreciseMillis = d.toNanos / 1000000.0
		
		/**
		  * @return This duration in seconds, but with double precision
		  */
		def toPreciseSeconds = toPreciseMillis / 1000
		
		/**
		  * @return Describes this duration in a suitable unit and precision
		  */
		def description = javaDurationToScalaDuration(d).description
	}
	
	implicit class ExtendedScalaDuration(val d: duration.Duration) extends AnyVal
	{
		/**
		  * @return A finite version of this duration. None for infinite durations.
		  */
		def finite = if (d.isFinite()) Some(FiniteDuration(d.length, d.unit)) else None
		
		/**
		  * @return This duration in milliseconds, but with double precision (converted from nanoseconds)
		  */
		def toPreciseMillis = d.toNanos / 1000000.0
		
		/**
		  * @return This duration in seconds, but with double precision (converted from nanoseconds)
		  */
		def toPreciseSeconds = toPreciseMillis / 1000
		
		/**
		  * @return Describes this duration in a suitable unit and precision
		  */
		def description =
		{
			val seconds = toPreciseSeconds
			if (seconds < 0.1)
			{
				val millis = toPreciseMillis
				if (millis < 0.1)
					s"${d.toNanos} nanos"
				else if (millis < 1)
					f"$millis%1.2f millis"
				else
					s"${millis.toInt.toString} millis"
			}
			else if (seconds >= 120)
			{
				val hoursPart = (seconds / 3600).toInt
				val minutesPart = ((seconds % 3600) / 60).toInt
				val secondsPart = (seconds % 60).toInt
				
				if (hoursPart > 0)
					s"$hoursPart h $minutesPart min"
				else
					s"$minutesPart min $secondsPart s"
			}
			else
				f"$seconds%1.2f seconds"
		}
	}
	
	implicit class ExtendedLocalDate(val d: LocalDate) extends AnyVal
	{
		/**
		  * @return Year of this date
		  */
		def year = Year.of(d.getYear)
		/**
		  * @return Month of this date
		  */
		def month = d.getMonth
		/**
		  * @return Week day of this date
		  */
		def weekDay = d.getDayOfWeek
		/**
		  * @return Year + month of this date
		  */
		def yearMonth = d.year + d.month
	}
	
	implicit class ExtendedYear(val y: Year) extends AnyVal
	{
		/**
		  * Adds month information to this year
		  * @param month Targeted month
		  * @return A monthYear based on this year and specified month
		  */
		def +(month: Month) = YearMonth.of(y.getValue, month)
	}
	
	implicit class ExtendedYearMonth(val ym: YearMonth) extends AnyVal
	{
		// COMPUTED	----------------------
		
		/**
		  * @return Year portion of this year month
		  */
		def year = Year.of(ym.getYear)
		
		/**
		  * @return Dates in this month
		  */
		def dates = (1 to ym.lengthOfMonth()).map(apply)
		
		/**
		  * @return The year month previous to this one
		  */
		def previous = this - 1
		
		/**
		  * @return The year month following this one
		  */
		def next = this + 1
		
		
		// OTHER	----------------------
		
		/**
		  * Adjusts this month by specified amount
		  * @param monthAdjust Adjustment to month count
		  * @return Adjusted month
		  */
		def +(monthAdjust: Int) = ym.plusMonths(monthAdjust)
		
		/**
		  * Adjusts this month by specified amount
		  * @param monthAdjust Adjustment to month count
		  * @return Adjusted month
		  */
		def -(monthAdjust: Int) = ym.minusMonths(monthAdjust)
		
		/**
		  * @param dayNumber Targeted day number
		  * @return Targeted date
		  */
		def apply(dayNumber: Int) = LocalDate.of(ym.getYear, ym.getMonth, dayNumber)
		
		/**
		  * Separates this month to weeks
		  * @param firstDayOfWeek The first day of a week (default = Monday)
		  * @return A vector that contains all weeks in this month, first and last week may contain less than 7
		  *         days.
		  */
		def weeks(firstDayOfWeek: DayOfWeek = DayOfWeek.MONDAY) =
		{
			val d = dates
			// Month may start at the middle of the week
			val incompleteStart = d.takeWhile { _.weekDay.getValue > firstDayOfWeek.getValue }.toVector
			
			val weeksBuffer = new VectorBuilder[Vector[LocalDate]]()
			if (incompleteStart.nonEmpty)
				weeksBuffer += incompleteStart
			
			var nextWeekStartIndex = incompleteStart.size
			while (nextWeekStartIndex < d.size)
			{
				weeksBuffer += d.slice(nextWeekStartIndex, nextWeekStartIndex + 7).toVector
				nextWeekStartIndex += 7
			}
			
			weeksBuffer.result()
		}
	}
	
	implicit class TimeNumber[T](val i: T) extends AnyVal
	{
		private def nanoPrecision(mod: Long)(implicit n: Numeric[T]) = FiniteDuration((n.toDouble(i) * mod).toLong, TimeUnit.NANOSECONDS)
		
		/**
		  * @param n implicit numeric
		  * @return This number amount of nano seconds
		  */
		def nanos(implicit n: Numeric[T]) = nanoPrecision(1)
		/**
		  * @param n implicit numeric
		  * @return This number amount of milli seconds (provides nano precision with doubles)
		  */
		def millis(implicit n: Numeric[T]) = nanoPrecision(1000000)
		/**
		  * @param n implicit numeric
		  * @return This number amount of seconds (provides nano precision with doubles)
		  */
		def seconds(implicit n: Numeric[T]) = nanoPrecision(1000000l * 1000)
		/**
		  * @param n implicit numeric
		  * @return This number amount of minutes (provides nano precision with doubles)
		  */
		def minutes(implicit n: Numeric[T]) = nanoPrecision(1000000l * 1000 * 60)
		/**
		  * @param n implicit numeric
		  * @return This number amount of hours (provides nano precision with doubles)
		  */
		def hours(implicit n: Numeric[T]) = nanoPrecision(1000000l * 1000 * 60 * 60)
	}
	
	/**
	 * Converts a java duration to a scala duration
	 */
	implicit def javaDurationToScalaDuration(duration: java.time.Duration): FiniteDuration =
	        FiniteDuration(duration.toNanos, TimeUnit.NANOSECONDS)
	
	/**
	 * Converts a java duration option to scala duration
	 */
	implicit def javaDurationOptionToScalaDuration(duration: Option[java.time.Duration]): scala.concurrent.duration.Duration =
	        duration.map(javaDurationToScalaDuration).getOrElse(scala.concurrent.duration.Duration.Inf)
	
	/**
	 * Converts a finite scala duration to a java duration
	 */
	implicit def scalaDurationToJavaDuration(duration: FiniteDuration): Duration = java.time.Duration.ofNanos(duration.toNanos)
}