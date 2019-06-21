package utopia.flow.util

import scala.language.implicitConversions
import java.time.Instant
import java.time.temporal.TemporalAmount
import java.time.ZoneId
import java.time.Duration

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

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