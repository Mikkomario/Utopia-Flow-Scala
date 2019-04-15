package utopia.flow.util

import scala.language.implicitConversions
import java.time.Instant
import java.time.temporal.TemporalAmount
import java.time.ZoneId
import java.time.Duration

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
	     * The this duration as milliseconds, but with double precision
	     */
	    def toPreciseMillis = d.toNanos / 1000000.0
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