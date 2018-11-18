package utopia.flow.util

import java.time.Instant
import java.time.temporal.TemporalAmount
import java.time.ZoneId
import java.time.Duration

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
	    def toLocalDateTime = i.atZone(ZoneId.systemDefault()).toLocalDateTime()
	    
	    /**
	     * The date time value of this instant in the UTC 'zulu' time zone
	     */
	    def toUTCDateTime = i.atZone(ZoneId.of("Z")).toLocalDateTime()
	    
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
	}
}