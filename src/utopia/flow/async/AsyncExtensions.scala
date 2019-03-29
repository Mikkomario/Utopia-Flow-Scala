package utopia.flow.async

import scala.language.implicitConversions

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/**
* This object contains extensions for asynchronous / concurrent classes
* @author Mikko Hilpinen
* @since 29.3.2019
**/
object AsyncExtensions
{
    /**
     * This implicit class provides extra features to Future
     */
	implicit class RichFuture[T](val f: Future[T]) extends AnyVal
	{
	    /**
	     * Waits for the result of this future (blocks) and returns it once it's ready
	     * @param timeout the maximum wait duration. If timeout is reached, a failure will be returned
	     * @return The result of the future. A failure if the future failed or if timeout was reached
	     */
	    def waitFor(timeout: Duration = Duration.Inf) = Try(Await.ready(f, timeout).value.get).flatten
	}
	
	/**
	 * Converts a java duration to a scala duration
	 */
	implicit def javaDurationToScalaDuration(duration: java.time.Duration) = 
	        FiniteDuration(duration.toNanos(), TimeUnit.NANOSECONDS);
	
	/**
	 * Converts a finite scala duration to a java duration
	 */
	implicit def scalaDurationToJavaDuration(duration: FiniteDuration) = java.time.Duration.ofNanos(duration.toNanos)
}