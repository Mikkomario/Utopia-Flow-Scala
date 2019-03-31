package utopia.flow.util

import utopia.flow.util.TimeExtensions._
import utopia.flow.util.RichComparable._

import java.time.Instant
import java.time.Duration
import scala.concurrent.duration.FiniteDuration
import utopia.flow.util.WaitTarget.WaitDuration
import utopia.flow.util.WaitTarget.UntilNotified
import utopia.flow.util.WaitTarget.Until

/**
 * WaitUtils contains a number of utility tools for waiting on a thread. This utility object handles 
 * possible interruptedExceptions as well as synchronization
 * @author Mikko Hilpinen
 * @since 8.2.2018
 */
object WaitUtils
{
    /**
     * Waits the duration of the specified wait target
     */
    def wait(target: WaitTarget, lock: AnyRef) = target.waitWith(lock)
    
    /**
     * Waits for a certain amount of time (blocking), then releases the lock
     */
    def wait(duration: Duration, lock: AnyRef): Unit = wait(WaitDuration(duration), lock)
    
    /**
     * Waits for a certain amount of time (blocking), then releases the lock
     */
    def wait(duration: FiniteDuration, lock: AnyRef): Unit = wait(WaitDuration(duration), lock)
    
    /**
     * Waits until the lock is notified
     * @see #notify(AnyRef)
     */
    def waitUntilNotified(lock: AnyRef) = wait(UntilNotified, lock)
    
    /**
     * Notifies the lock, so that threads waiting on it will be released
     * @see #waitUntilNotified(AnyRef)
     */
    def notify(lock: AnyRef) = lock.synchronized { lock.notifyAll() }
    
    /**
     * Waits until the specified time has been reached
     */
    def waitUntil(targetTime: Instant, lock: AnyRef) = wait(Until(targetTime), lock)
}