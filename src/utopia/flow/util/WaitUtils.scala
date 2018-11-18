package utopia.sapling.util

import utopia.flow.util.TimeExtensions._
import utopia.flow.util.RichComparable._

import java.time.Instant
import java.time.Duration

/**
 * WaitUtils contains a number of utility tools for waiting on a thread. This utility object handles 
 * possible interruptedExceptions as well as synchronization
 * @author Mikko Hilpinen
 * @since 8.2.2018
 */
object WaitUtils
{
    /**
     * Waits for a certain amount of time (blocking), then releases the lock
     */
    def wait(lock: AnyRef, duration: Duration) = waitUntil(lock, Instant.now() + duration)
    
    /**
     * Waits until the lock is notified
     * @see #notify(AnyRef)
     */
    def waitUntilNotified(lock: AnyRef) = 
    {
        lock.synchronized
        {
            var waiting = true
            while (waiting)
            {
                try
                {
                    lock.wait()
                    waiting = false
                }
                catch
                {
                    // InterrupredExceptions are ignored
                    case _: InterruptedException => Unit
                }
            }
        }
    }
    
    /**
     * Notifies the lock, so that threads waiting on it will be released
     * @see #waitUntilNotified(AnyRef)
     */
    def notify(lock: AnyRef) = lock.synchronized { lock.notifyAll() }
    
    private def waitUntil(lock: AnyRef, targetTime: Instant) = 
    {
        lock.synchronized
        {
            var currentTime = Instant.now()
            while (currentTime < targetTime)
            {
                try
                {
                    lock.wait((targetTime - currentTime).toMillis())
                }
                catch
                {
                    case _: InterruptedException => Unit
                }
                
                currentTime = Instant.now()
            }
        }
    }
}