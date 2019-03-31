package utopia.flow.util

import utopia.flow.util.TimeExtensions._

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration
import scala.util.Try

object WaitTarget
{
    /**
     * This waitTarget waits until the lock is notified
     */
    case object UntilNotified extends WaitTarget
    {
        protected val targetTime = None
        val breaksOnNotify = true
        
        def breakable = this
    }
    
    /**
     * This waitTarget always waits a specified duration (unless broken)
     */
    case class WaitDuration(val duration: Duration, val breaksOnNotify: Boolean = true) extends WaitTarget
    {
        protected val targetTime = Some(Left(duration))
        
        def breakable: WaitDuration = if (breaksOnNotify) this else WaitDuration(duration, true)
    }
    
    /**
     * This waitTarget waits until a specified time instant (unless broken). Once that 
     * time instant is reached, no waiting is done anymore
     */
    case class Until(val time: Instant, val breaksOnNotify: Boolean = true) extends WaitTarget
    {
        protected val targetTime = Some(Right(time))
        
        def breakable: Until = if (breaksOnNotify) this else Until(time, true)
    }
}

/**
* A wait target specifies a waiting time: either until a specified instant, 
* a specific duration or an infinite time period
* @author Mikko Hilpinen
* @since 31.3.2019
**/
sealed trait WaitTarget
{
    // ABSTRACT    --------------
    
    protected def targetTime: Option[Either[Duration, Instant]]
    
    /**
     * Whether waits will stop once this wait target is notified
     */
    def breaksOnNotify: Boolean
    
    /**
     * A breakable version of this wait target that will stop once the lock is notified
     */
    def breakable: WaitTarget
    
    
	// COMPUTED    --------------
    
    /**
     * Whether this wait target has a maximum duration
     */
    def isInfinite = targetTime.isEmpty
    
    /**
     * Whether this wait target only stops when the lock is notified
     */
    def isFinite = !isInfinite
    
    /**
     * the duration of this target or None if this target had infinite duration
     */
    def toFiniteDuration = targetTime.map { _ match 
    {
        case Left(duration) => duration
        case Right(time) => time - Instant.now()
    }}
    
    /**
     * The duration of this target. May be infinite
     */
    def toDuration: duration.Duration = toFiniteDuration
    
    /**
     * @return the ending time of this target, after which no waiting is done
     */
    def endTime = targetTime.map { _ match 
    {
        case Left(duration) => Instant.now() + duration
        case Right(time) => time
    }}
    
    /**
     * Whether this wait target is specified by wait duration
     */
    def durationIsSpecified = targetTime.exists { _.isLeft }
    
    /**
     * Whether this wait target is specified by end time
     */
    def endTimeIsSpecified = targetTime.exists { _.isRight }
    
    // TODO: To wait
    
    
    // OTHER    -----------------
    
    /**
     * Blocks the current thread until this wait target is reached
     * @param lock the lock on which the waiting is done
     * @see notify(lock)
     */
    def waitWith(lock: AnyRef) = 
    {
        var waitCompleted = false
        
        if (isFinite)
        {
            val targetTime = endTime.get
            
            lock.synchronized
            {
                var currentTime = Instant.now()
                
                while (!waitCompleted && (currentTime < targetTime))
                {
                    val waitDuration = targetTime - currentTime
                    // Performs the actual wait here (nano precision)
                    // Exceptions are ignored
                    Try
                    {
                        lock.wait(waitDuration.toMillis(), waitDuration.getNano / 1000)
                        
                        if (breaksOnNotify)
                            waitCompleted = true
                    }
                    
                    currentTime = Instant.now()
                }
            }
        }
        else
        {
            lock.synchronized
            {
                while (!waitCompleted)
                {
                    // Waits until notfified, exceptions are ignored
                    Try
                    {
                        lock.wait()
                        waitCompleted = true
                    }
                }
            }
        }
    }
    
    /**
     * Notifies a lock, possibly breaking any wait that is waiting on that lock. Unbreakable waits 
     * won't be affected.
     */
    def notify(lock: AnyRef)
    {
        lock.synchronized { lock.notifyAll() }
    }
    
    /**
     * Creates a new wait instance based on this wait target
     */
    def newWait() = new SingleWait(this)
}