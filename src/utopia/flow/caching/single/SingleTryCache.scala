package utopia.flow.caching.single

import java.time.Instant

import utopia.flow.util.TimeExtensions._

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
  * This cache may succeed or fail in requesting an item. A failure will be cached only for a certain period of time
  * while a success is cached indefinitely
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait SingleTryCache[A] extends ClearableSingleCacheLike[Try[A]]
{
	// ATTRIBUTES	----------------
	
	private var success: Option[Success[A]] = None
	private var lastFailure: Option[(Failure[A], Instant)] = None
	
	
	// ABSTRACT	--------------------
	
	/**
	  * The duration which this cache holds its value
	  */
	protected val failCacheDuration: FiniteDuration
	
	/**
	  * @return Requests a new value for this cache
	  */
	protected def request(): Try[A]
	
	
	// COMPUTED	--------------------
	
	/**
	  * @return Whether a failure is currently cached
	  */
	def isFailureCached = lastFailure.exists { _._2 > Instant.now() - failCacheDuration }
	
	/**
	  * @return Whether a success is currently cached
	  */
	def isSuccessCached = success.isDefined
	
	private def cachedFailure = lastFailure.filter { _._2 > Instant.now() - failCacheDuration }.map { _._1 }
	
	
	// IMPLEMENTED	----------------
	
	override def cached = success orElse cachedFailure
	
	override def clear() =
	{
		lastFailure = None
		success = None
	}
	
	// Uses a cached failure if one is still in effect
	override def apply() =
	{
		success.getOrElse
		{
			cachedFailure.getOrElse
			{
				request() match
				{
					case s: Success[A] =>
						success = Some(s)
						s
					case f: Failure[A] =>
						lastFailure = Some(f -> Instant.now())
						f
				}
			}
		}
	}
}