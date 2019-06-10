package utopia.flow.caching

import java.time.Instant

import utopia.flow.util.TimeExtensions._

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
  * This cache may succeed or fail in requesting an item
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait SingleTryCache[+A] extends SingleCacheLike[Try[A]]
{
	// ATTRIBUTES	----------------
	
	private var lastFailure: Option[(Throwable, Instant)] = None
	
	
	// ABSTRACT	--------------------
	
	/**
	  * The duration which this cache holds its value
	  */
	protected val failCacheDuration: FiniteDuration
	
	/**
	  * @return Requests a new value for this cache
	  */
	protected def request(): Try[A]
	
	
	// IMPLEMENTED	----------------
	
	// Uses a cached failure if one is still in effect
	override def apply() = if (lastFailure.exists { _._2 > Instant.now() - failCacheDuration }) Failure(lastFailure.get._1) else
	{
		request() match
		{
			case s: Success[A] => s
			case f: Failure[A] =>
				lastFailure = Some(f.exception -> Instant.now())
				f
		}
	}
}
