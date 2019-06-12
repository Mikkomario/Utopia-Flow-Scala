package utopia.flow.caching.single

import java.time.Instant

import utopia.flow.async.AsyncExtensions._
import utopia.flow.util.TimeExtensions._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object SingleAsyncCache
{
	/**
	  * Creates a new single asynchronous cache
	  * @param failCacheDuration The duration which a failed result will be cached
	  * @param makeRequest A function for making a new request
	  * @tparam A The type of returned item
	  * @return A new cache that requests items asynchronously
	  */
	def apply[A](failCacheDuration: FiniteDuration)(makeRequest: => Future[A]): SingleAsyncCache[A] =
		new SingleAsyncCacheImpl(failCacheDuration, () => makeRequest)
}

/**
  * This cache caches asynchronous requests
  * @author Mikko Hilpinen
  * @since 12.6.2019, v1.5+
  */
trait SingleAsyncCache[A] extends ClearableSingleCacheLike[Future[A]]
{
	// ATTRIBUTES	--------------
	
	private var lastRequestTime: Option[Instant] = None
	private var lastRequest: Option[Future[A]] = None
	
	
	// ABSTRACT	------------------
	
	/**
	  * Requests a new value
	  * @return The asynchronous request results
	  */
	protected def request(): Future[A]
	
	/**
	  * The duration how long a failed request will be cached
	  */
	protected val failCacheDuration: FiniteDuration
	
	
	// IMPLEMENTED	--------------
	
	// Cached request doesn't count if it has already failed and failure duration has passed since the request was made
	override def cached = lastRequest.filter { r => !r.isCompleted || r.waitFor().isSuccess ||
		lastRequestTime.exists { Instant.now() < _ + failCacheDuration } }
	
	override def apply() =
	{
		cached.getOrElse
		{
			val result = request()
			lastRequestTime = Some(Instant.now())
			lastRequest = Some(result)
			
			result
		}
	}
	
	override def clear() =
	{
		lastRequest = None
		lastRequestTime = None
	}
}

private class SingleAsyncCacheImpl[A](protected val failCacheDuration: FiniteDuration,
									  private val makeRequest: () => Future[A]) extends SingleAsyncCache[A]
{
	override protected def request() = makeRequest()
}
