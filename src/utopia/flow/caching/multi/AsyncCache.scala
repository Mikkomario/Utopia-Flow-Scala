package utopia.flow.caching.multi

import utopia.flow.caching.single.{ExpiringSingleCache, SingleAsyncCache}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/**
  * This cache requests items asynchronously
  * @author Mikko Hilpinen
  * @since 12.6.2019, v1.5+
  */
object AsyncCache
{
	/**
	  * Creates a new asynchronous cache
	  * @param failResultDuration How long a failed result will be kept
	  * @param requestAsync A function for making asynchronous requests
	  * @tparam Key The type of key for this cache
	  * @tparam Value The value provided by this cache
	  * @return A new cache that requests items asynchronously
	  */
	def apply[Key <: AnyRef, Value](failResultDuration: FiniteDuration)(requestAsync: Key => Future[Value]) =
		MultiCache[Key, Future[Value], SingleAsyncCache[Value]] { key => SingleAsyncCache(failResultDuration){ requestAsync(key) } }
	
	/**
	  * Creates a new asynchronous cache that expires both success and failure results
	  * @param failResultDuration How long a failed result is cached
	  * @param maxResultDuration How long a success result is cached (should be larger than failResultDuration)
	  * @param requestAsync A function for performing asynchronous requests
	  * @tparam Key The type of key for this cache
	  * @tparam Value The type of values asynchronously returned through this cache
	  * @return A cache that requests items asynchronously and also expires them
	  */
	def expiring[Key <: AnyRef, Value](failResultDuration: FiniteDuration, maxResultDuration: FiniteDuration)(
		requestAsync: Key => Future[Value]) = ExpiringCache[Key, Future[Value]] {
		key: Key => ExpiringSingleCache.wrap(SingleAsyncCache(failResultDuration){ requestAsync(key) }, maxResultDuration) }
}
