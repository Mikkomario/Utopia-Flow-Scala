package utopia.flow.caching

import java.time.Instant

import utopia.flow.async.AsyncExtensions._
import utopia.flow.util.TimeExtensions._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
  * This cache only holds up to one item
  * @author Mikko Hilpinen
  * @since 7.6.2019, v1.5+
  * @tparam A The type of cached item
  */
trait SingleCache[A]
{
	// ATTRIBUTES	------------------
	
	private var cachedResult: Option[A] = None
	private var lastFailure: Option[(Instant, Throwable)] = None
	private var _future: Option[Future[Try[A]]] = None
	
	
	// ABSTRACT	----------------------
	
	/**
	  * @return How long a failure result should be cached
	  */
	protected val failDuration: Duration
	
	/**
	  * @return Requests new data
	  */
	protected def request(): Try[A]
	
	
	// COMPUTED		------------------
	
	/**
	  * @return Whether this cache contains a cached success
	  */
	def isSuccessCached = cachedResult.isDefined
	
	/**
	  * @return Whether this cache contains a cached failure
	  */
	def isFailureCached = lastFailure.exists { data => failDuration.finite.forall { data._1 < Instant.now() - _ } }
	
	/**
	  * @return Whether this cache contains a cached result (success or failure)
	  */
	def isCached = isSuccessCached || isFailureCached
	
	/**
	  * @param context Asynchronous excecution context (used when performing request, implicit)
	  * @return The item in this cache. May fail.
	  */
	// Provides cached result if there is one
	def synchronous(implicit context: ExecutionContext) = getResult { _.waitFor().flatten }
	
	/**
	  * @param context Asynchronous execution context (implicit, might not be used)
	  * @return An asynchronous result for the data retrieval
	  */
	// Doesn't go asynchronous if there's a cached result available
	def future(implicit context: ExecutionContext) = getResult { f => f }
	
	// Will assume that there is no cached result
	private def getNew =
	{
		// May return cached failure
		if (isFailureCached)
			Failure(lastFailure.get._2)
		else
		{
			// Performs the request & caches result
			request() match
			{
				case Success(value) =>
					cachedResult = Some(value)
					Success(value)
				case Failure(exception) => lastFailure = Some(Instant.now() -> exception)
					Failure(exception)
			}
		}
	}
	
	
	// OTHER	---------------------
	
	/**
	  * Clears all cached data
	  */
	def clear() =
	{
		cachedResult = None
		lastFailure = None
		_future = None
	}
	
	private def getResult[B](handleFuture: Future[Try[A]] => B)(implicit context: ExecutionContext) =
		cachedResult.map { r => handleFuture(Future.successful(Success(r))) }.getOrElse
		{
			// May return a future from an already started operation, but will ignore completed futures
			if (_future.forall { _.isCompleted })
			{
				val newFuture = Future(getNew)
				_future = Some(newFuture)
				handleFuture(newFuture)
			}
			else
				handleFuture(_future.get)
		}
}
