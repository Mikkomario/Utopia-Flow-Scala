package utopia.flow.caching.multi

import java.time.Instant

import utopia.flow.util.TimeExtensions._

import scala.collection.immutable.HashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
  * Caches are used for storing read data
  * @author Mikko Hilpinen
  * @since 7.6.2019, v1.5+
  * @tparam Key The key provided by the user when searching for data
  * @tparam Resource The result the user is seeking
  */
trait CacheOld[Key, Resource]
{
	// ATTRIBUTES	----------------
	
	private var results: Map[Key, Resource] = HashMap()
	private var futures: Map[Key, Future[Try[Resource]]] = HashMap()
	private var failedRequests: Map[Key, (Instant, Throwable)] = HashMap()
	
	
	// ABSTRACT	--------------------
	
	/**
	  * Requests for new data (synchronous)
	  * @param key The key provided by the user
	  * @return Request results
	  */
	protected def request(key: Key): Try[Resource]
	
	/**
	  * @return The duration which a failure result should be remembered. While remembering a
	  *         failure result, no new requests for that key are made.
	  */
	protected def requestFailDuration: Duration
	
	
	// OPERATORS	-----------------
	
	/**
	  * Reads data for key
	  * @param key A key for finding data
	  * @return Data for key. May fail.
	  */
	def apply(key: Key) =
	{
		// First checks for cached data
		results.get(key).map(Success(_)).getOrElse
		{
			// Then checks if there are failed requests that still apply
			val failure = failedRequests.get(key)
			if (failure.exists { data => requestFailDuration.finite.forall { data._1 < Instant.now() - _ } })
				Failure(failure.get._2)
			else
			{
				// If not, requests for new data
				request(key) match
				{
					case Success(data) =>
						results += key -> data
						Success(data)
					case Failure(exception) =>
						failedRequests += key -> (Instant.now() -> exception)
						Failure(exception)
				}
			}
		}
	}
	
	
	// OTHER	----------------------
	
	/**
	  * Reads data asynchronously
	  * @param key A key for finding data
	  * @param context Execution context used for making asynchronous requests (implicit)
	  * @return Asynchronous result for the search
	  */
	def future(key: Key)(implicit context: ExecutionContext) =
	{
		if (futures.contains(key))
			futures(key)
		else
		{
			val future = Future { apply(key) }
			futures = futures.filterNot { _._2.isCompleted } + (key -> future)
			future
		}
	}
}
