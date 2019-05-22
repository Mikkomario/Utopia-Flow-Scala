package utopia.flow.async

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Try

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
	
	implicit class ManyFutures[A](val futures: TraversableOnce[Future[A]]) extends AnyVal
	{
		/**
		  * Waits until all of the futures inside this traversable item have completed
		  * @param cbf A can build from
		  * @tparam C Resulting collection type
		  * @return The results of the waiting (each item as a try)
		  */
		def waitFor[C]()(implicit cbf: CanBuildFrom[_, Try[A], C]) =
		{
			val buffer = cbf()
			buffer ++= futures.map { _.waitFor() }
			buffer.result()
		}
		
		/**
		  * Waits until all of the futures inside this traversable item have completed
		  * @param cbf A can build from
		  * @tparam C Resulting collection type
		  * @return The successful results of the waiting (no failures will be included)
		  */
		def waitForSuccesses[C]()(implicit cbf: CanBuildFrom[_, A, C]) =
		{
			val buffer = cbf()
			buffer ++= futures.flatMap { _.waitFor().toOption }
			buffer.result()
		}
		
		/**
		  * @param context Execution context
		  * @param cbf A can build from
		  * @tparam C result collection type
		  * @return A future of the completion of all of these items. Resulting collection contains all results wrapped in try
		  */
		def future[C](implicit context: ExecutionContext, cbf: CanBuildFrom[_, Try[A], C]): Future[C] = Future { waitFor() }
		
		/**
		  * @param context Execution context
		  * @param cbf A can build from
		  * @tparam C result collection type
		  * @return A future of the completion of all of these items. Resulting collection contains only successful completions
		  */
		def futureSuccesses[C](implicit context: ExecutionContext, cbf: CanBuildFrom[_, A, C]): Future[C] = Future { waitForSuccesses() }
	}
}