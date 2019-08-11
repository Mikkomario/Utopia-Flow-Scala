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
	     * @return The result of the future. A failure if this future failed or if timeout was reached
	     */
	    def waitFor(timeout: Duration = Duration.Inf) = Try(Await.ready(f, timeout).value.get).flatten
		
		/**
		  * @return Whether this future was already completed successfully
		  */
		def isSuccess = f.isCompleted && f.waitFor().isSuccess
		
		/**
		  * @return Whether this future has already failed
		  */
		def isFailure = f.isCompleted && f.waitFor().isFailure
		
		/**
		  * @return The current result of this future. None if not completed yet
		  */
		def current = if (f.isCompleted) Some(waitFor()) else None
	}
	
	implicit class TryFuture[A](val f: Future[Try[A]]) extends AnyVal
	{
		/**
		  * Waits for the result of this future (blocks) and returns it once it's ready
		  * @param timeout the maximum wait duration. If timeout is reached, a failure will be returned
		  * @return The result of the future. A failure if this future failed, if timeout was reached or if result was a failure
		  */
		def waitForResult(timeout: Duration = Duration.Inf): Try[A] = f.waitFor(timeout).flatten
		
		/**
		  * @return Whether this future already contains a success result
		  */
		def containsSuccess = f.isCompleted && waitForResult().isSuccess
		
		/**
		  * @return Whether this future already contains a failure result
		  */
		def containsFailure = f.isCompleted && waitForResult().isFailure
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