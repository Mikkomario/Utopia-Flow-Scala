package utopia.flow.caching

import java.time.Instant

import utopia.flow.util.TimeExtensions._
import utopia.flow.util.WaitUtils

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
  * This single cache expires its data after a while
  * @author Mikko Hilpinen
  * @since 7.6.2019, v1.5+
  */
trait ExpiringSingleCache[A] extends SingleCache[A]
{
	// ATTRIBUTES	----------------
	
	private var dataCacheTime: Option[Instant] = None
	private var autoClearFuture: Option[Future[Unit]] = None
	
	
	// ABSTRACT	--------------------
	
	/**
	  * How long data will be cached before it is considered to be expired
	  */
	protected val cacheDuration: FiniteDuration
	
	/**
	  * Whether data should be cleared as soon as it expires (true) or only cleared on requests (false)
	  */
	protected val autoClearIsUsed: Boolean
	
	
	// IMPLEMENTED	----------------
	
	override def synchronous(implicit context: ExecutionContext) = handleExpiration[Try[A]](super.synchronous, (r, f) => f(r))
	
	override def future(implicit context: ExecutionContext) = handleExpiration[Future[Try[A]]](
		super.future, (future, func) => future.foreach(func))
	
	override def clear() =
	{
		dataCacheTime = None
		super.clear()
	}
	
	
	// OTHER	------------------------
	
	private def handleExpiration[B](getResult: => B, callFinalize: (B, Try[A] => Unit) => Unit)
								   (implicit context: ExecutionContext) =
	{
		// Checks if result has already expired
		if (!autoClearIsUsed && dataCacheTime.exists { _ < Instant.now() - cacheDuration })
			clear()
		
		// If a successful result was cached, sets data cache time
		val result = getResult
		callFinalize(result, r =>
		{
			if (dataCacheTime.isEmpty && r.isSuccess)
			{
				dataCacheTime = Some(Instant.now())
				
				// On auto-clear, waits cache duration and then clears this cache (only one wait at a time)
				if (autoClearIsUsed && autoClearFuture.isEmpty)
				{
					autoClearFuture = Some(Future
					{
						WaitUtils.wait(cacheDuration, new AnyRef())
						autoClearFuture = None
						clear()
					})
				}
			}
		})
		
		result
	}
}
