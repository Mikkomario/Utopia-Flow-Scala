package utopia.flow.caching

import java.time.Instant

import utopia.flow.util.TimeExtensions._

import scala.concurrent.duration.FiniteDuration

/**
  * This cache's value expires after a while, after which a new value will be requested
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ExpiringSingleCache[A] extends ExpiringSingleCacheLike[A]
{
	// ATTRIBUTES	---------------
	
	private var dataOriginTime: Option[Instant] = None
	
	
	// ABSTRACT	-------------------
	
	/**
	  * @return The cache used for holding the value of this cache
	  */
	protected def cache: ClearableSingleCacheLike[A]
	
	/**
	  * The duration which an item is cached
	  */
	protected val cacheDuration: FiniteDuration
	
	
	// IMPLEMENTED	---------------
	
	def isDataExpired = dataOriginTime.exists { _ < Instant.now() - cacheDuration }
	
	override def apply() =
	{
		// May clear the underlying cache
		clearIfExpired()
		
		if (!cache.isValueCached)
			dataOriginTime = Some(Instant.now())
		
		cache()
	}
	
	def clearIfExpired() = if (isDataExpired) cache.clear()
}
