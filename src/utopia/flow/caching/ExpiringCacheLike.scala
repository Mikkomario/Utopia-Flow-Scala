package utopia.flow.caching

/**
  * This cache holds values which expire after a while
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ExpiringCacheLike[-Key, +Value] extends CacheLike[Key, Value]
{
	// ABSTRACT	--------------------
	
	/**
	  * @return The currently used caches
	  */
	protected def currentCaches: Traversable[ExpiringSingleCacheLike[Value]]
	
	/**
	  * @param key A key
	  * @return A suitable cache for the key
	  */
	protected def cacheForKey(key: Key): ExpiringSingleCacheLike[Value]
	
	
	// IMPLEMENTED	----------------
	
	override def apply(key: Key) =
	{
		// Clears expired data
		clearExpiredData()
		
		// Retrieves current data
		cacheForKey(key)()
	}
	
	
	// OTHER	---------------------
	
	/**
	  * Clears all expired values from this cache
	  */
	def clearExpiredData() = currentCaches.foreach { _.clearIfExpired() }
}
