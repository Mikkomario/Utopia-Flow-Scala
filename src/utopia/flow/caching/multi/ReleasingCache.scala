package utopia.flow.caching.multi

import scala.collection.immutable.HashMap
import scala.ref.WeakReference

object ReleasingCache
{
	/**
	  * Creates a new releasing cache
	  * @param cache An expiring cache used for requesting and releasing values
	  * @tparam Key The type of cache key
	  * @tparam Value The type of cache value
	  * @return A cache that releases references but keeps weak references
	  */
	def apply[Key, Value <: AnyRef](cache: ExpiringCacheLike[Key, Value]): ReleasingCache[Key, Value] =
		new ReleasingCacheImpl[Key, Value](cache)
}

/**
  * This cache releases its references after a while
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ReleasingCache[Key, Value <: AnyRef] extends CacheLike[Key, Value]
{
	// ATTRIBUTES	---------------
	
	private var weakRefs: Map[Key, WeakReference[Value]] = HashMap()
	
	
	// ABSTRACT	-------------------
	
	/**
	  * @return The cache that provides the values for this cache
	  */
	protected def source: ExpiringCacheLike[Key, Value]
	
	
	// IMPLEMENTED	---------------
	
	override def cached(key: Key) = source.cached(key) orElse weakRefs.get(key).flatMap { _.get }
	
	override def apply(key: Key) =
	{
		// Expires old values first
		source.clearExpiredData()
		
		// Tries to use a cached or a weakly cached value
		cached(key).getOrElse
		{
			// But may have to request a new value
			val newValue = source(key)
			weakRefs += (key -> WeakReference(newValue))
			newValue
		}
	}
}

private class ReleasingCacheImpl[Key, Value <: AnyRef](protected val source: ExpiringCacheLike[Key, Value])
	extends ReleasingCache[Key, Value]