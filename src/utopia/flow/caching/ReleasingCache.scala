package utopia.flow.caching

import scala.collection.immutable.HashMap
import scala.ref.WeakReference

/**
  * This cache releases its references after a while
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ReleasingCache[Key, Value] extends CacheLike[Key, Value]
{
	// ATTRIBUTES	---------------
	
	private var weakRefs: Map[Key, WeakReference[Value]] = HashMap()
	
	
	// ABSTRACT	-------------------
	
	/**
	  * @return The cache that provides the values for this cache
	  */
	protected def source: ExpiringCacheLike[Key, Value]
	
	
	// IMPLEMENTED	---------------
	
	override def apply(key: Key) =
	{
		// Expires old values first
		source.clearExpiredData()
		
		// Tries to use a cached or a weakly cached value
		val available = source.cached(key) orElse weakRefs.get(key).flatMap { _.get }
		if (available.isDefined)
			available.get
		else
		{
			// But may have to request a new value
			val newValue = source(key)
			weakRefs += key -> WeakReference(newValue)
			newValue
		}
	}
}
