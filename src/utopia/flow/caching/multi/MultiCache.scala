package utopia.flow.caching.multi

import utopia.flow.caching.single.SingleCacheLike

import scala.collection.mutable

object MultiCache
{
	/**
	  * Creates a new multi cache
	  * @param makeCache A function for producing new caches
	  * @tparam Key The type of cache key
	  * @tparam Value The type of cache value
	  * @tparam Part The type of cache used to hold the values
	  * @return A new multi cache
	  */
	def apply[Key <: AnyRef, Value, Part <: SingleCacheLike[Value]](makeCache: Key => Part) =
		new MultiCache[Key, Value, Part](makeCache)
}

/**
  * This cache uses multiple single caches for caching its data
  * @author Mikko Hilpinen
  * @since 12.6.2019, v1.5+
  */
class MultiCache[Key <: AnyRef, +Value, Part <: SingleCacheLike[Value]](private val makeCache: Key => Part)
	extends MultiCacheLike[Key, Value, Part]
{
	// ATTRIBUTES	-------------------
	
	private val caches: mutable.Map[Key, Part] = mutable.HashMap()
	
	
	// IMPLEMENTED	-------------------
	
	override protected def cacheForKey(key: Key) = caches.getOrElseUpdate(key, makeCache(key))
}
