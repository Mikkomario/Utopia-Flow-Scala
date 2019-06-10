package utopia.flow.caching

import scala.collection.immutable.HashMap

/**
  * This is a simple implementation of the CacheLike trait
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
class Cache[Key, Value](private val request: Key => Value) extends CacheLike[Key, Value]
{
	// ATTRIBUTES	------------------
	
	private var cachedItems: Map[Key, Value] = HashMap()
	
	
	// IMPLEMENTED	------------------
	
	override def apply(key: Key) =
	{
		if (cachedItems.contains(key))
			cachedItems(key)
		else
		{
			val value = request()
			cachedItems += key -> value
			value
		}
	}
	
	override def cached(key: Key) = cachedItems.get(key)
}
