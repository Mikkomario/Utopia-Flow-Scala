package utopia.flow.caching

/**
  * This single item cache may be cleared
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
class ClearableSingleCache[A](private val request: () => A) extends ClearableSingleCacheLike[A]
{
	// ATTRIBUTES	---------------
	
	private var _cached: Option[A] = None
	
	
	// IMPLEMENTED	---------------
	
	override def cached = _cached
	
	override def clear() = _cached = None
	
	override def apply() = if (isValueCached) cached.get else
	{
		val value = request()
		_cached = Some(value)
		value
	}
}
