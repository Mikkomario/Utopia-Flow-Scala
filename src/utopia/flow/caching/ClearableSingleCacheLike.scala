package utopia.flow.caching

/**
  * This cache may be cleared
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ClearableSingleCacheLike[+A] extends SingleCacheLike[A]
{
	// ABSTRACT	-----------------
	
	/**
	  * Clears this cache, which means that a new value will be requested afterwards
	  */
	def clear(): Unit
}
