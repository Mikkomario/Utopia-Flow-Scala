package utopia.flow.caching

/**
  * This is a template trait for expiring single caches
  * @author Mikko Hilpinen
  * @since 10.6.2019, v1.5+
  */
trait ExpiringSingleCacheLike[+A] extends SingleCacheLike[A]
{
	/**
	  * @return Whether the currently held data has already expired
	  */
	def isDataExpired: Boolean
	
	/**
	  * Clears the data in this cache if it has expired already
	  */
	def clearIfExpired(): Unit
}
