package utopia.flow.util

/**
* Generators generate new values and are supposed to be able to generate unlimited amount of items
* @author Mikko Hilpinen
* @since 12.5.2018
**/
trait Generator[T]
{
    // ABSTRACT    ------------------
    
    /**
     * Generates a new value
     */
	def next(): T
	
	
	// OTHER METHODS    -------------
	
	/**
	 *  An infinite iterator for this generator
	 */
	def iterator = Iterator.continually(next)
}