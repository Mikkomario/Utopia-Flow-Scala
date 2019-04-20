package utopia.flow.datastructure.mutable

object Pointer
{
    /**
     * Creates a new pointer for value
     */
    def apply[T](value: T) = new Pointer(value)    
}

/**
* This is a simple structure for holding a single mutable value
* @author Mikko Hilpinen
* @since 23.3.2019
**/
class Pointer[T](var value: T)
{
    // COMPUTED    -------------------------
    
    /**
     * The current value in this pointer
     */
    def get = value
    
    
    // IMPLEMENTED    ----------------------
    
	override def toString() = value.toString()
	
	
	// OTHER    ------------------------
	
	/**
	 * Updates the value in this pointer
	 */
	def set(newVal: T) = value = newVal
	
	/**
	 * Whether this pointer contains the specified value
	 */
	def contains(item: Any) = value == item
}