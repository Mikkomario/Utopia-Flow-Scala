package utopia.flow.datastructure.mutable

object Lazy
{
    /**
      * Creates a new lazy
      * @param make A function that makes the value (call by name)
      * @tparam T The type of cached item
      * @return A new lazy container
      */
    def apply[T](make: => T) = new Lazy(() => make)
}

/**
* This is a mutable version of a lazy variable, meaning that the value may be changed and reset. 
* The lazy caches the results of a function on the first call and after calls following a reset.
* @author Mikko Hilpinen
* @since 26.2.2019
**/
class Lazy[T](val generator: () => T)
{
	// ATTRIBUTES    ----------------
    
    private var item: Option[T] = None
    
    
    // COMPUTED    ------------------
    
    /**
     * The currently held item (None if not initialized yet)
     */
    def current = item
    
    /**
     * The item in this lazy container, either cached or generated
     */
    def get = 
    {
        if (current.isDefined)
            current.get
        else
        {
            val newItem = generator()
            item = Some(newItem)
            newItem
        }
    }
    
    
    // IMPLEMENTED    ---------------
    
    override def toString = current.map(c => s"Lazy($c)") getOrElse "Lazy"
    
    
    // OTHER    ---------------------
    
    /**
     * Updates the value in this container
     */
    @deprecated("Use of this method is discouraged since it may be removed in a future release", "v1.5")
    def set(newValue: T) = item = Some(newValue)
    
    /**
     * Resets this container so that the next time #get is called, a new item will be generated
     */
    def reset() = item = None
}