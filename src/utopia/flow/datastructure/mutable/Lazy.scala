package utopia.flow.datastructure.mutable

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
    
    override def toString() = current.map(c => s"Lazy($c)") getOrElse "Lazy"
    
    
    // OTHER    ---------------------
    
    /**
     * Resets this container so that the next time #get is called, a new item will be generated
     */
    def reset() = item = None
}