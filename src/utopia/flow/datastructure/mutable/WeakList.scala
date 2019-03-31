package utopia.flow.datastructure.mutable

import scala.ref.WeakReference
import scala.collection.mutable.Traversable

/**
* This mutable list only contains weak references to its items, which means that the items 
* will be removed once no other object is referencing them. This version of the class is mutable 
* and shouldn't be passed around.
* @author Mikko Hilpinen
* @since 31.3.2019
**/
class WeakList[T <: AnyRef](initialItems: TraversableOnce[T]) extends Traversable[T]
{
	// ATTRIBUTES    ------------------
    
    private var items = initialItems.map { WeakReference(_) }.toVector
    
    
    // COMPUTED    --------------------
    
    /**
     * A strongly referenced vector based on this weak list
     */
    def strong = updateItems.flatMap { _.get }
    
    private def filtered = items.filter { _.get.isDefined }
    
    private def updateItems = 
    {
        items = filtered
        items
    }
    
    
    // IMPLEMENTED    -----------------
    
    def foreach[U](f: T => U) = items.foreach { _.get.foreach(f) }
    
    
    // OTHER    -----------------------
    
    /**
     * Adds a new item to this list
     */
    def +=(item: T) = items = filtered :+ WeakReference(item)
    
    /**
     * Adds multiple new items to this list
     */
    def ++=(newItems: TraversableOnce[T]) = items = filtered ++ newItems.map { WeakReference(_) }
    
    /**
     * Removes an item from this list
     */
    def -=(item: Any) = items = items.filter { _.get.forall { _ != item } }
    
    /**
     * Clears all items from this list
     */
    def clear() = items = Vector()
}