package utopia.flow.async

import utopia.flow.util.CollectionExtensions._

object VolatileList
{
    /**
     * Creates a new empty list
     */
    def apply[T]() = new VolatileList[T](Vector())
    
    /**
     * Creates a new list with a single item
     */
    def apply[T](item: T) = new VolatileList[T](Vector(item))
    
    /**
     * Creates a new list with multiple items
     */
    def apply[T](first: T, second: T, more: T*) = new VolatileList(Vector(first, second) ++ more)
}

/**
* VolatileList is a mutable list that handles items in a thread-safe manner
* @author Mikko Hilpinen
* @since 28.3.2019
**/
class VolatileList[T](list: Vector[T]) extends Volatile(list) with Seq[T]
{
    // IMPLEMENTED    ---------------
    
	def iterator = get.iterator
	
	def apply(idx: Int) = get(idx)
	
	def length: Int = get.length
	
	
	// OPERATORS    ----------------
	
	/**
	 * Adds a new item to the end of this list
	 */
	def :+=(item: T) = update { _ :+ item }
	
	/**
	 * Adds a new item to the beginning of this list
	 */
	def +:=(item: T) = update { _.+:(item) }
	
	/**
	 * Adds multiple new items to this list
	 */
	def ++=(items: TraversableOnce[T]) = update { _ ++ items }
	
	/**
	 * Adds multiple new items to this list
	 */
	def ++=(first: T, second: T, more: T*): Unit = ++=(Vector(first, second) ++ more)
	
	/**
	 * Removes an item from this list
	 */
	def -=(item: Any) = update { _ filterNot { _ == item } }
	
	/**
	 * Removes multiple items from this list
	 */
	def --=(items: Traversable[Any]) = update { _ filterNot { my => items.exists(_ == my) } }
	
	
	// OTHER    --------------------
	
	/**
	 * Clears all items from this list
	 */
	def clear() = set(Vector())
	
	/**
	 * Removes and returns the first item in this list
	 */
	def pop(): Option[T] = pop { v => v.headOption -> v.drop(1) }
	
	/**
	 * Removes and returns the first item in this list that satisfies the provided predicate
	 */
	def popFirst(find: T => Boolean) = pop 
	{
	    items => items.indexWhereOption(find).map 
	    {
	        index => Some(items(index)) -> (items.take(index) ++ items.drop(index + 1))
	    
	    }.getOrElse(None -> items)
	}
}