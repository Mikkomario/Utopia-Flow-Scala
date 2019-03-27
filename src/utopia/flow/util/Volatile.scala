package utopia.flow.util

object Volatile
{
    /**
     * Creates a new volatile value
     */
    def apply[T](value: T) = new Volatile(value)    
}

/**
* This class wraps a value that may be changed from multiple threads. The class itself is 
* mutable, but should only be used with types that have value semantics.
* @author Mikko Hilpinen
* @since 27.3.2019
**/
class Volatile[T](@volatile private var value: T)
{
    // COMPUTED    -----------------
    
    /**
     * The current value of this volatile container
     */
    def get = this.synchronized { value }
    
    
    // OTHER    --------------------
    
    /**
     * Sets a new value to this container
     */
    def set(newValue: T) = this.synchronized { value = newValue }
    
    /**
     * Safely updates the value in this container
     */
    def update(mutate: T => T) = this.synchronized { value = mutate(value) }
    
    /**
     * Updates a value in this container. Returns the state before the update.
     */
    def takeAndUpdate[B](taker: T => B)(updater: T => T) = this.synchronized
    {
        val result = taker(value)
        value = updater(value)
        result
    }
    
    /**
     * Updates a value in this container. Also returns a result value.
     */
    def pop[B](mutate: T => (B, T)) = this.synchronized
    {
        val (result, next) = mutate(value)
        value = next
        result
    }
    
    /**
     * Locks the value in this container from outside sources during the operation. Use with caution.
     */
    def lock(operation: T => Unit) = this.synchronized { operation(value) }
}