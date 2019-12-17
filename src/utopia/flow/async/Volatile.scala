package utopia.flow.async

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
     * Safely updates the value in this container, then returns it
     */
    def updateAndGet(mutate: T => T) = this.synchronized
    {
        value = mutate(value)
        value
    }
    
    /**
     * Updates this volatile only if specified condition is met
     * @param condition A condition for updating
     * @param mutate A mutating function
     */
    def updateIf(condition: T => Boolean)(mutate: T => T) = this.synchronized
    {
        if (condition(value))
            value = mutate(value)
    }
    
    /**
     * Updates this volatile only if specified condition is met
     * @param condition A condition for updating
     * @param mutate A mutating function
     * @return Value of this volatile after operation
     */
    def updateIfAndGet(condition: T => Boolean)(mutate: T => T) = this.synchronized
    {
        if (condition(value))
            value = mutate(value)
        value
    }
    
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
      * @tparam U The result type of the operation
      * @return the result of the operation
     */
    def lock[U](operation: T => U) = this.synchronized { operation(value) }
    
    /**
     * Reads the current value of this volatile container and then changes it
     * @param newValue the new value for this volatile container
     * @return the value before the assignment
     */
    def getAndSet(newValue: T) = pop { v => v -> newValue }
}