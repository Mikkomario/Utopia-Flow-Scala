package utopia.flow.async

/**
* A volatile flag is used for marking singular events (flags) in a multi-threaded environment
* @author Mikko Hilpinen
* @since 28.3.2019
**/
class VolatileFlag extends Volatile[Boolean](false)
{
    // COMPUTED    ---------------
    
    /**
     * Whether this flag is currently set
     */
    def isSet = get
    
    
	// OTHER    ------------------
    
    /**
     * Sets this flag (same as set(true))
     */
    def set(): Unit = set(true)
    
    /**
     * Resets this flag (same as set(false))
     */
    def reset() = set(false)
    
    /**
     * Sets this flag and also returns the state before conversion
     */
    def getAndSet(): Boolean = getAndSet(true)
    
    /**
     * If this flag is not set, performs the operation. Locks this flag while the operation runs.
     */
    def doIfNotSet[U](action: => U) = lock { status => if (!status) action }
    
    /**
     * If this flag is not set, performs the operation and then sets this flag. 
     * Locks this flag during the operation.
     */
    def runAndSet[U](action: => U) = update
    {
        status => 
            if (!status)
                action
            true
    }
}