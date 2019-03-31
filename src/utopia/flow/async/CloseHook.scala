package utopia.flow.async

import java.time.Duration

/**
* This object stops some operations before jvm closes
* @author Mikko Hilpinen
* @since 31.3.2019
**/
object CloseHook
{
	// ATTRIBUTES    ----------------
    
    var maxShutdownTime = Duration.ofSeconds(5)
    
    private val additionalShutdowntime = Duration.ofMillis(200)
    // private var loops = WeakList
}