package utopia.flow.filesearch
import scala.concurrent.ExecutionContext

/**
 * A group of miners that can then split into individual workers
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
// TODO: Gather common features (like moving in mines) to a single trait
class Entourage(override val origin: Mine, size: Int) extends Explorer
{
	// IMPLEMENTED	-------------------
	
	// Moves forward in the mines until a split is found
	override def explore()(implicit exc: ExecutionContext) = ???
	
	
	// OTHER	-----------------------
	
	private def move() =
	{
		// If current mine shaft has been completed, backtracks to a previous mine
		if (currentLocation.status.isCompleted)
			backtrack()
		else
		{
			// Checks whether this entourage should split or move on as a single unit
			// In case of a dead-end, leaves a single miner behind while the rest backtrack
			if (currentLocation.isDeadEnd)
			{
			
			}
		}
	}
}
