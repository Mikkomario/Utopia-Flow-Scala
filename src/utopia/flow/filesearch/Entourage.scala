package utopia.flow.filesearch
import java.nio.file.Path

import utopia.flow.async.{Volatile, VolatileOption}
import utopia.flow.util.WaitTarget.UntilNotified
import utopia.flow.util.{SingleWait, WaitUtils}

import scala.concurrent.ExecutionContext

/**
 * A group of miners that can then split into individual workers
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
// TODO: Gather common features (like moving in mines) to a single trait
class Entourage(override val origin: Mine, size: Int, val searchStyle: SearchStyle)
			   (val searchCondition: Path => Boolean) extends Explorer
{
	// ATTRIBUTES	-------------------
	
	private val deployedMinerCount = Volatile(0)
	private val outOfMinersWait: VolatileOption[SingleWait] = VolatileOption()
	
	
	// IMPLEMENTED	-------------------
	
	// Moves forward in the mines until a split is found
	override def explore()(implicit exc: ExecutionContext) = ???
	
	
	// OTHER	-----------------------
	
	private def move()(implicit exc: ExecutionContext) =
	{
		// Finds a passage that hasn't been started yet
		if (findUnexploredRoot())
		{
			// Checks whether this entourage should split or move on as a single unit
			// In case of a dead-end, leaves a single miner behind while the rest backtrack
			val pathsToExplore = currentLocation.numberOfExplorablePaths // TODO: Request paths instead
			if (pathsToExplore == 0)
			{
				deploySingleLocationMiner()
				backtrack()
			}
			// If there's only a single way to go, the entourage moves there
			else if (pathsToExplore == 1)
				goDeeper() // TODO: Specify which mine
			// if there are multiple possible pathways, splits the entourage between the pathways
			else
			{
				// Reserves all remaining miners
				val minersToSend = deployedMinerCount.getAndSet(0)
				
				// There may be too few miners to send between different paths, in which case the entourage will
				// deploy miners as they become available
				if (minersToSend < pathsToExplore)
				{
					// TODO: Deploy
				}
				// If there are more miners than pathways, deploys them as sub-groups (where available)
				else
				{
					val minMinersPerPath = minersToSend / pathsToExplore
					var remainingExtraMiners = minersToSend % pathsToExplore
					
					// TODO: If some other miner started the mine already, miners will return immediately
					
					// TODO: Implement
				}
			}
		}
	}
	
	private def deploySingleLocationMiner()(implicit exc: ExecutionContext) =
	{
		val outOfMiners = deployedMinerCount.updateAndGet { _ - 1 } == 0
		val preparedWait = if (outOfMiners) Some(prepareWait()) else None
		new Miner(currentLocation, searchStyle)(searchCondition).mineCurrentLocation().foreach { _ =>
			deployedMinerCount.update { _ + 1 }
			// TODO: Collect results
			endWait()
		}
		
		// If ran out of miners, has to wait until one of them completes
		preparedWait.foreach(waitUntilMinerReturns)
	}
	
	private def prepareWait() = outOfMinersWait.setOneIfEmptyAndGet { () => new SingleWait(UntilNotified) }
	
	private def waitUntilMinerReturns(wait: SingleWait) = wait.run()
	
	private def endWait() = outOfMinersWait.pop().foreach { _.stop() }
}
