package utopia.flow.filesearch

import scala.concurrent.{ExecutionContext, Future}

import utopia.flow.util.CollectionExtensions._

/**
 * Explorers can traverse in the mines
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
trait Explorer
{
	// ABSTRACT	------------------------
	
	val origin: Mine
	
	def explore()(implicit exc: ExecutionContext): Future[Unit]
	
	
	// ATTRIBUTES	--------------------
	
	private var currentRoute: Vector[Mine] = Vector()
	
	
	// COMPUTED	------------------------
	
	def currentLocation = currentRoute.lastOption.getOrElse(origin)
	
	def isAtOrigin = currentRoute.isEmpty
	
	
	// OTHER	-----------------------
	
	protected def backtrack() =
	{
		if (isAtOrigin)
			false
		else
		{
			currentRoute = currentRoute.dropRight(1)
			true
		}
	}
	
	protected def findDeadEnd() =
	{
		// May need to backtrack a little first
		if (findUnexploredRoot())
		{
			// Traverses deeper into the mine, preferring routes that haven't been explored or started yet
			while (goDeeper()) {  }
			!currentLocation.status.isStarted
		}
		else
			false
	}
	
	private def goDeeper() =
	{
		// Finds the next path, preferring those that haven't been explored yet
		currentLocation.pathWays.filter { _.hasUncheckedPaths }.bestMatch(
			Vector(!_.status.isTraversed, !_.status.isStarted, !_.status.hasResults)).headOption match
		{
			case Some(nextPath) =>
				nextPath.declareTraversed()
				currentRoute :+= nextPath
				true
			case None => false
		}
	}
	
	private def findUnexploredRoot() =
	{
		// Backtracks to an incomplete passage
		while (currentLocation.status.isStarted && backtrack()) {
			// Condition moves this explorer
		}
		currentLocation.status.isStarted
	}
}
