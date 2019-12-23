package utopia.flow.filesearch

import java.nio.file.Path

import utopia.flow.util.CollectionExtensions._
import utopia.flow.util.FileExtensions._
import utopia.flow.filesearch.PathType.{Directory, File}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Used for searching for files or directories in a file system
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
class Miner(override val origin: Mine, val searchStyle: SearchStyle)(val searchCondition: Path => Boolean) extends Explorer
{
	// ATTRIBUTES	---------------------
	
	private var foundResults: Vector[Path] = Vector()
	
	
	// IMPLEMENTED	---------------------
	
	override def explore()(implicit exc: ExecutionContext) =
	{
		Future
		{
			// Traverses forward until a suitable dead-end is found
			while (findDeadEnd())
			{
				// Mines the found location
				currentLocation.declareStarted()
				mine()
			}
			
			// Returns once the whole passage has been completed
		}
	}
	
	
	// OTHER	------------------------
	
	/**
	 * Mines the current location (asynchronously)
	 * @param exc Implicit execution context
	 * @return Asynchronous completion of the mining operation
	 */
	def mineCurrentLocation()(implicit exc: ExecutionContext) =
	{
		currentLocation.declareStarted()
		Future { mine() }
	}
	
	private def mine() =
	{
		// Either checks the directory itself or the files under the directory
		searchStyle.conditionType match
		{
			case Directory =>
				val wasSuccess = searchCondition(currentLocation.directory)
				currentLocation.declareCompleted(wasSuccess)
				foundResults :+= currentLocation.directory
			case File =>
				val files = currentLocation.directory.children.getOrElse(Vector())
				// Finds the first search result
				files.indexWhereOption(searchCondition) match
				{
					case Some(firstResultIndex) =>
						searchStyle.resultType match
						{
							// If a directory is returned, doesn't need to check the other files
							case Directory =>
								currentLocation.declareCompleted(true)
								foundResults :+= currentLocation.directory
							case File =>
								currentLocation.declarePromising()
								foundResults :+= files(firstResultIndex)
								foundResults ++= files.drop(firstResultIndex + 1).filter(searchCondition)
								currentLocation.declareCompleted(true)
						}
					case None => currentLocation.declareCompleted(false)
				}
		}
	}
}
