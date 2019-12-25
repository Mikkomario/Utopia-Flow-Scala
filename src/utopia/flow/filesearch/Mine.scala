package utopia.flow.filesearch

import java.nio.file.Path

import utopia.flow.util.FileExtensions._
import utopia.flow.async.{Volatile, VolatileLazy}
import utopia.flow.filesearch.ExcavationStatus.{Finished, Passed, Started, Unexplored}

/**
 * Represents a portion of a file system with a mutable search status
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 * @param directory Directory represented by this mine
 * @tparam R Type of results collected from this mine
 */
class Mine[R](val directory: Path)
{
	// ATTRIBUTES	--------------------
	
	private val _status: Volatile[ExcavationStatus] = Volatile(Unexplored)
	// Underlying pathways are lazily initialized
	private val _pathWays = VolatileLazy { directory.subDirectories.getOrElse(Vector()).map { new Mine[R](_) } }
	private var foundResults: Option[R] = None
	
	
	// COMPUTED	------------------------
	
	/**
	 * @return Current status of this mine
	 */
	def status = _status.get
	// TODO: Possibly add events
	def status_=(newStatus: ExcavationStatus) = _status.set(newStatus)
	
	/**
	 * @return Pathways under this mine
	 */
	def pathWays = _pathWays.get
	
	/**
	 * @return Whether this mineshaft diverges into multiple paths
	 */
	def diverges = pathWays.size > 1
	
	/**
	 * @return Whether this is the end of this mineshaft
	 */
	def isDeadEnd = pathWays.isEmpty
	
	/**
	 * @return Paths directly under this mine which haven't been explored yet
	 */
	def unexploredPaths = pathWays.filter { _.isExplorable }
	
	/*
	 * @return Number of explorable paths directly under this pathway
	 */
	// def numberOfExplorablePaths = pathWays.count { _.isExplorable }
	
	/**
	 * @return Whether this mineshaft has any room left to explore
	 */
	def isExplorable: Boolean =
	{
		if (status.isStarted)
			false
		else if (status.isTraversed)
			isDeadEnd || pathWays.exists { _.isExplorable }
		else
			true
	}
	
	
	// OTHER	-------------------------
	
	/**
	 * Declares that this mineshaft has been entered
	 */
	def declareTraversed() = _status.update { current => if (!current.isTraversed) Passed else current }
	
	/**
	 * Declares that this mineshaft's exploration / mining has started
	 */
	def declareStarted() = _status.update { current => if (!current.isStarted) Started else current }
	
	/**
	 * Declares this mineshaft's exploration completed
	 */
	def declareCompleted(results: R) =
	{
		_status.set(Finished)
		foundResults = Some(results)
	}
}
