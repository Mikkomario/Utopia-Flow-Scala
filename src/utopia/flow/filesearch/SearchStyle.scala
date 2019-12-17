package utopia.flow.filesearch

import utopia.flow.filesearch.PathType.{Directory, File}

/**
 * Common trait for different search target types
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
sealed trait SearchStyle
{
	/**
	 * @return The type of path used when searching for a result
	 */
	def conditionType: PathType
	/**
	 * @return The type of path returned when returning a result
	 */
	def resultType: PathType
}

object SearchStyle
{
	/**
	 * Searches for regular files
	 */
	case object SearchForFile extends SearchStyle
	{
		override def conditionType = File
		override def resultType = File
	}
	
	/**
	 * Searches for directories based on a directory condition
	 */
	case object SearchForDirectory extends SearchStyle
	{
		override def conditionType = Directory
		override def resultType = Directory
	}
	
	/**
	 * Searches for a directory that contains a specific file
	 */
	case object SearchForFileDirectory extends SearchStyle
	{
		override def conditionType = File
		override def resultType = Directory
	}
}
