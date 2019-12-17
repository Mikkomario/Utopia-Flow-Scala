package utopia.flow.filesearch

/**
 * Common trait that determines the role of a file (directory or a regular file)
 * @author Mikko Hilpinen
 * @since 17.12.2019, v1.6.1+
 */
sealed trait PathType

object PathType
{
	/**
	 * Type for paths that represent regular files
	 */
	case object File extends PathType
	
	/**
	 * Type for paths that represent directories
	 */
	case object Directory extends PathType
	
	/**
	 * All introduced path types
	 */
	val values = Vector(File, Directory)
}
