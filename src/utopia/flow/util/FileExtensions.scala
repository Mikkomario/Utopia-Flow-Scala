package utopia.flow.util

import java.nio.file.{DirectoryNotEmptyException, Files, Path, Paths, StandardCopyOption}

import scala.language.implicitConversions
import utopia.flow.util.StringExtensions._
import utopia.flow.util.CollectionExtensions._
import utopia.flow.util.AutoClose._
import utopia.flow.util.NullSafe._

import scala.util.{Failure, Success, Try}

/**
 * Provides some extensions to be used with java.nio.file classes
 * @author Mikko Hilpinen
 * @since 17.11.2019, v1+
 */
object FileExtensions
{
	/**
	 * Converts a string to a path
	 */
	implicit def stringToPath(pathString: String): Path = Paths.get(pathString)
	
	implicit class RichPath(val p: Path) extends AnyVal
	{
		/**
		 * @return Whether this file exists in the file system (false if undetermined)
		 */
		def exists = Files.exists(p)
		
		/**
		 * @return Whether this file doesn't exist in the file system (false if undetermined)
		 */
		def notExists = Files.notExists(p)
		
		/**
		 * @return File name portion of this path
		 */
		def fileName = p.getFileName.toOption.map { _.toString }.getOrElse("")
		
		/**
		 * @return The last modified time of this file (may fail)
		 */
		def lastModified = Try { Files.getLastModifiedTime(p).toInstant }
		
		/**
		 * @return the type of this file (portion after last '.'). Returns an empty string for directories and
		 *         for files without type.
		 */
		def fileType = fileName.afterLast(".")
		
		/**
		 * @param another A sub-path
		 * @return This path extended with another path
		 */
		def /(another: Path) = p.resolve(another)
		
		/**
		 * @param another A sub-path
		 * @return This path extended with another path
		 */
		def /(another: String) = p.resolve(another)
		
		/**
		 * @return An absolute path based on this path (if this path is already absolute, returns this)
		 */
		def absolute = p.toAbsolutePath
		
		/**
		 * @return Whether this path represents an existing directory
		 */
		def isDirectory = Files.isDirectory(p)
		
		/**
		 * @return Whether this path represents an existing regular file (non-directory)
		 */
		def isRegularFile = Files.isRegularFile(p)
		
		/**
		 * @return A parent path for this path. None if this path is already a root path
		 */
		def parentOption = p.getParent.toOption
		
		/**
		 * @return A parent path for this path. Return this path if already a root path
		 */
		def parent = parentOption.getOrElse(p)
		
		/**
		 * @return All children (files and directories) directly under this directory (empty vector if not directory). May fail.
		 */
		def children =
		{
			// Non-directory paths don't have children
			if (isDirectory)
				Files.list(p).tryConsume { _.collect(new VectorCollector[Path]) }
			else
				Success(Vector())
		}
		
		/**
		 * Performs an operation on all files directly under this path
		 * @param filter A filter applied to child paths (default = no filter)
		 * @param operation Operation performed for each path
		 * @return A try that may contain a failure if this operation failed
		 */
		def forChildren(filter: Path => Boolean = _ => true)(operation: Path => Unit): Try[Unit] =
		{
			if (isDirectory)
				Files.list(p).tryConsume { _.filter(p => filter(p)).forEach(p => operation(p)) }
			else
				Success(Unit)
		}
		
		/**
		 * Merges values of child paths into a single value
		 * @param start Starting value
		 * @param filter A filter applied to the childre (default = no filtering)
		 * @param f A folding function
		 * @tparam A Type of fold result
		 * @return Fold result. May contain a failure.
		 */
		def foldChildren[A](start: A, filter: Path => Boolean = _ => true)(f: (A, Path) => A) =
		{
			if (isDirectory)
			{
				Files.list(p).tryConsume { stream =>
					var result = start
					stream.filter(p => filter(p)).forEach(p => result = f(result, p))
					result
				}
			}
			else
				Success(start)
		}
		
		/**
		 * Moves this file / directory to another directory
		 * @param targetDirectory Target parent directory for this file
		 * @param replaceIfExists Whether a file already existing at target path should be replaced with this one,
		 *                        if present (default = true)
		 * @return Link to the target path. Failure if file moving failed or if couldn't replace an existing file
		 */
		def moveTo(targetDirectory: Path, replaceIfExists: Boolean = true) = Try
		{
			val newLocation = targetDirectory/fileName
			if (replaceIfExists)
				Files.move(p, newLocation, StandardCopyOption.REPLACE_EXISTING)
			else
				Files.move(p, newLocation)
		}
		
		/**
		 * Moves this file / directory to another directory
		 * @param targetDirectory Target parent directory for this file
		 * @param replaceIfExists Whether a file already existing at target path should be replaced with this one,
		 *                        if present (default = true)
		 * @return Link to the target path. Failure if file moving failed or if couldn't replace an existing file
		 */
		def copyTo(targetDirectory: Path, replaceIfExists: Boolean = true) = Try
		{
			val newLocation = targetDirectory/fileName
			if (replaceIfExists)
				Files.copy(p, newLocation, StandardCopyOption.REPLACE_EXISTING)
			else
				Files.copy(p, newLocation)
		}
		
		/**
		 * Renames this file or directory
		 * @param newFileName New name for this file or directory (just file name, not the full path)
		 * @return Path to the newly named file. Failure if renaming failed.
		 */
		def rename(newFileName: String) = Try { Files.move(p, p.parentOption.map { _/newFileName }.getOrElse(newFileName),
			StandardCopyOption.REPLACE_EXISTING) }
		
		/**
		 * Overwrites this path with file from another path
		 * @param anotherPath A path leading to the file that will overwrite this one
		 * @return Path to this file. May contain failure.
		 */
		def overwriteWith(anotherPath: Path) = Try { Files.copy(anotherPath, p, StandardCopyOption.REPLACE_EXISTING) }
		
		/**
		 * Deletes this file or directory
		 * @param allowDeletionOfDirectoryContents Whether deletion of a non-empty directory should be allowed
		 *                                         (resulting in deletion of all files under it) (default = true)
		 * @return Whether any files were deleted (false if this file didn't exist).
		 *         May contain a failure if some of the files couldn't be deleted.
		 */
		def delete(allowDeletionOfDirectoryContents: Boolean = true): Try[Boolean] = {
			// In case of a directory, may need to clear contents first
			if (isDirectory)
			{
				// If any of child deletion fails, the whole process is interrupted
				if (allowDeletionOfDirectoryContents)
					children.flatMap { _.findMap { c => Some(c.delete()).filter { _.isFailure } }.getOrElse(Success(true)) }
						.flatMap { _ => Try { Files.deleteIfExists(p) }} // Deletes this directory once empty
				else
					Failure(new DirectoryNotEmptyException(s"Targeted directory $p is not empty and recursive deletion is disabled"))
			}
			else
				Try { Files.deleteIfExists(p) }
		}
		
		/**
		 * @return Deletes all child paths from under this directory. Returns failure if deletion of some file failed.
		 */
		def deleteChildren() = children.flatMap { _.findMap {
			c => Some(c.delete()).filter { _.isFailure }.map { _.map { _ => Unit } } }.getOrElse(Success(Unit)) }
	}
}
