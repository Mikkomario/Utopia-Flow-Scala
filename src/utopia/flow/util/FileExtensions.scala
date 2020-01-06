package utopia.flow.util

import java.nio.file.{DirectoryNotEmptyException, Files, Path, Paths, StandardCopyOption}

import utopia.flow.filesearch.PathType.{Directory, File}
import utopia.flow.parse.JSONConvertible

import scala.language.implicitConversions
import utopia.flow.util.StringExtensions._
import utopia.flow.util.CollectionExtensions._
import utopia.flow.util.AutoClose._
import utopia.flow.util.NullSafe._

import scala.io.Codec
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
		 * @return Type of this path (directory or regular file)
		 */
		def pathType = if (isDirectory) Directory else File
		
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
		 * @return Directories directly under this one (returns empty vector for regular files). May fail.
		 */
		def subDirectories =
		{
			if (isDirectory)
				Files.list(p).tryConsume { _.filter { p => p.isDirectory }.collect(new VectorCollector[Path]) }
			else
				Success(Vector())
		}
		
		/**
		 * @return The size of this file in bytes. If called for a directory, returns the combined size of all files and
		 *         directories under this directory. Please note that this method may take a while to complete.
		 */
		def size: Try[Long] =
		{
			// Size of a regular file is delegated to java.nio.Files while size of a directory is calculated recursively
			if (isRegularFile)
				Try { Files.size(p) }
			else
				children.flatMap { _.tryMap { _.size }.map { _.sum } }
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
			// Directories with content will have to be first copied, then removed
			if (isDirectory)
				copyTo(targetDirectory, replaceIfExists).flatMap { newDir => delete().map { _ => newDir } }
			else
			{
				val newLocation = targetDirectory/fileName
				if (replaceIfExists)
					Files.move(p, newLocation, StandardCopyOption.REPLACE_EXISTING)
				else
					Files.move(p, newLocation)
			}
		}
		
		/**
		 * Moves this file / directory to another directory
		 * @param targetDirectory Target parent directory for this file
		 * @param replaceIfExists Whether a file already existing at target path should be replaced with this one,
		 *                        if present (default = true)
		 * @return Link to the target path. Failure if file moving failed or if couldn't replace an existing file
		 */
		def copyTo(targetDirectory: Path, replaceIfExists: Boolean = true) =
			recursiveMoveCopy(targetDirectory, (a, b) => if (replaceIfExists)
				Files.copy(a, b, StandardCopyOption.REPLACE_EXISTING) else Files.copy(a, b))
		
		private def recursiveMoveCopy(targetDirectory: Path, operation: (Path, Path) => Path): Try[Path] = Try {
			operation(p, targetDirectory/fileName) }
			.flatMap { newParent => newParent.children
				.flatMap { _.tryForEach { c => new RichPath(c).recursiveMoveCopy(newParent, operation) } }.map { _ => newParent }}
		
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
		def overwriteWith(anotherPath: Path) =
		{
			val copyResult = Try { Files.copy(anotherPath, p, StandardCopyOption.REPLACE_EXISTING) }
			// May need to copy directory contents
			if (isDirectory)
				copyResult.flatMap { newDir => children.flatMap { _.tryForEach { _.copyTo(newDir) } }.map { _ => newDir } }
			else
				copyResult
		}
		
		/**
		 * Overwrites this path with file from another path, but only if the file was changed (had different last modified time)
		 * @param anotherPath Another file that will overwrite this one
		 * @return Path to this file. May contain a failure
		 */
		def overwriteWithIfChanged(anotherPath: Path) = if (hasSameLastModifiedAs(anotherPath)) Success(p) else overwriteWith(anotherPath)
		
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
		
		/**
		 * Creates this directory (and ensures existence of parent directories as well). If this is not a directory,
		 * simply creates parent directories.
		 * @return This path. Failure if couldn't create directories.
		 */
		def createDirectories() =
		{
			if (notExists)
			{
				// Checks whether this file should be a directory (doesn't have a file type) or a regular file
				// (has file type)
				if (fileType.isEmpty)
					Try { Files.createDirectories(p) }
				else
					createParentDirectories()
			}
			else
				Success(p)
		}
		
		/**
		 * Creates directories above this path. Eg. for path "dir1/dir2/fileX.txt" would ensure existence of dir1 and dir2
		 * @return This path, failure if couldn't create directories
		 */
		def createParentDirectories() = parentOption.map { dir => Try[Unit] { Files.createDirectories(dir) } }
			.getOrElse(Success(Unit)).map { _ => p }
		
		/**
		 * @param another Another file
		 * @return Whether these two files have same last modified time
		 */
		def hasSameLastModifiedAs(another: Path) = lastModified.success.exists { another.lastModified.success.contains }
		
		/**
		 * Writes specified text to this file (creates or empties file if necessary)
		 * @param text Text to be written to this file
		 * @param codec Charset / codec used (implicit)
		 * @return This path. Failure if writing failed.
		 */
		def write(text: String)(implicit codec: Codec) = Try { Files.write(p, text.getBytes(codec.charSet)) }
		
		/**
		 * Writes a json-convertible instance to this file
		 * @param json A json-convertible instance that will produce contents of this file
		 * @return This path. Failure if writing failed.
		 */
		def writeJSON(json: JSONConvertible) = write(json.toJSON)(Codec.UTF8)
	}
}
