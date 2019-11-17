package utopia.flow.util

import scala.util.Try

/**
 * Contains some utility extensions that extend the capabilities of standard strings
 * @author Mikko Hilpinen
 * @since 1.11.2019, v1.6.1+
 */
object StringExtensions
{
	/**
	 * Extends standard scala string
	 * @param s String to extend
	 */
	implicit class ExtendedString(val s: String) extends AnyVal
	{
		/**
		 * @return Words that belong to this string. <b>This includes all non-whitespace characters but not newline characters</b>
		 */
		def words = s.lines.toVector.flatMap { _.split(" ").toVector.map { _.trim }.filter { _.nonEmpty } }
		
		/**
		 * @return The first word in this string (may include any characters except whitespace)
		 */
		def firstWord = untilFirst(" ")
		
		/**
		 * @return The last word in this string (may include any characters except whitespace)
		 */
		def lastWord = afterLast(" ")
		
		/**
		 * @return A non-empty copy of this string or None
		 */
		def notEmpty = if (s.isEmpty) None else Some(s)
		
		/**
		 * @return This string cast to an integer, None if this string couldn't be parsed
		 */
		def toIntOption = Try(s.toInt).toOption
		
		/**
		 * @return This string cast to a double, None if this string couldn't be parsed
		 */
		def toDoubleOption = Try(s.toDouble).toOption
		
		/**
		 * @return A copy of this string without any non-letter characters
		 */
		def letters = s.filter { _.isLetter }
		
		/**
		 * @return A copy of this string without any non-digit characters
		 */
		def digits = s.filter { _.isDigit }
		
		/**
		 * @param other Another string
		 * @return Whether this string contains specified substring (case-insensitive)
		 */
		def containsIgnoreCase(other: String) = s.toLowerCase.contains(other.toLowerCase)
		
		/**
		 * @param strings A number of strings
		 * @return Whether this string contains all of the provided sub-strings (case-sensitive)
		 */
		def containsAll(strings: TraversableOnce[String]) = strings.forall(s.contains)
		
		/**
		 * @param first A string
		 * @param second Another string
		 * @param more More strings
		 * @return Whether this string contains all of the provided sub-strings (case-sensitive)
		 */
		def containsAll(first: String, second: String, more: String*): Boolean = containsAll(Vector(first, second) ++ more)
		
		/**
		 * @param strings A number of strings
		 * @return Whether this string contains all of the provided sub-strings (case-insensitive)
		 */
		def containsAllIgnoreCase(strings: TraversableOnce[String]) =
		{
			val lower = s.toLowerCase
			strings.forall { searched => lower.contains(searched.toLowerCase) }
		}
		
		/**
		 * @param first A string
		 * @param second Another string
		 * @param more More strings
		 * @return Whether this string contains all of the provided sub-strings (case-insensitive)
		 */
		def containsAllIgnoreCase(first: String, second: String, more: String*): Boolean = containsAllIgnoreCase(
			Vector(first, second) ++ more)
		
		/**
		 * @param prefix A prefix
		 * @return Whether this string starts with specified prefix (case-insensitive)
		 */
		def startsWithIgnoreCase(prefix: String) = s.toLowerCase.startsWith(prefix.toLowerCase)
		
		/**
		 * @param suffix A suffix
		 * @return Whether this string ends with specified suffix (case-insensitive)
		 */
		def endsWithIgnoreCase(suffix: String) = s.toLowerCase.endsWith(suffix.toLowerCase)
		
		/**
		 * @param str A searched string
		 * @return Index of the beginning of specified string in this string. None if specified string isn't a
		 *         substring of this string
		 */
		def optionIndexOf(str: String) =
		{
			val raw = s.indexOf(str)
			if (raw < 0) None else Some(raw)
		}
		
		/**
		 * @param str A searched string
		 * @return Last index of the beginning of specified string in this string. None if specified string isn't a
		 *         substring of this string
		 */
		def optionLastIndexOf(str: String) =
		{
			val raw = s.lastIndexOf(str)
			if (raw < 0) None else Some(raw)
		}
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes after the first occurrence of specified string (empty string if
		 *         specified string is not a substring of this string), (case-sensitive)
		 */
		def afterFirst(str: String) = optionIndexOf(str).map { i => s.drop(i + str.length) }.getOrElse("")
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes after the last occurrence of specified string (empty string if
		 *         specified string is not a substring of this string), (case-sensitive)
		 */
		def afterLast(str: String) = optionLastIndexOf(str).map { i => s.drop(i + str.length) }.getOrElse("")
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes after the first occurrence of specified string,
		 *         including the searched string (returns an empty string if
		 *         specified string is not a substring of this string), (case-sensitive)
		 */
		def dropUntil(str: String) = optionIndexOf(str).map(s.drop).getOrElse("")
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes after the last occurrence of specified string,
		 *         including the searched string (returns an empty string if
		 *         specified string is not a substring of this string), (case-sensitive)
		 */
		def dropUntilLast(str: String) = optionLastIndexOf(str).map(s.drop).getOrElse("")
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes after the first occurrence of specified string
		 *         (returns this string if specified string is not a substring of this string), (case-sensitive)
		 */
		def untilFirst(str: String) = optionIndexOf(str).map(s.take).getOrElse(s)
		
		/**
		 * @param str A string
		 * @return A portion of this string that comes before the last occurrence of specified string
		 *         (returns this string if specified string is not a substring of this string), (case-sensitive)
		 */
		def untilLast(str: String) = optionLastIndexOf(str).map(s.take).getOrElse(s)
		
		/**
		 * A comparison of two strings in a case-insensitive manner
		 * @param another Another string
		 * @return Whether this string equals the other string when case is ignored
		 */
		def ~==(another: String) = s.equalsIgnoreCase(another)
	}
}
