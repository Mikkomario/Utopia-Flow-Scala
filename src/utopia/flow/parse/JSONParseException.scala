package utopia.flow.parse

/**
 * These exceptions are thrown when JSON parsing fails for some reason
 * @author Mikko Hilpinen
 * @since 13.12.2016
 */
case class JSONParseException(message: String, cause: Throwable = null) extends Exception(message, cause) 