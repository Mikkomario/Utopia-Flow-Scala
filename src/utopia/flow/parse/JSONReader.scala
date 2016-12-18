package utopia.flow.parse

/**
 * These readers are able to read meaning from JSON data
 * @author Mikko Hilpinen
 * @since 17.12.2016
 */
trait JSONReader
{
    /**
     * This method will be called each time a special marker is found from JSON data
     * @param event The event that occurred
     * @param lastIndex The index of the last marker / event
     * @param currentIndex: The index at which the new event / marker was found
     * @param json: The json string that is being parsed
     * @return The index at which the parsing should continue, should always be larger than 
     * the provided starting index (currentIndex)
     */
    def onReadEvent(event: JSONReadEvent, lastIndex: Int, currentIndex: Int, json: String): Int
}