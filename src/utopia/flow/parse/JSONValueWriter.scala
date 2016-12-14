package utopia.flow.parse

import scala.collection.immutable.HashMap
import utopia.flow.generic.DataType
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.ConversionHandler

/**
 * This object provides an interface that allows converting of values into valid JSON data
 * @author Mikko Hilpinen
 * @since 13.12.2016
 */
object JSONValueWriter
{
    // PROPERTIES    -----------
    
    private var writers = HashMap[DataType, JSONValueWriter]()
    
    
    // OTHER METHODS    --------
    
    /**
     * Introduces a new writer implementation to this interface. If the writer provides
     * implementation for some already covered data type, the new implementation will override the
     * previous one
     * @param writer The writer that is added to this interface
     */
    def introduce(writer: JSONValueWriter) = writer.supportedTypes.foreach { writers += Tuple2(_, writer) }
    
    @throws(classOf[JSONParseException])
    def apply(value: Value) = 
    {
        /*
        // Casts the value to a compatible type
        val casted = ConversionHandler.safeCast(value, writers.keySet)
        
        if (casted.isDefined)
        {
            // Searches for a direct writer first, then an indirect writer
            val directWriter = writers.get(casted.get.dataType)
            if (directWriter.isDefined)
            {
                directWriter.get.write(casted.get, casted.get.dataType)
            }
            else
            {
                val wrappedType = writers.keys.find { casted.get.dataType isOfType _ }
                if (wrappedType.isDefined)
                {
                    writers(wrappedType.get).write(casted.get, wrappedType.get)
                }
                else
                {
                    throw new JSONParseException(s"No JSON value writer for type ${casted.get.dataType}")
                }
            }
        }
        else
        {
            throw new JSONParseException(s"$value couldn't be converted to JSON compatible type (${writers.keySet})")
        }*/
    }
}

/**
 * Value writer instances are used for writing values of certain data type into JSON data
 * @author Mikko Hilpinen
 * @since 13.12.2016
 */
trait JSONValueWriter
{
    /**
     * All of the data types this writer is able to directly write into JSON data
     */
    def supportedTypes: Set[DataType]
    
    /**
     * Writes the contents of the 'value' that can be assumed to be of type 'dataType' into a string
     * @param value The value that is written
     * @param dataType the data type of the provided value. The type is always
     * one of the supported data types of this writer. This can be matched against the supported 
     * types to find the correct parse method.
     */
    @throws(classOf[JSONParseException])
    def write(value: Value, dataType: DataType): String
}