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
    
    
    // INITIAL CODE    ---------
    
    // Adds the basic JSON writing capabilities
    introduce(BasicJSONValueWriter)
    
    
    // OTHER METHODS    --------
    
    /**
     * Introduces a new writer implementation to this interface. If the writer provides
     * implementation for some already covered data type, the new implementation will override the
     * previous one. This method should be called for each new JSONValueWriter implementation
     * (except for the BasicJSONValueWriter, which is introduced by default)
     * @param writer The writer that is added to this interface
     */
    def introduce(writer: JSONValueWriter) = writer.supportedTypes.foreach { writers += Tuple2(_, writer) }
    
    /**
     * Writes the value as a JSON string
     * @param value The value that will be converted to JSON
     * @return The JSON representation of the value or None if the value is not convertible to JSON
     */
    def apply(value: Value) = 
    {
        // Casts the value to a compatible type
        val casted = ConversionHandler.cast(value, writers.keySet)
        
        // Only non-empty values are parsed
        if (casted.exists { _.isDefined })
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
                    None
                }
            }
        }
        else
        {
            None
        }
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
     * Writes the contents of the 'value' that can be assumed to be of type 'dataType' as a
     * JSON string. No empty values will be offered for write.
     * @param value The value that is written. Not empty.
     * @param dataType the data type of the provided value. The type is always
     * one of the supported data types of this writer. This can be matched against the supported 
     * types to find the correct parse method.
     * @return JSON representation of the value or None if the value can't or should be written as
     * JSON
     */
    def write(value: Value, dataType: DataType): Option[String]
}