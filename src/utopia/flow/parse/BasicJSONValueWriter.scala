package utopia.flow.parse

import scala.collection.immutable.HashSet
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DataType
import utopia.flow.generic.StringType
import utopia.flow.generic.VectorType
import utopia.flow.generic.ModelType
import utopia.flow.generic.IntType
import utopia.flow.generic.DoubleType
import utopia.flow.generic.FloatType
import utopia.flow.generic.LongType
import utopia.flow.generic.BooleanType

/**
 * This JSON value writer is able to write instances of basic data types into JSON
 * @author Mikko Hilpinen
 * @since 17.12.2016
 */
object BasicJSONValueWriter extends JSONValueWriter
{
    override def supportedTypes = HashSet(StringType, VectorType, ModelType, IntType, DoubleType, 
            FloatType, LongType, BooleanType)
    
    override def write(value: Value, dataType: DataType) = 
    {
        dataType match 
        {
            case StringType => Some("\"" + value.stringOr() + "\"")
            case VectorType =>
            {
                val v = value.vectorOr()
                val s = new StringBuilder()
                s += '['
                
                val jsonValues = v.flatMap { _.toJSON }
                if (!jsonValues.isEmpty)
                {
                    s ++= jsonValues.head
                    jsonValues.tail.foreach { json => s ++= s", $json" }
                }
                
                s += ']'
                Some(s.toString())
            }
            case ModelType => value.model.map { _.toJSON }
            case _ => value.string
        }
    }
}