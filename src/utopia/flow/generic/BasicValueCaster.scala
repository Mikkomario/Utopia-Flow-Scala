package utopia.flow.generic

import scala.collection.immutable.HashSet
import scala.annotation.switch

/**
 * This value caster handles the basic data types
 * @author Mikko Hilpinen
 * @since 19.11.2016
 */
object BasicValueCaster extends ValueCaster
{
    // ATTRIBUTES    --------------
    
    override lazy val conversions = HashSet[Conversion]()
    
    
    // IMPLEMENTED METHODS    ----
    
    @throws(classOf[ValueCastException])
    override def cast(value: Value, toType: DataType) = 
    {
        val castedValue = toType match 
        {
            // Any object can be cast into a string
            case StringType => value.content.toString()
            case IntType => intOf(value)
            case _ => throw new ValueCastException(value, toType)
        }
        
        new Value(None, AnyType)
    }
    
    
    // OTHER METHODS    ---------
    
    private def intOf(value: Value) = 
    {
        // Double, long, float and boolean can be cast to integers
        // String needs to be parsed
        value.dataType match 
        {
            case DoubleType => value.toDouble.intValue()
            case LongType => value.toLong.intValue()
            case FloatType => value.toFloat.intValue()
            case BooleanType => if (value.toBoolean) 1 else 0
            case StringType => {
                try {value.toString.toDouble.toInt} 
                catch 
                {
                    case e: Exception => throw new ValueCastException(value, IntType, e)
                }
            }
            case _ => throw new ValueCastException(value, IntType)
        }
    }
}