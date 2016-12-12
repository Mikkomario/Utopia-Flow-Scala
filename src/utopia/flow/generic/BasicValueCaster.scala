package utopia.flow.generic

import scala.collection.immutable.HashSet
import scala.annotation.switch
import utopia.flow.generic.ConversionReliability.PERFECT
import utopia.flow.generic.ConversionReliability.DATA_LOSS
import utopia.flow.generic.ConversionReliability.DANGEROUS
import utopia.flow.generic.ConversionReliability.MEANING_LOSS
import utopia.flow.datastructure.immutable.Value
import java.time.Instant
import java.time.format.DateTimeParseException

/**
 * This value caster handles the basic data types
 * @author Mikko Hilpinen
 * @since 19.11.2016
 */
object BasicValueCaster extends ValueCaster
{
    // ATTRIBUTES    --------------
    
    override lazy val conversions = HashSet(
            Conversion(AnyType, StringType, DATA_LOSS), 
            // Vector -> String
            Conversion(DoubleType, IntType, DATA_LOSS), 
            Conversion(LongType, IntType, DATA_LOSS), 
            Conversion(FloatType, IntType, DATA_LOSS), 
            Conversion(BooleanType, IntType, PERFECT), 
            Conversion(StringType, IntType, DANGEROUS), 
            Conversion(IntType, DoubleType, PERFECT), 
            Conversion(FloatType, DoubleType, PERFECT), 
            Conversion(LongType, DoubleType, PERFECT), 
            Conversion(StringType, DoubleType, DANGEROUS), 
            Conversion(IntType, FloatType, PERFECT), 
            Conversion(DoubleType, FloatType, DATA_LOSS), 
            Conversion(LongType, FloatType, DATA_LOSS), 
            Conversion(StringType, FloatType, DANGEROUS), 
            Conversion(IntType, LongType, PERFECT), 
            Conversion(DoubleType, LongType, DATA_LOSS), 
            Conversion(FloatType, LongType, DATA_LOSS), 
            Conversion(StringType, LongType, DANGEROUS), 
            Conversion(InstantType, LongType, DATA_LOSS), 
            Conversion(IntType, BooleanType, MEANING_LOSS),  
            Conversion(StringType, BooleanType, MEANING_LOSS), 
            Conversion(LongType, InstantType, PERFECT), 
            Conversion(StringType, InstantType, DANGEROUS), 
            Conversion(AnyType, VectorType, MEANING_LOSS))
    
    
    // IMPLEMENTED METHODS    ----
    
    @throws(classOf[ValueCastException])
    override def cast(value: Value, toType: DataType) = 
    {
        val castedValue = toType match 
        {
            // Any object can be cast into a string
            case StringType => stringOf(value)
            case IntType => intOf(value)
            case DoubleType => doubleOf(value)
            case FloatType => floatOf(value)
            case LongType => longOf(value)
            case BooleanType => booleanOf(value)
            case InstantType => instantOf(value)
            case VectorType => vectorOf(value)
            case _ => throw new ValueCastException(value, toType)
        }
        
        new Value(castedValue, toType)
    }
    
    
    // OTHER METHODS    ---------
    
    private def stringOf(value: Value) = 
    {
        value.dataType match 
        {
            // Vectors have a special formatting like "[a, b, c, d]" 
            // This is in order to form JSON -compatible output
            case VectorType =>
            {
                val vector = value.toVector
                val s = new StringBuilder()
                s += '['
                if (!vector.isEmpty)
                {
                    s ++= vector.head.toString()
                    vector.tail.foreach { s ++= ", " + _ }
                }
                s += ']'
                
                s.toString()
            }
            case _ => value.content.toString()
        }
    }
    
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
    
    private def doubleOf(value: Value) = 
    {
        value.dataType match 
        {
            case IntType => value.toInt.toDouble
            case LongType => value.toLong.toDouble
            case FloatType => value.toFloat.toDouble
            case StringType => {
                try {value.toString().toDouble} 
                catch 
                {
                    case e: Exception => throw new ValueCastException(value, DoubleType, e)
                }
            }
            case _ => throw new ValueCastException(value, DoubleType)
        }
    }
    
    private def floatOf(value: Value) = 
    {
        value.dataType match 
        {
            case IntType => value.toInt.toFloat
            case LongType => value.toLong.toFloat
            case DoubleType => value.toDouble.toFloat
            case StringType => {
                try {value.toString().toFloat} 
                catch 
                {
                    case e: Exception => throw new ValueCastException(value, FloatType, e)
                }
            }
            case _ => throw new ValueCastException(value, FloatType)
        }
    }
    
    private def longOf(value: Value) = 
    {
        value.dataType match 
        {
            case IntType => value.toInt.toLong
            case DoubleType => value.toDouble.toLong
            case FloatType => value.toFloat.toLong
            case InstantType => value.toInstant.getEpochSecond
            case StringType => {
                try {value.toString().toDouble.toLong} 
                catch 
                {
                    case e: Exception => throw new ValueCastException(value, LongType, e)
                }
            }
            case _ => throw new ValueCastException(value, LongType)
        }
    }
    
    private def booleanOf(value: Value) = 
    {
        value.dataType match 
        {
            case IntType => value.toInt != 0
            case StringType => value.toString().toLowerCase() == "true"
            case _ => throw new ValueCastException(value, BooleanType)
        }
    }
    
    private def instantOf(value: Value) = 
    {
        value.dataType match 
        {
            case LongType => Instant.ofEpochSecond(value.toLong)
            case StringType => {
                try { Instant.parse(value.toString()) }
                catch
                {
                    case e: DateTimeParseException => throw new ValueCastException(value, InstantType, e)
                }
            }
        }
    }
    
    private def vectorOf(value: Value) = Vector(value)
}