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
    
    override def cast(value: Value, toType: DataType) = 
    {
        val castResult: Option[Any] = toType match 
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
            case _ => None
        }
        
        castResult.map { objValue => new Value(Some(objValue), toType) }
    }
    
    
    // OTHER METHODS    ---------
    
    private def stringOf(value: Value): Option[String] = 
    {
        value.dataType match 
        {
            // Vectors have a special formatting like "[a, b, c, d]" 
            // This is in order to form JSON -compatible output
            case VectorType =>
            {
                val vector = value.vectorOr()
                val s = new StringBuilder()
                s += '['
                if (!vector.isEmpty)
                {
                    s ++= vector.head.toString()
                    vector.tail.foreach { s ++= ", " + _ }
                }
                s += ']'
                
                Some(s.toString())
            }
            case _ => value.content.map { _.toString() }
        }
    }
    
    private def intOf(value: Value): Option[Int] = 
    {
        // Double, long, float and boolean can be cast to integers
        // String needs to be parsed
        value.dataType match 
        {
            case DoubleType => Some(value.doubleOr().intValue())
            case LongType => Some(value.longOr().intValue())
            case FloatType => Some(value.floatOr().intValue())
            case BooleanType => Some(if (value.booleanOr()) 1 else 0)
            case StringType => {
                try { Some(value.stringOr("0").toDouble.toInt) } 
                catch { case e: Exception => None }
            }
            case _ => None
        }
    }
    
    private def doubleOf(value: Value): Option[Double] = 
    {
        value.dataType match 
        {
            case IntType => Some(value.intOr().toDouble)
            case LongType => Some(value.longOr().toDouble)
            case FloatType => Some(value.floatOr().toDouble)
            case StringType => {
                try { Some(value.stringOr("0").toDouble) } 
                catch { case e: Exception => None }
            }
            case _ => None
        }
    }
    
    private def floatOf(value: Value): Option[Float] = 
    {
        value.dataType match 
        {
            case IntType => Some(value.intOr().toFloat)
            case LongType => Some(value.longOr().toFloat)
            case DoubleType => Some(value.doubleOr().toFloat)
            case StringType => {
                try { Some(value.stringOr("0").toFloat) } 
                catch { case e: Exception => None }
            }
            case _ => None
        }
    }
    
    private def longOf(value: Value): Option[Long] = 
    {
        value.dataType match 
        {
            case IntType => Some(value.intOr().toLong)
            case DoubleType => Some(value.doubleOr().toLong)
            case FloatType => Some(value.floatOr().toLong)
            case InstantType => Some(value.instantOr().getEpochSecond)
            case StringType => {
                try { Some(value.stringOr("0").toDouble.toLong) } 
                catch { case e: Exception => None }
            }
            case _ => None
        }
    }
    
    private def booleanOf(value: Value): Option[Boolean] = 
    {
        value.dataType match 
        {
            case IntType => Some(value.intOr() != 0)
            case StringType => Some(value.stringOr().toLowerCase() == "true")
            case _ => None
        }
    }
    
    private def instantOf(value: Value): Option[Instant] = 
    {
        value.dataType match 
        {
            case LongType => Some(Instant.ofEpochSecond(value.longOr()))
            case StringType => {
                try { Some(Instant.parse(value.toString())) }
                catch { case e: DateTimeParseException => None }
            }
        }
    }
    
    private def vectorOf(value: Value) = Some(Vector(value))
}