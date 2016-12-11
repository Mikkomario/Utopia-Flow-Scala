package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template.Node
import utopia.flow.util.Equatable
import scala.Vector
import utopia.flow.generic.BooleanType
import utopia.flow.generic.ConversionHandler
import utopia.flow.generic.DataType
import utopia.flow.generic.DoubleType
import utopia.flow.generic.FloatType
import utopia.flow.generic.IntType
import utopia.flow.generic.LongType
import utopia.flow.generic.StringType
import utopia.flow.generic.ValueCastException

object Value
{
    /**
     * Wraps a string into a value
     */
    def of(s: String) = new Value(s, StringType)
    /**
     * Wraps an integer into a value
     */
    def of(i: Int) = new Value(i, IntType)
    /**
     * Wraps a double into a value
     */
    def of(d: Double) = new Value(d, DoubleType)
    /**
     * Wraps a floating point number into a value
     */
    def of(f: Float) = new Value(f, FloatType)
    /**
     * Wraps a long number into a value
     */
    def of(l: Long) = new Value(l, LongType)
    /**
     * Wraps a boolean into a value
     */
    def of(b: Boolean) = new Value(b, BooleanType)
}

/**
 * Values can wrap an object value and associate it with a certain data type. Values can be cast 
 * to different data types. They are immutable.
 */
class Value(val content: Any, val dataType: DataType) extends Node[Any] with Equatable
{
    // INITIAL CODE    ---------
    
    // The content must be of correct type
    require(dataType.isInstance(content), s"$content is not of type $dataType")
    
    
    // COMP. PROPERTIES    -----
    
    override def properties = Vector(content, dataType)
    
    /**
     * The description of this value, describing both content and data type
     */
    def description = s"$content ($dataType)"
    
    
    // IMPLEMENTED METHODS    ---
    
    /**
     * The contents of this value cast to a string
     */
    override def toString = toObject(StringType).asInstanceOf[String]
    
    
    // OTHER METHODS    ---------
    
    /**
     * Casts this value to a new data type
     * @param dataType The target data type for the new value
     * @return This value casted to a new data type
     * @throws ValueCastException if the cast failed
     */
    @throws(classOf[ValueCastException])
    def withType(dataType: DataType) = ConversionHandler.cast(this, dataType)
    
    /**
     * Casts this value to a new data type
     * @param toType The target data type for the new value
     * @return This value casted to the provided data type or None if the casting failed
     */
    def safeCast(toType: DataType) = ConversionHandler.safeCast(this, toType)
    
    /**
     * Returns the contents of this value, casted to the desired type range
     * @param ofType The targeted data type
     * @return The value's contents as an instance of the provided type
     * @throws ValueCastException If the value content's couldn't be cast to the desired type
     */
    @throws(classOf[ValueCastException])
    def toObject(ofType: DataType) = withType(ofType).content
    
    /**
     * The contents of this value casted to an integer
     */
    def toInt = toObject(IntType).asInstanceOf[Int]
    
    /**
     * The contents of this value casted to a double
     */
    def toDouble = toObject(DoubleType).asInstanceOf[Double]
    
    /**
     * The contents of this value casted to a floating point number
     */
    def toFloat = toObject(FloatType).asInstanceOf[Float]
    
    /**
     * The contents of this value casted to a long number
     */
    def toLong = toObject(LongType).asInstanceOf[Long]
    
    /**
     * The contents of this value casted to a boolean value
     */
    def toBoolean = toObject(BooleanType).asInstanceOf[Boolean]
}