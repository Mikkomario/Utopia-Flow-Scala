package utopia.flow.generic

import utopia.flow.datastructure.template.Node
import utopia.flow.util.Equatable

object Value
{
    /**
     * Wraps a string into a value
     */
    def ofString(s: String) = new Value(s, StringType)
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
     * Returns the contents of this value, casted to the desired type range
     * @param ofType The targeted data type
     * @return The value's contents as an instance of the provided type
     * @throws ValueCastException If the value content's couldn't be cast to the desired type
     */
    @throws(classOf[ValueCastException])
    def toObject(ofType: DataType) = ConversionHandler.cast(this, ofType).content
    
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