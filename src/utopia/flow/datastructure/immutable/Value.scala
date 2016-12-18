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
import java.time.Instant
import utopia.flow.generic.InstantType
import utopia.flow.generic.VectorType
import utopia.flow.generic.ModelType
import utopia.flow.generic.AnyType
import utopia.flow.parse.JSONValueWriter

object Value
{
    /**
     * Creates a new empty value that represents / mimics the provided data type
     */
    def empty(dataType: DataType = AnyType) = new Value(None, dataType)
    
    /**
     * Wraps a string into a value
     */
    def of(s: String) = new Value(Some(s), StringType)
    /**
     * Wraps an integer into a value
     */
    def of(i: Int) = new Value(Some(i), IntType)
    /**
     * Wraps a double into a value
     */
    def of(d: Double) = new Value(Some(d), DoubleType)
    /**
     * Wraps a floating point number into a value
     */
    def of(f: Float) = new Value(Some(f), FloatType)
    /**
     * Wraps a long number into a value
     */
    def of(l: Long) = new Value(Some(l), LongType)
    /**
     * Wraps a boolean into a value
     */
    def of(b: Boolean) = new Value(Some(b), BooleanType)
    /**
     * Wraps an instant into a value
     */
    def of(time: Instant) = new Value(Some(time), InstantType)
    /**
     * Wraps a value vector into a value
     */
    def of(v: Vector[Value]) = new Value(Some(v), VectorType)
    /**
     * Wraps a model into a value
     */
    def of(m: Model[Constant]) = new Value(Some(m), ModelType)
}

/**
 * Values can wrap an object value and associate it with a certain data type. Values can be cast 
 * to different data types. They are immutable.
 */
class Value(val content: Option[Any], val dataType: DataType) extends Node[Option[Any]] with Equatable
{
    // INITIAL CODE    ---------
    
    // The content must be of correct type, if defined
    require(content.forall { dataType.isInstance(_) }, s"$content is not of type $dataType")
    
    
    // COMP. PROPERTIES    -----
    
    override def properties = Vector(content, dataType)
    
    /**
     * The description of this value, describing both content and data type
     */
    def description = s"'${stringOr()}' ($dataType)"
    
    /**
     * Whether this value has a real object value associated with it
     */
    def isDefined = content.isDefined
    
    /**
     * Whether this value doesn't have a real object value associated with it
     */
    def isEmpty = content.isEmpty
    
    
    // IMPLEMENTED METHODS    ---
    
    /**
     * The contents of this value cast to a string
     */
    override def toString = string.getOrElse("")
    
    
    // OPERATORS    -------------
    
    /**
     * Finds a value from this value as if this value was a model
     * @param propertyName The name of the requested property
     * @return The value of the requested property 
     */
    def apply(propertyName: String) = modelOr()(propertyName)
    
    /**
     * Finds a value from this value as if this value was a vector
     * @param index The index at which the value is searched
     * @return The value from the provided index from a vector within this value or empty value if 
     * this value doesn't contain a vector or index was out of range
     */
    def apply(index: Int) = 
    {
        val vector = vectorOr()
        if (index < 0 || index >= vector.length)
        {
            Value.empty()
        }
        else
        {
            vector(index)
        }
    }
    
    
    // OTHER METHODS    ---------
    
    /**
     * Checks whether this value is of the specified data type
     */
    def isOfType(dataType: DataType) = this.dataType.isOfType(dataType)
    
    /**
     * Casts the value to a certain data type. Returns None if the casting failed
     */
    def castTo(dataType: DataType) = ConversionHandler.cast(this, dataType)
    
    /**
     * Casts this value to a new data type
     * @param dataType The target data type for the new value
     * @return This value casted to a new data type. If the value couldn't be casted, an empty 
     * value is returned instead
     */
    def withType(dataType: DataType) = ConversionHandler.cast(this, dataType).getOrElse(Value.empty(dataType))
    
    /**
     * Converts the value into a JSON string
     */
    def toJSON = JSONValueWriter(this)
    
    /**
     * Returns the contents of this value, casted to the desired type range
     * @param ofType The targeted data type
     * @return The value's contents as an instance of the provided type
     * @throws ValueCastException If the value content's couldn't be cast to the desired type
     */
    @throws(classOf[ValueCastException])
    def objectValue(ofType: DataType) = withType(ofType).content
    
    /**
     * The string value of this value or None if the value can't be casted
     */
    def string = objectValue(StringType).map { _.asInstanceOf[String]}
    
    /**
     * The integer value of this value or None if the value can't be casted
     */
    def int = objectValue(IntType).map { _.asInstanceOf[Int] }
    
    /**
     * The double value of this value or None if the value can't be casted
     */
    def double = objectValue(DoubleType).map { _.asInstanceOf[Double]}
    
    /**
     * The float value of this value or None if the value can't be casted
     */
    def float = objectValue(FloatType).map { _.asInstanceOf[Float]}
    
    /**
     * The long value of this value or None if the value can't be casted
     */
    def long = objectValue(LongType).map { _.asInstanceOf[Long]}
    
    /**
     * The boolean value of this value or None if the value can't be casted
     */
    def boolean = objectValue(BooleanType).map { _.asInstanceOf[Boolean]}
    
    /**
     * The instant value of this value or None if the value can't be casted
     */
    def instant = objectValue(InstantType).map { _.asInstanceOf[Instant]}
    
    /**
     * The vector value of this value or None if the value can't be casted
     */
    def vector = objectValue(VectorType).map { _.asInstanceOf[Vector[Value]]}
    
    /**
     * The model value of this value or None if the value can't be casted
     */
    def model = objectValue(ModelType).map { _.asInstanceOf[Model[Constant]]}
    
    /**
     * The contents of this value casted to a string, or if that fails, the default value ''
     */
    def stringOr(default: String = "") = string.getOrElse(default)
    
    /**
     * The contents of this value casted to an integer, or if that fails, the default value 0
     */
    def intOr(default: Int = 0) = int.getOrElse(default)
    
    /**
     * The contents of this value casted to a double, or if that fails, the default value 0
     */
    def doubleOr(default: Double = 0) = double.getOrElse(default)
    
    /**
     * The contents of this value casted to a float, or if that fails, the default value 0
     */
    def floatOr(default: Float = 0) = float.getOrElse(default)
    
    /**
     * The contents of this value casted to a long, or if that fails, the default value 0
     */
    def longOr(default: Long = 0) = long.getOrElse(default)
    
    /**
     * The contents of this value casted to a boolean, or if that fails, the default value false
     */
    def booleanOr(default: Boolean = false) = boolean.getOrElse(default)
    
    /**
     * The contents of this value casted to an instant, or if that fails, the default value
     * (current instant)
     */
    def instantOr(default: Instant = Instant.now()) = instant.getOrElse(default)
    
    /**
     * The contents of this value casted to a vector, or if that fails, the default value (empty
     * vector)
     */
    def vectorOr(default: Vector[Value] = Vector[Value]()) = vector.getOrElse(default)
    
    /**
     * The contents of this value casted to a model, or if that fails, the default value (empty
     * model)
     */
    def modelOr(default: Model[Constant] = new Model(Vector[Constant]())) = model.getOrElse(default)
}