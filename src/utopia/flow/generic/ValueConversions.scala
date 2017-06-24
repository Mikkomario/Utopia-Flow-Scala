package utopia.flow.generic

import utopia.flow.datastructure.immutable.Value
import java.time.Instant

import scala.language.implicitConversions

/**
 * This object offers implicit conversions from basic data types to the valueConvertible trait 
 * (and from there, Value)
 * @author Mikko Hilpinen
 * @since 19.6.2017
 */
object ValueConversions
{
    implicit def flattenValueOption[V](option: Option[V])(implicit f: V => Value): Value = 
            option.map(f).getOrElse(Value.empty());
    
    implicit def unwrapConvertible[C1](c: C1)(implicit f: C1 => ValueConvertible): Value = c.toValue
    
    implicit class ValueOfString(val s: String) extends ValueConvertible
    {
        def toValue = new Value(Some(s), StringType)
    }
    
    implicit class ValueOfInt(val i: Int) extends ValueConvertible
    {
        def toValue = new Value(Some(i), IntType)
    }
    
    implicit class ValueOfDouble(val d: Double) extends ValueConvertible
    {
        def toValue = new Value(Some(d), DoubleType)
    }
    
    implicit class ValueOfFloat(val f: Float) extends ValueConvertible
    {
        def toValue = new Value(Some(f), FloatType)
    }
    
    implicit class ValueOfLong(val l: Long) extends ValueConvertible
    {
        def toValue = new Value(Some(l), LongType)
    }
    
    implicit class ValueOfBoolean(val b: Boolean) extends ValueConvertible
    {
        def toValue = new Value(Some(b), BooleanType)
    }
    
    implicit class ValueOfInstant(val i: Instant) extends ValueConvertible
    {
        def toValue = new Value(Some(i), InstantType)
    }
    
    implicit class ValueOfVector(val v: Vector[Value]) extends ValueConvertible
    {
        def toValue = new Value(Some(v), VectorType)
    }
    
    /*
    implicit class ValueOfOption[C1](val option: Option[C1])(implicit f: C1 => ValueConvertible) extends ValueConvertible
    {
        def toValue = if (option.isDefined) option.get.toValue else Value.empty()
    }*/
}