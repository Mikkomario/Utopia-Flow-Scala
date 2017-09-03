package utopia.flow.test

import utopia.flow.generic.DataType
import utopia.flow.generic.AnyType
import utopia.flow.generic.StringType
import utopia.flow.generic.ConversionReliability
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DoubleType
import utopia.flow.generic.BooleanType
import java.time.Instant
import utopia.flow.datastructure.immutable.Model
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.generic.ConversionHandler
import scala.collection.immutable.HashSet
import utopia.flow.generic.IntType

import utopia.flow.generic.ValueConversions._

object DataTypeTest extends App
{
    DataType.setup()
    DataType.values.foreach { println(_) }
    
    assert(StringType isOfType StringType)
    assert(StringType.isOfType(AnyType))
    assert(!StringType.isOfType(DoubleType))
    
    assert(ConversionReliability.DANGEROUS < ConversionReliability.NO_CONVERSION)
    
    val str = "123.45".toValue
    val str2 = "true".toValue
    val i = 213.toValue
    val d = 123.4567891234.toValue
    val f = 123.45f.toValue
    val l = 9999999999l.toValue
    val b = true.toValue
    val time = Instant.now().toValue
    val vector = Vector(i, d, f, l).toValue
    val model = new Model(Vector(new Constant("attributeName", i))).toValue
    
    /*
    DataType.values.foreach { fromType => DataType.values.foreach { toType => 
        println(s"Route from $fromType to $toType: " + ConversionHandler.routeString(fromType, toType)) } }
    DataType.values.foreach { fromType => DataType.values.foreach { toType => 
        println(s"Route cost $fromType to $toType: " + ConversionHandler.costOfRoute(fromType, toType)) } }
    */
    
    assert(str.doubleOr() == 123.45)
    assert(str.intOr() == 123)
    assert(str.longOr() == 123)
    assert(str.booleanOr() == false)
    assert(str.string == str.content)
    assert(str2.booleanOr() == true)
    
    assert(i.doubleOr() == 213)
    assert(i.booleanOr() == true)
    assert(i.longOr() == 213)
    assert(i.stringOr() == "213")
    
    assert(d.intOr() == 123)
    assert(d.booleanOr() == true)
    assert(d.longOr() == 123)
    
    assert(f.intOr() == 123)
    assert(f.longOr() == 123)
    assert(f.booleanOr() == true)
    
    assert(l.doubleOr() == 9999999999.0)
    assert(l.booleanOr() == true)
    
    assert(b.intOr() == 1)
    assert(b.stringOr() == "true")
    assert(b.doubleOr() == 1.0)
    
    assert(time.longOr() > 0)
    
    val timeToString = time.toString()
    val timeStringToTime = timeToString.instantOr()
    assert(time.long == timeStringToTime.long)
    
    println(vector.toString())
    assert(vector.vectorOr().length == 4)
    assert(vector.toString().startsWith("["))
    assert(model.vectorOr().length == 1)
    
    assert(str.orElse(i) == str)
    assert(Value.empty().orElse(i) == i)
    
    println(model.toString())
    
    // Tests Multi type conversion
    assert(ConversionHandler.cast(d, HashSet(StringType, IntType)).exists { 
        _.dataType isOfType IntType })
    
    println("Success")
}