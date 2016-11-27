package utopia.flow.test

import utopia.flow.generic.DataType
import utopia.flow.generic.AnyType
import utopia.flow.generic.StringType
import utopia.flow.generic.ConversionReliability
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DoubleType
import utopia.flow.generic.BooleanType

object DataTypeTest extends App
{
    DataType.values.foreach { println(_) }
    
    assert(StringType isOfType StringType)
    assert(StringType.isOfType(AnyType))
    assert(!StringType.isOfType(DoubleType))
    
    assert(ConversionReliability.DANGEROUS < ConversionReliability.NO_CONVERSION)
    
    val str = Value of "123.45"
    val str2 = Value of "true"
    val i = Value of 213
    val d = Value of 123.4567891234
    val f = Value of 123.45f
    val l = Value of 9999999999l
    val b = Value of true
    
    /*
    DataType.values.foreach { fromType => DataType.values.foreach { toType => 
        println(s"Route from $fromType to $toType: " + ConversionHandler.routeString(fromType, toType)) } }
    DataType.values.foreach { fromType => DataType.values.foreach { toType => 
        println(s"Route cost $fromType to $toType: " + ConversionHandler.costOfRoute(fromType, toType)) } }
    */
    
    assert(str.toDouble == 123.45)
    assert(str.toInt == 123)
    assert(str.toLong == 123)
    assert(str.toBoolean == false)
    assert(str.toString() == str.content)
    assert(str2.toBoolean == true)
    
    assert(i.toDouble == 213)
    assert(i.toBoolean == true)
    assert(i.toLong == 213)
    assert(i.toString() == "213")
    
    assert(d.toInt == 123)
    assert(d.toBoolean == true)
    assert(d.toLong == 123)
    
    assert(f.toInt == 123)
    assert(f.toLong == 123)
    assert(f.toBoolean == true)
    
    assert(l.toDouble == 9999999999.0)
    assert(l.toBoolean == true)
    
    assert(b.toInt == 1)
    assert(b.toString() == "true")
    assert(b.toDouble == 1.0)
    
    println("Success")
}