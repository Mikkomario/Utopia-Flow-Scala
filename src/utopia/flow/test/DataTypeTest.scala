package utopia.flow.test

import utopia.flow.generic.DataType
import utopia.flow.generic.AnyType
import utopia.flow.generic.StringType
import utopia.flow.generic.ConversionReliability

object DataTypeTest extends App
{
    //println(StringType)
    
    DataType.values.foreach { println(_) }
    println("Success")
    
    assert(ConversionReliability.DANGEROUS < ConversionReliability.NO_CONVERSION)
}