package utopia.flow.test

import utopia.flow.generic.DataType
import utopia.flow.generic.AnyType
import utopia.flow.generic.StringType

object DataTypeTest extends App
{
    //println(StringType)
    
    DataType.values.foreach { println(_) }
    println("Success")
}