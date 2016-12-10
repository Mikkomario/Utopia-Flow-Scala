package utopia.flow.test

import utopia.flow.util.SimpleVariableGenerator
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DataType
import utopia.flow.generic.ConversionHandler
import utopia.flow.generic.StringType

object ModelTest extends App
{
    DataType.setup()
    
    //println(ConversionHandler)
    //println(DataType)
    
    // Tests variable creation
    val variableGenerator = new SimpleVariableGenerator(None)
    
    assert(variableGenerator("Test", None).isEmpty)
    assert(variableGenerator("Test", Some(Value of 2)).isDefined)
}