package utopia.flow.test

import utopia.flow.util.SimpleVariableGenerator
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DataType

object ModelTest extends App
{
    println(DataType)
    
    // Tests variable creation
    val variableGenerator = new SimpleVariableGenerator(None)
    
    assert(variableGenerator("Test", None).isEmpty)
    assert(variableGenerator("Test", Some(Value of 2)).isDefined)
}