package utopia.flow.test

import utopia.flow.generic.SimpleVariableGenerator
import utopia.flow.datastructure.immutable
import utopia.flow.datastructure.mutable
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DataType
import utopia.flow.generic.ConversionHandler
import utopia.flow.generic.StringType
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.generic.SimpleConstantGenerator

object ModelTest extends App
{
    DataType.setup()
    
    //println(ConversionHandler)
    //println(DataType)
    
    // Tests variable creation
    val generator2 = new SimpleVariableGenerator(Value of 0)
    
    assert(generator2.defaultValue.isDefined)
    val generated = generator2("Test", None)
    assert(generated.content == generator2.defaultValue)
    
    // Tests models
    // 1) Model with no default value
    val model1 = new mutable.Model()
    model1("Test") = Value of 2
    model1("Another") = Value of "Hello"
    
    assert(model1.attributes.size == 2)
    assert(model1("Test").content.get == 2)
    
    model1("another") = Value of "Hello2"
    
    assert(model1.attributes.size == 2)
    assert(model1("Another").content.get == "Hello2")
    
    // 2) model with default value
    val model2 = new mutable.Model(generator2)
    assert(!model2.findExisting("Test").isDefined)
    assert(model2("Test").content.get == 0)
    
    // 3) immutable model with no default value
    val constants = Vector(new Constant("Test1", Value of 1), new Constant("Test2", Value of 2))
    val model3 = new immutable.Model(constants)
    
    assert(model3.attributeGenerator == model3.attributeGenerator)
    assert(model3.attributeGenerator == new SimpleConstantGenerator())
    
    assert(model3 == new immutable.Model(constants))
    assert(model3.attributes.size == 2)
    assert(model3("Test1").content.get == 1)
    
    val model4 = model3 + new Constant("Test3", Value of 3)
    
    assert(model4.attributes.size == 3)
    assert(model4("Test3").content.get == 3)
    
    // 4) Immutable model with a default value
    val generator3 = new SimpleConstantGenerator(Value of 0)
    val model5 = new immutable.Model(constants, generator3)
    
    assert(model5 != model3)
    assert(model5("nonexisting").content.get == 0)
    assert(model5.attributes.size == constants.size)
    
    println(model5.toString())
    
    println("Success")
}