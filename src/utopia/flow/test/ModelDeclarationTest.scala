package utopia.flow.test

import utopia.flow.datastructure.immutable.PropertyDeclaration
import utopia.flow.generic.IntType
import utopia.flow.generic.DataType
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.StringType
import utopia.flow.datastructure.immutable.ModelDeclaration
import utopia.flow.generic.BooleanType
import utopia.flow.generic.DeclarationConstantGenerator
import utopia.flow.generic.DeclarationVariableGenerator

object ModelDeclarationTest extends App
{
    DataType.setup()
    
    // Tests property declarations
    val prop1 = new PropertyDeclaration("test1", IntType)
    val prop2 = new PropertyDeclaration("test2", Value of 0)
    
    // (not the usual use case but possible)
    val prop3 = new PropertyDeclaration("test3", StringType, Some(Value of 3))
    
    assert(prop1 == new PropertyDeclaration("test1", IntType))
    assert(prop1.defaultValue.isEmpty)
    assert(prop2.defaultValue.isDefined)
    
    // Tests model declaration
    val modelDec = new ModelDeclaration(prop1, prop2, prop3)
    
    assert(modelDec.find("TEST1").isDefined)
    assert(modelDec.find("kkk").isEmpty)
    assert(modelDec.declarations.size == 3)
    
    val modelDec2 = modelDec + new PropertyDeclaration("Test4", BooleanType)
    
    assert(modelDec2.declarations.size == 4)
    
    // Tests constant generation
    // 1) Generator with no default value
    val generator1 = new DeclarationConstantGenerator(modelDec2)
    
    assert(generator1("test1").content.isEmpty)
    assert(generator1("test2").content.isDefined)
    assert(generator1("test3").content.dataType == StringType)
    assert(generator1("not here").content.isEmpty)
    
    // 2) Generator with a default value
    val generator2 = new DeclarationConstantGenerator(modelDec2, Value of 0)
    
    assert(generator2("test1").content.isDefined)
    assert(generator2("test3").content.dataType == StringType)
    assert(generator2("test4").content.content.get == false)
    assert(generator2("something else").content.isDefined)
    
    // Quick test of variable generation
    val generator4 = new DeclarationVariableGenerator(modelDec2)
    
    assert(generator4("test4", Some(Value of 1)).content.content.get == true)
    
    println("Success")
}