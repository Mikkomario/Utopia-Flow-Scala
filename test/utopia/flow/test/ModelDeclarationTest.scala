package utopia.flow.test

import utopia.flow.datastructure.immutable.PropertyDeclaration
import utopia.flow.generic.IntType
import utopia.flow.generic.DataType
import utopia.flow.generic.StringType
import utopia.flow.datastructure.immutable.ModelDeclaration
import utopia.flow.generic.BooleanType
import utopia.flow.generic.DeclarationConstantGenerator
import utopia.flow.generic.DeclarationVariableGenerator

import utopia.flow.generic.ValueConversions._

object ModelDeclarationTest extends App
{
    DataType.setup()
    
    // Tests property declarations
    val prop1 = PropertyDeclaration("test1", IntType)
    val prop2 = PropertyDeclaration("test2", 0)
    
    // (not the usual use case but possible)
    val prop3 = PropertyDeclaration("test3", StringType, Some(3))
    
    assert(prop1 == PropertyDeclaration("test1", IntType))
    assert(prop1.defaultValue.isEmpty)
    assert(prop2.defaultValue.isDefined)
    
    // Tests model declaration
    val modelDec = new ModelDeclaration(prop1, prop2, prop3)
    
    assert(modelDec.find("TEST1").isDefined)
    assert(modelDec.find("kkk").isEmpty)
    assert(modelDec.declarations.size == 3)
    
    val modelDec2 = modelDec + PropertyDeclaration("Test4", BooleanType)
    
    assert(modelDec2.declarations.size == 4)
    
    // Tests constant generation
    // 1) Generator with no default value
    val generator1 = new DeclarationConstantGenerator(modelDec2)
    
    assert(generator1("test1").value.isEmpty)
    assert(generator1("test2").value.isDefined)
    assert(generator1("test3").value.dataType == StringType)
    assert(generator1("not here").value.isEmpty)
    
    // 2) Generator with a default value
    val generator2 = new DeclarationConstantGenerator(modelDec2, 0)
    
    assert(generator2("test1").value.isDefined)
    assert(generator2("test3").value.dataType == StringType)
    assert(generator2("test4").value.content.get == false)
    assert(generator2("something else").value.isDefined)
    
    // Quick test of variable generation
    val generator4 = new DeclarationVariableGenerator(modelDec2)
    
    assert(generator4("test4", Some(1)).value.content.get == true)
    
    println("Success")
}