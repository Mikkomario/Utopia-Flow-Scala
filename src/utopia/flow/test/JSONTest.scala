package utopia.flow.test

import utopia.flow.datastructure.immutable.Value
import java.time.Instant
import utopia.flow.generic.StringType
import utopia.flow.generic.DataType
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Model
import utopia.flow.parse.JSONReader
import utopia.flow.generic.IntType
import utopia.flow.generic.ModelType

import utopia.flow.generic.ValueConversions._

object JSONTest extends App
{
    DataType.setup()
    
    def assertJSON(value: Value, json: String) = 
    {
        val result = value.toJSON
        println(s"${value.description} => ${result.getOrElse("???")}")
        assert(result.exists { _ == json })
    }
    
    // Tests value JSON parsing first
    val i = 123.toValue
    val d = 222.222.toValue
    val s = "Hello World!".toValue
    val time = Instant.now().toValue
    val b = true.toValue
    val empty = Value.empty(StringType)
    val v = Vector(empty, b, i).toValue
    
    assert(empty.toJSON.isEmpty)
    
    assertJSON(i, "123")
    assertJSON(d, "222.222")
    assertJSON(s, "\"Hello World!\"")
    assertJSON(time, s"${time.longOr()}")
    assertJSON(b, "true")
    assertJSON(v, "[true, 123]")
    
    // Tests Property writing next
    val prop1 = new Constant("test1", i)
    val prop2 = new Constant("test2", s)
    val prop3 = new Constant("test3", empty)
    
    assert(prop3.toJSON.isEmpty)
    assert(prop2.toJSON.isDefined)
    assert(prop1.toJSON.exists { _ == "\"test1\": 123" })
    
    // Tests / prints model writing
    val model = new Model(Vector(prop1, prop2, prop3))
    println(model.toJSON)
    
    // Tests model reading
    val readModel1 = JSONReader.parseSingle(model.toJSON)
    assert(readModel1.isDefined)
    println(readModel1.get)
    
    val readModel2 = JSONReader.parseSingle("{\"name\" : \"Matti\", \"age\": 39}")
    
    assert(readModel2.isDefined)
    println(readModel2.get)
    
    assert(readModel2.get("name").stringOr() == "Matti")
    assert(readModel2.get("age").dataType == IntType)
    
    // Tests more difficult data types
    val prop4 = new Constant("test4", v)
    val prop5 = new Constant("test5", model)
    val prop6 = new Constant("test6", time)
    
    val model2 = new Model(Vector(prop4, prop5, prop6))
    
    println(model2)
    
    val readModel3 = JSONReader.parseSingle(model2.toJSON)
    
    assert(readModel3.isDefined)
    println(readModel3.get)
    
    assert(readModel3.get("test6").longOr(-1) > 0)
    assert(readModel3.get("test4").vectorOr().length == 2)
    assert(readModel3.get("test5").dataType == ModelType)
    
    println("Success!")
}