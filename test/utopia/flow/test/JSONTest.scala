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
        println(s"${value.description} => $result")
        assert(result == json)
    }
    
    // Tests value JSON parsing first
    val i = 123.toValue
    val d = 222.222.toValue
    val s = "Hello World!".toValue
    val time = Instant.now().toValue
    val b = true.toValue
    val empty = Value.empty(StringType)
    val v = Vector(empty, b, i).toValue
    
    assertJSON(empty, "null")
    
    assertJSON(i, "123")
    assertJSON(d, "222.222")
    assertJSON(s, "\"Hello World!\"")
    assertJSON(time, s"${time.longOr()}")
    assertJSON(b, "true")
    assertJSON(v, "[null, true, 123]")
    assertJSON(Vector[Value](), "[]")
    assertJSON(Model.empty, "{}")
    
    // Tests Property writing next
    val prop1 = new Constant("test1", i)
    val prop2 = new Constant("test2", s)
    val prop3 = new Constant("test3", empty)
    
    assert(prop3.toJSON == "\"test3\": null")
    assert(prop1.toJSON == "\"test1\": 123")
    
    // Tests / prints model writing
    val model = Model.withConstants(Vector(prop1, prop2, prop3))
    println(model.toJSON)
    
    // Tests value reading
    assert(JSONReader.parseValue("1").get.intOr() == 1)
    assert(JSONReader.parseValue("[1, 2, 3]").get.vectorOr() == Vector[Value](1, 2, 3))
    
    // Tests model reading
    val readModel1 = JSONReader.parseSingle(model.toJSON)
    assert(readModel1.isDefined)
    println(readModel1.get)
    // assert(readModel1 == model)
    
    val readModel2 = JSONReader.parseSingle("{\"name\" : \"Matti\", \"age\": 39}")
    
    assert(readModel2.isDefined)
    println(readModel2.get)
    
    assert(readModel2.get("name").stringOr() == "Matti")
    assert(readModel2.get("age").dataType == IntType)
    
    assert(readModel2 == JSONReader.parseSingle(readModel2.get.toJSON))
    
    // Tests more difficult data types
    val prop4 = new Constant("test4", v)
    val prop5 = new Constant("test5", model)
    val prop6 = new Constant("test6", time)
    
    val model2 = Model.withConstants(Vector(prop4, prop5, prop6))
    
    println(model2)
    
    val readModel3 = JSONReader.parseSingle(model2.toJSON)
    
    assert(readModel3.isDefined)
    println(readModel3.get)
    
    assert(readModel3.get("test6").longOr(-1) > 0)
    assert(readModel3.get("test4").vectorOr().length == 3)
    assert(readModel3.get("test5").dataType == ModelType)
    
    // Tests value reading vs. model reading
    assert(JSONReader.parseSingle(readModel2.get.toJSON).get == readModel2.get)
    assert(JSONReader.parseValue(readModel2.get.toJSON).get.model == readModel2)
    
    // This kind of setting was causing a problem earlier
    val test = Vector(1)
    println(test)
    println(test.toValue)
    
    // Tests model parsing with empty vector values
    println()
    println("Testing empty vectors and models")
    val model4 = Model(Vector("vec" -> Vector[Value](), "normal" -> "a"))
    
    println(model4)
    val parsed = JSONReader.parseSingle(model4.toJSON).get
    println(parsed)
    assert(parsed == model4)
    
    val model5 = Model(Vector("mod" -> Model(Vector())))
    
    println(model5)
    val parsed5 = JSONReader.parseSingle(model5.toJSON).get
    println(parsed5)
    assert(parsed5 == model5)
    
    assert(JSONReader.parseValue("[]").get == Vector[Value]().toValue)
    assert(JSONReader.parseValue("[ ]").get == Vector[Value]().toValue)
    assert(JSONReader.parseValue("[null]").get == Vector(Value.empty()).toValue)
    assert(JSONReader.parseValue("[,]").get == Vector(Value.empty(), Value.empty()).toValue)
    
    println("Success!")
}