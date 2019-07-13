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
    assertJSON(time, "\"" + time + "\"")
    assertJSON(b, "true")
    assertJSON(v, "[null, true, 123]")
    assertJSON(Vector[Value](), "[]")
    assertJSON(Model.empty, "{}")
    
    // Tests Property writing next
    val prop1 = Constant("test1", i)
    val prop2 = Constant("test2", s)
    val prop3 = Constant("test3", empty)
    
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
    assert(readModel1.isSuccess)
    println(readModel1.get)
    // assert(readModel1 == model)
    
    val readModel2 = JSONReader.parseSingle("{\"name\" : \"Matti\", \"age\": 39}")
    
    assert(readModel2.isSuccess)
    println(readModel2.get)
    
    assert(readModel2.get("name").stringOr() == "Matti")
    assert(readModel2.get("age").dataType == IntType)
    
    assert(readModel2 == JSONReader.parseSingle(readModel2.get.toJSON))
    
    // Tests more difficult data types
    val prop4 = Constant("test4", v)
    val prop5 = Constant("test5", model)
    val prop6 = Constant("test6", time)
    
    val model2 = Model.withConstants(Vector(prop4, prop5, prop6))
    
    println(model2)
    
    val readModel3 = JSONReader.parseSingle(model2.toJSON)
    
    assert(readModel3.isSuccess)
    println(readModel3.get)
    
    val readTime = readModel3.get("test6").instant
    assert(readTime.contains(time.getInstant))
    assert(readModel3.get("test4").vectorOr().length == 3)
    assert(readModel3.get("test5").dataType == ModelType)
    
    // Tests value reading vs. model reading
    assert(JSONReader.parseSingle(readModel2.get.toJSON).get == readModel2.get)
    assert(JSONReader.parseValue(readModel2.get.toJSON).get.getModel == readModel2.get)
    
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
    
    // Testing JSON reading when quoted portion contains json markers
    val jsonWithQuotes = Model(Vector("Test1" -> "This portion contains, special values",
        "Test2" -> "This one is also { tough }", "Even worse [when, array, in, property, name]" -> true)).toJSON
    val parsed6 = JSONReader.parseSingle(jsonWithQuotes).get
    
    assert(parsed6("Test1").getString == "This portion contains, special values")
    assert(parsed6("Test2").getString == "This one is also { tough }")
    assert(parsed6("Even worse [when, array, in, property, name]").getBoolean)
    
    println("Success!")
}