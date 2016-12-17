package utopia.flow.test

import utopia.flow.datastructure.immutable.Value
import java.time.Instant
import utopia.flow.generic.StringType
import utopia.flow.generic.DataType
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Model

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
    val i = Value of 123
    val d = Value of 222.222
    val s = Value of "Hello World!"
    val time = Value of Instant.now()
    val b = Value of true
    val empty = Value.empty(StringType)
    val v = Value of Vector(empty, b, i)
    
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
    assert(prop1.toJSON.exists { _ == "test1: 123" })
    
    // Finally tests / prints model writing
    println(new Model(Vector(prop1, prop2, prop3)).toJSON)
}