package utopia.flow.test

import utopia.flow.generic.ValueConversions._

import utopia.flow.generic.DataType
import utopia.flow.parse.XmlElement
import utopia.flow.datastructure.immutable.Model
import java.io.File
import utopia.flow.parse.XmlWriter

/**
 * This app tests xml writing and reading functions
 * @author Mikko Hilpinen
 * @since 17.1.2018 (v1.3)
 */
object XmlTest extends App
{
    DataType.setup()
    
    // Creates the xml elements
    val grandChild1 = new XmlElement("c", "Test & Values", Model(Vector("att1" -> 1, "att2" -> "b")))
    val grandChild2 = new XmlElement("d", 123456)
    val grandChild3 = new XmlElement("e")
    
    val child = new XmlElement(name = "b", children = Vector(grandChild1, grandChild2, grandChild3))
    val root = new XmlElement(name = "a", attributes = Model(Vector("id" -> 34)), children = Vector(child))
    
    // Test prints
    println(root.toXml)
    println(root.toJSON)
    
    val parsed = XmlElement(root.toModel)
    
    println(parsed.get.toXml)
    println(parsed.get.toJSON)
    
    // Makes sure model parsing works for xml elements
    assert(parsed.contains(root))
    
    // Tries to write the xml data to a file
    val testFile = new File("test/XmlTest.xml")
    testFile.getParentFile.mkdirs()
    assert(XmlWriter.writeElementToFile(testFile, root).isSuccess)
    
    println("Success!")
}