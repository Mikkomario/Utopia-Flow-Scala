package utopia.flow.parse

import java.io.OutputStream
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLOutputFactory
import java.io.OutputStreamWriter
import scala.util.Try
import javax.xml.stream.XMLStreamException
import java.io.IOException
import scala.collection.immutable.HashMap

/**
 * This writer is used for writing xml documents
 * @author Mikko Hilpinen
 * @since 17.1.2018 (v1.3)
 */
@throws(classOf[XMLStreamException])
class XmlWriter(stream: OutputStream, charset: Charset = StandardCharsets.UTF_8) extends AutoCloseable
{
    // ATTRIBUTES    --------------------------
    
    private val writer = XMLOutputFactory.newInstance().createXMLStreamWriter(
            new OutputStreamWriter(stream, charset));
    
    
    // IMPLEMENTED METHODS    -----------------
    
    @throws(classOf[XMLStreamException])
    override def close() = 
    {
        writer.flush()
        writer.close()
    }
    
    
    // OTHER METHODS    ------------------------
    
    /**
     * Writes a complete xml document
     * @param contentWriter a function that is used for writing the contents of the document
     */
    def writeDocument(contentWriter: () => Unit) = 
    {
        writer.writeStartDocument()
        contentWriter()
        writer.writeEndDocument()
    }
    
    /**
     * Writes an element with content. Closes the element afterwards
     * @param elementName the name of the element
     * @param attributes the attributes written to element (optional)
     * @param text the text written to element (optional)
     * @param contentWriter the function that is used for writing the element contents
     */
    def writeElement(elementName: String, attributes: Map[String, String] = HashMap(), 
            text: Option[String] = None, contentWriter: () => Unit = () => Unit) = 
    {
        // Writes element start, attributes & text
        writer.writeStartElement(elementName)
        attributes.foreach{ case (key, value) => writer.writeAttribute(key, value) }
        text.foreach(writer.writeCharacters)
        // Writes other content
        contentWriter()
        // finally closes the element
        writer.writeEndElement()
    }
    
    /**
     * Writes a simple element with only text data
     * @param elementName the name of the element
     * @param text the text written inside the element
     */
    def writeTextElement(elementName: String, text: String) = 
    {
        writer.writeStartElement(elementName)
        writer.writeCharacters(text)
        writer.writeEndElement()
    }
    
    /**
     * Writes an element with only attribute data
     * @param elementName the name of the element
     * @param attributes the attributes written to the element (optional)
     */
    def writeEmptyElement(elementName: String, attributes: Map[String, String] = HashMap()) = 
    {
        writer.writeStartElement(elementName)
        attributes.foreach{ case (key, value) => writer.writeAttribute(key, value) }
        writer.writeEndElement()
    }
    
    /**
     * Writes a complete xml element tree to the document
     * @param element the element tree that is written
     */
    def write(element: XmlElement): Unit = writeElement(element.name, 
            element.attributes.attributeMap.mapValues(a => a.value.stringOr()), element.text, 
            () => element.children.foreach(write));
}