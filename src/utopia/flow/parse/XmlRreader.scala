package utopia.flow.parse

import utopia.flow.generic.ValueConversions._

import java.io.InputStream
import java.nio.charset.Charset
import javax.xml.stream.XMLInputFactory
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import utopia.flow.datastructure.immutable.Model
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.StringType
import scala.collection.immutable.VectorBuilder

/**
 * XMLReaders can be used for parsing and traversing through an xml document. The reader supports 
 * both SAX and DOM approaches to xml parsing
 * @author Mikko Hilpinen
 * @since 24.1.2018
 */
class XmlRreader(stream: InputStream, charset: Charset = StandardCharsets.UTF_8) extends AutoCloseable
{
    // ATTRIBUTES    ------------------------
    
    private val reader = XMLInputFactory.newInstance().createXMLStreamReader(
            new InputStreamReader(stream, charset));
    
    
    // INITIAL CODE    ----------------------
    
    // Moves the reader to the first element start
    _toNextElementStart()
    
    
    // COMPUTED PROPERTIES    ---------------
    
    /**
     * Whether the reader has reached the end of the document
     */
    def isAtDocumentEnd = !reader.hasNext()
    
    /**
     * The name of the current element. None if at the end of the document
     */
    def currentElementName = if (isAtDocumentEnd) None else Some(reader.getLocalName)
    
    /**
     * The attributes in the current element in model format. An empty model if at the end of 
     * the document.
     */
    def currentElementAttributes = 
    {
        if (isAtDocumentEnd) 
            Model.empty 
        else 
            Model(parseAttributes().mapValues(_.toValue).toVector)
    }
    
    private def currentEvent = 
    {
        if (reader.isStartElement())
        {
            ElementStart
        }
        else if (reader.isEndElement())
        {
            ElementEnd
        }
        else if (reader.isCharacters())
        {
            Text
        }
        else
        {
            nextEvent()
        }
    }
    
    
    // IMPLEMENTED METHODS    ---------------
    
    override def close() = reader.close()
    
    
    // OTHER METHODS    ---------------------
    
    /**
     * Parses the contents of a single xml element, including all its children. The reader is then 
     * moved to the next sibling element or higher
     * @return the parsed element
     */
    def readElement() = if (isAtDocumentEnd) None else Some(_readElement()._1.toXmlElement)
    
    /**
     * Parses the contents of all remaining elements under the current parent element. The reader 
     * is then moved to the parent's next sibling or higher
     * @return the parsed elements
     */
    def readSiblings() = 
    {
        val elementsBuffer = new VectorBuilder[XmlElement]()
        var depth = 0
        
        while (depth >= 0 && !isAtDocumentEnd)
        {
            val result = _readElement()
            elementsBuffer += result._1.toXmlElement
            depth += result._2
        }
        
        elementsBuffer.result()
    }
    
    /**
     * Moves the reader to the next element (child, sibling, etc.)
     * @return how much the 'depth' of the reader changed in the process (1 for child, 
     * 0 for sibling, -1 for parent level and so on)
     */
    def toNextElement() = _toNextElementStart()
    
    /**
     * Moves the reader to the next element (child, sibling, etc.) with a name that is accepted 
     * by the provided filter
     * @param nameFilter a filter that determines whether the name is accepted or not
     * @return how much the 'depth' of the reader changed in the process (1 for child, 
     * 0 for sibling, -1 for parent level and so on)
     */
    def toNextElementWithName(nameFilter: String => Boolean) = 
    {
        var depthChange = toNextElement()
        while (currentElementName.exists(!nameFilter(_)))
        {
            depthChange += toNextElement()
        }
        depthChange
    }
    
    /**
     * Moves the reader to the next element (child, sibling, etc.) with the specified name
     * @param searchedName the name the targeted element must have (case-insensitive)
     * @return how much the 'depth' of the reader changed in the process (1 for child, 
     * 0 for sibling, -1 for parent level and so on)
     */
    def toNextElementWithName(searchedName: String): Int = toNextElementWithName { 
            searchedName.equalsIgnoreCase(_) }
    
    /**
     * Skips this element and moves to the next sibling, parent or higher
     * @return how much the 'depth' of the reader changed in the process 
     * (0 for sibling, -1 for parent level and so on)
     */
    def skipElement() = skip(0)
    
    /**
     * Skips this element as well as any siblings this element may have and moves to the parent's 
     * next sibling or higher
     * @return how much the 'depth' of the reader changed in the process (-1 for parent level, 
     * -2 for grandparent level and so on)
     */
    def skipParent() = skip(-1)
    
    private def skip(depthChangeRequirement: Int) = 
    {
        var depth = _toNextElementStart()
        
        while (depth > depthChangeRequirement && !isAtDocumentEnd)
        {
            depth += _toNextElementStart()
        }
        
        depth
    }
    
    private def _readElement(): Tuple2[UnfinishedElement, Int] = 
    {
        val element = new UnfinishedElement(reader.getLocalName, parseAttributes())
        var depthChange = _toNextElementStart(Some(element))
        
        while (depthChange > 0)
        {
            val nextResult = _readElement()
            element.children :+= nextResult._1
            depthChange += nextResult._2
        }
        
        element -> depthChange
    }
    
    private def parseAttributes() = 
    {
        val attCount = reader.getAttributeCount
        (0 until attCount).toVector.map(
                i => reader.getAttributeLocalName(i) -> reader.getAttributeValue(i)).toMap
    }
    
    // Updates openElement text, returns the depth change
    private def _toNextElementStart(openElement: Option[UnfinishedElement] = None): Int = 
    {
        nextEvent match 
        {
            case ElementStart => 1
            case ElementEnd => _toNextElementStart(openElement) - 1
            case Text => 
                openElement.foreach(_.text += reader.getText)
                _toNextElementStart(openElement)
            case DocumentEnd => 0
        }
    }
    
    private def nextEvent(): XmlReadEvent = 
    {
        if (reader.hasNext())
        {
            reader.next()
            currentEvent
        }
        else
        {
            DocumentEnd
        }
    }
}

private sealed trait XmlReadEvent
private case object ElementStart extends XmlReadEvent
private case object ElementEnd extends XmlReadEvent
private case object Text extends XmlReadEvent
private case object DocumentEnd extends XmlReadEvent

private class UnfinishedElement(val name: String, val attributes: Map[String, String])
{
    // ATTRIBUTES    -----------------------
    
    var children = Vector[UnfinishedElement]()
    var text = ""
    
    
    // COMPUTED PROPERTIES    --------------
    
    def toXmlElement: XmlElement = 
    {
        val attributesModel = Model(attributes.mapValues(_.toValue).toVector)
        new XmlElement(name, if (text.isEmpty()) Value.empty(StringType) else text, 
                attributesModel, children.map(_.toXmlElement))
    }
}