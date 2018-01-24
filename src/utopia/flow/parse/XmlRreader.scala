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
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.io.File
import java.io.FileInputStream

object XmlReader
{
    /**
     * Reads the contents of a stream using the specified reader function
     * @param stream the target stream
     * @param charset the charset of the stream contents
     * @param contentReader the function that uses the reader to parse the stream contents
     * @return The data parsed from the stream (may fail)
     */
    def readStream[T](stream: InputStream, charset: Charset = StandardCharsets.UTF_8, 
            contentReader: XmlReader => Try[T]) = 
    {
        def read() = 
        {
            val reader = new XmlReader(stream, charset)
            try
            {
                contentReader(reader)
            }
            finally
            {
                reader.close()
            }
        }
        
        Try(read()).flatten
    }
    
    /**
     * Reads the contents of an xml file using the specified reader function
     * @param file the target file
     * @param charset the charset of the file contents
     * @param contentReader the function that uses the reader to parse the file contents
     * @return The data parsed from the file (may fail)
     */
    def readFile[T](file: File, charset: Charset = StandardCharsets.UTF_8, contentReader: XmlReader => Try[T]) = 
    {
        val stream = Try(new FileInputStream(file))
        stream match 
        {
            case Success(stream) => try { readStream(stream, charset, contentReader) } finally { stream.close() }
            case Failure(ex) => Failure(ex)
        }
    }
    
    /**
     * Parses the contents of a stream into an xml element
     * @param stream the target stream
     * @param charset the charset of the stream contents
     * @return The element parsed from the stream (may fail)
     */
    def parseStream(stream: InputStream, charset: Charset = StandardCharsets.UTF_8) = readStream(
            stream, charset, reader => 
            {
                val element = reader.readElement()
                if (element.isDefined) Success(element.get) else Failure(new NoSuchElementException())
            })
    
    /**
     * Parses the contents of an xml file into an xml element
     * @param file the target file
     * @param charset the target charset
     * @return The element parsed from the file (may fail)
     */
    def parseFile(file: File, charset: Charset = StandardCharsets.UTF_8) = 
    {
        val stream = Try(new FileInputStream(file))
        stream match 
        {
            case Success(stream) => try { parseStream(stream, charset) } finally { stream.close() }
            case Failure(ex) => Failure(ex)
        }
    }
}

/**
 * XMLReaders can be used for parsing and traversing through an xml document. The reader supports 
 * both SAX and DOM approaches to xml parsing
 * @author Mikko Hilpinen
 * @since 24.1.2018
 */
class XmlReader(stream: InputStream, charset: Charset = StandardCharsets.UTF_8) extends AutoCloseable
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
     * Parses the contents of a single xml element, including all its children. this reader is then 
     * moved to the next sibling element or higher
     * @return the parsed element
     */
    def readElement() = if (isAtDocumentEnd) None else Some(_readElement()._1.toXmlElement)
    
    /**
     * Parses the contents of all remaining elements under the current parent element. this reader 
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
     * Moves this reader to the next element (child, sibling, etc.)
     * @return how much the 'depth' of this reader changed in the process (1 for child, 
     * 0 for sibling, -1 for parent level and so on)
     */
    def toNextElement() = _toNextElementStart()
    
    /**
     * Moves this reader to the next element (child, sibling, etc.) with a name that is accepted 
     * by the provided filter
     * @param nameFilter a filter that determines whether the name is accepted or not
     * @return how much the 'depth' of this reader changed in the process (1 for child, 
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
     * Moves this reader to the next element (child, sibling, etc.) with the specified name
     * @param searchedName the name the targeted element must have (case-insensitive)
     * @return how much the 'depth' of this reader changed in the process (1 for child, 
     * 0 for sibling, -1 for parent level and so on)
     */
    def toNextElementWithName(searchedName: String): Int = toNextElementWithName { 
            searchedName.equalsIgnoreCase(_) }
    
    /**
     * Moves this reader to the next element with a name accepted by the provided filter. Limits the 
     * search to elements under the current element (children, grand children, etc.). If no such 
     * element is found, stops at the next sibling, parent or higher.
     * @param nameFilter the filter that defines whether an element name is accepted
     * @return Whether such a child element was found (if true, this reader is now at the searched element)
     */
    def toNextChildWithName(nameFilter: String => Boolean) = 
    {
        var depthChange = toNextElement()
        while (depthChange > 0 && currentElementName.exists(!nameFilter(_)))
        {
            depthChange += toNextElement()
        }
        depthChange > 0
    }
    
    /**
     * Moves this reader to the next element with the specified name. Limits the 
     * search to elements under the current element (children, grand children, etc.). If no such 
     * element is found, stops at the next sibling, parent or higher.
     * @param searchedName the name of the searched element (case-insensitive)
     * @return Whether such a child element was found (if true, this reader is now at the searched element)
     */
    def toNextChildWithName(searchedName: String): Boolean = toNextChildWithName { 
            searchedName.equalsIgnoreCase(_) }
    
    /**
     * Moves this reader to the next sibling element that has a name that is accepted by the provided 
     * filter. If no such sibling is found, stops at the next parent level element or higher.
     * @param nameFilter the filter that determines whether an element name is accepted
     * @return Whether such a sibling was found (if true, this reader is now at the searched element)
     */
    def toNextSiblingWithName(nameFilter: String => Boolean) = 
    {
        var depthChange = skipElement()
        while (depthChange == 0 && currentElementName.exists(!nameFilter(_)))
        {
            depthChange += skipElement()
        }
        depthChange == 0
    }
    
    /**
     * Moves this reader to the next sibling element that has the specified name. If no such 
     * sibling is found, stops at the next parent level element or higher.
     * @param searchedName the name of the searched element (case-insensitive)
     * @return Whether such a sibling was found (if true, this reader is now at the searched element)
     */
    def toNextSiblingWithName(searchedName: String): Boolean = toNextSiblingWithName { 
            searchedName.equalsIgnoreCase(_) }
    
    /**
     * Skips this element and moves to the next sibling, parent or higher
     * @return how much the 'depth' of this reader changed in the process 
     * (0 for sibling, -1 for parent level and so on)
     */
    def skipElement() = skip(0)
    
    /**
     * Skips this element as well as any siblings this element may have and moves to the parent's 
     * next sibling or higher
     * @return how much the 'depth' of this reader changed in the process (-1 for parent level, 
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