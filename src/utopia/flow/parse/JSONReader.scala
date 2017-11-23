package utopia.flow.parse

import scala.collection.mutable.ListBuffer
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.DoubleType
import utopia.flow.generic.LongType
import utopia.flow.generic.IntType
import utopia.flow.datastructure.immutable.Model

import utopia.flow.generic.ValueConversions._
import scala.util.Try

/**
 * This object provides an interface that reads valid JSON data into model format
 * @author Mikko Hilpinen
 * @since 18.12.2016
 */
object JSONReader
{
    /**
     * Parses a model out of JSON data. The parsing will start at the first object start ('{') and
     * end at the end of that object. Only a single model will be parsed, even if there are multiple
     * siblings available
     * @param json The JSON string
     * @return The parsed model or None if there was no sufficient data or it was malformed.
     */
    def parseSingle(json: String) = 
    {
        // Starts at the first object start
        val objectStart = findNext(json, 0, ObjectStart)
        
        // Parse object contents
        try
        {
            objectStart.map { case (_, index) => parseObject(json, index)._1 }
        }
        catch 
        {
            case e: InvalidFormatException => None
        }
    }
    
    /**
     * Parses models out of JSON data. The parsing will start at the first object start ('{') and
     * continue until each of the objects have been parsed. If the data is malformed, the parsing 
     * will be stopped.
     * @param json The JSON string
     * @return The parsed models
     */
    def parseMany(json: String) = 
    {
        val objects = ListBuffer[Model[Constant]]()
        
        try
        {
            var index = findNext(json, 0, ObjectStart).map { _._2 }.getOrElse(-1)
            while (index >= 0)
            {
                val parsedObject = parseObject(json, index)
                
                objects += parsedObject._1
                index = findNext(json, index, ObjectStart).map { _._2 }.getOrElse(-1)
            }
        }
        catch 
        {
            // Exception is ignored, parsing is stopped
            case e: InvalidFormatException => Unit
        }
        
        objects.toVector
    }
    
    /**
     * Parses a single value from the provided JSON. Only the first value will be read, whether it 
     * is an array, object or a simple value. Fails if the provided json is malformed.
     */
    def parseValue(json: String) = Try(parsePropertyValue(json, 0)._1)
    
    private def parseObject(json: String, objStartIndex: Int) = 
    {   
        val properties = new ListBuffer[Constant]()
        var index = objStartIndex
        
        // Parses values until it stops at object end marker
        while (json.charAt(index) != ObjectEnd.marker)
        {
            // Finds the next property name start or the end of the object
            val nextEvent = next(json, index + 1, Quote, ObjectEnd)
            
            nextEvent._1 match 
            {
                case Quote => 
                {
                    // Parses the property
                    val parsedProperty = parseProperty(json, nextEvent._2)
                    index = parsedProperty._2
                    properties += parsedProperty._1 // NB: Previously checked if value was empty
                }
                // Doesn't parse the property and doesn't continue the loop
                case _ => index = nextEvent._2
            }
        }
        
        Tuple2(new Model(properties), index)
    }
    
    private def parseProperty(json: String, propertyStartIndex: Int) = 
    {
        // Parses the property name first
        val nameEnd = next(json, propertyStartIndex + 1, Quote)
        
        val nameEndIndex = nameEnd._2
        val propertyName = json.substring(propertyStartIndex + 1, nameEndIndex)
        
        // Skips the assignment operator
        val assignment = next(json, nameEndIndex + 1, Assignment)
        
        // Parses the property value
        val parsedValue = parsePropertyValue(json, assignment._2 + 1)
        
        // Returns the parsed property + the end marker index
        Tuple2(new Constant(propertyName, parsedValue._1), parsedValue._2)
    }
    
    // Ends at the next separator or container (array or object) end. Never inside content.
    private def parsePropertyValue(json: String, propertyStartIndex: Int): (Value, Int) = 
    {
        // Finds either a) start of object, b) start of array, 
        // c) end marker (separator, end object, end array)
        val nextEvent = findNext(json, propertyStartIndex, ObjectStart, ArrayStart, ObjectEnd, 
                ArrayEnd, Separator);
        
        if (nextEvent.isDefined)
        {
            nextEvent.get._1 match 
            {
                // case object: Parses the object and wraps it into a value
                case ObjectStart => 
                {
                    val nextObject = parseObject(json, nextEvent.get._2)
                    (nextObject._1, nextObject._2 + 1) // +1 to escape content range
                }
                case ArrayStart => 
                {
                    val nextArray = parseArray(json, nextEvent.get._2)
                    (nextArray._1, nextArray._2 + 1) // +1 to escape content range
                }
                case _ => (parseSimpleValue(json.substring(propertyStartIndex, nextEvent.get._2)), 
                        nextEvent.get._2)
            }
        }
        else 
        {
            // If didn't find a proper marker to handle, just reads the remaining string as a single 
            // simple value
            (parseSimpleValue(json.substring(propertyStartIndex)), json.length())
        }
    }
    
    private def parseArray(json: String, arrayStartIndex: Int) = 
    {
        // Checks first if the array is empty
        val firstArrayEndIndex = findNext(json, arrayStartIndex, ArrayEnd).map(_._2)
        val onlySpacesInArray = firstArrayEndIndex.exists(endIndex => 
            json.substring(arrayStartIndex + 1, endIndex).trim().isEmpty());
        
        if (onlySpacesInArray)
        {
            Tuple2(Vector(), firstArrayEndIndex.get)
        }
        else
        {
            val buffer = new ListBuffer[Value]()
            var index = arrayStartIndex
            
            // Parses values until it stops at array end marker
            while (json.charAt(index) != ArrayEnd.marker)
            {
                val parsedValue = parsePropertyValue(json, index + 1)
                index = parsedValue._2
                buffer += parsedValue._1 // NB: Previously checked that value is defined
            }
            
            Tuple2(buffer.toVector, index)
        }
    }
    
    private def parseSimpleValue(str: String): Value = 
    {
        val trimmed = str.trim()
        
        // Values may be empty in some special cases
        // also, 'null' (without quotations) is a synonym for empty value
        if (trimmed.isEmpty() || trimmed.equalsIgnoreCase("null"))
        {
            Value.empty()
        }
        // If there are any quotation markers, the contents are considered to be a string
        else if (trimmed.contains(Quote.marker))
        {
            trimmed.replace(Quote.marker + "", "")
        }
        // 'true' / 'false' are considered to be boolean
        else if (trimmed.equalsIgnoreCase("true"))
        {
            true
        }
        else if (trimmed.equalsIgnoreCase("false"))
        {
            false
        }
        else 
        {
            val parsedNumber = 
            {
                // Double is the only number format that contains a '.'
                if (trimmed.contains('.'))
                {
                    trimmed withType DoubleType
                }
                // Very long numbers are considered to be of type long
                else if (trimmed.length() >= 10)
                {
                    trimmed withType LongType
                }
                else
                {
                    trimmed withType IntType
                }
            }
            
            // If the number couldn't be parsed, returns the value as a string instead
            parsedNumber.orElse(trimmed)
        }
    }
    
    private def next(json: String, startIndex: Int, events: JSONReadEvent*) = 
    {
        findNext(json, startIndex, events: _*).getOrElse(throw new InvalidFormatException())
    }
    
    private def findNext(json: String, startIndex: Int, events: JSONReadEvent*) = 
    {
        if (events.isEmpty)
        {
            None
        }
        else
        {
            var best: Option[JSONReadEvent] = None
            var bestIndex: Option[Int] = None
            
            for (event <- events)
            {
                val index = json.indexOf(event.marker, startIndex)
                if (index >= 0 && !bestIndex.exists { _ < index })
                {
                    best = Some(event)
                    bestIndex = Some(index)
                }
            }
            
            if (best.isDefined) Some(Tuple2(best.get, bestIndex.get)) else None
        }
    }
    
    // These exceptions are thrown to interrupt the program flow when the provided JSON has invalid 
    // format
    private class InvalidFormatException extends Exception
}