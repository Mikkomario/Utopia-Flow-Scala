package utopia.flow.parse

import utopia.flow.datastructure.immutable
import utopia.flow.datastructure.mutable

import scala.collection.mutable.ListBuffer
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Value

class JSONValueReader
{
    def parse(json: String) = 
    {
        val objects = ListBuffer[immutable.Model[Constant]]()
        var index = 0
        
        while (index >= 0)
        {
            // Starts after the first object start
            val objectStart = findNext(json, index, ObjectStart)
            
            if (objectStart.isDefined)
            {
                // Parse object contents
                val parsedModel = parseObject(json, objectStart.get._2)
                
                //objects :+ parsedModel._1
                //index = parsedModel._2 + 1
            }
            else
            {
                index = -1
            }
        }
        
        // TODO: Catch invalid format exception
        
        objects.toVector
    }
    
    private def parseObject(json: String, objStartIndex: Int) = 
    {
        val model = new mutable.Model()
        
        // Parses each of the properties
        var continue = true
        var index = objStartIndex
        while (continue)
        {
            // Finds the next property name start or the end of the object
            val nextEvent = next(json, index + 1, Quote, ObjectEnd)
            
            nextEvent._1 match 
            {
                case Quote => Unit // TODO: Parse property (name + value)
                // Returns the parsed model when the object end is reached
                case _ => continue = false
            }
            
            index = nextEvent._2
        }
        
        Tuple2(model.immutableCopy(), index)
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
        val parsedValue = parsePropertyValue(json, assignment._2)
        
        // TODO: return property + index
    }
    
    private def parsePropertyValue(json: String, assignmentIndex: Int) = 
    {
        // Finds either a) start of object, b) start of array, 
        // c) end marker (separator, end object, end array)
        val nextEvent = next(json, assignmentIndex + 1, ObjectStart, ArrayStart, ObjectEnd, ArrayEnd, Separator)
        
        nextEvent._1 match 
        {
            // case object: Parses the object and wraps it into a value
            case ObjectStart => None //parseObject(json, nextEvent.get._2).map { 
                //case (model, index) => Tuple2(Value of model, index) }
            case ArrayStart => None // Parse array
            case _ => None // Parse simple value
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