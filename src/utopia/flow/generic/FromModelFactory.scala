package utopia.flow.generic

import utopia.flow.datastructure.template.Model
import utopia.flow.datastructure.template.Property
import utopia.flow.parse.JSONReader

/**
 * This trait is extended by instance factories that can convert model data into object data. 
 * The factory may make assumptions about the type of model data and may give more sensible results 
 * with other models than with others.
 */
trait FromModelFactory[+T]
{
    // ABSTRACT METHODS    ----------------------
    
    /**
     * Parses an instance by reading the data from a model instance
     * @return an instance parsed from model data. None if no instance could be parsed.
     */
    def apply(model: Model[Property]): Option[T]
    
    
    // OTHER METHODS   --------------------------
    
    /**
     * Parses an instance from a JSON string. Returns none if either the JSON string couldn't be 
     * parsed or if the instance couldn't be parsed from read data.
     */
    def fromJSON(json: String) = JSONReader.parseSingle(json).toOption.flatMap(apply)
}