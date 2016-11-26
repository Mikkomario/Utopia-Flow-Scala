package utopia.flow.datastructure.template

import utopia.flow.generic.Value
import utopia.flow.util.Equatable

/**
 * Properties are named and contain a value
 * @author Mikko Hilpinen
 * @since 26.11.2016
 */
trait Property extends Node[Value]
{
    // PROPERTIES    -----------
    
    def name: String
    
    
    // COMP. PROPERTIES    -----
    
    /**
     * The value of this property as a string
     */
    def stringValue = content.toString
    
    /**
     * The value of this property as an integer
     */
    def intValue = content.toInt
    
    /**
     * The value of this property as a double
     */
    def doubleValue = content.toDouble
    
    /**
     * The value of this property as a float
     */
    def floatValue = content.toFloat
    
    /**
     * The value of this property as a long
     */
    def longValue = content.toLong
    
    /**
     * The value of this property as a boolean
     */
    def booleanValue = content.toBoolean
    
    /**
     * The value of this property
     */
    def anyValue = content.content
}