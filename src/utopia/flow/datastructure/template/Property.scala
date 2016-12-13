package utopia.flow.datastructure.template

import utopia.flow.datastructure.immutable.Value
import utopia.flow.util.Equatable
import utopia.flow.generic.DataType

/**
 * Properties are named and contain a value in a certain data type
 * @author Mikko Hilpinen
 * @since 26.11.2016
 */
trait Property extends Node[Value]
{
    // PROPERTIES    -----------
    
    /**
     * The name of the property
     */
    def name: String
    
    /**
     * The data type of this property and its contents
     */
    def dataType: DataType
    
    
    // COMP. PROPERTIES    -----
    
    override def toString = s"$name: $content"
    
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