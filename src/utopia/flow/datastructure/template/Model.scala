package utopia.flow.datastructure.template

import utopia.flow.datastructure.immutable.Value
import java.util.NoSuchElementException

/**
 * Models are used for storing named values
 * @author Mikko Hilpinen
 * @since 26.11.2016
 * @param Attribute The type of the properties stored within this model
 */
trait Model[Attribute <: Property]
{
    // ATTRIBUTES    --------------
    
    /**
     * The attributes stored in this model. The key is the attribute's name (lower case), 
     * the value is the attribute itself
     */
    def attributeMap: Map[String, Attribute]
    
    
    // COMP. PROPERTIES    --------
    
    override def toString = toJSON
    
    /**
     * Converts this model into a JSON string. Only non-empty properties will be included.
     */
    def toJSON = 
    {
        val s = new StringBuilder()
        s += '{'
        
        val jsonProps = attributes.toSeq.flatMap { _.toJSON }
        if (!jsonProps.isEmpty)
        {
            s ++= jsonProps.head
            jsonProps.tail.foreach { json => s ++= s", $json"}
        }
        
        s += '}'
        s.toString()
    }
    
    def attributes = attributeMap.values.toSet
    
    /**
     * The names of the attributes stored in this model
     */
    def attributeNames = attributeMap.keySet
    
    /**
     * The attributes which have a defined value
     */
    def attributesWithValue = attributes.filter { _.value.isDefined }
    
    /**
     * A model is empty when no attributes have been assigned to it. It doesn't matter whether the
     * defined attributes actually contain a value or not.
     */
    def isEmpty = attributeMap.isEmpty
    
    
    // TRAIT METHODS    -----------
    
    /**
     * Generates a new attribute with the provided name
     * @param attName The name of the new attribute
     * @return The new attribute
     */
    protected def generateAttribute(attName: String): Attribute
    
    
    // OPERATORS    ---------------
    
    /**
     * Gets the value of a single attribute in this model
     * @param attName The name of the attribute from which the value is taken
     * @return The value of the attribute with the provided name
     */
    def apply(attName: String) = get(attName).value
    
    
    // OTHER METHODS    -----------
    
    /**
     * Finds an existing attribute from this model. No new attributes will be generated
     * @param attName The name of the attribute
     * @return an attribute in this model with the provided name or None if no such attribute 
     * exists
     */
    def findExisting(attName: String) = attributeMap.get(attName.toLowerCase())
    
    /**
     * Finds an attribute from this model. Generating one if necessary.
     * @param attName The name of the attribute
     * @return The attribute from this model (possibly generated)
     */
    def get(attName: String) = findExisting(attName).getOrElse(generateAttribute(attName))
}