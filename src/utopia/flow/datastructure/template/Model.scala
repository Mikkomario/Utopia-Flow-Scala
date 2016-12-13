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
     * The attributes stored in this model
     */
    def attributes: Set[Attribute]
    
    
    // COMP. PROPERTIES    --------
    
    override def toString = 
    {
        val s = new StringBuilder()
        s += '{'
        
        val iterator = attributes.iterator
        if (iterator.hasNext)
        {
            s ++= iterator.next().toString()
        }
        while (iterator.hasNext)
        {
            s ++= ", "
            s ++= iterator.next().toString()
        }
        
        s += '}'
        s.toString()
    }
    
    /**
     * The names of the attributes stored in this model
     */
    def attributeNames = attributes.map { _.name }
    
    
    // TRAIT METHODS    -----------
    
    /**
     * Generates a new attribute with the provided name
     * @param attName The name of the new attribute
     * @return The new attribute or None if generation was not possible
     */
    protected def generateAttribute(attName: String): Option[Attribute]
    
    
    // OPERATORS    ---------------
    
    /**
     * Gets the value of a single attribute in this model
     * @param attName The name of the attribute from which the value is taken
     * @return The value of the attribute with the provided name
     * @throws NoSuchAttributeException if no such attribute was present and one couldn't be generated 
     * either
     */
    @throws(classOf[NoSuchAttributeException])
    def apply(attName: String) = get(attName).content
    
    
    // OTHER METHODS    -----------
    
    /**
     * Finds an attribute from this model. Generating one if necessary.
     * @param attName The name of the attribute
     * @return The attribute, if this model contains such an attribute
     */
    def find(attName: String) = 
        attributes.find { _.name.equalsIgnoreCase(attName) } orElse generateAttribute(attName)
    
    /**
     * Gets the attribute from this model, generating one if necessary
     * @param attName the name of the attribute
     * @return The attribute from this model with the provided name
     * @throws NoSuchAttributeException If the model didn't contain such an attribute
     */
    def get(attName: String) = find(attName).getOrElse(
            throw new NoSuchAttributeException(s"This model doesn't contain attribute named $attName"))
        
    /**
     * Finds the contents of a single attribute
     * @param attName The name of the attribute
     * @return The value of the attribute, if one exists
     */
    def valueOf(attName: String) = find(attName).map { _.content }
}