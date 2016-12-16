package utopia.flow.datastructure.mutable

import utopia.flow.datastructure.template
import scala.collection.immutable.HashSet
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.SimpleVariableGenerator
import utopia.flow.generic.PropertyGenerator

/**
 * This is a mutable implementation of the Model template
 * @author Mikko Hilpinen
 * @since 27.11.2016
 * @param Attribute The type of attribute stored within this model
 * @param attributeGenerator The variable generator used for generating new values on this model
 */
class Model[Attribute <: Variable](val attributeGenerator: PropertyGenerator[Attribute] = 
        new SimpleVariableGenerator()) extends template.Model[Attribute]
{
    // ATTRIBUTES    --------------
    
    private var _attributes = HashSet[Attribute]()
    def attributes = _attributes
    
    
    // IMPLEMENTED METHODS    -----
    
    override protected def generateAttribute(attName: String) = generateAttribute(attName, None)
    
    
    // OPERATORS    ---------------
    
    /**
     * Updates the value of a single attribute within this model
     * @param attName The name of the updated attribute
     * @value The value assigned to the attribute
     */
    def update(attName: String, value: Value) = 
    {
        val existing = findExisting(attName)
        if (existing.isDefined) existing.get.content = value else generateAttribute(attName, Some(value))
        Unit
    }
    
    /**
     * Adds a new attribute to this model. If an attribute with the same name already exists, it 
     * is overwritten
     * @param attribute The attribute added to this model
     */
    def +=(attribute: Attribute) = 
    {
        _attributes = attributes.filterNot { _.name.equalsIgnoreCase(attribute.name) } + attribute
    }
    
    /**
     * Adds a number of attributes to this model
     * @param attributes The attributes added to this model
     */
    def ++=(attributes: Traversable[Attribute]) = attributes.foreach { this += _ }
    
    /**
     * Removes an attribute from this model
     * @param attribute The attribute that is removed from this model
     */
    def -=(attribute: Attribute) = _attributes -= attribute
    
    
    // OTHER METHODS    -----------
    
    protected def generateAttribute(attName: String, value: Option[Value]) = 
    {
        // In addition to creating the attribute, adds it to the model
        val attribute = attributeGenerator(attName, value)
        _attributes += attribute
        attribute
    }
}