package utopia.flow.datastructure.mutable

import utopia.flow.datastructure.template
import utopia.flow.datastructure.immutable
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.SimpleVariableGenerator
import utopia.flow.generic.PropertyGenerator
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.generic.SimpleConstantGenerator
import scala.collection.immutable.HashMap

object Model
{
    /**
     * Creates a new model with existing set of attributes.
     * @param content The attribute name value pairs used for generating the model's attributes
     * @param generator The attribute generator
     * @return A generated model
     */
    def apply[Attribute <: Variable](content: Traversable[(String, Value)], 
            generator: PropertyGenerator[Attribute] = new SimpleVariableGenerator()) = 
    {
        val model = new Model(generator)
        content.foreach { case (name, value) => model(name) = value }
        model
    }    
}

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
    
    private var _attributeMap = HashMap[String, Attribute]()
    def attributeMap = _attributeMap
    
    
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
        if (existing.isDefined) existing.get.value = value else generateAttribute(attName, Some(value))
        Unit
    }
    
    /**
     * Adds a new attribute to this model. If an attribute with the same name already exists, it 
     * is overwritten
     * @param attribute The attribute added to this model
     */
    def +=(attribute: Attribute) = _attributeMap += attribute.name.toLowerCase() -> attribute
    
    /**
     * Adds a number of attributes to this model
     * @param attributes The attributes added to this model
     */
    def ++=(attributes: Traversable[Attribute]) = attributes.foreach { this += _ }
    
    /**
     * Removes an attribute from this model
     * @param attribute The attribute that is removed from this model
     */
    def -=(attribute: Attribute) = _attributeMap = _attributeMap.filter { case (_, att) => att != attribute }
    
    
    // OTHER METHODS    -----------
    
    /**
     * Creates an immutable version of this model by using the provided attribute generator
     * @param generator The attribute generator used by the new model. Default is a simple constant 
     * generator that generates instances of Constant
     */
    def immutableCopy[T <: Constant](generator: PropertyGenerator[T] = new SimpleConstantGenerator()) = 
        new immutable.Model(attributes.map { att => generator(att.name, Some(att.value)) }, generator)
    
    protected def generateAttribute(attName: String, value: Option[Value]) = 
    {
        // In addition to creating the attribute, adds it to the model
        val attribute = attributeGenerator(attName, value)
        this += attribute
        attribute
    }
}