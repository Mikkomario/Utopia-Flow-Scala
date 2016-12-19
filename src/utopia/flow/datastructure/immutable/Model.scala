package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template
import utopia.flow.util.Equatable
import utopia.flow.generic.PropertyGenerator
import utopia.flow.generic.SimpleConstantGenerator
import utopia.flow.generic.PropertyGenerator
import utopia.flow.datastructure.mutable
import utopia.flow.generic.SimpleVariableGenerator

object Model
{
    /**
     * Creates a new model with input format that is more friendly to literals
     * @param content The attribute name value pairs used for generating the model's attributes
     * @param generator The attribute generator that will generate the attributes
     * @return The newly generated model
     */
    def apply[Attribute <: Constant](content: Traversable[(String, Value)], 
            generator: PropertyGenerator[Attribute] = new SimpleConstantGenerator()) = 
            new Model(content.map { case (name, value) => generator(name, Some(value)) }, generator)
}

/**
 * This is the immutable model implementation
 * The model will only accept constant properties
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Model[Attribute <: Constant](content: Traversable[Attribute], 
        val attributeGenerator: PropertyGenerator[Attribute] = new SimpleConstantGenerator()) extends 
        template.Model[Attribute] with Equatable
{
    // ATTRIBUTES    --------------
    
    // Filters out duplicates (case-insensitive)
    val attributes = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // COMP. PROPERTIES    -------
    
    override def properties = Vector(attributes, attributeGenerator)
    
    
    // IMPLEMENTED METHODS    ----
    
    override def generateAttribute(attName: String) = attributeGenerator(attName, None)
    
    
    // OPERATORS    --------------
    
    /**
     * Creates a new model with the provided attribute added
     */
    def +(attribute: Attribute) = withAttributes(attributes + attribute)
    
    /**
     * Creates a new model with the provided attributes added
     */
    def ++(attributes: Traversable[Attribute]) = withAttributes(this.attributes ++ attributes)
    
    /**
     * Creates a new model that contains the attributes from both of the models. The new model 
     * will still use this model's attribute generator
     */
    def ++(other: Model[Attribute]): Model[Attribute] = this ++ other.attributes
    
    /**
     * Creates a new model without the provided attribute
     */
    def -(attribute: Attribute) = new Model(attributes.filterNot { _ == attribute}, attributeGenerator)
    
    /**
     * Creates a new model without the provided attributes
     */
    def --(attributes: Set[Attribute]): Model[Attribute] = new Model(
            this.attributes.filterNot { attributes.contains(_) }, attributeGenerator)
    
    /**
     * Creates a new model without any attributes within the provided model
     */
    def --(other: Model[Attribute]): Model[Attribute] = this -- other.attributes
    
    
    // OTHER METHODS    ------
    
    /**
     * Creates a new model with the same generator but different attributes
     */
    def withAttributes(attributes: Traversable[Attribute]) = new Model(attributes, attributeGenerator)
    
    /**
     * Creates a new model with the same attributes but a different attribute generator
     */
    def withGenerator(generator: PropertyGenerator[Attribute]) = new Model(attributes, generator)
    
    /**
     * Creates a mutable copy of this model
     * @param generator The property generator used for creating the properties of the new model
     * @return A mutable copy of this model using the provided property generator
     */
    def mutableCopy[T <: mutable.Variable](generator: PropertyGenerator[T] = new SimpleVariableGenerator()) = 
    {
        val copy = new mutable.Model(generator)
        attributes.foreach { att => copy(att.name) = att.value }
        
        copy
    }
}