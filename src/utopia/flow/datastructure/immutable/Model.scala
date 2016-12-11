package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template
import utopia.flow.util.Equatable
import utopia.flow.generic.ConstantGenerator

/**
 * This is the immutable model implementation
 * The model will only accept constant properties
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Model[Attribute <: Constant](content: Traversable[Attribute], 
        val attributeGenerator: Option[ConstantGenerator[Attribute]] = None) extends 
        template.Model[Attribute] with Equatable
{
    // ATTRIBUTES    --------------
    
    // Filters out duplicates (case-insensitive)
    val attributes = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // COMP. PROPERTIES    -------
    
    override def properties = Vector(attributes, attributeGenerator)
    
    
    // IMPLEMENTED METHODS    ----
    
    override def generateAttribute(attName: String) = attributeGenerator.flatMap { _(attName) }
    
    
    // OPERATORS    --------------
    
    /**
     * Creates a new model with the provided attribute added
     */
    def +(attribute: Attribute) = new Model(attributes + attribute, attributeGenerator)
    
    /**
     * Creates a new model with the provided attributes added
     */
    def ++(attributes: Traversable[Attribute]) = new Model(this.attributes ++ attributes, attributeGenerator)
    
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
}