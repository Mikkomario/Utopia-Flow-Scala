package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template
import utopia.flow.util.Equatable

/**
 * This is the immutable model implementation
 * The model will only accept constant properties
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Model[Attribute <: Constant](val attributeFactory: (String) => Option[Attribute], 
        content: Traversable[Attribute]) extends template.Model[Attribute] with Equatable
{
    // ATTRIBUTES    --------------
    
    // Filters out duplicates (case-insensitive)
    val attributes = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // COMP. PROPERTIES    -------
    
    override def properties = Vector(attributes, attributeFactory)
    
    
    // CONSTRUCTOR OVERLOAD    ---
    
    def this(attributeFactory: (String) => Option[Attribute], content: Attribute*) = 
            this(attributeFactory, content)
    
    
    // IMPLEMENTED METHODS    ----
    
    override def generateAttribute(attName: String) = attributeFactory(attName)
    
    
    // OPERATORS    --------------
    
    def +(attribute: Attribute) = new Model(attributeFactory, attributes + attribute)
    
    def ++(attributes: Traversable[Attribute]) = new Model(attributeFactory, this.attributes ++ attributes)
    
    def ++(other: Model[Attribute]) = new Model(attributeFactory, attributes ++ other.attributes)
    
    def -(attribute: Attribute) = new Model(attributeFactory, attributes.filterNot { _ == attribute})
    
    def --(attributes: Set[Attribute]): Model[Attribute] = new Model(attributeFactory, 
            this.attributes.filterNot { attributes.contains(_) })
    
    def --(other: Model[Attribute]): Model[Attribute] = this -- other.attributes
}