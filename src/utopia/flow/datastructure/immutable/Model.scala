package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template

/**
 * This is the immutable model implementation
 * The model will only accept constant properties
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Model[Attribute <: Constant](val attributeFactory: (String) => Option[Attribute], 
        content: Traversable[Attribute]) extends template.Model[Attribute]
{
    // ATTRIBUTES    --------------
    
    // Filters out duplicates (case-insensitive)
    val attributes = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // CONSTRUCTOR OVERLOAD    ---
    
    def this(attributeFactory: (String) => Option[Attribute], content: Attribute*) = 
            this(attributeFactory, content)
    
    
    // IMPLEMENTED METHODS    ----
    
    override def generateAttribute(attName: String) = attributeFactory(attName)
    
    
    // OPERATORS    --------------
    
    def +(attribute: Attribute) = new Model(attributeFactory, attributes + attribute)
    
    def ++(attributes: Traversable[Attribute]) = new Model(attributeFactory, this.attributes ++ attributes)
    
    def ++(other: Model[Attribute]) = new Model(attributeFactory, attributes ++ other.attributes)
}