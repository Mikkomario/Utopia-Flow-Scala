package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template

/**
 * This is the immutable model implementation
 * The model will only accept constant properties
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Model[Attribute <: Constant](content: Traversable[Attribute], 
        attributeFactory: (String) => Option[Attribute]) extends template.Model[Attribute]
{
    // ATTRIBUTES    ------------
    
    // Filters out duplicates (case-insensitive)
    val attributes = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // IMPLEMENTED METHODS    --
    
    override def generateAttribute(attName: String) = attributeFactory(attName)
}