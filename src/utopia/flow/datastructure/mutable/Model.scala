package utopia.flow.datastructure.mutable
import utopia.flow.datastructure.template
import utopia.flow.datastructure.template.Property
import scala.collection.immutable.HashSet
import utopia.flow.generic.Value

/**
 * This is a mutable implementation of the Model template
 * @author Mikko Hilpinen
 * @since 27.11.2016
 * @param Attribute The type of attribute stored within this model
 * @param attributeFactory The function used for instantiating new attributes in this model. The 
 * first function parameter is the name of the attribute and the second is a possible preset 
 * value for the attribute.
 */
class Model[Attribute <: Property](val attributeFactory: (String, Option[Value]) => Option[Attribute]) 
        extends template.Model[Attribute]
{
    // ATTRIBUTES    --------------
    
    private var _attributes = HashSet[Attribute]()
    def attributes = _attributes
    
    
    // IMPLEMENTED METHODS    -----
    
    override protected def generateAttribute(attName: String) = generateAttribute(attName, None)
    
    
    // OPERATORS    ---------------
    
    
    // OTHER METHODS    -----------
    
    protected def generateAttribute(attName: String, value: Option[Value]) = 
    {
        // In addition to creating the attribute, adds it to the model
        val attribute = attributeFactory(attName, value)
        attribute.foreach { _attributes += _ }
        attribute
    }
}