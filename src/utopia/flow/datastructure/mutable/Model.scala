package utopia.flow.datastructure.mutable
import utopia.flow.datastructure.template
import scala.collection.immutable.HashSet
import utopia.flow.datastructure.immutable.Value

/**
 * This is a mutable implementation of the Model template
 * @author Mikko Hilpinen
 * @since 27.11.2016
 * @param Attribute The type of attribute stored within this model
 * @param attributeFactory The function used for instantiating new attributes in this model. The 
 * first function parameter is the name of the attribute and the second is a possible preset 
 * value for the attribute.
 */
class Model[Attribute <: Variable](val attributeFactory: (String, Option[Value]) => Option[Attribute]) 
        extends template.Model[Attribute]
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
        val existing = find(attName)
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
        find(attribute.name).foreach { _attributes -= _ }
        _attributes += attribute
    }
    
    /**
     * Removes an attribute from this model
     * @param attribute The attribute that is removed from this model
     */
    def -=(attribute: Attribute) = _attributes -= attribute
    
    
    // OTHER METHODS    -----------
    
    protected def generateAttribute(attName: String, value: Option[Value]) = 
    {
        // In addition to creating the attribute, adds it to the model
        val attribute = attributeFactory(attName, value)
        attribute.foreach { _attributes += _ }
        attribute
    }
}