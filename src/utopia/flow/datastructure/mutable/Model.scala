package utopia.flow.datastructure.mutable

import utopia.flow.datastructure.template
import utopia.flow.datastructure.immutable
import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.SimpleVariableGenerator
import utopia.flow.generic.PropertyGenerator
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.generic.SimpleConstantGenerator

import scala.collection.immutable.HashMap
import utopia.flow.datastructure.template.Property
import utopia.flow.event.{PropertyChangeEvent, PropertyChangeListener}

object Model
{
    /**
      * @return An empty model
      */
    def apply() = new Model(new SimpleVariableGenerator())
    
    /**
     * Creates a new model with existing set of attributes.
     * @param content The attribute name value pairs used for generating the model's attributes
     * @param generator The attribute generator
     * @return A generated model
     */
    def apply[Attribute <: Variable](content: TraversableOnce[(String, Value)], generator: PropertyGenerator[Attribute]) =
    {
        val model = new Model(generator)
        content.foreach { case (name, value) => model(name) = value }
        model
    }
    
    /**
      * Creates a new model with existing set of attributes.
      * @param content The attribute name value pairs used for generating the model's attributes
      * @return A generated model
      */
    def apply(content: TraversableOnce[(String, Value)]): Model[Variable] = apply(content, new SimpleVariableGenerator())
}

/**
 * This is a mutable implementation of the Model template
 * @author Mikko Hilpinen
 * @since 27.11.2016
 * @tparam Attribute The type of attribute stored within this model
 * @param attributeGenerator The variable generator used for generating new values on this model
 */
class Model[Attribute <: Variable](val attributeGenerator: PropertyGenerator[Attribute]) extends template.Model[Attribute]
{
    // ATTRIBUTES    --------------
    
    private var _attributeMap = HashMap[String, Attribute]()
    def attributeMap = _attributeMap
    
    /**
      * The listeners that are interested in changes in this model
      */
    var listeners = Vector[PropertyChangeListener]()
    
    
    // IMPLEMENTED METHODS    -----
    
    override protected def generateAttribute(attName: String) = generateAttribute(attName, None)
    
    
    // OPERATORS    ---------------
    
    /**
     * Updates the value of a single attribute within this model
     * @param attName The name of the updated attribute
     * @param value The value assigned to the attribute
     */
    def update(attName: String, value: Value) = 
    {
        // Replaces value & generates events, may generate a new attribute
        val existing = findExisting(attName)
        if (existing.isDefined)
        {
            val oldValue = existing.get.value
            existing.get.value = value
            lazy val event = PropertyChangeEvent(existing.get.name, oldValue, existing.get.value)
            listeners.foreach { _.onPropertyChanged(event) }
        }
        else
        {
            val generated = generateAttribute(attName, Some(value))
            lazy val event = PropertyChangeEvent.propertyAdded(generated)
            listeners.foreach { _.onPropertyChanged(event) }
        }
    }
    
    /**
     * Updates the value of a single attribute within this model
     * @param property a name value pair that will be updated or added
     */
    def update(property: Property): Unit = update(property.name, property.value)
    
    /**
     * Updates values of multiple attributes in this model
     */
    def update(data: template.Model[Property]): Unit = data.attributes.foreach(update)
    
    /**
     * Adds a new attribute to this model. If an attribute with the same name already exists, it 
     * is overwritten
     * @param attribute The attribute added to this model
     */
    def +=(attribute: Attribute) =
    {
        _attributeMap += attribute.name.toLowerCase() -> attribute
        lazy val event = PropertyChangeEvent.propertyAdded(attribute)
        listeners.foreach { _.onPropertyChanged(event) }
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
    def -=(attribute: Attribute) =
    {
        if (_attributeMap.valuesIterator.contains(attribute))
        {
            _attributeMap = _attributeMap.filter { case (_, att) => att != attribute }
            lazy val event = PropertyChangeEvent.propertyRemoved(attribute)
            listeners.foreach { _.onPropertyChanged(event) }
        }
    }
    
    
    // OTHER METHODS    -----------
    
    /**
     * Creates an immutable version of this model by using the provided attribute generator
     * @param generator The attribute generator used by the new model. Default is a simple constant 
     * generator that generates instances of Constant
     */
    def immutableCopy[T <: Constant](generator: PropertyGenerator[T]) =
        new immutable.Model(attributes.map { att => generator(att.name, Some(att.value)) }, generator)
    
    /**
      * Creates an immutable version of this model by using the provided attribute generator
      */
    def immutableCopy(): immutable.Model[Constant] = immutableCopy(new SimpleConstantGenerator())
    
    protected def generateAttribute(attName: String, value: Option[Value]) = 
    {
        // In addition to creating the attribute, adds it to the model
        val attribute = attributeGenerator(attName, value)
        this += attribute
        attribute
    }
}