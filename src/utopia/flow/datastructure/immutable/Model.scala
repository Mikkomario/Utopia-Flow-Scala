package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template
import utopia.flow.util.Equatable
import utopia.flow.generic.PropertyGenerator
import utopia.flow.generic.SimpleConstantGenerator
import utopia.flow.generic.PropertyGenerator
import utopia.flow.datastructure.mutable
import utopia.flow.generic.SimpleVariableGenerator
import utopia.flow.generic.ValueConvertible
import utopia.flow.generic.ModelType

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
class Model[+Attribute <: Constant](content: Traversable[Attribute], 
        val attributeGenerator: PropertyGenerator[Attribute] = new SimpleConstantGenerator()) extends 
        template.Model[Attribute] with Equatable with ValueConvertible
{
    // ATTRIBUTES    --------------
    
    // Filters out duplicates (case-insensitive) (if there are duplicates, last instance is used)
    val attributeMap = content.groupBy { _.name.toLowerCase() }.map { case (name, atts) => name -> atts.last }
    
    
    // COMP. PROPERTIES    -------
    
    override def properties = Vector(attributes, attributeGenerator)
    
    override def toValue = new Value(Some(this), ModelType)
    
    
    // IMPLEMENTED METHODS    ----
    
    override def generateAttribute(attName: String) = attributeGenerator(attName, None)
    
    
    // OPERATORS    --------------
    
    /**
     * Creates a new model with the provided attribute added
     */
    def +[B >: Attribute <: Constant](attribute: B) = withAttributes(attributes :+ attribute)
    
    /**
     * Creates a new model with the provided attributes added
     */
    def ++[B >: Attribute <: Constant](attributes: Traversable[B]) = 
            withAttributes(this.attributes ++ attributes);
    
    /**
     * Creates a new model that contains the attributes from both of the models. The new model 
     * will still use this model's attribute generator
     */
    def ++[B >: Attribute <: Constant](other: Model[B]): Model[B] = this ++ other.attributes
    
    /**
     * Creates a new model without the provided attribute
     */
    def -[B >: Attribute <: Constant](attribute: B) = new Model(attributes.filterNot { 
            _ == attribute}, attributeGenerator)
    
    /**
     * Creates a new model without an attribute with the provided name (case-insensitive)
     */
    def -(attributeName: String) = new Model(attributes.filterNot { 
            _.name.toLowerCase == attributeName.toLowerCase }, attributeGenerator)
    
    /**
     * Creates a new model without the provided attributes
     */
    def --[B >: Attribute <: Constant](attributes: Seq[B]): Model[Attribute] = new Model(
            this.attributes.filterNot { attributes.contains(_) }, attributeGenerator)
    
    /**
     * Creates a new model without any attributes within the provided model
     */
    def --[B >: Attribute <: Constant](other: Model[B]): Model[B] = this -- other.attributes
    
    
    // OTHER METHODS    ------
    
    /**
     * Creates a new model with the same generator but different attributes
     */
    def withAttributes[B >: Attribute <: Constant](attributes: Traversable[B]) = 
            new Model[B](attributes, attributeGenerator);
    
    /**
     * Creates a new model with the same attributes but a different attribute generator
     */
    def withGenerator[B >: Attribute <: Constant](generator: PropertyGenerator[B]) = 
            new Model[B](attributes, generator);
    
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