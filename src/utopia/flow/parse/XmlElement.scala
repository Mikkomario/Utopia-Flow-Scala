package utopia.flow.parse

import utopia.flow.generic.ValueConversions._
import utopia.flow.datastructure.template

import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.StringType
import utopia.flow.datastructure.immutable.Model
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.generic.ModelConvertible
import utopia.flow.generic.FromModelFactory
import utopia.flow.datastructure.template.Property
import utopia.flow.util.Equatable
import scala.collection.immutable.VectorBuilder

object XmlElement extends FromModelFactory[XmlElement]
{
    def apply(model: template.Model[Property]) = 
    {
        // If the name is not provided by user, it is read from the model
        model("name").string.map(XmlElement.apply(_, model))
    }
    
    /**
     * Parses an xml element from a model
     * @param name the name for the xml element
     * @param model the model that contains the element attributes. The following attribute names 
     * are used:<br>
     * - value / text: Element value<br>
     * - children: Element children (array of models)<br>
     * - attributes: Element attributes (model)<br>
     * Unused attributes are converted into children or attributes if some of the primary attributes 
     * were missing.
     */
    def apply(name: String, model: template.Model[Property]): XmlElement = 
    {
        // Value is either in 'value' or 'text' attribute
        val valueAttribute = model.findExisting("value")
        val value = valueAttribute.map(_.value).orElse(
                model.findExisting("text").map(_.value)).getOrElse(Value.empty(StringType))
        
        // There may be some unused / non-standard attributes in the model
        val unspecifiedAttributes = model.attributes.filter(att => 
            att.name != valueAttribute.map(_.name).getOrElse("text") && att.name != "attributes" && 
            att.name != "children" && att.name != "name");
        
        // Children are either read from 'children' attribute or from the unused attributes
        val specifiedChildren = model.findExisting("children").map(_.value.vectorOr().flatMap(
                _.model).flatMap(XmlElement(_)));
        val children = specifiedChildren.getOrElse
        {
            // Expects model type but parses other types as well
            val modelChildren = unspecifiedAttributes.flatMap(att => 
            {
                val modelValue = att.value.model
                modelValue.map((att.name, _))
            }).map(p => XmlElement(p._1, p._2))
            
            // Other types are parsed into simple xml elements
            val nonModelChildren = unspecifiedAttributes.filterNot(att => 
                modelChildren.exists(_.name == att.name)).map(att => 
                new XmlElement(att.name, att.value));
            
            modelChildren ++ nonModelChildren
        }
        
        // Attributes are either read from 'attributes' attribute or from the unused attributes
        val specifiedAttributes = model.findExisting("attributes").map(_.value.modelOr())
        val attributes = specifiedAttributes.getOrElse
        {
            if (specifiedChildren.isDefined)
            {
                new Model(unspecifiedAttributes.map(att => new Constant(att.name, att.value)))
            }
            else
            {
                // If unused attributes were parsed into children, doesn't parse them into attributes
                Model.empty
            }
        }
        
        new XmlElement(name, value, attributes, children)
    }
}

/**
 * XML Elements are used for representing XML data
 * @author Mikko Hilpinen
 * @since 13.1.2017 (v1.3)
 */
class XmlElement(val name: String, val value: Value = Value.empty(StringType), 
        val attributes: Model[Constant] = Model(Vector()), val children: Seq[XmlElement] = Vector()) 
        extends ModelConvertible with Equatable
{
    // COMPUTED PROPERTIES    ------------------
    
    /**
     * The text inside this xml element. None if the element doesn't contain any text
     */
    def text = value.string
    override def toModel: Model[Constant] = 
    {
        val atts = new VectorBuilder[Tuple2[String, Value]]
        atts += ("name" -> name)
        
        if (!value.isEmpty)
        {
            atts += ("value" -> value)
        }
        
        // Children are only included if necessary
        if (!children.isEmpty)
        {
            atts += ("children" -> children.map(_.toModel).toVector)
        }
        
        // Attributes are also only included if necessary
        if (!attributes.isEmpty)
        {
            atts += ("attributes" -> attributes)
        }
        
        Model(atts.result())
    }
    
    override def properties = Vector(name, value, attributes, children)
    
    def toXml: String = 
    {
        // Case: Empty element
        if (text.forall(_.isEmpty) && children.isEmpty)
        {
            // Eg. <foo att1="2"/>
            s"<$name${ attributesString.map(" " + _).getOrElse("") }/>"
        }
        else
        {
            // Eg. <foo att1="2">Test value</foo>
            // Or <foo><bar/></foo>
            s"<$name${ attributesString.map(" " + _).getOrElse("") }>${ text.getOrElse("") }${ 
                    children.map(_.toXml).reduceLeftOption(_ + _).getOrElse("") }</$name>"
        }
    }
    
    // Eg. 'att1="abc" att2="3"'. None if empty
    private def attributesString = attributes.attributes.map(a => 
            s"${a.name}=${"\""}${a.value.stringOr()}${"\""}").reduceOption(_ + " " + _);
    
    
    // OTHER METHODS    ------------------------
    
    /**
     * Finds the first child with the provided name
     */
    def childWithName(name: String) = children.find(_.name.equalsIgnoreCase(name))
    
    /**
     * Finds the children with the provided name
     */
    def childrenWithName(name: String) = children.filter(_.name.equalsIgnoreCase(name))
    
    /**
     * Finds the value for an attribute with the specified name
     */
    def valueForAttribute(attName: String) = attributes(attName)
}