package utopia.flow.datastructure.immutable

import utopia.flow.util.CollectionExtensions._
import utopia.flow.datastructure.template.{NoSuchAttributeException, Property}
import utopia.flow.datastructure.template
import utopia.flow.generic.DataType

import scala.collection.immutable.VectorBuilder
import scala.util.{Failure, Success}

object ModelDeclaration
{
    /**
      * Creates a new model declaration
      * @param declarations Property declarations
      */
    def apply(declarations: Seq[PropertyDeclaration]) = new ModelDeclaration(declarations.distinctWith {
        case (a, b) => a.name.equalsIgnoreCase(b.name) }.toSet)
    
    /**
      * Creates a model declaration with a single property
      * @param declaration property declaration
      */
    def apply(declaration: PropertyDeclaration) = new ModelDeclaration(Set(declaration))
    
    /**
      * Creates a new model declaration
      */
    def apply(first: PropertyDeclaration, second: PropertyDeclaration, more: PropertyDeclaration*): ModelDeclaration =
        apply(Vector(first, second) ++ more)
    
    /**
      * Creates a new model declaration from property name - data type -pairs
      */
    def apply(first: (String, DataType), second: (String, DataType), more: (String, DataType)*): ModelDeclaration =
        apply((Vector(first, second) ++ more).map { case (name, t) => PropertyDeclaration(name, t) })
}

/**
 * Used to access a number of property declarations
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
case class ModelDeclaration private(declarations: Set[PropertyDeclaration])
{
    // COMP. PROPERTIES    ----
    
    /**
     * The names of the properties declared in this declaration
     */
    def propertyNames = declarations.map { _.name }
    
    
    // OPERATORS    -----------
    
    /**
     * Creates a new declaration with the provided declaration included
     */
    def +(declaration: PropertyDeclaration) = new ModelDeclaration(declarations + declaration)
    
    /**
     * Creates a new declaration with the provided declarations included
     */
    def ++(declarations: Traversable[PropertyDeclaration]) = new ModelDeclaration(
            this.declarations ++ declarations)
    
    /**
     * Creates a new declaration that also contains the declarations of the other declaration
     */
    def ++(other: ModelDeclaration): ModelDeclaration = this ++ other.declarations
    
    /**
     * Creates a new declaration without the provided property declaration
     */
    def -(declaration: PropertyDeclaration) = new ModelDeclaration(declarations - declaration)
    
    /**
     * Creates a new declaration without any of the provided declarations
     */
    def --(declarations: Traversable[PropertyDeclaration]) = new ModelDeclaration(
            this.declarations -- declarations)
    
    /**
     * Creates a new declaration wihthout any declarations from the provided model
     */
    def --(other: ModelDeclaration): ModelDeclaration = this -- other.declarations
    
    
    // OTHER METHODS    -------
    
    /**
     * Finds a property declaration with the provided name, if one exists
     * @param propertyName The name of the property
     * @return the declaration for the property, if one exists
     */
    def find(propertyName: String) = declarations.find { _.name.equalsIgnoreCase(propertyName) }
    
    /**
     * Finds a property declaration with the provided name or fails
     * @param propertyName The name of the property
     * @return The declaration for the property
     * @throws NoSuchAttributeException if there is no such declaration
     */
    @throws(classOf[NoSuchAttributeException])
    def get(propertyName: String) = find(propertyName).getOrElse(
            throw new NoSuchAttributeException(s"No property named '$propertyName' declared"))
    
    /*
    def validate(model: template.Model[Property]) =
    {
        // Tries to convert all declared model properties to required types and checks that each declared (non-default)
        // property has been defined
        val castValuesBuilder = new VectorBuilder[Constant]()
        
        
        
        /*
        val castValues = new VectorBuilder[(String, Value)]
    
        // Checks each requirement
        val error = requirements.findMap
        {
            case (paramName, dataType) =>
                if (!params.contains(paramName))
                    Some(Failure(new IllegalArgumentException(requiredParameterMessage)))
                else
                {
                    val cast = params(paramName).castTo(dataType)
                    if (cast.isEmpty)
                        Some(Failure(new IllegalArgumentException(s"$paramName must be of type $dataType")))
                    else
                    {
                        castValues += (paramName -> cast.get)
                        None
                    }
                }
        }
    
        // Either returns failure or the new parameters
        error.getOrElse
        {
            val nonCast = params.filterNot { param => requirements.exists { _._1 == param.name } }
            Success(nonCast ++ Model(castValues.result()))
        }*/
    }*/
}