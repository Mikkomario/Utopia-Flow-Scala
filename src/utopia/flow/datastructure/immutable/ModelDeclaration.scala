package utopia.flow.datastructure.immutable

import utopia.flow.util.Equatable
import utopia.flow.datastructure.template.NoSuchAttributeException

/**
 * A Model Declaration is an interface to a number of variable declarations
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
class ModelDeclaration(content: Traversable[PropertyDeclaration]) extends Equatable
{
    // PROPERTIES    -----------
    
    /**
     * The unique property declarations within this model declaration. No two instances with 
     * the same name exist
     */
    val declarations = content.groupBy { _.name.toLowerCase() }.values.map { _.head }.toSet
    
    
    // COMP. PROPERTIES    ----
    
    override def properties = Vector(declarations)
    
    /**
     * The names of the properties declared in this declaration
     */
    def propertyNames = declarations.map { _.name }
    
    
    // CONSTRUCTOR OVERLOAD    ---
    
    def this(content: PropertyDeclaration*) = this(content)
    
    
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
}