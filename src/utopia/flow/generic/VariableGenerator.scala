package utopia.flow.generic

import utopia.flow.datastructure.mutable.Variable
import utopia.flow.datastructure.immutable.Value

/**
 * Variable generators are able to generate new variables on demand. The generator should operate 
 * like a function and be stateless, returning similar value when it is called numerous times with 
 * identical attributes.
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
trait VariableGenerator[T <: Variable]
{
    /**
     * Generates a new variable with the provided name and possible value.
     * @param varName The name of the new variable
     * @param value The value assigned to the variable (optional)
     * @return The generated variable or None if a variable couldn't be generated
     */
    def apply(varName: String, value: Option[Value]): Option[T]
}