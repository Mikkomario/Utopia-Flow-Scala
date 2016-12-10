package utopia.flow.util

import utopia.flow.generic.AnyType
import utopia.flow.generic.DataType
import utopia.flow.datastructure.immutable.Value
import utopia.flow.datastructure.mutable.Variable

/**
 * This simple variable generator implementation is able to create variables using a possible 
 * default value
 * @author Mikko Hilpinen
 * @since 1.12.2016
 */
case class SimpleVariableGenerator[T <: Variable](val defaultValue: Option[Value] = None, 
        createVariable: (String, Value) => T = new Variable(_, _))
{
    /**
     * Generates a new variable
     * @param varName The name of the new variable
     * @param value the value assigned to the variable (optional)
     */
    def apply(varName: String, value: Option[Value]) = value.orElse(defaultValue).flatMap { 
        value => Some(createVariable(varName, value)) }
}