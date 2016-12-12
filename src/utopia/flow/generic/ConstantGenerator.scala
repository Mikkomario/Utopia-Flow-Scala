package utopia.flow.generic

import utopia.flow.datastructure.immutable.Constant

/**
 * ConstantGenerators are used for generating new constants quickly. The generators should be 
 * stateless so that they return the same (or similar) constant for each call
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
trait ConstantGenerator[T <: Constant]
{
    /**
     * Generates a new constant with the provided name
     * @param constantName The name for the new constant
     * @return A new constant with the provided name or None if the constant couldn't be generated
     */
    def apply(constantName: String): Option[T]
}