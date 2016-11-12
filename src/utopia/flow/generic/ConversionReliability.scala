package utopia.flow.generic

/**
 * A conversion reliability defines how costly a conversion between two data types is
 * @author Mikko Hilpinen
 * @since 7.11.2016
 * @param cost The cost of this conversion (in some arbitrary relative units)
 */
sealed abstract class ConversionReliability(val cost: Int)
{
    // OPERATORS    -----------
    
    /**
     * Checks whether this conversion reliability is higher (better) than the other
     * @param other The reliability this one is compared against
     * @return Whether this reliability is higher than the other
     */
    def > (other: ConversionReliability) = cost < other.cost
    
    /**
     * Checks whether this conversion reliability is lower (worse) than the other
     * @param other The reliability this one is compared against
     * @return Whether this reliability is lower than the other
     */
    def < (other: ConversionReliability) = cost > other.cost
    
    /**
     * Checks whether this conversion reliability is higher than or equal to the other
     * @param other The reliability this one is compared against
     * @return Whether this reliability is higher than or equal to the other
     */
    def >= (other: ConversionReliability) = !(this < other)
    
    /**
     * Checks whether this conversion reliability is lower than or equal to the other
     * @param other The reliability this one is compared against
     * @return Whether this reliability is lower than or equal to the other
     */
    def <= (other: ConversionReliability) = !(this > other)
}

object ConversionReliability
{
    /**
	 * The data type doesn't have to be casted at all, and already represents the target type. 
	 * A cast from an integer to a number would be this kind of operation.
	 */
    case object NO_CONVERSION extends ConversionReliability(1)
    /**
	 * The data type cast will never fail and preserves the value so that when the value 
	 * is cast back to the original data type, the value would stay equal. A conversion 
	 * from an integer to a double number would be a perfect conversion (1 -> 1.0).
	 */
    case object PERFECT extends ConversionReliability(5)
    /**
	 * The data type cast will never fail, but the value may lose some of its data. The 
	 * remaining data preserves its meaning and will work properly, however.
	 * A conversion from a double number to an integer would be a reliable conversion 
	 * (1.23 -> 1).
	 */
    case object DATA_LOSS extends ConversionReliability(26)
    /**
	 * The data type cast will never fail, but the meaning of the data may be lost. A conversion 
	 * from a String representing an integer 2015 to boolean would be this kind of conversion 
	 * ("2015" -> false)
	 */
    case object MEANING_LOSS extends ConversionReliability(100)
    /**
	 * The data type cast may fail, depending from the casted value. The meaning of the 
	 * value may also be lost. A conversion from a String "Jones" to a double would be this kind 
	 * of conversion conversion ("Jones" -> ).
	 */
    case object DANGEROUS extends ConversionReliability(154)
}