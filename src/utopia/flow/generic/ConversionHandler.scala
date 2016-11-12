package utopia.flow.generic

import utopia.flow.datastructure.mutable.GraphNode
import utopia.flow.generic.ConversionReliability.NO_CONVERSION

/**
 * This object oversees all value conversion operations
 * @author Mikko Hilpinen
 * @since 12.11.2016
 */
object ConversionHandler
{
    // ATTRIBUTES    -------------------
    
    //private val conversionGraph = new GraphNode[DataType, ConversionStep](
    
    
    // NESTED CLASSES    ---------------
    
    private case class ConversionRoute(val steps: Seq[ConversionStep])
    {
        // COMPUTED PROPERTIES    -----
        
        lazy val cost = steps.foldLeft(0)((totalCost, step) => totalCost + step.cost)
        
        lazy val reliability = steps.foldLeft(NO_CONVERSION: ConversionReliability)(
                (minReliability, step) => ConversionReliability.min(minReliability, step.reliability));
        
        
        // OPERATORS    ---------------
        
        @throws(classOf[DataTypeException])
        def apply(value: Value) = steps.foldLeft(value)((value, step) => step(value))
    }
    
    private case class ConversionStep(val caster: ValueCaster, val conversion: Conversion)
    {
        // COMP. PROPS    -----------
        
        def reliability = conversion.reliability
        
        def cost = reliability.cost
        
        
        // OPERATORS    -------------
        
        @throws(classOf[DataTypeException])
        def apply(value: Value) =
        {
            if (value.dataType != conversion.source)
                throw DataTypeException(s"Input of $value in conversion $conversion")
            caster.cast(value, conversion.target)
        }
    }
}