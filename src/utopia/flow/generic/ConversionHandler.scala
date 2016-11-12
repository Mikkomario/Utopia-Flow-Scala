package utopia.flow.generic

import utopia.flow.datastructure.mutable.GraphNode
import utopia.flow.generic.ConversionReliability.NO_CONVERSION
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * This object oversees all value conversion operations
 * @author Mikko Hilpinen
 * @since 12.11.2016
 */
object ConversionHandler
{
    // TYPES    ------------------------
    
    private type ConversionNode = GraphNode[DataType, ConversionStep]
    
    
    // ATTRIBUTES    -------------------
    
    private var _conversionGraph = HashMap[DataType, ConversionNode]()
    private def conversionGraph = _conversionGraph
    private def conversionGraph_=(graph: HashMap[DataType, ConversionNode]) = 
    {
        // Optimal routes are reset every time conversion graph updates
        optimalRoutes.clear()
        _conversionGraph = graph
    }
    
    private val optimalRoutes = mutable.HashMap[Tuple2[DataType, DataType], Option[ConversionRoute]]()
    
    
    // OTHER METHODS    ----------------
    
    private def addConversion(conversion: Conversion, caster: ValueCaster) =
    {
        // Optimal routes are deprecated when new connections are introduced
        optimalRoutes.clear()
        
        
    }
    
    
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
    
    private class SuperTypeCaster extends ValueCaster
    {
        // Allows conversion to any supertype
        override lazy val conversions = DataType.values.flatMap { dataType => dataType.superType.map { 
            superType => Conversion(dataType, superType, NO_CONVERSION) } }
        
        // No conversion is required since the value already represents an instance of the supertype
        override def cast(value: Value, toType: DataType) = value
    }
}