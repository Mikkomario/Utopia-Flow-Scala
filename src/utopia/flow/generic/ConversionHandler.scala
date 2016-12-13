package utopia.flow.generic

import utopia.flow.datastructure.mutable.GraphNode
import utopia.flow.generic.ConversionReliability.NO_CONVERSION
import scala.collection.immutable.HashMap
import scala.collection.mutable
import utopia.flow.datastructure.immutable.Value

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
    
    private var conversionGraph = HashMap[DataType, ConversionNode]()
    
    private val optimalRoutes = mutable.HashMap[Tuple2[DataType, DataType], Option[ConversionRoute]]()
    
    
    // OTHER METHODS    ----------------
    
    /**
     * Introduces a new value caster to be used by the conversion handler
     * @param caster The new caster that is introduced
     */
    def addCaster(caster: ValueCaster) = caster.conversions.foreach { addConversion(_, caster) }
    
    /**
     * Casts a value to the desired data type (or any of the target type's sub types)
     * @param value The source value that is being casted
     * @param toType The desired target data type
     * @return The value casted to the target data type or any of the sub types of the target data
     * type
     * @throws ValueCastException if there was no way to convert the value to the desired data type
     * or if the conversion failed at some point
     */
    @throws(classOf[ValueCastException])
    def cast(value: Value, toType: DataType) = 
    {
        // If value is already of desired type, doesn't need to cast
        if (value.dataType isOfType toType)
        {
            value
        }
        else
        {
            // Finds the possible ways to cast the value to the target type or any target sub type
            val targetTypes = toType.subTypes :+ toType
            try
            {
                _cast(value, targetTypes)
            }
            catch 
            {
                case e: ValueCastException => throw new ValueCastException(value, toType, e)
            }
        }
    }
    
    private def _cast(value: Value, targetTypes: Traversable[DataType]) = 
    {
        val routes = targetTypes.flatMap { optimalRouteTo(value.dataType, _) }
            
        // Only works if at least a single conversion was found
        if (routes.isEmpty)
        {
            throw new ValueCastException(value, targetTypes.head)
        }
        else
        {
            // Casts the value using the optimal route / target type
            routes.reduceLeft { 
                (route1, route2) => if (route2.cost < route1.cost) route2 else route1 }(value)
        }
    }
    
    /**
     * Casts a value to a different data type
     * @param value The value that is being cast
     * @param toType The target data type
     * @return The value, if it was casted properly, None otherwise
     */
    def safeCast(value: Value, toType: DataType) = 
    {
        try
        {
            val castValue = cast(value, toType)
            Some(castValue)
        }
        catch
        {
            case _: DataTypeException => None
        }
    }
    
    /**
     * Casts the value to a value of any of the provided data types
     * @param value The value that is being casted
     * @param targetTypes The targeted data types
     * @return The value cast to one of the data types, None if casting failed or was not possible
     */
    def safeCast(value: Value, targetTypes: Set[DataType]) = 
    {
        // Checks if the value already is of any of the types
        if (targetTypes.exists { value.dataType isOfType _ })
        {
            Some(value)
        }
        else
        {
            // The targeted data types include the provided types, plus each of their sub types
            val allTargetTypes = targetTypes.flatMap { datatype => datatype.subTypes :+ datatype }
            
            if (allTargetTypes.isEmpty)
            {
                None
            }
            else
            {
                try
                {
                    val castValue = _cast(value, allTargetTypes)
                    Some(castValue)
                }
                catch 
                {
                    case e: ValueCastException => None
                }
            }
        }
    }
    
    //def routeString(from: DataType, to: DataType) = optimalRouteTo(from, to).fold("No route")(_.toString())
    //def costOfRoute(from: DataType, to: DataType) = optimalRouteTo(from, to).fold(9999)(_.cost)
    
    private def addConversion(conversion: Conversion, caster: ValueCaster) =
    {
        // Optimal routes are deprecated when new connections are introduced
        optimalRoutes.clear()
        
        // Makes sure both nodes exist
        val sourceNode = nodeForType(conversion.source)
        val targetNode = nodeForType(conversion.target)
        
        // Creates a new connection if one doesn't exist yet
        val existingConnection = sourceNode.edgeTo(targetNode)
        if (!existingConnection.isDefined)
        {
            sourceNode.connect(targetNode, ConversionStep(caster, conversion))
        }
        // Otherwise overwrites an existing connection if the new one is better or equally good
        else if (conversion.reliability >= existingConnection.get.content.reliability)
        {
            sourceNode.setConnection(targetNode, ConversionStep(caster, conversion))
        }
    }
    
    private def nodeForType(dataType: DataType) = conversionGraph.getOrElse(dataType, {
            val node = new ConversionNode(dataType)
            conversionGraph = conversionGraph.updated(dataType, node)
            node
        });
    
    private def optimalRouteTo(sourceType: DataType, targetType: DataType) = 
        optimalRoutes.getOrElseUpdate(Tuple2(sourceType, targetType), {
            val route = nodeForType(sourceType).cheapestRouteTo(nodeForType(targetType), { _.content.cost } )
            if (route.isDefined) Some(ConversionRoute(route.get.map( { _.content } ))) else None
        });
    
    
    // NESTED CLASSES    ---------------
    
    private case class ConversionRoute(val steps: Seq[ConversionStep])
    {
        // COMPUTED PROPERTIES    -----
        
        lazy val cost = steps.foldLeft(0)((totalCost, step) => totalCost + step.cost)
        
        lazy val reliability = steps.foldLeft(NO_CONVERSION: ConversionReliability)(
                (minReliability, step) => ConversionReliability.min(minReliability, step.reliability));
        
        
        // IMPLEMENTED METHODS    -----
        
        override def toString = if (steps.isEmpty) "Empty route" else steps.tail.foldLeft(
                steps.head.conversion.toString())((str, step) => str + " => " + step.conversion.toString())
        
        
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
            if (!value.dataType.isOfType(conversion.source))
                throw DataTypeException(s"Input of $value in conversion $conversion")
            caster.cast(value, conversion.target)
        }
    }
}