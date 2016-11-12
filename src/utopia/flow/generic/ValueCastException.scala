package utopia.flow.generic

/**
 * These exceptions are thrown when value casting fails
 * @author Mikko Hilpinen
 * @since 12.11.2016
 */
class ValueCastException(val sourceValue: Value, val targetType: DataType, cause: Throwable = null) 
        extends DataTypeException(s"Failed to cast $sourceValue to $targetType", cause)