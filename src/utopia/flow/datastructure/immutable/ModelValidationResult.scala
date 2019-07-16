package utopia.flow.datastructure.immutable

import utopia.flow.generic.DataType

/**
  * A simple struct that contains model validation results (model on success or missing information on failure)
  * @author Mikko Hilpinen
  * @since 16.7.2019, v1.6+
  * @param success The successfully validated model. None if validation wasn't successful.
  * @param missingProperties The properties that were missing from the model
  * @param invalidConversions Properties that failed to convert to desired data type
  */
case class ModelValidationResult private(success: Option[Model[Constant]], missingProperties: Set[PropertyDeclaration],
										 invalidConversions: Set[(Constant, DataType)])
{
	// COMPUTED	-----------------
	
	/**
	  * @return Whether the validation was a success
	  */
	def isSuccess = success.isDefined
	
	/**
	  * @return Whether validation failed
	  */
	def isFailure = !isSuccess
	
	/**
	  * @return Names of missing properties
	  */
	def missingPropertyNames = missingProperties.map { _.name }
}
