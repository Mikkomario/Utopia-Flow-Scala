package utopia.flow.event

/**
  * Changing instances generate change events
  * @author Mikko Hilpinen
  * @since 26.5.2019, v1.4.1+
  */
trait Changing[A]
{
	// ATTRIBUTES	-----------------
	
	/**
	  * The listeners interested in this mutable item's changes
	  */
	var listeners = Vector[ChangeListener[A]]()
	
	
	// ABSTRACT	---------------------
	
	/**
	  * @return The current value of this changing element
	  */
	def value: A
	
	
	// OTHER	--------------------
	
	/**
	  * Adds a new listener to this mutable
	  * @param changeListener A change listener that will be informed when the value of this mutable changes
	  */
	def addListener(changeListener: ChangeListener[A]) = listeners :+= changeListener
	
	/**
	  * Removes a listener from the informed change listeners
	  * @param listener A listener that will be removed
	  */
	def removeListener(listener: Any) = listeners = listeners.filterNot { _ == listener }
	
	/**
	  * Fires a change event for all the listeners
	  * @param oldValue The old value of this changing element (call-by-name)
	  */
	protected def fireChangeEvent(oldValue: => A) =
	{
		lazy val event = ChangeEvent(oldValue, value)
		listeners.foreach { _.onChangeEvent(event) }
	}
}
