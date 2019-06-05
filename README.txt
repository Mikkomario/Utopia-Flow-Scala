UTOPIA FLOW --------------------------------

Purpose
-------

    Utopia Flow is the base building block for other Utopia Libraries. Flow offers various tools for advanced data
    handling, as well as many generally required features.


Main Features
-------------

    Typeless data handling
        - Value and Model classes offer a way to handle data without type information, conveniently converting between
        various supported types like Double, Int, String and Vector
        - This allows one to represent and handle typeless data like SQL or JSON values

    JSON and XML support & Integration with models and typeless values
        - Flow offers full support for JSON and XML parsing + writing that fully utilizes the power of the typeless values
        - Conversion between JSON and Model / Value is seamless and converts between supported types under the hood

    Various data structures
        - Tree and Graph supports
        - Support for weakly referenced lists and pointer-like data structures
        - Mutable concurrent collections (Volatile, VolatileFlag, VolatileList and VolatileOption)

    Tools for asynchronous programs
        - ThreadPool implementation for generating a scaling ExecutionContext
        - WaitUtils for synchronous waiting
        - Loop and Breakable traits for looping, asynchronous operations, as well as support for shutdown hooks
        - WaitFor -simplification of Future waiting available through extension


Usage Notes
-----------

    When using typeless values, please call utopia.flow.generic.DataType.setup() on program startup. Please also
    import utopia.flow.generic.ValueConversions._ when creating new typeless values.


Available Extensions
--------------------

    utopia.flow.util.CollectionExtensions
        - Collection utility updates, like support for multimaps and optional return values instead of indexOutOfBounds

    utopia.flow.util.TimeExtensions
        - Functional additions to java.time classes
        - Conversion between java.time.Duration and scala.concurrent.duration.Duration
        - Easy creation of scala.concurrent.duration.FiniteDuration by using numeric extension

    utopia.flow.async.AsyncExtensions
        - Utility update to Future

    utopia.flow.generic.ValueConversions
        - Implicit conversions from value supported classes (Int, Double, String, Vector[Value], etc.) to Value

v1.5  ------------------------------------

    New Features
    ------------

        CollectionExtensions updated
            - Map merge feature added
            - divideBy added
            - foreachWith added for simultaneous iterations
            - Repeating iterations added

        Trees updated
            - replace and findAndReplace added to immutable TreeLike
            - allContent added to TreeLike

        AsyncExtensions updated
            - Traversable items containing futures can now be treated like a future themselves with
            waitFor, waitForSuccesses, future and futureSuccesses

        ActionQueue added
            - Allows sequential completion of multiple operations
            - Variable width allows control over how many actions are performed simultaneously

        Pointers with events added + property change events added to mutable models
            - Now you can listen to changes within a model or a pointer by adding a listener to it

        Easier duration creation added through importing TimeExtensions._
            - You can now write 3.seconds, 5.millis etc.

    Changes & Updates
    -----------------

        java.time.Duration usage changed to scala.concurrent.duration.Duration

    Bugfixes
    -----------

        Major bugfix in ThreadPool, which didn't queue actions correctly


v1.4  --------------------------------------

    New Features
    ------------

        ThreadPool class for creating an asynchronous ExecutionContext

        Breakable tasks and loops for asynchronous operations

        Extensions to Future

        Volatile classes as safe multithreaded collections

        CloseHook for ShutDownHook implementation


    Updates & Changes
    -----------------

        Package structure changed

        Tree refactored
        template.Tree renamed to TreeLike. Added immutable and mutable TreeLikes as well. XmlElement extends immutable.TreeLike

        Deprecated functions removed from Value. Value is now a case class

        getX methods added to Value

        Template version of GraphNode and GraphEdge added. Graph classes refactored.

        BugFix to Counter class

        Model constructors updated