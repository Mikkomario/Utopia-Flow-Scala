package utopia.flow.test

/**
  * This test runs all of the other tests
  * @author Mikko Hilpinen
  * @since 8.5.2019, v1+
  */
object AllTests extends App
{
	def run(test: App) =
	{
		println(s"\nRunning ${test.getClass.getName}	------------------------------")
		test.main(Array())
	}
	
	run(TimeNumberTest)
	run(DataTypeTest)
	run(CollectionTest)
	run(CounterTest)
	run(TreeNodeTest)
	run(GraphTest)
	run(ValueAccessorTest)
	run(ModelDeclarationTest)
	run(ModelTest)
	run(ModelConvertibleTest)
	run(JSONTest)
	run(XmlTest)
	run(VolatileTest)
	run(WeakListTest)
	run(AsyncTest)
	run(LoopTest)
	
	println("All test completed")
}
