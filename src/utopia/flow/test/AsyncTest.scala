package utopia.flow.test

import utopia.flow.util.TimeExtensions._
import utopia.flow.async.AsyncExtensions._

import scala.concurrent.{ExecutionContext, Future}
import utopia.flow.async.ThreadPool
import utopia.flow.util.WaitUtils
import java.time.Duration

import utopia.flow.collection.VolatileList

/**
 * This app tests some asynchronous functions
 */
object AsyncTest extends App
{
    // Creates the thread pool and the execution context
    implicit val context: ExecutionContext = new ThreadPool("test-main", 3, 6,
            Duration.ofSeconds(2), e => e.printStackTrace()).executionContext
    
    val starts = VolatileList[Int]()
    val ends = VolatileList[Int]()
    
    // Function for starting asynchronous processes
    def makeFuture(index: Int) = Future 
    {
        println(s"Starting future $index")
        starts :+= index
        WaitUtils.wait(Duration.ofSeconds(2), new AnyRef())
        println(s"Finishing future $index")
        ends :+= index
    }
    
    // Starts 20 asynchronous processes. Only 6 of them should run at any time
    val futures = (1 to 20).map(makeFuture)
    
    // Waits until all of the futures have completed
    println("Waiting for all futures to complete")
    futures.foreach { _.waitFor() }
    
    println("All futures completed, checks results")
    
    val finalStarts = starts.get.sorted
    val finalEnds = ends.get.sorted
    
    println("Started:")
    println(finalStarts.map { _.toString() }.reduce { _ + ", " + _ })
    println("Ended:")
    println(finalEnds.map { _.toString() }.reduce { _ + ", " + _ })
    
    assert(finalStarts.size == 20)
    assert(finalEnds.size == 20)
    
    println("Success!")
}