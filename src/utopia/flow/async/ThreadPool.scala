package utopia.flow.async

import utopia.flow.async.AsyncExtensions._
import utopia.flow.util.CollectionExtensions._

import java.util.concurrent.Executor
import utopia.flow.util.Counter
import utopia.flow.util.WaitUtils
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.concurrent.ExecutionContext

/**
* This class handles thread reuse and distribution
* @author Mikko Hilpinen
* @since 28.3.2019
**/
class ThreadPool(val name: String, coreSize: Int, val maxSize: Int, val maxIdleDuration: FiniteDuration, 
        val errorHandler: Throwable => Unit) extends Executor
{
    // ATTRIBUTES    ---------------------
    
    private val indexCounter = new Counter(1)
    // Creates the core threads from the very beginning
    private val threads = new VolatileList(Vector.fill(coreSize)(WorkerThread.core(nextCoreName(), errorHandler, () => nextQueueTask)))
    private val queue = VolatileList[() => Unit]()
    
    /**
     * An execution context based on this thread pool
     */
    lazy val executionContext = ExecutionContext.fromExecutor(this)
    
    
    // IMPLEMENTED    --------------------
    
	def execute(action: Runnable): Unit = execute(() => action.run())
	
	
	// OTHER    --------------------------
	
	/**
	 * Executes a task asynchronously. If maximum amount of simultaneous tasks has been reached, 
	 * the execution of the task will wait until some of the previous tasks have been handled
	 */
	def execute(task: () => Unit) = 
	{
        threads.update
        {
            current => 
                
                val filtered = current.filterNot { _.isEnded }
                
                // First checks if any of the existing threads accepts the task
                if (filtered.exists { _ offer task })
                    filtered
                else
                {
                    // If all were busy, tries to create a new thread
                    if (filtered.size < maxSize)
                        filtered :+ WorkerThread.temp(nextThreadName(), maxIdleDuration, task, errorHandler, () => nextQueueTask())
                    else
                    {
                        // If max thread limit is reached, pushes the task to queue
                        queue :+= task
                        filtered
                    }
                }
        }
	}
	
	private def nextCoreName() = s"$name-core-${indexCounter.next()}"
    
    private def nextThreadName() = s"$name-${indexCounter.next()}"
    
    private def nextQueueTask() = queue.pop()
}

private object WorkerThread
{
    def core(name: String, errorHandler: Throwable => Unit, getWaitingTask: () => Option[() => Unit]) = 
    {
        val t = new WorkerThread(name, Duration.Inf, None, errorHandler, getWaitingTask)
        t.start()
        t
    }
    
    def temp(name: String, maxIdleDuration: Duration, initialTask: () => Unit, 
            errorHandler: Throwable => Unit, getWaitingTask: () => Option[() => Unit]) = 
    {
        val t = new WorkerThread(name, maxIdleDuration, Some(initialTask), errorHandler, getWaitingTask)
        t.start()
        t
    }
}

private class WorkerThread(name: String, val maxIdleDuration: Duration, initialTask: Option[() => Unit], 
        val errorHandler: Throwable => Unit, val getWaitingTask: () => Option[() => Unit]) extends Thread
{
    // ATTRIBUTES    ---------------------
    
    private val ended = new VolatileFlag()
    private val waiting = new VolatileFlag()
    private val nextTask = new Volatile(Promise[() => Unit]())
    
    
    // INITIAL CODE    -------------------
    
    setName(name)
    setDaemon(true)
    
    initialTask.foreach { nextTask.get.success(_) }
    
    
    // COMPUTED    -----------------------
    
    def isEnded = ended.isSet
    
    
    // IMPLEMENTED    --------------------
    
    override def run() = 
    {
        var waitingTask: Option[() => Unit] = None
        
        while (!ended.isSet)
        {
            // Finds the next task to perform, may fail if maximum idle duration is reached
            val next = waitingTask orElse 
            {
                val nextFuture = nextTask.get.future
                if (!nextFuture.isCompleted)
                    waiting.set
                nextFuture.waitFor(maxIdleDuration).toOption
            }
            
            // If no task was found, ends
            if (next.isEmpty)
                ended.set()
            else
            {
                // Otherwise performs the task (caches errors)
                Try(next.get()).failure.foreach(errorHandler)
                
                // Takes the next task right away, if one is available
                waitingTask = getWaitingTask()
            }
        }
        
        // TODO: Clear memory?
    }
    
    
    // OTHER    -------------------------
    
    /**
     * Offers a new task for this thread. This thread will accept the task if it's not busy already
     * @param task the task to be performed
     * @return whether this thread accepted the task
     */
    def offer(task: () => Unit) = 
    {
        // Only accepts new tasks if not busy already
        if (!ended.isSet)
        {
            waiting.pop
            {
                isWaiting =>
                    
                    if (isWaiting)
                    {
                        nextTask.update
                        {
                            current => 
                                // Completes the current task and prepares the next promise
                                current.success(task)
                                Promise()
                        }
                        true -> false
                    }
                    else
                        false -> isWaiting
            }
        }
        else
            false
    }
}