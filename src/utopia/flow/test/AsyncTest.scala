package utopia.flow.test

import scala.concurrent.Promise

/**
 * This app tests some asynchronous functions
 */
object AsyncTest extends App
{
    val promise = Promise[Boolean]()
    promise.success(true)
    
}