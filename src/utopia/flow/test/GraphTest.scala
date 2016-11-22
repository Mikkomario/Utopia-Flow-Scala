package utopia.flow.test

import utopia.flow.datastructure.mutable.GraphNode

/**
 * This test tests the features implemented in graph, graphNode and graphEdge
 */
object GraphTest
{
    def main(args: Array[String]): Unit = 
	{
        type IntNode = GraphNode[Int, Int]
        
        // Creates a test graph first
        val node1 = new IntNode(1)
        val node2 = new IntNode(2)
        val node3 = new IntNode(3)
        val node4 = new IntNode(4)
        val node5 = new IntNode(5)
        
        // Connects the nodes (1 -> 2 -> 3 -> 5, 1 -> 4 -> 5) using weighted edges
        node1.connect(node2, 1)
        node2.connect(node3, 5)
        node3.connect(node5, 1)
        
        node1.connect(node4, 4)
        node4.connect(node5, 4)
        
        // Makes sure there are correct number of edges in nodes
        assert(node1.leavingEdges.size == 2)
        assert(node2.leavingEdges.size == 1)
        assert(node5.leavingEdges.isEmpty)
        
        // Finds the routes from 1 to 5. Should have 2 routes
        assert(node1.routesTo(node5).size == 2)
        
        // The shortest route should be 1 -> 4 -> 5
        assert(node1.shortestRouteTo(node5).get.toVector.size == 2)
        // The cheapest route (weights considered) should be 1 -> 2 -> 3 -> 5
        val cheapestRoute = node1.cheapestRouteTo(node5, edge => edge.content)
        assert(cheapestRoute.get.toVector.size == 3)
        
        // After disconnecting node 5 from node 4. Only one route should remain
        node4.disconnect(node5)
        assert(node1.routesTo(node5).size == 1)
        
        print("Success")
	}
}