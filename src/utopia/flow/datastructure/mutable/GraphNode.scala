package utopia.flow.datastructure.mutable

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.List
import scala.collection.immutable.HashSet
import scala.Vector
import utopia.flow.datastructure.immutable.GraphEdge
import utopia.flow.datastructure.template.Node

/**
 * Graph nodes contain content and are connected to other graph nodes via edges
 * @author Mikko Hilpinen
 * @since 28.10.2016
 */
class GraphNode[NodeContent, EdgeContent](var content: NodeContent) 
        extends Node[NodeContent]
{
    // TYPES    --------------------
    
    type Node = GraphNode[NodeContent, EdgeContent]
    type AnyNode = GraphNode[_, _]
    type Edge = GraphEdge[NodeContent, EdgeContent]
    type AnyEdge = GraphEdge[_, _]
    type Route = List[Edge]
    
    
    // ATTRIBUTES    ---------------
    
    private lazy val emptyRoute = List[Edge]()
    
    /**
     * The edges that leave from this node, pointing to other nodes
     */
    var leavingEdges = HashSet[Edge]()
    
    
    // COMPUTED PROPERTIES    -------
    
    /**
     * The nodes accessible from this node
     */
    def endNodes = leavingEdges.map(edge => edge.end)
    
    
    // CONSTRUCTOR OVERLOAD    -----
    
    /**
     * Creates a copy of another node
     * @param other The node that is copied
     */
    def this(other: GraphNode[NodeContent, EdgeContent]) =
    {
        this(other.content)
        leavingEdges ++= other.leavingEdges
    }
    
    
    // OTHER METHODS    ------------
    
    /**
     * Connects this node to another node, creating a new edge. This will always create a new edge, 
     * even when there exists a connection already.
     * @param node The node this node will be connected to
     * @param edgeContent The contents for the edge that is generated (optional, default value
     * (provided in node constructor) is used by default)
     */
    def connect(node: Node, edgeContent: EdgeContent) = leavingEdges += new GraphEdge(edgeContent, node)
    
    /**
     * Replaces any existing connections to a certain node with a new edge
     * @param node The node this node will be connected to
     * @param edgeContent The content of the edge connecting the nodes
     */
    def setConnection(node: Node, edgeContent: EdgeContent) = 
    {
        if (edgeTo(node).isDefined)
        {
            disconnect(node)
        }
        connect(node, edgeContent)
    }
    
    /**
     * Removes any connection(s) to the provided node from this node. The provided node may still 
     * contain edges towards this node afterwards.
     * @param node The node that is disconnected from this node
     */
    def disconnect(node: AnyNode) = leavingEdges = leavingEdges.filterNot(edge => edge.end == node)
    
    /**
     * Finds an edge pointing to another node, if there is one
     * @param other The node this node may be connected to
     * @return an edge connecting the two nodes, if there is one. If there are multiple edges
     * pointing toward the specified node, returns one of them chosen randomly.
     * @see edgesTo(GraphNode[_, _])
     */
    def edgeTo(other: AnyNode) = leavingEdges.find(edge => edge.end == other)
    
    /**
     * Finds all edges pointing from this node to the provided node.
     * @param other another node
     * @return The edges pointing towards the provided node from this node
     */
    def edgesTo(other: AnyNode) = leavingEdges.filter(edge => edge.end == other)
    
    /**
     * Finds the shortest (least amount of edges) route from this node to another node
     * @param node the node traversed to
     * @return The shortest route from this node to the provided node, if any exist
     */
    def shortestRouteTo(node: AnyNode) = cheapestRouteTo(node, { edge => 1 })
    
    /**
     * Finds the 'cheapest' route from this node to the provided node, if there is one. A special 
     * function is used for calculating the route cost.
     * @param node The node traversed to
     * @param costOf The function used for calculating the cost of a single edge (based on the edge
     * contents, for example)
     * @return The cheapest route found, if any exist
     */
    def cheapestRouteTo(node: AnyNode, costOf: (Edge) => Double) = 
    {
        var cheapestRoute: Option[Route] = None
        var smallestCost = 0.0
        
        for (route <- routesTo(node))
        {
            val cost = route.foldLeft(0.0)((currentCost, edge) => currentCost + costOf(edge))
            if (cheapestRoute.isEmpty || cost < smallestCost)
            {
                cheapestRoute = Option(route)
                smallestCost = cost
            }
        }
        
        cheapestRoute
    }
    
    /**
     * Finds all routes (edge combinations) that connect this node to the provided node. Routes 
     * can't contain the same edge multiple times so no looping routes are included.
     * @param node The node this node may be connected to
     * @return All possible routes to the provided node. In case this node is the searched node,
     * however, a single empty route will be returned. The end node will always be at the end of
     * each route and nowhere else. If there are no connecting routes, an empty array is returned.
     */
    def routesTo(node: AnyNode): Vector[Route] = routesTo(node, Array[AnyNode]())
    
    // Uses recursion
    private def routesTo(node: AnyNode, visitedNodes: Array[AnyNode]): Vector[Route] = 
    {
        if (node == this)
        {
            Vector(emptyRoute)
        }
        else
        {
        	val routes = ArrayBuffer[Route]()
    
    		// Tries to find the destination from each connected edge that leads to a new node
    		for (edge <- leavingEdges.filterNot(edge => visitedNodes.contains(edge.end)))
    		{
    		    // Records each found route. The traversed edge is included in the returned route(s).
    	        routes ++= edge.end.routesTo(node, visitedNodes :+ this).map { 
    	                route => route.::(edge)}
    		}
        	
        	routes.toVector
        }
    }
}