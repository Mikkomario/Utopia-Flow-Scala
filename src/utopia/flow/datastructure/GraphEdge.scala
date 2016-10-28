package utopia.flow.datastructure

/**
 * Graph edges are used for connecting two nodes and storing data. Edges are immutable.
 * @author Mikko Hilpinen
 * @since 28.10.2016
 */
class GraphEdge[NodeContent, EdgeContent](val content: EdgeContent, 
        val end: GraphNode[NodeContent, EdgeContent]) extends Node[EdgeContent]
{
    // OTHER METHODS    ----------
    
    /**
     * Creates a new edge that has different content
     * @param content The contents of the new edge
     */
    def withContent(content: EdgeContent) = new GraphEdge(content, end)
    
    /**
     * Creates a new edge that has a different end node
     * @param node The node the new edge points towards
     */
    def pointingTo[T](node: GraphNode[T, EdgeContent]) = new GraphEdge(content, node)
}