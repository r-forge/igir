% 
% TODO: Write documentation 
% 
\name{RDyNetGraph}
\alias{RDyNetGraph}
\title{
	RDyNetGraph class
}
\description{
	RDyNetGraph is a class defining a graph in R for DyNet. It includes all features of the graph users want to visualise. Once creating a RDyNetGraph, users can change 
	attributes of the graph, specified nodes or links. Users can also specify local layout of sub-groups of nodes.      
}
\usage{
	RDyNetGraph(node_tab, link_tab, graph_attr=NULL)
}
\arguments{
	\item{node_tab}{
		a table includes labels, ids, and all other attributes of nodes. 
	}
	\item{link_tab}{
		a table includes labels, source_ids, target_ids and all other attributes of links.
	}
	\item{graph_attr}{
		a vector of characters specifying attributes of graph. 
	}
}
\value{
	a RDyNetGraph object
}
\details{
	Once a RDyNetGraph object has been created, the attributes of nodes and links can be changed by referring to its members .node and .link (see RDyNetNode and RDyNetLink 
	for details). Users can also change the attribute of the entire graph by member function SetAttr. The local or global layout can be specified by member function Layout.  
}
\author{
	Xin Wang
}
\examples{
	# demo("demo", "IGIR")
}

