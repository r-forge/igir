% 
% TODO: Write documentation 
% 
\name{RDyNetLink}
\alias{RDyNetLink}
\title{
	RDyNetLink class
}
\description{
	RDyNetLink is a class defining labels, source_ids, target_ids and all other attributes of links in a RDyNetGraph.      
}
\usage{
	RDyNetNode(link_tab)
}
\arguments{
	\item{link_tab}{
		a table includes labels, source_ids, target_ids and all other attributes of links. 
	}
}
\value{
	a RDyNetLink object
}
\details{
	Once a RDyNetLink object has been created, the attributes of links can be changed by referring to its member function SetLinkAttr.
}
\author{
	Xin Wang
}
\examples{
	# demo("demo", "IGIR")
}


