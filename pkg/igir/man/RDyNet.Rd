% 
% TODO: Write documentation 
% 
\name{RDyNet}
\alias{RDyNet}
\title{
	RDyNet class
}
\description{
	This class acts as the interface class between RDyNetGraph and network graphic engines.   
}
\usage{
	RDyNet(rdynet_graph, save_filename=NULL, dynet_path=NULL)
}
\arguments{
	\item{rdynet_graph}{
		a RDyNetGraph object. (See RDyNetGraph for details) 
	}
	\item{save_filename}{
		the name of the temporary file to store in XGMML format.
	}
	\item{dynet_path}{
		the path of DyNet software. 
	}
}
\value{
	a RDyNet object
}
\details{
	To interact with stand-alone network graphic softwares, IGIR adopts XGMML as the standard format. It takes RDyNetGraph as input, generates XGMML file and invoke stand-alone graphic softwares 
	to visualise.  
}
\author{
	Xin Wang
}
\examples{
	# demo("demo", "IGIR")
}
