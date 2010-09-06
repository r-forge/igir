###############################################################################
# TODO: 
# Xin Wang <xw264@cam.ac.uk>
# Advisor: Florian Markowetz <florian.markowetz@cancer.org.uk> 
# University of Cambridge Deparment of Oncology
# Cancer Research UK - Cambridge Research Institute
# At 15:33:09, on 19 Aug 2010
###############################################################################
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Constructor~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setConstructorS3(
		name		=	"RDyNetGraph", 
		definition	=	function(node_tab=NULL, link_tab=NULL, graph_attr=NULL) {

							#set graph attributes
							##graph attributes template; can be changed here
							graph_temp	<-	data.frame(
									type				=	as.character(rep("real",4)),
									name 				= 	as.character(c("backgroundColor","GRAPH_VIEW_ZOOM","GRAPH_VIEW_CENTER_X","GRAPH_VIEW_CENTER_Y")),
									value				=	as.character(c("#ffffff","1.0","400","300")),
									stringsAsFactors	=	FALSE
							)
							if(!is.null(graph_attr)) {
								graph_temp.names	<-	graph_temp[, "name"]
								#check input
								if(!is.character(graph_attr))
									stop("[RDyNetGraph]: graph_attr must be in character format! \n")
								if(!all(names(graph_attr) %in% graph_temp.names))
									stop("[RDyNetGraph]: attributes in graph_attr must have correct names! \n")
								#
								graph_xml			<-	graph_temp
								for(g in names(graph_attr)) {
									graph_xml[graph_xml[,"name"]==g,"value"]	<-	graph_attr[[g]]
								}
							} else {
								graph_attr	<-	NULL
								graph_xml	<-	graph_temp
							}
							#set up igraph
							if(!is.null(node_tab) & !is.null(link_tab)) {
								rdynet_node		<-	RDyNetNode(node_tab)
								rdynet_link		<-	RDyNetLink(link_tab)
								rdynet_igraph.node	<-	data.frame(name=node_tab[,"label"],stringAsFactors=FALSE)
								
								rdynet_igraph.link	<-	data.frame(
															from	=	node_tab[match(link_tab[,"source"],node_tab[,"id"]),"label"],
															to		=	node_tab[match(link_tab[,"target"],node_tab[,"id"]),"label"]
														)
								rdynet_igraph	<-	igraph::graph.data.frame(rdynet_igraph.link, directed=TRUE, vertices=rdynet_igraph.node)
							} else {
								rdynet_node		<-	NULL
								rdynet_link		<-	NULL
								rdynet_igraph	<-	NULL
							}
							#R.oo routine
							extend(
									Object(),
									"RDyNetGraph",
									.node		=	rdynet_node,
									.link		=	rdynet_link,
									.graph_temp	=	graph_temp,
									.graph_attr	=	graph_attr,
									.graph_xml	=	graph_xml,
									.igraph		=	rdynet_igraph
							)
						}
)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Methods~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setMethodS3(
		name		=	"SetAttr",
		class		=	"RDyNetGraph",
		definition	=	function(	
								this, 
								value=NULL,  # values of attributes to be changed; character
								...
						) {
							#cat("[RDyNetGraph]: Set attributes for graph! \n")
							all.attr	<-	this$.graph_temp[,"name"]
							if(class(value)=="character")
								attr<-names(value)
							else 
								stop("[RDyNetGraph]: only support value in character format! ")
							if(is.null(attr))
								stop("[RDyNetGraph]: make sure to input at least one attribute! \n")
							if(!all(attr%in%all.attr))
								stop("[RDyNetGraph]: make sure to input the right names of attributes! \n")
							#if character, then all links' values are supposed to be changed
							for(g in attr) {
								this$.graph_xml[this$.graph_xml["name"]==g,"value"]	<-	value[[g]]
							}
						}
)

###Layout of nodes
setMethodS3(
		name		=	"Layout",
		class		=	"RDyNetGraph",
		definition	=	function(this, label, constraint, layout, ...) {
							#cat("[RDyNetGraph]: Set up layout of nodes! \n")			
							#check inputs
							if(class(label)!="character" | is.null(label) | !all(label %in% names(this$.node$.node_xml))) 
								stop("[RDyNetGraph]: incorrect label!\n ")
							if(class(constraint)!="list" | !all(names(constraint) %in% c("w","h","x","y")))
								stop("[RDyNetGraph]: incorrect constraint!\n ")
							if(class(layout)!="character" | length(layout)!=1 | !all(layout %in% c("random","circle","kamada.kawai","spring")))
								stop("[RDyNetGraph]: incorrect layout! \n")
							#create a subgraph with only input labels of nodes
							sub.graph <- igraph::subgraph(graph = this$.igraph, v = label)
							#
							if(layout=="circle")
								sub.graph.newpos<-(-1)*igraph::layout.circle(sub.graph)
							else if(layout=="kamada.kawai")
								sub.graph.newpos<-(-1)*igraph::layout.kamada.kawai(sub.graph)
							else if(layout=="random")
								sub.graph.newpos<-(-1)*igraph::layout.random(sub.graph)
							else if(layout=="spring")
								sub.graph.newpos<-(-1)*igraph::layout.spring(sub.graph)
							colnames(sub.graph.newpos)<-c("g.x","g.y")
							newpos<-data.frame(	g.x=as.character(floor((sub.graph.newpos[,"g.x"])*constraint$w/2+constraint$x)),
												g.y=as.character(floor((sub.graph.newpos[,"g.y"])*constraint$h/2+constraint$y)),stringsAsFactors=FALSE)
							rownames(newpos)<-V(sub.graph)$name
							
							this$.node$SetNodeAttr(value=newpos)
						}
)






























