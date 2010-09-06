###############################################################################
# TODO: 
# Xin Wang <xw264@cam.ac.uk>
# Advisor: Florian Markowetz <florian.markowetz@cancer.org.uk> 
# University of Cambridge Deparment of Oncology
# Cancer Research UK - Cambridge Research Institute
# At 02:24:34, on 20 Aug 2010
###############################################################################
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Constructor~~~~~~~~~~~~~~~~~~~~~~~~~~~##

setConstructorS3(
		name		=	"RDyNet", 
		definition	=	function(rdynet_graph=NULL, save_filename=NULL, dynet_path=NULL) {
							#check input
							extend(
									Object(),
									"RDyNet",
									.rdynet_graph		=	rdynet_graph,
									.rdynet_temp_file	=	save_filename,
									.dynet_path			=	file.path(.path.package("IGIR"),"exec")
							)
						}
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Methods~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Transfer between graph formats
setMethodS3(
		name		=	"GenXML",
		class		=	"RDyNet",
		definition	=	function(this,...) {
							cat("[RDyNet]: Generate XML file for DyNet! \n")	
							#print xml
							this$PrintHead(this$.rdynet_graph$.graph_xml)
							this$PrintNode(this$.rdynet_graph$.node$.node_xml)
							this$PrintLink(this$.rdynet_graph$.link$.link_xml)
							this$PrintTail()
						}
)
###
setMethodS3(
		name		=	"PrintHead",
		class		=	"RDyNet",
		definition	=	function(this, xml.graph, ...) {
							#cat("[RDyNet]: Print Graph! \n")
							xml.graph.attr<-""
							for(a in 1:nrow(this$.rdynet_graph$.graph_xml)) {
								xml.graph.attr<-paste(
													xml.graph.attr,"<att type=\"",
													this$.rdynet_graph$.graph_xml[a,"type"],"\""," name=\"",
													this$.rdynet_graph$.graph_xml[a,"name"],"\" value=\"",
													this$.rdynet_graph$.graph_xml[a,"value"],"\"/> \n",
													sep=""
												)
							}
							xml.head<-paste(	
									"<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n",
									"<!--Graph File Generated for DyNet--> \n",
									"<graph xmlns=\"http://www.cs.rpi.edu/XGMML\" label=\"Network\" directed=\"1\"> \n",
									xml.graph.attr,sep="" 
							)
							write(xml.head,file=this$.rdynet_temp_file)
						}
)
###
setMethodS3(
		name		=	"PrintNode",
		class		=	"RDyNet",
		definition	=	function(this, ...) {
							#cat("[RDyNet]: Print Nodes! \n")
							for(n in 1:length(this$.rdynet_graph$.node$.node_xml)) {
								xml.temp.node.att<-""
								for(a in 1:nrow(this$.rdynet_graph$.node$.node_temp$attr)) {
									xml.temp.node.att<-
											paste(	
													xml.temp.node.att,
													"<att type=\"",this$.rdynet_graph$.node$.node_xml[[n]]$attr[a,"type"],"\" name=\"",
													this$.rdynet_graph$.node$.node_xml[[n]]$attr[a,"name"],"\" value=\"",
													this$.rdynet_graph$.node$.node_xml[[n]]$attr[a,"value"],"\" /> \n",
													sep=""
											)
								}
								xml.temp.node<-
										paste(	
												"<node label=\"",this$.rdynet_graph$.node$.node_xml[[n]]$label,"\" id=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$id,"\"> \n", 
												xml.temp.node.att,"<graphics type=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.type,"\" h=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.h,"\" w=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.w,"\" x=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.x,"\" y=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.y,"\" fill=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.fill,"\" width=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.width,"\" outline=\"",
												this$.rdynet_graph$.node$.node_xml[[n]]$g.outline,"\" /> \n</node> ",
												sep=""
										)
								write(xml.temp.node,file=this$.rdynet_temp_file,append=TRUE)
							}
						}
)
###
setMethodS3(
		name		=	"PrintLink",
		class		=	"RDyNet",
		definition	=	function(this, ...) {
							#cat("[RDyNet]: Print Links! \n")
							xml.temp.link<-""
							for(l in 1:length(this$.rdynet_graph$.link$.link_xml)) {
								xml.temp.link<-paste(
										xml.temp.link, "<edge label=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$label,"\" source=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$source,"\" target=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$target,"\" direction=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$direction,"\">\n","<graphics width=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$g.width,"\" fill=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$g.fill,"\" edgeLineType=\"",
										this$.rdynet_graph$.link$.link_xml[[l]]$g.edgeLineType,"\" />\n","</edge>\n",sep=""
								)
							}
							write(xml.temp.link,file=this$.rdynet_temp_file,append=TRUE)
						}
)

setMethodS3(
		name		=	"PrintTail",
		class		=	"RDyNet",
		definition	=	function(this, ...) {
							#cat("[RDyNet]: Print everything else! \n")
							write("</graph>",file=this$.rdynet_temp_file,append=TRUE)				
						}
)

setMethodS3(
		name		=	"InvokeDyNet",
		class		=	"RDyNet",
		definition	=	function(this, ...) {
							#cat("[RDyNet]: Let's experience DyNet! \n")
							system(paste("java -jar ",file.path(this$.dynet_path,"dyNet.jar"), " xgmml ",this$.rdynet_temp_file,sep=""),wait=FALSE)
						}
)


