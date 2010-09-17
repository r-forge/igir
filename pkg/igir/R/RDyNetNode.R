###############################################################################
# TODO: 
# Xin Wang <xw264@cam.ac.uk>
# Advisor: Florian Markowetz <florian.markowetz@cancer.org.uk> 
# University of Cambridge Deparment of Oncology
# Cancer Research UK - Cambridge Research Institute
# At 14:55:46, on 19 Aug 2010
###############################################################################
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Constructor~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setConstructorS3(
		name		=	"RDyNetNode", 
		definition	=	function(node_tab=NULL) {

							if(!is.null(node_tab)) {
								#check input
								if(!is.data.frame(node_tab))
									stop("[RDyNetNode]: arrange input as a data.frame!\n")
								if(!all(c("label","id") %in% colnames(node_tab)))
									stop("[RDyNetNode]: node id and label must be provided!\n")
								#node template
								node_temp	=	list(
										label	=	"",
										id		=	"",
										attr	=	data.frame(
														type	=	as.character(rep("string",10)),
														name 	= 	as.character(c("container",	"gradient",	"alias","node.bend","node.size","node.labelColor",
																		"node.font",	"font.x.position",	"font.y.position",	"node.shape")),
														value	=	as.character(c("false","false","","50.0","20.0","#333333",
																		"Arial-0-12",	"10.0","12.0","ellipse")),
														stringsAsFactors	=	FALSE
													), 
										g.type		=	"ELLIPSE",
										g.h			=	"20.0",
										g.w			=	"20.0",
										g.x			=	"0",
										g.y			=	"0",
										g.fill		=	"#ff6666",
										g.width		=	"1.0",
										g.outline	=	"#ff0000"
								)
								cat("[RDyNetNode]: Set up nodes! \n")			
								#create node list
								node.names	<-	names(node_temp)
								node.attr.names	<-	as.character(node_temp$attr[,"name"])
								node.input	<-	colnames(node_tab)	
								node_xml	<-	list()
								for(i in 1:nrow(node_tab)) {
									node_xml[[i]]	<-	node_temp
									names(node_xml)[i] <- as.character(node_tab[i,"label"])
									for(j in node.input) {
										if(j %in% node.names) 
											node_xml[[i]][[j]]<-as.character(node_tab[i,j])
										else if(j %in% node.attr.names){
											node_xml[[i]][["attr"]][node_xml[[i]][["attr"]][,"name"]==j,"value"]<-as.character(node_tab[i,j])
										}
									}
								}
							} else {
								node_xml	<-	NULL
								node_temp	<-	NULL
								node_tab	<-	NULL
							}
				
							#R.oo 
							extend(
									Object(),
									"RDyNetNode",
									.node_temp	=	node_temp,
									.node_tab	=	node_tab,
									.node_xml	=	node_xml
							)
						}
)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Methods~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

setMethodS3(
		name		=	"SetNodeAttr",
		class		=	"RDyNetNode",
		definition	=	function(	
								this, 
								label=NULL,	# nodes of labels
								value=NULL,  # values of attributes to be changed; character or data.frame
										# if character[1 x attr]: set all nodes' values according to input values; each input value needs an attribute name
										# if data.frame[label x attr, stingAsFactors=FALSE]: set rowname-specified nodes' values according to input values; each column specifies an attribute 
								...
						) {
							#cat("[RDyNetNode]: Set attributes for nodes! \n")
							node.names	<-	names(this$.node_temp)
							node.attr.names	<-	as.character(this$.node_temp$attr[,"name"])
							all.attr<-c(node.names,node.attr.names)
							if(class(value)=="character")
								attr<-names(value)
							else if(class(value)=="data.frame")
								attr<-colnames(value)
							else
								stop("[RDyNetNode]: only support value with character or data.frame formats! ")
							if(is.null(attr))
								stop("[RDyNetNode]: make sure to input at least one attribute! \n")
							if(!all(attr%in%all.attr))
								stop("[RDyNetNode]: make sure to input the right names of attributes! \n")
							#if input value is attribute character, then label can be null which means to change all nodes
							#else if input value is data.frame, then label should be null, and label should be assigned to rownames of value
							if(class(value)=="character") {
								if(is.null(label))
									label_to_set<-this$.node_tab[,"label"]
								else 
									label_to_set<-label
								attr<-names(value)
								if(!all(attr %in% all.attr)) {
									stop("[RDyNetNode]: make sure set attribute names for input value! \n")
								} else {
									for(a in attr) {
										if(a %in% node.names) {
											sapply(label_to_set,function(l) {this$.node_xml[[l]][[a]]<-value[[a]]})
										} else if(a %in% node.attr.names) {
											sapply(label_to_set,function(l) {this$.node_xml[[l]][["attr"]][this$.node_xml[[l]][["attr"]][,"name"]==a,"value"]<-value[[a]]})
										}
									}
								}
							} else if(class(value)=="data.frame") {
								label_to_set<-rownames(value)
								if(!all(label_to_set%in% this$.node_tab[,"label"])) {
									stop("[RDyNetNode]: make sure to input the right id of nodes! \n")
								}
								if(!all(attr%in%all.attr)) {
									stop("[RDyNetNode]: make sure to input the right name of attributes! \n")
								} else {
									for(a in attr) {
										if(a %in% node.names) {
											sapply(label_to_set,function(l) {this$.node_xml[[l]][[a]]<-value[l,a]})	
										} else if(a %in% node.attr.names) {
											sapply(label_to_set,function(l) {this$.node_xml[[l]][["attr"]][this$.node_xml[[l]][["attr"]][,"name"]==a,"value"]<-value[l,a]})
										}
									}
								}
							} 
						}
)














