###############################################################################
# TODO: 
# Xin Wang <xw264@cam.ac.uk>
# Advisor: Florian Markowetz <florian.markowetz@cancer.org.uk> 
# University of Cambridge Deparment of Oncology
# Cancer Research UK - Cambridge Research Institute
# At 17:35:22, on 19 Aug 2010
###############################################################################
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Constructor~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setConstructorS3(
		name		=	"RDyNetLink", 
		definition	=	function(link_tab=NULL) {

							if(!is.null(link_tab)) {
								#check input
								if(!is.data.frame(link_tab))
									stop("[RDyNetLink]: arrange input as a data.frame!\n")
								if(!all(c("label","source","target") %in% colnames(link_tab)))
									stop("[RDyNetLink]: link label, source and target must be provided!\n")
								cat("[RDyNetLink]: Set up links! \n")
								#
								link_temp	=	list(	
										label			=	"",
										source			=	"",
										target			=	"",
										direction		=	"1",
										g.width			=	"1.0",
										g.fill			=	"#9999ff",
										g.edgeLineType	=	"SOLID"
								)
								#create link list
								link.names	<-	names(link_temp)
								link.input	<-	colnames(link_tab)
								link_xml	<-	NULL
								for(i in 1:nrow(link_tab)) {
									link_xml[[i]]	<-	link_temp
									names(link_xml)[i] <- as.character(link_tab[i,"label"])
									for(j in link.input) {
										if(j %in% link.names) 
											link_xml[[i]][[j]]<-as.character(link_tab[i,j])
									}
								}
							} else {
								link_tab	<-	NULL
								link_temp	<-	NULL
								link_xml	<-	NULL
							}
				
							extend(
									Object(),
									"RDyNetLink",
									.link_tab	=	link_tab,
									#default setting of DyNet link types
									.link_temp	=	link_temp,
									.link_xml	=	link_xml
							)
						}
)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Define Methods~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setMethodS3(
		name		=	"SetLinkAttr",
		class		=	"RDyNetLink",
		definition	=	function(	
								this, 
								label=NULL,
								value=NULL,  # values of attributes to be changed; character or data.frame
								# if character[1 x attr]: set all nodes' values according to input values; each input value needs an attribute name
								# if data.frame[link x attr]: set rowname-specified nodes' values according to input values; each column specifies an attribute 
								...
						) {
							#cat("[RDyNetLink]: Set attributes for links! \n")
							all.attr	<-	names(this$.link_temp)
							if(class(value)=="character")
								attr<-names(value)
							else if(class(value)=="data.frame")
								attr<-colnames(value)
							else
								stop("[RDyNetLink]: only support value with character or data.frame formats! ")
							if(is.null(attr))
								stop("[RDyNetLink]: make sure to input at least one attribute! \n")
							if(!all(attr%in%all.attr))
								stop("[RDyNetLink]: make sure to input the right names of attributes! \n")
							#if character, then all links' values are supposed to be changed
							if(class(value)=="character") {
								if(is.null(label))
									label_to_set<-this$.link_tab[,"label"]
								else 
									label_to_set<-label
								attr<-names(value)
								if(!all(attr %in% all.attr)) {
									stop("[RDyNetLink]: make sure set attribute names for input value! \n")
								} else {
									for(a in attr) {
										if(a %in% all.attr) {
											sapply(label_to_set,function(l) {this$.link_xml[[l]][[a]]<-value[[a]]})
										} 
									}
								}
							} else if(class(value)=="data.frame") {
								label_to_set<-rownames(value)
								if(!all(label_to_set%in% this$.link_tab[,"label"])) {
									stop("[RDyNetLink]: make sure to input the right labels of links! \n")
								}
								if(!all(attr%in%all.attr)) {
									stop("[RDyNetLink]: make sure to input the right name of attributes! \n")
								} else {
									for(a in attr) {
										if(a %in% all.attr) {
											sapply(label_to_set,function(l) {this$.link_xml[[l]][[a]]<-value[l,a]})	
										} 
									}
								}
							} 
						}
)






















