###############################################################################
# TODO: 
# Xin Wang <xw264@cam.ac.uk>
# Advisor: Florian Markowetz <florian.markowetz@cancer.org.uk> 
# University of Cambridge Deparment of Oncology
# Cancer Research UK - Cambridge Research Institute
# At 13:40:16, on 30 Aug 2010
###############################################################################
#width and height of DyNet software
dynet.w<-600
dynet.h<-400
##define node and link
node.label<-paste("n",1:10,sep="")
node	<-	data.frame(
		label	=	node.label,
		id		=	1:length(node.label),
		alias	=	node.label
)
link.label	<-	paste("l",1:9,sep="")
link	<-	data.frame(
		label	=	link.label,
		source	=	1:9,
		target	=	2:10
)
##create RDyNetGraph 
rdynet.graph	<-	RDyNetGraph(node, link)
dynet.region		<-	list(w=dynet.w/2,h=dynet.h/2,x=3*dynet.w/4,y=3*dynet.h/4)
##set graph layout
rdynet.graph$Layout(label=node.label, constraint=dynet.region, layout="random")				
rdynet<-RDyNet(rdynet_graph=rdynet.graph, save_filename="rdynet.demo.xgmml")
rdynet$GenXML()
rdynet$InvokeDyNet()
browser()
##set up specific attributes
rdynet.graph$.node$SetNodeAttr(
		label=node.label,
		value=c(node.size="20.0",g.h="20.0",g.w="20.0",g.fill="#ffffff",g.width=3,g.outline="#ff0000",container="false",gradient="false")
)

rdynet.graph$.link$SetLinkAttr(
		label=link.label,
		value=c(direction="1",g.width="3.0",g.fill="#eb9911")
)
rdynet$GenXML()
rdynet$InvokeDyNet()
browser()
##change layout
rdynet.graph$Layout(label=node.label, constraint=dynet.region, layout="circle")	
rdynet$GenXML()
rdynet$InvokeDyNet()