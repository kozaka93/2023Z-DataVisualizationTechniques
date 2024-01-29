#install.packages("igraph")
library(igraph)
edges<-read.csv("C:/Users/DELL/Downloads/network.csv (1)/edges.csv")
nodes<-read.csv("C:/Users/DELL/Downloads/network.csv (1)/nodes.csv")
gprops<-read.csv("C:/Users/DELL/Downloads/network.csv (1)/gprops.csv")
network <- graph_from_data_frame(d=edges, directed=F) 
unique(nodes$class)
png("network_plot.png", width = 1500, height = 1200)
plot(network,
     edge.width=E(network)$weight/2,
     vertex.size=7,
     edge.color="grey",
     vertex.label=nodes$name,
     vertex.label.cex=0.4,
     layout=layout.sphere,
     main="Blumenau drug interactions in 2019")
title(sub="Vertices represent drugs and width informs about severity of interaction between them")
dev.off()

