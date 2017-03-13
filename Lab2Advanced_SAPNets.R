# Ali Tafti
# Optional Advanced Lab 2: SAP Community Network
#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Adv Lab 2"
setwd(dir_path)

# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)
help("graph.data.frame")
# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub)
## Vertices
vcount(g_SAPSub)


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---
# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
help("simplify")
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
ecount(g_SAPSub_simpl)
E(g_SAPSub_simpl)$weight 
is.simple(g_SAPSub_simpl)
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl, mode=c("in"))==max(degree(g_SAPSub_simpl,mode=c("in")))]
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl, mode=c("out"))==max(degree(g_SAPSub_simpl,mode=c("out")))]

# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

# Use the inverse of log weight for some of the network measure calculations
E(g_SAPSub_simpl)$weight
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'))
neighbors(g_SAPSub_simpl, v=c('592540'))
plot.igraph(g_SAPSub_simpl)

## For example, there is only a path to 814 from 511; not from it. So there are outpaths from 511 to 814, but no in paths. (or the other vectors)
E(g_SAPSub_simpl)$weight <- inv_weight
help("is_connected")
reciprocity(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")

# Diameter with both kinds of weights
help("transitivity")
# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)
# Avg. path length and diameter
help("average.path.length")
average.path.length(g_SAPSub_simpl, directed=TRUE)
diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
diameter(g_SAPSub_simpl, weights= inv_weight)
# Summarize the graph structure
summary(g_SAPSub_simpl)
help("sapply")
# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight
help("constraint")
# Embeddedness/ inverse of structural hole access (see Burt 2004)
help("constraint")
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)
# Node betweenness
betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
help("aggregate")
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(betweens_SAP, constraints_SAP, clustering_SAP, degree_sap)
a_node<-aggregate(betweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")


# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

# Log-log degree distributino
par(mfrow=c(1, 2))
d.net <-degree(g_SAPSub_simpl)
dd.net <- degree.distribution(g_SAPSub_simpl)
d <- 1:max(d.net)-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(g_SAPSub_simpl,V(g_SAPSub_simpl))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

cohesion(g_SAPSub_simpl)
