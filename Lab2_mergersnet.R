#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Assignment2"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  
infile<-"MergerNet_Jan21_2016_forR.csv"
## Load package
library(igraph)
el=read.csv(infile, header = TRUE, sep = ",")
g_acq=graph.data.frame(el, directed = TRUE, vertices= NULL)

### List of all the years represented in the set
el[,"year"]
table(el[,"year"])
df <-data.frame(el)

df
class(df$weight)
length(df$weight)
# ---
#[1] "integer"
# ---

class(df$source)
# ---
# [1] "factor"
# ---

class(el)
# ---
# [1] "data.frame"
# ---

# Edges count
ecount(g_acq)
## Vertices count
vcount(g_acq)
#Is it a simple graph? No!
## Check whether Self_loops exist, as do multiple edges
is.simple(g_acq)
# ---
#[1] FALSE
# ---
E(g_acq)$weight 
length(E(g_acq)$weight)
g_acq_simpl<-simplify(g_acq)
help("simplify")
length(E(g_acq_simpl)$weight)
### The above should default to the option below, to sum the existing edge weights ### when combining them
##g_acq_simpl<-simplify(g_acq,edge.attr.comb="sum" )

E(g_acq_simpl)$weight 
# Will use the inverse of log weight for shortest path calculations
inv_weight<-1/log(E(g_acq_simpl)$weight  + 1)
num_weight<-E(g_acq_simpl)$weight 
length(inv_weight)
help("induced_subgraph")
packageVersion("igraph")
colors()
shapes()
# Creating the Induced subgraph 
sub_net <- induced_subgraph (g_acq_simpl, v=c('511', '541',
                                           '518', '519', '517', '325', '423', '446', '512', '523',
                                           '561', '621', '115', '482', '485', '487', '491', '492',
                                           '521', '712' ))

#install.packages("igraphdata")
#library(igraphdata)
help("plot")
# Plotting the Subgraph
plot(sub_net)
#Specifying the Edge-Color for all edges in the sub graph
E(sub_net)$edge.color <- "gray60"
#Specifying the Vertex Shape and Color for all vertices
V(sub_net)$shape<- "sphere"
V(sub_net)$color <- "Orange"
# Changing the color and shape for Vertices (511,541,519,518)
V(sub_net)[c("511","541","518","519")]$shape <- "circle"
V(sub_net)[c("511","541","518","519")]$color <- "green"
#plotting the Subgraph
plot.igraph(sub_net)
# Assigning the width of the edges to be proportional to the log of the weight of the edge.
E(sub_net)$width <- log(E(sub_net)$weight)
plot(sub_net)

# Making the node size proportional to the betweeness of the individual node.
temp_Size <- betweenness(sub_net)                
V(sub_net)$size <- temp_Size * 0.5
plot(sub_net)
# Using the Fruchterman Reingold Layout
plot(sub_net, layout= layout.fruchterman.reingold)

help("distances")

# Assigning the weights of edges

E(g_acq_simpl)$weight <- inv_weight

graph.strength(g_acq)
help("is.connected")
#checking if the network is strongly connected or weakly connected.
is.connected(g_acq, mode = "strong")
is.connected(g_acq, mode = "weak")
# Diameter with inverse weighting scheme
diameter(g_acq_simpl,weights = inv_weight)
help("betweenness")
#Betweeness Centrality metrics for the required nodes with inverse weighing scheme  
betweenness(g_acq_simpl,v=V(g_acq_simpl),directed = TRUE, weights = inv_weight,normalized = TRUE, nobigint = TRUE)
#Closeness Centrality with original weighing scheme
closeness(g_acq_simpl,mode=c("out"),weights = num_weight)
closeness(g_acq_simpl,mode=c("in"),weights = num_weight)

#Overall Global Clustering Coefficient
transitivity(g_acq_simpl, type="global")

#Shortest path from 711 node
shortest.paths(g_acq_simpl, v=V(g_acq_simpl)["711"], to = V(g_acq_simpl)[c("511","541","518","519")], mode = "out")
#Shortest path to the 814 node
shortest.paths(g_acq_simpl, v=V(g_acq_simpl)[c("511","541","518","519")], to = V(g_acq_simpl)["814"], mode = "out")
#Calculating the inverse weights for the induced subgraph
inv_weight_sub_net<-1/log(E(sub_net)$weight  + 1)
num_weight_sub_net<-E(sub_net)$weight
#Approx diameter of the induced subgraph
diameter(sub_net, weights=inv_weight_sub_net )
