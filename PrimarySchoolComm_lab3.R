getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Assignment3"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

# Edges
ecount(g_primschool)
## Vertices
vcount(g_primschool)
is.weighted(g_primschool)

V(g_primschool)$name
E(g_primschool)$weight
V(g_primschool)$gender

V(g_primschool)[V(g_primschool)$classname=="1B"]

is.simple(g_primschool)
is.connected(g_primschool)


# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
help("wilcox.test")
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")
# Does edge weight make any difference here?



# Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
c.m.fastgreedy <- membership(school_comm_fast)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m.fastgreedy, stud.class, useNA = c("no"))

# Here, we are testing community significance for just two of the communities. Students will complete tests for the remainder of communities for each algorithm. 
v_comp1 <- V(g_primschool)[c.m.fastgreedy==1]
v_comp2 <- V(g_primschool)[c.m.fastgreedy==2]
v_comp3 <- V(g_primschool)[c.m.fastgreedy==3]
v_comp4 <- V(g_primschool)[c.m.fastgreedy==4]
v_comp5 <- V(g_primschool)[c.m.fastgreedy==5]
v_comp6 <- V(g_primschool)[c.m.fastgreedy==6]
v_comp7 <- V(g_primschool)[c.m.fastgreedy==7]

community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)

plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=2)
dendPlot(school_comm_fast)
#Diiferent Community Algorithms
#Walk Trap Algorithm

help("walktrap.community")
wc <- walktrap.community(g_primschool,weights=V(g_primschool)$weight,membership = TRUE)
wc.member <- membership(wc)
table(wc.member,stud.class,useNA = c("no"))
plot(wc,g_primschool,vertex.label= NA, vertex.size=2)
v_comp_wc1 <- V(g_primschool)[wc.member==1]
v_comp_wc2 <- V(g_primschool)[wc.member==2]
v_comp_wc3 <- V(g_primschool)[wc.member==3]
v_comp_wc4 <- V(g_primschool)[wc.member==4]
v_comp_wc5 <- V(g_primschool)[wc.member==5]
v_comp_wc6 <- V(g_primschool)[wc.member==6]
community.significance.test(g_primschool, v_comp_wc1)
community.significance.test(g_primschool, v_comp_wc2)
community.significance.test(g_primschool, v_comp_wc3)
community.significance.test(g_primschool, v_comp_wc4)
community.significance.test(g_primschool, v_comp_wc5)
community.significance.test(g_primschool, v_comp_wc6)

#Label Propagation Community Algorithm

help("label.propagation.community")
lc <- label.propagation.community(g_primschool,weights=V(g_primschool)$weight)
lc.member <- membership(lc)
table(lc.member,stud.class,useNA = c("no"))
plot(lc,g_primschool,vertex.label= NA, vertex.size=2)

#Spinglass Community Algorithm

help("spinglass.community")
sg <- spinglass.community(g_primschool,weights=V(g_primschool)$weight)
sg.member <- membership(sg)
table(sg.member,stud.class,useNA = c("no"))
plot(sg,g_primschool,vertex.label= NA, vertex.size=2)


help(n)

v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)
stud.class.subgraph_grade1 <- get.vertex.attribute(subgraph_grade1, "classname")
stud.gender.subgraph_grade1<- get.vertex.attribute(subgraph_grade1, "gender")

stud.class.subgraph_grade5 <- get.vertex.attribute(subgraph_grade5, "classname")
stud.gender.subgraph_grade5<- get.vertex.attribute(subgraph_grade5, "gender")
gender_Segregation_grade1 <- fastgreedy.community(subgraph_grade1,weights=E(subgraph_grade1)$weight)
gen_Seg_grade1 <- membership(gender_Segregation_grade1)

table(gen_Seg_grade1, stud.gender.subgraph_grade1,stud.class.subgraph_grade1, useNA = c("no"))
table(gen_Seg_grade1,stud.gender.subgraph_grade1,useNA = c("no"))
gender_Segregation_grade5<- fastgreedy.community(subgraph_grade5, weights=E(subgraph_grade5)$weight)
gen_Seg_grade5 <- membership(gender_Segregation_grade5)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(gen_Seg_grade5, stud.gender.subgraph_grade5,stud.class.subgraph_grade5, useNA = c("no"))
table(gen_Seg_grade5,stud.gender.subgraph_grade5,useNA = c("no"))
g <- graph.formula(A-B,B-C,C-D, D-E, E-A)

E(g)$sign<-c(+1,-1, -1, +1, 1)
is.connected(g)


ggg <- graph.formula(A-B,A-C,A-D, B-C, B-D, C-D )

E(ggg)$sign<-c(+1,1, -1, 1, -1, 1)
is.connected(ggg)



