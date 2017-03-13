getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Adv Lab5"
setwd(dir_path)
library(igraph)
data_log <- read.csv("wide_twitter_daily_lab5.csv", header = TRUE, sep = ",")
str(data_log)
names(data_log)
data_Edge<-data_log[,2:93]
mycorr<-cor(data_Edge)
# CHUNK 9
# Fisher's transformation
z <- 0.5 * log((1 + mycorr) / (1 - mycorr))

# CHUNK 10
z.vec <- z[upper.tri(z)]
n <- dim(data_Edge)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0, 
                        sqrt(1 / (n-3)), lower.tail=FALSE)

# CHUNK 11
length(corr.pvals)


# CHUNK 12
# Benjamini-Hochberg adjustment to control for the false discovery rate
corr.pvals.adj <- p.adjust(corr.pvals, "BH")

# CHUNK 13
# Number of edges predicted: using statistical significance at the p < 0.05 threshold
length(corr.pvals.adj[corr.pvals.adj < 0.05])


# CHUNK 14
library(fdrtool)

# CHUNK 15
mycorr.vec <- mycorr[upper.tri(mycorr)]
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# Note the code of CHUNK 16 through CHUNK 19 uses partial correlations to predict edges
# CHUNK 16
pcorr.pvals <- matrix(0, dim(mycorr)[1], 
                      dim(mycorr)[2])
for(i in seq(1, 92)){
  for(j in seq(1, 92)){
    rowi <- mycorr[i, -c(i, j)]
    rowj <- mycorr[j, -c(i, j)]
    tmp <- (mycorr[i, j] - 
              rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
    tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4) * tmp.zvals
    tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 
                           0, 1, lower.tail=FALSE)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}


# CHUNK 17
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
# Benjamini-Hochberg adjustment to control for the false discovery rate
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# CHUNK 18
pcorr.edges <- (pcorr.pvals.adj < 0.01)
length(pcorr.pvals.adj[pcorr.edges])
# ---
# ---

# CHUNK 19
# Create the graph predicted by the statistically significant partial correlations
pcorr.A <- matrix(0, 92, 92)

length(lower.tri(pcorr.A))
length(as.numeric(pcorr.edges))
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")
g.pcorr=delete.vertices(g.pcorr,which(degree(g.pcorr)<1))
graph.pcorr <- set.vertex.attribute(g.pcorr, "name", value=names(data_Edge))
for(i in 1:length(V(graph.pcorr)$name)){
  if(V(graph.pcorr)$name[i]==names(data_Edge)[i]){
    V(graph.pcorr)$nodesize[i]<-nchar(names(data_Edge)[i])*3.5
  }
}


plot(graph.pcorr, layout=layout.fruchterman.reingold(graph.pcorr)*0.5 ,vertex.shape="circle",vertex.color="green",vertex.label.cex = 0.9 ,vertex.label.degree = 2,vertex.size=V(graph.pcorr)$nodesize)

