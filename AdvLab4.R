rm(list=ls())
setwd("C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Adv Lab 4")
library(igraph)

# Function to estimate the alpha parameters for a given graph
# Returns a data frame containing Alpha0 and ALpha1 values

getModelAlphas<- function (graf, dz=FALSE)
{
  if(!dz) {
    #This is the degree distribution
    count<-degree(graf)
    degree_dist<-unname(table(factor(count,levels = 0:max(degree(graf)))))
    degree_val<-seq.int(0, length(degree_dist)-1, 1)
  }else {
    #no zero frequency values
    df<-as.data.frame(table(degree(graf)))
    #The degree distribution
    degree_dist<-df$Freq
    #Corresponding degree values
    degree_val<-as.integer(as.character(df$Var1))
  }
  
  # Average Degree of Network
  tot_degree <- sum(degree_dist * degree_val)
  avg_degree <- tot_degree/sum(degree_dist)
  
  # Estimating Beta1 and Alpha1 iteratively starting with intial guess Alpha0
  
  #Setting up Y variable for the linear regression
  F_d<-cumsum(degree_dist)/sum(degree_dist)
  YVar <- log (1-F_d)
  
  # Set a seed value in alpha0
  vecLen<-1000
  alpha0 <- seq.int(1, vecLen-1, 1)
  alpha0<- alpha0/vecLen
  alpha1 <- seq.int(0, 0, length.out=vecLen-1)
  
  m <- avg_degree/2
  # Estimate alpha1
  for(j in 1:(vecLen-1))
  {
    #Setting up the X variables
    term <- 2*m*alpha0[j]/(1-alpha0[j])
    XVar <- log(degree_val + term)
    XY_frame = data.frame(XVar, YVar)
    # Compute the index of infinite entries in Y variable
    inf_indx <- which(YVar==-Inf)
    
    # Clear infinite entries from X and Y variables
    XY_frame$YVar[inf_indx] = NA
    XY_frame$XVar[inf_indx] = NA
    
    # Fit the model
    lm = lm(YVar ~ XVar, data=XY_frame, na.action=na.exclude)
    beta1 <- coefficients(lm)[2]
    alpha1[j] <- 1 + 2/beta1
  }
  
  #Create a data frame for returning
  alpha_frame = data.frame(alpha0, alpha1)
  
  return (alpha_frame)
}

#Function to get the best Alpha0 estimate for the min of absolute diff between alpha0 and alpha1
findBestAlpha <- function (alpha0, alpha1){
  vecLen <- length(alpha0)
  if (vecLen != length(alpha1)){
    stop("Alpha vectors unequal in length")
  }
  MinDiff <- abs(alpha1[1]-alpha0[1])
  BestAlpha <- alpha0[1]
  for (j in 1: (vecLen)){
    if (abs(alpha1[j]-alpha0[j]) < MinDiff) {
      MinDiff = abs(alpha1[j]-alpha0[j])
      BestAlpha = alpha0[j]
    }
  }
  retVec <- c(BestAlpha, MinDiff)
  return (retVec)
}
#Extract the biggest Cluster in a graph
getBiggestCluster <- function (graf){
  #Get the clusters
  c<-clusters(graf)
  #Get the cluster id of the largest cluster
  clust_id<-which.max(c$csize)
  
  #Get all the nodes belonging to the largest cluster
  nodes = vector(mode = "integer", length = 0)
  for (j in 1: length(c$membership)){
    if (c$membership[j]==clust_id){ nodes<-append(nodes, j)}
  }
  #create a subgraph of the nodes belonging to the largest cluster
  sub_graf<-induced_subgraph(graf, nodes, impl = "auto")
  #Return the subgraph
  return(sub_graf)
}


#Function to plot the degree distributions and log-log plots
plotDist <- function (graf1, graf2, nme){
  df1<-as.data.frame(table(degree(graf1)))
  df2<-as.data.frame(table(degree(graf2)))
  #Get the values and distributions
  deg_val1<-as.integer(as.character(df1$Var1))
  deg_dist1<-df1$Freq
  cum_deg_dist1<-cumsum(deg_dist1)
  deg_val2<-as.integer(as.character(df2$Var1))
  deg_dist2<-df2$Freq
  cum_deg_dist2<-cumsum(deg_dist2)
  
  #Calculate the log value
  log_deg_val1<-log(deg_val1)
  log_deg_dist1<-log(deg_dist1)
  log_cum_deg_dist1<-log(cum_deg_dist1)
  
  log_deg_val2<-log(deg_val2)
  log_deg_dist2<-log(deg_dist2)
  log_cum_deg_dist2<-log(cum_deg_dist2)
  
  par(mfrow=c(2,2))
  
  title<-paste(nme, "\nCount Vs Degree")
  plot(deg_val1,deg_dist1,ylim=range(c(deg_dist1,deg_dist2)),xlim=range(c(deg_val1,deg_val2)), type="p", col="darkgoldenrod1", xlab="Degree", ylab="Count", main = title)
  points(deg_val2,deg_dist2,col="midnightblue")
  
  title<-paste(nme, "\nLog(Count) Vs Log(Degree)")
  plot(log_deg_val1,log_deg_dist1, ylim=range(c(log_deg_dist1,log_deg_dist2)) ,xlim=range(c(log_deg_val1,log_deg_val2)), type="p", col="darkgoldenrod1", xlab="Log(Degree)", ylab="Log(Count)", main = title)
  points(log_deg_val2,log_deg_dist2,col="midnightblue")
  
  title<-paste(nme, "\nCum. Count Vs Degree")
  plot(deg_val1,cum_deg_dist1,ylim=range(c(cum_deg_dist1,cum_deg_dist2)),xlim=range(c(deg_val1,deg_val2)), type="p", col="darkgoldenrod1", xlab="Degree", ylab="Count", main = title)
  points(deg_val2,cum_deg_dist2, col="midnightblue")
  
  title<-paste(nme, "\nLog(Cum. Count) Vs Log(Degree)")
  plot(log_deg_val1,log_cum_deg_dist1, ylim=range(c(log_cum_deg_dist1,log_deg_dist2)),xlim=range(c(log_deg_val1,log_deg_val2)), type="p", col="darkgoldenrod1", xlab="Log(Degree)", ylab="Log(cumsum(Count))", main = title)
  points(log_deg_val2,log_cum_deg_dist2,col="midnightblue")
  
}

# Plot the values of alpha0 vs alpha1
plotAlphas<-function (alpha_frameY1, alpha_frameY2, nme){
  
  # plot of apha0 vs abs(alpha1 - alpha0)
  par(mfrow=c(1,3))
  
  Y1A0 <- alpha_frameY1$alpha0
  Y1A1 <- alpha_frameY1$alpha1
  Y2A0 <- alpha_frameY2$alpha0
  Y2A1 <- alpha_frameY2$alpha1
  
  
  #Plot alpha1 vs alpha0
  subTitle<- paste(nme, "\nAlpha1 Vs Alpha0")
  plot(Y1A0,Y1A1,ylim=range(c(Y1A1,Y2A1)),xlim=range(c(Y1A0,Y2A0)), type="p",col="darkgoldenrod1", xlab="alpha0", ylab="alpha1", main = subTitle)
  points(Y2A0,Y2A1,col="midnightblue")
  
  #Plot alpha1-alpha0 vs alpha0
  subTitle<- paste(nme, "\n(Alpha1-Alpha0) Vs Alpha0")
  plot(Y1A0,Y1A1-Y1A0,ylim=range(c(Y1A1-Y1A0,Y2A1-Y2A0)),xlim=range(c(Y1A0,Y2A0)), type="p",col="darkgoldenrod1", xlab="alpha0", ylab="alpha1 - alpha0", main = subTitle)
  points(Y2A0,Y2A1-Y2A0,col="midnightblue")
  
  #Plot abs(alpha1-alpha0) vs alpha0
  subTitle<- paste(nme, "\n ABS(Alpha1-Alpha0) Vs Alpha0")
  plot(Y1A0,abs(Y1A1-Y1A0),ylim=range(c(abs(Y1A1-Y1A0),abs(Y2A1-Y2A0))),xlim=range(c(Y1A0,Y2A0)), type="p",col="darkgoldenrod1", xlab="alpha0", ylab="abs(alpha1 - alpha0)", main = subTitle)
  points(Y2A0,abs(Y2A1-Y2A0),col="midnightblue")
  
}



#Main Code



infile<-"Share_corp_alliance_EdgeList_2007.csv"
data_frame=read.csv(infile, header = TRUE, sep = ",")
# Input a graph
g_alliance_2007=graph.data.frame(data_frame, directed = FALSE )
g_alliance_2007<-simplify(g_alliance_2007)

# largest subgraph
g_sub_2007=getBiggestCluster(g_alliance_2007)
degree(g_sub_2007)
plot(g_sub_2007)

remove(infile)
# Read the 2014 file
infile<-"Share_corp_alliance_EdgeList_2014.csv"
data_frame=read.csv(infile, header = TRUE, sep = ",")
g_alliance_2014=graph.data.frame(data_frame, directed = FALSE, vertices= NULL)
g_alliance_2014<-simplify(g_alliance_2014)
g_sub_2014=getBiggestCluster(g_alliance_2014)

plot(g_sub_2014)
table(degree(g_alliance_2007))
degree(g_sub_2014)
size7<-gsize(g_alliance_2007)
a7<-log(size7)/(log(log(size7)))
# Network diameters
# Diameter is essentially the longest path between two vertices
dia_07<-diameter(g_alliance_2007, directed="FALSE")
print(dia_07)

size14<-gsize(g_alliance_2014)
a14<-log(size14)/(log(log(size14)))

dia_14<-diameter(g_alliance_2014, directed="FALSE")
print(dia_14)

#Avg Degree centrality
avg_deg_07<-mean(degree(g_alliance_2007, mode="all"), na.rm=TRUE)
avg_deg_14<-mean(degree(g_alliance_2014, mode="all"), na.rm=TRUE)

avg_bet_07<- mean(betweenness(g_alliance_2007, directed = "FALSE"), na.rm=TRUE)
avg_bet_14<- mean(betweenness(g_alliance_2014, directed = "FALSE"), na.rm=TRUE)

#Avg clustering
avg_cl_07 <- mean(transitivity(g_alliance_2007, "local"), na.rm=TRUE)
avg_cl_14 <- mean(transitivity(g_alliance_2014, "local"), na.rm=TRUE)

# Assortativity
assrt07<- assortativity.degree(g_alliance_2007, FALSE)
assrt14<- assortativity.degree(g_alliance_2014, FALSE)

plotDist(g_alliance_2007, g_alliance_2014, "Full Graph - 2007 Vs 2014")

plotDist(g_sub_2007, g_sub_2014, "Biggest Component Graph- 2007 Vs 2014")

############### How much of this network is random/preferntial? ###########
#Full Graph 2007 - Estimate the Alpha parameters
alpha_frame_2007 <- getModelAlphas(g_alliance_2007)
BestAlpha07 <- findBestAlpha(alpha_frame_2007$alpha0, alpha_frame_2007$alpha1)


#Full Graph 2014 - Estimate the Alpha parameters
alpha_frame_2014 <- getModelAlphas(g_alliance_2014)
BestAlpha14 <- findBestAlpha(alpha_frame_2014$alpha0, alpha_frame_2014$alpha1)


#Sub COmponent 2007 - Estimate the Alpha parameters
alpha_sub_2007 <- getModelAlphas(g_sub_2007)
BestAlphaSub07 <- findBestAlpha(alpha_sub_2007$alpha0, alpha_sub_2007$alpha1)

#Sub Component 2014 - Estimate the Alpha parameters
alpha_sub_2014 <- getModelAlphas(g_sub_2014)
BestAlphaSub14 <- findBestAlpha(alpha_sub_2014$alpha0, alpha_sub_2014$alpha1)

# Full Graph 2007 and 2014 - plot apha0 vs alpha1
plotAlphas(alpha_frame_2007, alpha_frame_2014, "Full Graph - '07 Vs '14")

#Sub Components 2007 and 2014- plot apha0 vs alpha1
plotAlphas(alpha_sub_2007, alpha_sub_2014, "Biggest Cmpnt. - '07 Vs '14")





#dropping zeros in the distribution 

alpha_frame_2007_dz <- getModelAlphas(g_alliance_2007, TRUE)
BestAlpha07_dz <- findBestAlpha(alpha_frame_2007_dz$alpha0, alpha_frame_2007_dz$alpha1)

alpha_frame_2014_dz <- getModelAlphas(g_alliance_2014, TRUE)
BestAlpha14_dz <- findBestAlpha(alpha_frame_2014_dz$alpha0, alpha_frame_2014_dz$alpha1)

plotAlphas(alpha_frame_2007_dz, alpha_frame_2014_dz, "Full Graph - '07 Vs '14 (Dropped Zeros)")
plotAlphas(alpha_frame_2007, alpha_frame_2007_dz, "Full Graph - '07 Vs '07 Dropped Zeros")
plotAlphas(alpha_frame_2014, alpha_frame_2014_dz, "Full Graph - '14 Vs '14 Dropped Zeros")

