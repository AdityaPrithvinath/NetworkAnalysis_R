getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:\\Users\\adity\\OneDrive\\Documents\\UIC\\SocialMedi&Network Analysis\\Assignment4"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

## Load package
library(igraph)

infile<-"Macrae_table5.5_mjbook.csv"

macrae_frame=read.csv(infile, header = TRUE, sep = ",")
macrae_frame$nop <-macrae_frame$Number.of.prisoners
# This is the empirical cumulative distribution function; but it's not useful for this dataset
# It calculates F(d) for unaggregated data. But the current dataset is already aggregated; so you 
# should calculate F(d) using the cumulative sum instead
F_d<-ecdf(macrae_frame$nop)
plot(F_d)


degree_total <- sum(macrae_frame$nop * macrae_frame$Degree)
degree_avg <- degree_total/sum(macrae_frame$nop)
print(degree_avg)
m<-degree_avg/2
m #computing the m parameter
#alpha value to use for linear model
alpha_0 = 0.11
macrae_frame$fd<-cumsum(macrae_frame$nop)/sum(macrae_frame$nop)
#Y var computation for the linear model
macrae_frame$Y <- log (1-macrae_frame$fd)
#X var computation for linear model
macrae_frame$X <- log(macrae_frame$Degree + 2*m*alpha_0/(1-alpha_0))
#excluding the last row since it contain an undefined value -Inf given by log 0
macrae_frame_subset<-macrae_frame[-9,]
macrae.lm<-lm(macrae_frame_subset$Y ~ macrae_frame_subset$X , data=macrae_frame_subset)
summary(macrae.lm)
#checking the coefficients
coefficients(macrae.lm)


remove(alpha_0)
alpha_0=0.10
macrae_frame_0.1<-macrae_frame[,1:3]
macrae_frame_0.1$fd<-cumsum(macrae_frame_0.1$nop)/sum(macrae_frame_0.1$nop)
macrae_frame_0.1$Y <- log (1-macrae_frame_0.1$fd)
macrae_frame_0.1$X <- log(macrae_frame_0.1$Degree + 2*m*alpha_0/(1-alpha_0))
macrae_frame_0.1subset<-macrae_frame_0.1[-9,]
macrae.lm_0.1<-lm(macrae_frame_0.1subset$Y ~ macrae_frame_0.1subset$X , data=macrae_frame_0.1subset)
summary(macrae.lm_0.1)
coefficients(macrae.lm_0.1)
beta_1<-macrae.lm_0.1$coefficients[2]
beta_1
alpha_1 <- 1 + (2/beta_1)
alpha_1
remove(alpha_0)
#Alpha value to be used for alpha_1 estimation.
alpha_0 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)
#Creation of the vector before using in the loop.
beta_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
alpha_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

for(i in 1:length(alpha_0)) 
{
  macrae_frame_loop<-macrae_frame[,1:3]
  macrae_frame_loop$fd<-cumsum(macrae_frame_loop$nop)/sum(macrae_frame_loop$nop)
  macrae_frame_loop$Y <- log (1-macrae_frame_loop$fd)
  inf_index<-which(macrae_frame_loop$Y==-Inf) #Finding index of _Inf value
  macrae_frame_loop$X <- log(macrae_frame_loop$Degree + 2*m*alpha_0[i]/(1-alpha_0[i]))
  
  macrae_frame_loop<-macrae_frame_loop[-inf_index,] #excluding row with value -Inf
  # Fit the model
  macrae.lm <- lm(Y ~ X, data=macrae_frame_loop)
  beta_1[i] <- coefficients(macrae.lm)[2]
  alpha_1[i] <- 1 + 2/beta_1[i]
}
plot(alpha_0,alpha_1)


infile1<-"Coauthorship_GoyalEtAl.csv"
goyal_Auth<-read.csv(infile1, header = TRUE, sep = ",")
goyal_Auth$noa<-goyal_Auth$Number.of.authors

degree_total <- sum(goyal_Auth$noa*goyal_Auth$Degree)
degree_avg <- degree_total/sum(goyal_Auth$noa)
print(degree_avg)
m<-degree_avg/2
m
alpha_g0 <- c(0.1, 0.2, 0.3, 0.4,0.41,0.42,0.43,0.44,0.45,0.46) 
beta_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
alpha_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
for(i in 1:length(alpha_g0)) 
{
  goyal_Auth_loop<-goyal_Auth[,1:3]
  goyal_Auth_loop$fd<-cumsum(goyal_Auth_loop$noa)/sum(goyal_Auth_loop$noa)
  goyal_Auth_loop$Y <- log (1-goyal_Auth_loop$fd)
  inf_index<-which(goyal_Auth_loop$Y==-Inf)
  goyal_Auth_loop$X <- log(goyal_Auth_loop$Degree + 2*m*alpha_g0[i]/(1-alpha_g0[i]))
  
  goyal_Auth_loop<-goyal_Auth_loop[-inf_index,]
  # Fit the model
  goyal_Auth.lm <- lm(Y ~ X, data=goyal_Auth_loop)
  beta_1[i] <- coefficients(goyal_Auth.lm)[2]
  alpha_1[i] <- 1 + 2/beta_1[i]
}
alpha_1
plot(alpha_1,alpha_g0)



infile2<-"HamRadioOperators_Killworth.csv"
ham_frame<-read.csv(infile2, header = TRUE, sep = ",")
ham_frame$noo <-ham_frame$Number.of.Operators
degree_total <- sum(ham_frame$noo*ham_frame$Degree)
degree_avg <- degree_total/sum(ham_frame$noo)
print(degree_avg)
m<-degree_avg/2
m
alpha_h0 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)
beta_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
alpha_1<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
for(i in 1:length(alpha_h0)) 
{
  ham_frame_loop<-ham_frame[,1:3]
  ham_frame_loop$fd<-cumsum(ham_frame_loop$noo)/sum(ham_frame_loop$noo)
  ham_frame_loop$Y <- log (1-ham_frame_loop$fd)
  inf_index<-which(ham_frame_loop$Y==-Inf)
  ham_frame_loop$X <- log(ham_frame_loop$Degree + 2*m*alpha_h0[i]/(1-alpha_h0[i]))
  
  ham_frame_loop<-ham_frame_loop[-inf_index,]
  # Fit the model
  ham_frame_loop.lm <- lm(Y ~ X, data=ham_frame_loop)
  beta_1[i] <- coefficients(ham_frame_loop.lm)[2]
  alpha_1[i] <- 1 + 2/beta_1[i]
}

alpha_1
plot(alpha_1,alpha_h0)


# Some useful functions, Suggested help look-ups to learn more:
help(cumsum)
help(lm)
help(coefficients) # Run after lm, to get the value of your Beta slope estimate. Then convert it to the alpha estimate.
help(log)

# Execute  the entire for loop code block together 
for(i in 1:9) {
  #print(i) 
  alpha_0<-i/10
  print("Alpha 0: ")
  print (alpha_0)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
}

# This it is also useful to calculate alpha_1 values for the following
alpha_0<-0.99
alpha_0<-0.999
alpha_0<-0.9999

