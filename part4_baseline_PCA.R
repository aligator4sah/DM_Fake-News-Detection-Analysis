##Project part 5: baseline model
#PCA and MDS plot

#######################################################################################
#PCA and MDS based on author label
#get dataset5 from part3
#transfrom data variables to numeric so that we can generate distance matrix
table(dataset4$site_url)
dataset5$language<- as.numeric(as.factor(dataset5$language))
dataset5$author<- as.numeric(as.factor(dataset5$author))
dataset5$site_url<- as.numeric(as.factor(dataset5$site_url))
dataset5$country<- as.numeric(as.factor(dataset5$country))

#Get the correlation matrix from all variants
cor(dataset5[,-1])
## scale = TRUE: variables are first standardized. Default is FALSE
pcaunemp1 = prcomp(dataset5[,-1], scale=TRUE) 
pcaunemp1

## Get the loadings of PCA
pcaunemp1$rotation

## use 'predict' to project data onto the loadings
datapc1 = predict(pcaunemp1)
datapc1

#Generate a screeplot and determine the number of principle components
## plot the variances against the number of the principal component;
## retain only factors with eigenvalues greater than 1
plot(pcaunemp1, main="") ## same as screeplot(pcafood)
mtext(side=1, "Fake News Website Principal Components",  line=1, font=2)

#Plot the loadings for first principal component.
plot(pcaunemp1$rotation[,1],type='l')

#Get the mean value of each state and save them into columns
dataset6 = as.data.frame(t(dataset5))
colnames(dataset6) <- as.character(unlist(dataset6[1,]))
dataset6 = dataset6[-1, ]
emean<-colMeans(dataset6)
#####################################################################################
#######################################################################################
#Convert the dataframe into matrix and get the standard deviation for each column
unemp_matrix<-as.matrix(unemp1)
esd<-colSds(unemp_matrix)
unemp2<-cbind(unemplab,emean,esd)
unemp2[1:3,]

states = unemp2[,1]
n = length(states)
data = (as.matrix(unemp2[,-1]))
## 1) Center the data according to the mean
my.scaled.data = apply(data,2,function(x) (x-mean(x)))
plot(my.scaled.data,cex=0.9,col="blue",main="Plot of Scaled Data")
## 2) Calculate the covariance matrix
my.cov = cov(my.scaled.data)
my.cov

#3) Calculate the eigenvectors and eigenvalues of the covariance matrix
my.eigen = eigen(my.cov)
my.eigen

#3a) Plot the Eigenvectors over the scaled data
#Generate the scatter plot
plot(my.scaled.data,cex=0.9,col="blue",main="Plot of Scaled Data")
#From the scatter plot we can see, there are two components in the upper side of right corner
#should be excluded. In addtion, there are 2 points on the downside of right corner should be excluded too
#Therefore, there are 47 components should be the right number for the principal components.

#Plot the loadings for the first principal components
pc1.slope = my.eigen$vectors[2,1] /my.eigen$vectors[1,1]
abline(0,pc1.slope,col="red")
#Plot the loadings for the second pricipal components
pc2.slope = my.eigen$vectors[2,2] /my.eigen$vectors[1,2]
abline(0,pc2.slope,col="green")

#Generate a scatterplot to project states on the first two principal components.
## 4) Express the scaled data in terms of eigenvectors (principal components)
## get the P matrix (loadings, i.e., a matrix whose columns contain the eigenvectors)
loadings = my.eigen$vectors 
## project data onto the loadings
scores = my.scaled.data %*% loadings 
## plot the projected data on the first two PCs
plot(scores,ylim=c(-3,3),main='Data in terms of EigenVectors / PCs',xlab='PC1',ylab='PC2')
abline(0,0,col="red")
abline(0,90,col="green")

###############################################################################
#################################################################################
#MDS map
## calculate distance matrix
MDS<-function(dataset5){
  news_dist = dist(dataset5[,-9])
  
  ## visualize clusters
  news_mds <- cmdscale(news_dist)
  
  plot(news_mds, type = 'n')
  text(news_mds, labels=dataset5[,9])
  return(news_mds)
}

MDS(dataset5)

#dataset preparation -- stardazation
library(robustHD)
dataset6 = dataset5[,-9]
dataset6 = standardize(dataset6)
dataset6 = cbind(dataset6, dataset5[,9])
names(dataset6)[names(dataset6)=="dataset5[, 9]"] <- "type"
