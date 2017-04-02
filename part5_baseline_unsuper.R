#Project part 5: baseline model
#unsupervised method

#Get dataset4 from part3
#k-means cluster
set.seed(1) ## fix the random seed to produce the same results 
#k=2
#after standarzation clustering
data_k2 = kmeans(dataset6[-10],centers = 2)
data_k2

#k=8
#since there are 8 levels in TYPE
#after standarzation clustering
data_k8 = kmeans(dataset6[-10],centers = 8)
data_k8

data_sk2 = kmeans(dataset7, centers = 2)


library(cluster)
library(fpc)
#cluster general plot
library(fpc)
library(cluster)
plotcluster(dataset6[,-10],data_k2$cluster)
clusplot(dataset6[,-10], data_k2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
with(dataset5, pairs(dataset6[,-10], col=c(2:10)[data_k2$cluster])) 

#plotcluster can't be done due to the size of vector
## list the cluster assignments
o=order(data_sk2$cluster)
data.frame(dataset7$type[o],data_k2$cluster[o])

#Adjust the MDS function to make MDS map after clustering
adMDS<-function(dataset,cluName){
  ## calculate distance matrix
  news_dist = dist(dataset[,-10])
  
  ## visualize clusters
  news_mds <- cmdscale(news_dist)
  
  plot(news_mds, type = 'n')
  text(news_mds, labels=dataset[,10],col=data_k2$cluster+1)
  return(news_mds)
}
#MDS map for k-means with 4 cluster
adMDS(dataset6, data_k2)

## list the cluster assignments
o=order(data_sk8$cluster)
data.frame(dataset6$type[o],data_sk8$cluster[o])

#MDS map for k-means with 8 clusters
adMDS(dataset6, data_sk8)

##h-clustering with single-link
library(cluster)

## use hclust,cutree for hierarchical clustering
data.dist = dist(dataset6[,-10]) ## use dist to obtain distance matrix

hc_plot<-function(hc_agg,n){
  hc1 = cutree(hc_agg,k=n)
  hc1<-as.data.frame(hc1)
  names(hc1)[names(hc1)=="hc1"] <- "cluster"
  adMDS(dataset6, hc1)
  return(hc1)
}

#h-clustering with single-link
hc_s = hclust(data.dist,method='single')
plot(hc_s)
#k=2
grp3<-hc_plot(hc_s,2)
grp3
#k=8
grp4<-hc_plot(hc_s,8)
grp4

#h-clustering with complete-link
hc_c = hclust(data.dist,method='complete')
plot(hc_c)

#k=2
grp5<-hc_plot(hc_c,2)
grp5
#k=8
grp6<-hc_plot(hc_c,8)
grp6

#h-clustering with average-link
hc_a = hclust(data.dist,method='average')
plot(hc_a)

#k=2
grp7<-hc_plot(hc_a,2)
grp7
#k=8
grp8<-hc_plot(hc_a,8)
grp8

