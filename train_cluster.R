#train data exp

train <- read.csv("C:/Users/daisy/OneDrive/Study/DM/Project/dataset/final_train_top3.csv")
test <- read.csv("C:/Users/daisy/OneDrive/Study/DM/Project/dataset/final_test_top3.csv")
train[1:3,]
train = train[,-1]

set.seed(1) ## fix the random seed to produce the same results 
#k=2
#after standarzation clustering
train_k9 = kmeans(train[-1],centers = 8)
train_k9

test = test[,-1]
str(test)
test = na.omit(test)
test_k2 = kmeans(test, centers = 2)

library(cluster)
library(fpc)
#cluster general plot
plotcluster(train[,-1],train_k9$cluster)
clusplot(train[,-1], train_k9$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#plotcluster can't be done due to the size of vector
## list the cluster assignments
o=order(train_k9$cluster)
data.frame(train$label[o],train_k9$cluster[o])

## calculate distance matrix
news_dist = dist(train[,-1])
news_distt = dist(test)
  
## visualize clusters
news_mds <- cmdscale(news_dist)
news_mdst <- cmdscale(news_distt)
  
plot(news_mds, type = 'n')
text(news_mds, labels=train[,1],col=train_k9$cluster+1)
legend()

plot(news_mdst, type = 'n')
text(news_mdst, labels=test[,1],col=test_k2$cluster+1)

#Adjust the MDS function to make MDS map after clustering
adMDS<-function(dataset,cluName){
  ## calculate distance matrix
  news_dist = dist(dataset[,-1])
  
  ## visualize clusters
  news_mds <- cmdscale(news_dist)
  
  plot(news_mds, type = 'n')
  text(news_mds, labels=dataset[,1],col=cluName$cluster+1)
  return(news_mds)
}

hc_plot<-function(hc_agg,n){
  hc1 = cutree(hc_agg,k=n)
  hc1<-as.data.frame(hc1)
  names(hc1)[names(hc1)=="hc1"] <- "cluster"
  adMDS(train, hc1)
  return(hc1)
}

#h-clustering with single-link
hc_s = hclust(news_dist,method='single')
plot(hc_s)
#k=9
grp3<-hc_plot(hc_s,8)
grp3


#h-clustering with complete-link
hc_c = hclust(news_dist,method='complete')
plot(hc_c)

#k=9
grp4<-hc_plot(hc_c,8)
grp4


#h-clustering with average-link
hc_a = hclust(news_dist,method='average')
plot(hc_a)

#k=9
grp5<-hc_plot(hc_a,8)
grp5

##########################################################################
#evaluation
cluster.purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

cluster.entropy <- function(clusters,classes) {
  en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {if (p) -p*log2(p) else 0} ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}


p1<-cluster.purity(train_k9$cluster[o],train$label[o])
p2<-cluster.purity(grp3$cluster[o],train$label[o])
p3<-cluster.purity(grp4$cluster[o],train$label[o])
p4<-cluster.purity(grp5$cluster[o],train$label[o])

purity<-c(p1,p2,p3,p4)

e1<-cluster.entropy(train_k9$cluster[o],train$label[o])
e2<-cluster.entropy(grp3$cluster[o],train$label[o])
e3<-cluster.entropy(grp4$cluster[o],train$label[o])
e4<-cluster.entropy(grp5$cluster[o],train$label[o])

entropy<-c(e1,e2,e3,e4)

result = rbind(purity,entropy)
result = as.data.frame(result)
colnames(result) = c("k-means", "hc-single","hc-complete","hc-average")


library(knitr)
kable(result, caption = 'Table 1: Summary of Clustering')

library(ggplot2)
library(plyr)
library(reshape2)
result1<-as.data.frame(t(result))
result1$classification<- c("k-means", "hc-single","hc-complete","hc-average")

ggplot(result1, aes(x = classification, y = purity)) + 
  geom_bar(stat = "identity")

ggplot(result1, aes(x = classification, y = entropy)) + 
  geom_bar(stat = "identity")





