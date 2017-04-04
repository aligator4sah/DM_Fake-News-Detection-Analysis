setwd("D:/MS/DataMining/fake.csv/")
fake.data <- read.csv("fake.csv")

#Creating a dictionary of known fakenews sources
dict.sites <- list("americannews.com","bigamericannews.com", "christwire.org", "abcnews.com.co", "bloomberg.ma", "cnn-trending.com",
                   "drudgereport.com.co", "usatoday.com.co", "washingtonpost.com.co","yournewswire.com", 
                   "thereporterz.com", "nytimes.com","twitter.com/capnews", "christwire.org", "civictribune.com", 
                   "clickhole.com", "creambmp.com", "dcgazette.com", "dailycurrant.com", "dcclothesline.com", "
                   derfmagazine.com", "drudgereport.com.co", "duhprogressive.com", "empirenews.com", "enduringvision.com", 
                   "www.cc.com/indecision", "msnbc.co", "msnbc.website", "mediamass.net", "nationalreport.net", 
                   "newsbiscuit.com", "news-hound.com", "newsmutiny.com", "politicalears.com", "private-eye.co.uk", 
                   "realnewsrightnow.com", "rilenews.com", "sprotspickle.com", "thenewsnerd.com", "theuspatriot.com", 
                   "witscience.org","abcnews.com.co","abcnewsgo.co","24newsflash.com","alternativemediasyndicate.com",
                   "americannews.com","americanoverlook.com","100percentfedup.com","365usanews.com","4threvolutionarywar.wordpress.com",
                    "aheadoftheherd.com","americanlookout.com","americanpatriotdaily.com","americanthinker.com","AmmoLand.com",
                    "anotherdayintheempire.com","beforeitsnews.com","bighairynews.com","bostonleader.com","buzzfeedusa.com",
                    "cap-news.com","cbsnews.com.co","centerforsecuritypolicy.org","channel-7-news.com","charismanews.com",
                    "cityworldnews.com","civictribune.com","clashdaily.com","conservativedailypost.com","conservativefighters.com",
                    "conservativefiringline.com","dailyheadlines.com","dailyheadlines.net","denverguardian.com",
                    "departed.co","donaldtrumpnews.co","embols.com","empireherald.com","empirenews.net",
                   "floridasunpost.com","freedomdaily.com","gopthedailydose.com","goneleft.com","immediatesafety.org",
                    "infostormer.com","intrendtoday.com","itaglive.com","libertyblitzkrieg.com","mediamass.net","myfreshnews.com","myzonetoday.com",
                    "nationonenews.com","nbc.com.co","news4ktla.com","newsbbc.net","newsfrompolitics.com","newswithviews.com","now8news.com",
                    "onlineconservativepress.com","pakalertpress.com","politicalo.com","politicalsitenews.com","politicalupdator.com",
                  "prntly.com","proudcons.com","react365.com","realnewsrightnow.com","rhotv.com","rickwells.us","rilenews.com", 
                   "subjectpolitics.com","success-street.com","supremepatriot.com","theamericanindependent.wordpress.com",
                    "thebostontribune.com","thecommonsenseshow.com","thecontroversialfiles.net","thefreepatriot.org",
                   "thelastgreatstand.com","theracketreport.com","thereporterz.com","therightists.com","therightscoop.com",
                  "thetruthdivision.com","thetruthseeker.co.uk","trumpvision365.com","undergroundworldnews.com","usa2016elections.com",
                  "usadailypolitics.com","usadailytime.com","usahitman.com","usainfobox.com","usamagazinestudio.com","usanewsflash.com",
                  "usanewsinsider.com","usanewspolitics.com","usaonlinepolitics.com","usaphase.com","vigilantcitizen.com",
                  "viralliberty.com","worldpoliticsus.com","worldrumor.com","worldstoriestoday.com","wtoe5news.com")


dict.sites

#adding Gave_Fake feature
fake.data["Gave_Fake"] <- NA

df <- within(fake.data, Gave_Fake[!is.na(match(site_url,dict.sites))] <- 1)


#df$Gave_Fake <- factor (df$Gave_Fake)
#df$Gave_Fake<- as.character(df$Gave_Fake)
df$Gave_Fake[is.na(df$Gave_Fake)] <- 0

#Pre-preprocessing title and text
Sys.setenv(NOAWT= "true") 
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(df$title))
corpus = tm_map(corpus, tolower) 
inspect(corpus[1:3]) 

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus = tm_map(corpus, stemDocument, language = "english")
inspect(corpus[1:3]) 

df["stemmed_title"] <- corpus
df$stemmed_title[1:10]

textcorpus = Corpus(VectorSource(df$text))
textcorpus = tm_map(textcorpus, tolower) 
inspect(textcorpus[1:3]) 

textcorpus = tm_map(textcorpus, removePunctuation)
textcorpus = tm_map(textcorpus, removeNumbers)
textcorpus = tm_map(textcorpus, function(x) removeWords(x, stopwords("english")))
textcorpus = tm_map(textcorpus, stemDocument, language = "english")
inspect(textcorpus[1:3]) 


df["stemmed_text"] <- textcorpus

#update new list
# prepocessing key words


dict.keywords <- list("fake", "unreal", "unverified", "breaking", "action", "false", "parody", "untrue", "biased", "unbelievable",
                      "crazy","ridiculous","phony","opposing","ufo","alien","debunked","misleading","invalid","fabricate",
                      "unverified","claim","partial","distort","hypothetical","nonsense","bullshit","joke","fictional","misinterpret", "fraud");

df["text_title"] <- do.call(paste, c(df[c(22,23)], sep=""))
library(tm)

types <- levels(df$type)
lentypes <- length(types)


keywords <- c()

for(i in 1:lentypes){
  if(i != 2) {
    curr <- df[which(df$type == types[i] & df$type != "bs"),]
    corpus =  Corpus(VectorSource(curr$text_title))
    td.mat = as.matrix(TermDocumentMatrix(corpus))
    m = as.matrix(td.mat)
    v = sort(rowSums(m), decreasing = T)
    words = names(v)
    keywords <<- c(keywords,words[20:22])
  }
}


curr <- df[which(df$type == "bs"),][1:1000,]
corpus =  Corpus(VectorSource(curr$text_title))
td.mat = as.matrix(TermDocumentMatrix(corpus))
m = as.matrix(td.mat)
v = sort(rowSums(m), decreasing = T)
words = names(v)
keywords <- c(keywords,words[20:22])

lenFeatures <- length(keywords)
nrows <- dim(df)[1]

feature.df <- c()

for(i in 1:nrows) {
  cols <- c()
  for(j in 1:lenFeatures){
    if(regexpr(keywords[j], df[i,c("text_title")]) > 0) {
      #df[i,c("text_contains_keywords")] <- df[i,c("text_contains_keywords")] + 1
      cols <- c(cols, 1)
    }
    else {
      cols <- c(cols, 0)
    }
  }
  feature.df <<- rbind(feature.df,cols)
}

feature.df[1:3,]

dim(feature.df)
rownames(feature.df) <- NULL
      
#keyCorp = Corpus(VectorSource(dict.keywords))
#keyCorp <- tm_map(keyCorp, tolower) 

#keyCorp = tm_map(keyCorp, removePunctuation)
#keyCorp = tm_map(keyCorp, removeNumbers)
#keyCorp = tm_map(keyCorp, stemDocument, language = "english")
#inspect(keyCorp[1:3]) 

#adding text_contains_keywords feature
#df["text_contains_keywords"] <- 0
#df$contains_keywords

#nrows <- dim(df)[1]
#lenKeys <- length(keyCorp)
#keys <- c(keyCorp)

#for(i in 1:nrows) {
#  for(j in 1:lenKeys){
#    if(regexpr(keys$content[j], df[i,c("stemmed_text")]) > 0){
#      df[i,c("text_contains_keywords")] <- df[i,c("text_contains_keywords")] + 1
#    }
#    else {
#      
#    }
#  }
#}

#Adding title_contains_keywords feature

# df["title_contains_keywords"] <- 0
# 
# for(i in 1:nrows) {
#   for(j in 1:lenKeys){
#     if(regexpr(keys$content[j], df[i,c("stemmed_title")]) > 0){
#       df[i,c("title_contains_keywords")] <- df[i,c("title_contains_keywords")] + 1
#     }
#   }
# }


#Starting analysis with new features:


#final <- cbind.data.frame(df, feature.df)


test <- as.data.frame(feature.df)
test[1:3,]

vals <- lapply(test, function(x) as.factor(x))

final <- cbind.data.frame(df, vals)

final[1:3,]


data <- final[c(13,15:21,25:length(final))]

library(VGAM)
fit <- vglm(type~.,multinomial,data=data[,c(-6,-19)])

#Single Hidden Layer Feed Forward Neural Network
library(nnet)
data <- final[c(13,15:21,25:length(final))]
fit <- nnet(type~., data[,c(-6,-19)], size=8, linout = TRUE)
table(predict(fit,data[,c(-6,-19)],type="class"),data$type)


#Multinomial Logistic Regression
data <- final[c(13,15:21,25:length(final))]
fit <- multinom(data$type~., data[,c(-6,-19)])
table(predict(fit,data[,c(-6,-19)],type="class"),data$type)


#Baseline Analysis
data <- final[c(13,15:21,25:length(final))]
dataset = data[c(-6,-19,-7)]
target = data[c(7)]

total_length = length(data$spam_score)
ratio = 0.7
n_train = total_length * ratio
n_test = total_length * (1- ratio)

#Sampling the elements of the dataset
train = sample(1:total_length,n_train,  replace=FALSE)

#Building the training and test dataset
train_data = dataset[train,]
test_data = dataset[-train,]
train_target = target[train,]
test_target = target[-train,]

#Single Hidden Layer Feed Forward Neural Network
library(nnet)
fit <- nnet(d~.,data=data.frame(d=train_target,train_data), size=8, linout = TRUE)
table(predict(fit,test_data,type="class"),test_target)


#Multinomial Logistic Regression
fit <- multinom(d~.,data=data.frame(d=train_target,train_data))
predicted_target <- predict(fit,test_data,type="class")
table(predicted_target,test_target)


check.model.accuracy <- function(predicted.class, actual.class){
  
  result.tbl <- as.data.frame(table(predicted.class,actual.class ) ) 
  
  result.tbl$Var1 <- as.character(result.tbl$predicted.class)
  result.tbl$Var2 <- as.character(result.tbl$actual.class)
  
  colnames(result.tbl)[1:2] <- c("Pred","Act")
  
  cntr <- 0  
  for (pred.class in unique(result.tbl$Pred) ){
    cntr <- cntr+ 1
    tp <- sum(result.tbl[result.tbl$Pred==pred.class & result.tbl$Act==pred.class, "Freq"])
    tp.fp <- sum(result.tbl[result.tbl$Pred == pred.class , "Freq" ])
    tp.fn <- sum(result.tbl[result.tbl$Act == pred.class , "Freq" ])
    precision <- tp/tp.fp 
    recall <- tp/tp.fn
    F.score <- 2*precision*recall/(precision+recall)
    if (cntr == 1 ) F.score.row <- cbind(pred.class, precision,recall,F.score)
    if (cntr > 1 ) F.score.row <- rbind(F.score.row,cbind(pred.class,precision,recall,F.score))
  }
  
  DF <- as.data.frame(F.score.row)
  return(DF)
}

check.model.accuracy(predicted_target,test_target) 
# For multiclass, average across all classes
table(test_target)



grp8$cluster

library(cluster)
library(fpc)

grp8 = kmeans(dataset, centers=8)

distmat <- dist(dataset)

mdsplot <- cmdscale(distmat)

plot(grp8$cluster)
points(grp8$centers, col=1:8, pch=20, cex=1)


colors=rainbow(8)[grp8$cluster]
text(test_target, font = 4, cex=.7, col=colors, pch=20,labels=test_target)
