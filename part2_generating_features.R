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
                   ,"floridasunpost.com","freedomdaily.com","gopthedailydose.com","goneleft.com","immediatesafety.org"
                    "infostormer.com","intrendtoday.com","itaglive.com","libertyblitzkrieg.com","mediamass.net","myfreshnews.com","myzonetoday.com",
                    "nationonenews.com","nbc.com.co","news4ktla.com","newsbbc.net","newsfrompolitics.com","newswithviews.com","now8news.com",
                    "onlineconservativepress.com","pakalertpress.com","politicalo.com","politicalsitenews.com","politicalupdator.com"
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
df$Gave_Fake

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
corpus = tm_map(corpus, stemDocument, language = "english")
inspect(corpus[1:3]) 

df["stemmed_title"] <- corpus
df$stemmed_title[1:10]

textcorpus = Corpus(VectorSource(df$text))
textcorpus = tm_map(textcorpus, tolower) 
inspect(textcorpus[1:3]) 

textcorpus = tm_map(textcorpus, removePunctuation)
textcorpus = tm_map(textcorpus, removeNumbers)
textcorpus = tm_map(textcorpus, stemDocument, language = "english")
inspect(textcorpus[1:3]) 


df["stemmed_text"] <- textcorpus


# prepocessing key words
dict.keywords <- list("fake", "unreal", "unverified", "breaking", "action", "false", "parody", "untrue", "biased", "unbelievable",
                      "crazy","ridiculous","phony","opposing","ufo","alien","debunked","misleading","invalid","fabricate",
                      "unverified","claim","partial","distort","hypothetical","nonsense","bullshit","joke","fictional","misinterpret", "fraud");
keyCorp = Corpus(VectorSource(dict.keywords))
keyCorp <- tm_map(keyCorp, tolower) 


keyCorp = tm_map(keyCorp, removePunctuation)
keyCorp = tm_map(keyCorp, removeNumbers)
keyCorp = tm_map(keyCorp, stemDocument, language = "english")
inspect(keyCorp[1:3]) 

#adding text_contains_keywords feature
df["text_contains_keywords"] <- 0
df$contains_keywords

nrows <- dim(df)[1]
lenKeys <- length(keyCorp)
keys <- c(keyCorp)

for(i in 1:nrows) {
  for(j in 1:lenKeys){
    if(regexpr(keys$content[j], df[i,c("stemmed_text")]) > 0){
      df[i,c("text_contains_keywords")] <- df[i,c("text_contains_keywords")] + 1
    }
  }
}

#Adding title_contains_keywords feature

df["title_contains_keywords"] <- 0

for(i in 1:nrows) {
  for(j in 1:lenKeys){
    if(regexpr(keys$content[j], df[i,c("stemmed_title")]) > 0){
      df[i,c("title_contains_keywords")] <- df[i,c("title_contains_keywords")] + 1
    }
  }
}


