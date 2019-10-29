#Load the necessary libraries
library(NLP)
library(tm)
library(cluster)
library(fpc) 
library(SnowballC)
library (slam)
library(philentropy)
library(factoextra)
library(ggsci)
#Importing the file
df1=read.csv("C:/Users/user/Desktop/tweetsOfTexts2.csv",sep=";",header=TRUE, stringsAsFactor=FALSE)
attach(df1)
names(df1)
#Creation of corpus- Preprocessing of the text 
xCorpus=VCorpus(VectorSource(df1$text))
clean.corpus <- tm_map(xCorpus,content_transformer(tolower))
clean.corpus <- tm_map(clean.corpus, removeNumbers)
myStopList=read.table("C:/Users/user/Desktop/stop.txt",header=FALSE,sep="\n",strip.white=TRUE)
clean.corpus<-tm_map(clean.corpus,removeWords,myStopList[,1])
clean.corpus<-tm_map(clean.corpus,removeWords,stopwords("english"))
removeUsersNames<-(function(x)gsub("@\\w+", "", x))
clean.corpus<- tm_map(clean.corpus,content_transformer(removeUsersNames))
removeURL0<- (function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
clean.corpus<-tm_map(clean.corpus,content_transformer(removeURL0))
removeStrangeCharac<- (function(x) gsub("[^[:alpha:][:space:]]*","",x))
clean.corpus<-tm_map(clean.corpus,content_transformer(removeStrangeCharac), lazy = TRUE)
clean.corpus <- tm_map(clean.corpus, removePunctuation)
clean.corpus <- tm_map(clean.corpus, stemDocument)
clean.corpus <- tm_map(clean.corpus, stripWhitespace)
#Splitting text documents into words 
dtm1<-DocumentTermMatrix(clean.corpus,control=list(weighting=weightTfIdf))
rowTotals<-slam::row_sums(dtm1)
dtm1Final<-dtm1[rowTotals>0, ]
dtmsFinal<-removeSparseTerms(dtm1Final,0.98)
matrDtmsFinal<-as.matrix(dtmsFinal)
#K-means plot with five clusters 
dis<-distance(matrDtmsFinal,method="jaccard") 
kfit <- kmeans(dis, centers=5,iter.max=15,nstart=50) 
fviz_cluster(kfit,data=dtmsFinal,ellipse.type="norm",geom="point",show.clust.cent=TRUE,ellipse.alpha=0.8,palette="uchicago")