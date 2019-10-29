# Load the necessary libraries
library(NLP)
library(tm)
library(topicmodels)
library(tidytext)
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
clean.corpus <- tm_map(clean.corpus, stripWhitespace)
#Splitting text documents into words 
dtm1<-DocumentTermMatrix(clean.corpus)
rowTotals<-apply(dtm1,1,sum)
dtm1.new<-dtm1[rowTotals>0, ]
#Compute the LDA model using Gibbs sampling 
text_lda<-LDA(dtm1.new,k=5,method=”Gibbs”,control=list(nstart=5,seed=c(2003,5,63,100001,765),burnin=4000,iter=2000,thin=500,best=TRUE))
#Find the top 10 terms in each topic 
theResults<-posterior(text_lda)
topTenTermsEachTopic<-terms(text_lda,10)
#Find the documents topic probabilities 
text_docs<-tidy(text_lda, matrix=”gamma”)
