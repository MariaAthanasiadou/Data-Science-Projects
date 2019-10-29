#Load the necessary libraries 
library(tm)
library(wordcloud)
library(Rgraphviz)
library(e1071)
library(caret)
library(SnowballC)
#Step 1-Data preparation- Importing of the file
df1<- read.csv("C:/Users/pc/Desktop/tweetsOfTexts.csv",sep=";",header=TRUE,stringsAsFactor=FALSE)
attach(df1)
names(df1)
str(df1)
#Shuffle rows in the dataframe
set.seed(1986)
df1<-df1[sample(nrow(df1)),]
str(df)
df1$music_genres<-as.factor(df1$music_genres)
str(df1) 
#Step 2-Data Preparation- Create the corpus- Preprocessing of the text
xCorpus<-VCorpus(VectorSource(df1$text))
clean.corpus <- tm_map(xCorpus,content_transformer(tolower))
clean.corpus <- tm_map(clean.corpus, removeNumbers)
myStopList=read.table("C:/Users/pc/Desktop/stop.txt",header=FALSE,sep="\n",strip.white=TRUE)
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
#Step 3- Data preparation - Create the DTM 
clean.corpus.dtm <- DocumentTermMatrix(clean.corpus)
#Visualizing textual data- Create the WordClouds
#dtm<-TermDocumentMatrix(clean.corpus)
#dtmMat<-removeSparseTerms(dtm,sparse=0.985) 
#dtm_matrix<-as.matrix(dtmMat)
#v<-sort(rowSums(dtm_matrix),decreasing=TRUE)
#d<-data.frame(word = names(v),freq=v)
#head(d, 6)
#wordcloud(words=d$word,freq=d$freq,min.freq=1,random.order=FALSE, #colors=brewer.pal(8, "Dark2"),scale = c(5, 0.5))
#Word association
#dtm<-TermDocumentMatrix(clean.corpus)
#dtmMat<-removeSparseTerms(dtm,sparse=0.97) 
#findFreqTerms(dtmMat, lowfreq=15)
#Find words associations
#findAssocs(dtmMat,"song", corlimit=0.01)
#Step 4-Data preparation- Creating training and test datasets
df1.train<-df1[1:7937,]
df1.test<-df1[7938:10583,]
dtm.train<-clean.corpus.dtm[1:7937,]
dtm.test<-clean.corpus.dtm[7938:10583,]
clean.corpus.train<-clean.corpus[1:7937]
clean.corpus.test<-clean.corpus[7938:10583]
#Step 5- Data preparation-creating indicator features for frequent words
dim(dtm.train)
frequent_terms <- findFreqTerms(dtm.train,5)
dtm.train.nb<-DocumentTermMatrix(clean.corpus.train,control=list(dictionary=frequent_terms))
dtm.test.nb<-DocumentTermMatrix(clean.corpus.test,control=list(dictionary= frequent_terms))
#Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count<-function(x){
 y<-ifelse(x>0,1,0)
     y<-factor(y,levels=c(0,1),labels=c("No","Yes"))
     y
  }
trainNB<-apply(dtm.train.nb,MARGIN=2,convert_count)
testNB<-apply(dtm.test.nb,MARGIN=2,convert_count)
#Constructing model and making prediction
naiveBayesModel<-train(trainNB,df1.train$music_genres,method=”nb”,trControl=trainControl(method=”cv”,number=10))
naiveBayes.pred <- predict(naiveBayesModel, newdata=testNB) 
#Creation of Confusion Matrix 
naiveBayesConfMat<-confusionMatrix(naiveBayes.pred, df1.test$music_genres)
