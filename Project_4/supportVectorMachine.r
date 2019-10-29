#Load the necessary libraries 
library(tm) 
library(e1071)
library(caret)
library(dplyr)
library(SnowballC)
#Importing of the file
df1<-read.csv("C:/Users/pc/Desktop/tweetsOfTexts.csv",sep=";",header=TRUE,stringsAsFactor=FALSE)
attach(df1)
names(df1)
str(df1)
#Shuffle rows in the dataframe 
set.seed(1986)
df1<-df1[sample(nrow(df1)),]
str(df)
df1$music_genres<-as.factor(df1$music_genres)
str(df1) 
#Creation of corpus- Preprocessing of the text 
xCorpus<-VCorpus(VectorSource(df1$text))
clean.corpus <- tm_map(xCorpus,content_transformer(tolower))
clean.corpus <- tm_map(clean.corpus, removeNumbers)
myStopList<-read.table("C:/Users/pc/Desktop/stop.txt",header=FALSE,sep="\n",strip.white=TRUE)
clean.corpus<-tm_map(clean.corpus,removeWords,myStopList[,1])
clean.corpus<-tm_map(clean.corpus,removeWords,stopwords("english"))
clean.corpus <- tm_map(clean.corpus, removePunctuation)
clean.corpus <- tm_map(clean.corpus, stemDocument)
clean.corpus <- tm_map(clean.corpus, stripWhitespace)
#Splitting text documents into words 
clean.corpus.dtm<-DocumentTermMatrix(clean.corpus,control=list(weighting=function(x)weightTfIdf(x,normalize=FALSE)))
#Creating indicator features for frequent words
frequent_terms<-findFreqTerms(clean.corpus.dtm,10)
clean.corpus.dtm2 <- DocumentTermMatrix(clean.corpus, list(global = c(2, Inf),dictionary = frequent_terms))
#Creating training and test datasets 
train_idx<-createDataPartition(df1$music_genres, p=0.75, list=FALSE)
trainSet <- df1[train_idx,]
testSet <- df1[-train_idx,]
trainSet2 <-clean.corpus[train_idx]
testSet2 <- clean.corpus[-train_idx]
#Creating indicator features for frequent words
frequent_terms2<- findFreqTerms(clean.corpus.dtm2, lowfreq=10)
#merging training and testing datasets with the dictionary of these frequent terms 
trainSVM <- DocumentTermMatrix(trainSet2, list(dictionary=frequent_terms2))
testSVM <- DocumentTermMatrix(testSet2, list(dictionary=frequent_terms2))
#converts the DTM into a categorical form for modeling
convert_counts <- function(x) {
x <- ifelse(x > 0, 1, 0)
# x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}
trainSVM <- trainSVM %>% apply(MARGIN=2, FUN=convert_counts)
testSVM <- testSVM %>% apply(MARGIN=2, FUN=convert_counts)
trainSVM <- as.data.frame(trainSVM)
testSVM <- as.data.frame(testSVM)
trainSVM1 <- cbind(music_genres=as.factor(trainSet$music_genres), trainSVM)
testSVM1 <- cbind(music_genres=as.factor(testSet$music_genres), testSVM)
trainSVM1<-as.data.frame(trainSVM1)
testSVM1<-as.data.frame(testSVM1)
#Constructing model and making prediction
svmModel<-svm(music_genres~.,data=trainSVM1,method=”C-classification”,kernel=”radial”)
svm.pred <- predict(svmModel, na.omit(testSVM1))  
#Creation of Confusion Matrix 
svmConfMat<-confusionMatrix(svm.pred,testSVM1$music_genres)
