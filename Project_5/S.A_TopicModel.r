# Load the necessary libraries
library(NLP)
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(textdata)
library(syuzhet)
library(utils)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggrepel)
#Importing the file
df1=read.csv("C:/Users/user/Desktop/tweetsOfTexts2.csv",sep=";",header=TRUE, stringsAsFactor=FALSE)
attach(df1)
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
text_lda<-LDA(dtm1.new,k=5,control=list(nstart=5,seed=c(2003,5,63,100001,765),best=TRUE))
m_tidy<-tidy(text_lda)
#Building the NRC lexicon
nrc<-get_sentiments("nrc")
nrc_words <- inner_join(m_tidy,nrc,by = c("term"="word"))
sentim<-nrc_words%>%group_by(sentiment)%>%tally%>%arrange(desc(n))
sentim$percent<-(sentim$n/sum(sentim$n))*100
p10<-ggplot(sentim, aes(entiment,percent)) +
geom_bar(aes(fill = sentiment), stat = "identity") +
theme(legend.position = "none") +
xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment analysis based on lexicon: 'NRC'")
nrc_words_plot<-nrc_words%>%group_by(sentiment)%>%slice(seq_len(10))%>%ungroup()%>%arrange(desc(beta))
p<-nrc_words_plot%>%
ggplot(aes(term,1,label=term,fill=sentiment))+
geom_point(color="transparent")+
geom_label_repel(force=1,nudge_y= .5,
direction="y",
box.padding=0.05,
segment.color="transparent",
size=3)+
face_grid(~sentiment)+
theme(axis,text.y=element_blank(),axis.text.x=element_blank(),
axis.title.x=element_text(size=6),
panel.grid=element_blank(),panel.background=element_blank(),
panel.border=element_rect("lightgray",fill=NA),
strip.text.x=element_text(size=9)+
xlab(NULL)+ylab(NULL)+
ggtitle("NRC Sentiment")+
coord_flip()
# Building the BING lexicon
bing<-get_sentiments("bing")
bing_words <- inner_join(m_tidy,bing,by = c("term"="word"))
bing_words_plot<-bing_words%>%group_by(sentiment)%>%
slice(seq_len(10))%>%
ungroup()%>%
arrange(desc(beta))
p1<-bing_words_plot%>%
ggplot(aes(term,1,label=term,fill=sentiment))+
geom_point(color="transparent")+
geom_label_repel(force=1,nudge_y= .5,
direction="y",
box.padding=0.05,
segment.color="transparent",
size=3)+
face_grid(~sentiment)+
theme(axis,text.y=element_blank(),axis.text.x=element_blank(),
axis.title.x=element_text(size=6),
panel.grid=element_blank(),panel.background=element_blank(),
panel.border=element_rect("lightgray",fill=NA),
strip.text.x=element_text(size=9)+
xlab(NULL)+ylab(NULL)+
ggtitle("BING Sentiment")+
coord_flip()
#Bulding the AFINN lexicon
afinn<-get_sentiments("afinn")
afinn_words <- inner_join(m_tidy,afinn,by = c("term"="word"))
afinn_words_plot<-afinn_words%>%group_by(value)%>%ungroup()%>%arrange(desc(beta))
table(afinn_words_plot$value)
qplot(afinn_words_plot$value,col="orange",xlab="Score of terms",ylab="Count",main="Sentiment analysis based on ")
posT<-afinn_words_plot$term[which(af$value>0)]%>%head(n=10)
negT<-afinn_words_plot$term[which(af$value<0)]%>%head(n=10)
veryPosT<-afinn_words_plot$term[which(af$value==5)& (af$value==4)] 