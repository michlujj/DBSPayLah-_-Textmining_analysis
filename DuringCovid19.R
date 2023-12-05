library(tm) # for text mining
library(SnowballC) # for text stemming
library(readxl) # to read in dataset in excel format
library(writexl) # to write dataset in excel format
library(RColorBrewer) # for colour palettes
library(wordcloud) # for word cloud generator
library(sentimentr)# for sentiment analysis

# to import excel file containing consumer reviews
txtpath<-file.path("C:/Users/miche/ANL488/DuringCovid19.xlsx")
txt<-read_xlsx(txtpath)
# to create corpus
comments_corpus<- VCorpus(DataframeSource(txt))
# to create copy of corpus and implement operations on copy
comments<-comments_corpus
# case normalisation
comments<-tm_map(comments,content_transformer(tolower))
# removal of punctuations
comments<-tm_map(comments,removePunctuation)
# removal of numbers
comments<-tm_map(comments,removeNumbers)
# to remove non-English words
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ",x))
comments <- tm_map(comments, toSpace, "[^A-Za-z']")
# removal of stop words
mystopwords<-scan(file="C:/Users/miche/ANL488/mystopwords.data", what=character(),sep="\n")
comments<-tm_map(comments,removeWords,mystopwords[2:672])
comments<-tm_map(comments, removeWords, c(stopwords("english"),"nil", "na", "amp","abc"))
# stemming
comments<-tm_map(comments,stemDocument)
# to remove white space
comments<-tm_map(comments,stripWhitespace)


# to replace synonyms with correct spelling of words
synonyms <- list(list(word="install", syns=c("unistalled", "reinstall")),
                 list(word="register", syns=c("regitered","registering")),
                 list(word="convenient", syns=c("convenience", "convinient")),
                 list(word="freeze", syns=c("freezes", "frozen")),
                 list(word="payment", syns=c("e-payment", "payin")))

replaceSynonyms <- content_transformer (function(x, syn =NULL)
{Reduce(function(a,b) {gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)})
comments <- tm_map(comments, replaceSynonyms, synonyms)


# Build a term-document matrix
comments_dtm<-TermDocumentMatrix(comments)
dtm_m <- as.matrix(comments_dtm) 
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) 
dtm_d<- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 25 most frequent words
head(dtm_d, 25)

# Find associations
findAssocs(comments_dtm, terms = c("app","payment","easi","servic"), corlimit = 0.2)


#to generate world cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 20,
          max.words=80, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Sentiment Analysis, get sentences from the text field
mytext<-get_sentences(txt)
sentiment_by_sentence<-sentiment(mytext)
sentiment_by_tweet<-sentiment_by(mytext)
new<-cbind(txt,sentiment_by(mytext))

# to convert raw sentiment scores to predicted scores
new$pre_sentiment[new$ave_sentiment>0]<-1
new$pre_sentiment[new$ave_sentiment==0]<-0
new$pre_sentiment[new$ave_sentiment<0]<--1

# to view predicted scores for sentiment analysis using 'filter' function 
View(new)

# plot histogram of sentiment analysis scores
hist(new$ave_sentiment,main="During Covid-19", col ="blue")
write_xlsx(new,"C:/Users/user/Documents/ANL488/PaylahSAduringCovid19.xlsx", col_names = TRUE)








