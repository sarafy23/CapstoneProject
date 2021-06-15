library(tidytext) #needed for unnest_tokens
library(plyr) #needed for count function
library(dplyr)
library(data.table)
library(tm) # need for corpus
library("stringr") #needed for str_trim
library(lexicon) # needed for lexicon::profanity_banned


setwd("C:/Users/Notebook/Documents/R/Capstone/Capstone")
stopwords<-read.csv("stopwords.csv") #if want to add to this anything just to c(stopwords,"ENTER TEXT")

#load files
twitter<-readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)
news<-readLines("en_US.news.txt",  encoding = "UTF-8",skipNul=TRUE,warn = F)
blogs<-readLines("en_US.blogs.txt",  encoding = "UTF-8", skipNul=TRUE)

#must convert to  DF to use function sample_n
twitter <- data.frame(text = twitter)
news    <- data.frame(text = news)
blogs   <- data.frame(text = blogs)

set.seed(123345)
sampleSize = .01

sampleTwitter <- twitter %>% sample_n(nrow(twitter)*sampleSize,replace = F)
sampleNews <- news %>% sample_n(nrow(news)*sampleSize,replace=F)
sampleBlogs <- blogs %>% sample_n(nrow(blogs)*sampleSize,replace = F)

#aggregate sample files
sampleData <- bind_rows(sampleTwitter, sampleNews, sampleBlogs) 

#sampleData <-c(sampleTwitter,sampleNews,sampleBlogs)

#remove files to clear up memory
rm(twitter, news,blogs,sampleTwitter,sampleNews,sampleBlogs)

# remove all non-English characters from the sampled data
sampleData <- iconv(sampleData, "latin1", "ASCII", sub = "")


## Build the corpus and process it
corpus<-VCorpus(VectorSource(sampleData)) #convert to corpus in order to use tm_map
#df12<-Corpus(VectorSource(sampleData),readerControl=list(readPlain, language="en_US", load=TRUE))
df11 <- tm_map(corpus,   content_transformer(tolower))
changetospace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #create function to convert to blank spaces
df11 <- tm_map(df11, changetospace, "/|@|\\|") #convert listed symbols to blank spaces
df11 <- tm_map(df11, removeWords, stopwords("english")) #remove pointless words
profanity_list <- unique(lexicon::profanity_banned) #List containing profanity stop words
df11 <- tm_map(df11, removeWords, profanity_list,lazy=T) #remove profanity
df11 <- tm_map(df11, stemDocument)
df11 <- tm_map(df11, removePunctuation)
df11 <- tm_map(df11, removeNumbers)
df11 <- tm_map(df11, stripWhitespace)
finalCorpus <- tm_map(df11, PlainTextDocument)

#saveRDS(finalCorpus, file = "./finalCorpus1.rds")

#to clear memory
rm(df11)
rm(corpus)

# convert corpus to a dataframe which will be used to tokenize
library(textreg)

finalCorpusDF3 <- setDT(data.frame(text = sapply(finalCorpus, as.character), stringsAsFactors=F)) #conver to DF then to DT
rm(finalCorpus,sampleData)


