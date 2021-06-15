
library(dplyr) #need for operator
library(tidyr) #need for separate
library(tokenizers) #need for tokenize_ngrams


#create ngrams

#create function that tokenizes, tabluates (freq count), turns it to dataframe, then ranks them based on freq count
ngramTokenizer <- function(theCorpus, ngramCount) {
        
        ngramFunction  <- tokenize_ngrams(sapply(theCorpus, as.character), n=ngramCount, n_min=ngramCount )  #tokenize
        ngramFunction  <-  table(unlist(ngramFunction)) #tabulate
        ngramFunction  <- data.frame(ngramFunction) # turn to dataframe
        ngramFunction <-  ngramFunction[order(ngramFunction$Freq,decreasing = TRUE),]  #rank by freq
        ngramFunction
}

#create ngrams using the function above
#unigram<-ngramTokenizer(finalCorpus,1)



ngramTwo <-ngramTokenizer(finalCorpusDF3,2)  #same as if you had done ngramTwo2 <-ngramTokenizer(finalCorpus,2) where finalCorpus is file that was not converted to DF 
ngramThree <-ngramTokenizer(finalCorpusDF3,3)
ngramFour<-ngramTokenizer(finalCorpusDF3,4)

#ngramFive<-ngramTokenizer(finalCorpusDF3,5)


#' ## Separate words
ngramTwos <-  ngramTwo %>% separate(Var1 , c("word1", "word2"), sep = " ")
ngramThrees <- ngramThree %>% separate(Var1 , c("word1", "word2", "word3"), sep = " ")
ngramFours<- ngramFour %>% separate(Var1 , c("word1", "word2", "word3", "word4"), sep = " ")




#ngramFives<- ngramFour %>% separate(Var1 , c("word1", "word2", "word3", "word4","word5"), sep = " ")


