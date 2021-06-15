library(stylo)  # needed for txt.to.words.ext
library(stringr) #str_replace_all 

library(tm) # need for corpus


ngramTwos   <- readRDS("ngramTwos01.RDS")
ngramThrees <- readRDS("ngramThrees01.RDS")
ngramFours  <- readRDS("ngramFours01.RDS")

#saveRDS(ngramTwos,"ngramTwos01.RDS")
#saveRDS(ngramThrees,"ngramThrees01.RDS")
#saveRDS(ngramFours,"ngramFours01.RDS")



cleanInput <- function(text){
        textInput <- tolower(text)
        textInput <- removePunctuation(textInput)
        textInput <- removeNumbers(textInput)
        textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
        textInput <- stripWhitespace(textInput)
        textInput <- txt.to.words.ext(textInput, corpus.lang ="English.all", preserve.case = TRUE)
        return(textInput)
}



predictNextWord <- function(input)
{
        
      
        #Cleaning the input
        wordInput <- cleanInput(input)
        #Getting the number of words in the input
        wordCount <- length(wordInput)
        #Initializing response
        prediction <- c()
        
        #Trimming input to the last three words
        if(wordCount>3)
        {
                textInput <- wordInput[(wordCount-2):wordCount] 
                
        }
        
        else if(wordCount==2) {
                textInput <- c(NA,wordInput)   
        }
        
        else {
                textInput <- c(NA,NA,wordInput)
        }
        
        wordPrediction <- as.character(ngramFours[ngramFours$word1==textInput[1] & 
                                                          ngramFours$word2==textInput[2] & 
                                                          ngramFours$word3==textInput[3],][1,]$word4)  #give me 1st row of ngramfours where words match exact input. and give me the word4 col
        
        if(is.na(wordPrediction)) {
                wordPrediction <- as.character(ngramFours[ngramFours$word2==textInput[2] & 
                                                ngramFours$word3==textInput[3],][1,]$word4) 
                
                #if(is.na(wordPrediction)) {
                 #       wordPrediction <- as.character(ngramFours[ngramFours$word1==textInput[1] & 
                                                                          #ngramFours$word3==textInput[3],][1,]$word4) 
                
                if(is.na(wordPrediction)) {
                        wordPrediction <- as.character(ngramFours[ngramFours$word1==textInput[2] & 
                                                        ngramFours$word2==textInput[3],][1,]$word3)
                        
                       
                 #3GRAM
        if(is.na(wordPrediction)) { 
                wordPrediction <- as.character(ngramThrees[ngramThrees$word1==textInput[2] & 
                                                                                  ngramThrees$word2==textInput[3],][1,]$word3) #3 and 2
                
                #if(is.na(wordPrediction)) {
                 #       wordPrediction <- as.character(ngramThrees[ngramThrees$word1==textInput[1] & 
                                                                      #     ngramThrees$word2==textInput[3],][1,]$word3) #3 and 1
                
                
        if(is.na(wordPrediction)) {
                wordPrediction <- as.character(ngramThrees[ngramThrees$word1==textInput[1] & 
                                                                    ngramThrees$word2==textInput[2],][1,]$word3)   #2 and 1
                
                if(is.na(wordPrediction)) {
                        wordPrediction <- as.character(ngramThrees[ngramThrees$word2==textInput[3],][1,]$word3)   
                        
                        if(is.na(wordPrediction)) {
                                wordPrediction <- as.character(ngramThrees[ngramThrees$word1==textInput[3],][1,]$word2)   
                #2GRAM              
                
                
                        if(is.na(wordPrediction)) {
                                wordPrediction <- as.character(ngramTwos[ngramTwos$word1==textInput[3],][1,]$word2)
                        
                      
                        
                }               
                }
        }
                }
                }
        }
                             print(wordPrediction)

                                }
                        }
                
        

