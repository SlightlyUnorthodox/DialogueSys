# HW03
# Christine Moore
# Due September 19th, 2016

useLibraries <- function(){
  # figure out which libraries I'll need
  library(openNLP)
}

loadData <- function(){
  # pull the text files
  mandelaCorpus <- dir()[grep(".txt", dir())]
  lincolnCorpus <- dir()[grep(".txt", dir())]
  
  # clean up the names
  manCorpus <- gsub(".txt", "", local.corpora)
  linCorpus <- gsub(".txt", "", local.corpora)
}
  

makeNgrams <- function(specificCorpus, num){
    # make blank list to hold all the ngrams that were calculated.
    ngramList <- list();

    # work with Mandela's first 
    numberCorpuses = num
    for (corpusIter in 1:length(specificCorpus)) {
      currCorpus <- specificCorpus[corpusIter]
      
      # call ngram function
      # double check the parameters
      createdNgrams <- ngramrr(currCorpus, ngmin = numberCorpuses, ngmax = numberCorpuses)
      
      # create a data fram from the ngrams created
      ngramDF <- data.frame(createdNgrams)
      
      # add them into our list of total ngrams for mandela
      ngramList[names(specificCorpus)[corpusIter]] <- ngramDF
    }
    return(ngramList)
}
  
