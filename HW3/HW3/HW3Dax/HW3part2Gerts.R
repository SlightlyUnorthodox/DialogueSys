# Title: CIS4930 HW3 Part 2 Gerts
# Author: Dax Gerts
# Date: September 19, 2016

# Function definitions
setup <- function() {
    # Load required packages
    library(dplyr)
    library(plyr)
    library(ggplot2)
    library(ngramrr)
    library(NLP)
    library(openNLP)
    library(stringr)

    # Load local corpora
    local.corpora <- dir()[grep(".txt", dir())]
    local.names <- gsub(".txt", "", local.corpora)
    corpora <- list()

    for (i in 1:length(local.corpora)) {
        # Open connection to file
        con <- file(local.corpora[i])
        open(con)

        # Get lines from corpus
        lines.list <- list()
        current.line <- 1
        while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
            lines.list[current.line] <- as.character(line)[1]
            current.line <- current.line + 1
        }

        # Close connection to file
        close(con)

        # Collapse lines to usable data
        corpus <- paste(unlist(lines.list), collapse = " ", sep = "")

        # Add new corpus to corpora list
        corpora[local.names[i]] <- corpus
    }
 
    # Return list of corpora
    return(corpora)

}

normalize.corpora <- function(corpora) {
    # Apply the following operations for each corpus
    for (i in 1:length(corpora)) {
        # Select working corpus
        corpus <- corpora[i]

        # Remove special characters
        normalized.corpus <- gsub("[-#\\(\\)]", "", corpus)

        # Create unique tokens for periods
        normalized.corpus <- gsub("\\.", " . ", normalized.corpus)

        # Condense spacing
        normalized.corpus <- str_replace(gsub("\\s+", " ", str_trim(normalized.corpus)), "B", "b")

        # Bind normalized corpus back into corpora list
        corpora[names(corpora)[i]] <- normalized.corpus
    }

    # Return list of normalized corpora
    return(corpora)
}

create.ngrams <- function(corpora, size = 2) {
    # Ngrams list
    ngrams <- list()

    # Apply the following operations for each corpus
    for (i in 1:length(corpora)) {
        # Select working corpus
        corpus <- corpora[i]

        # Ngram form
        ngram.corpus <- ngramrr(corpus, ngmin = size, ngmax = size)

        # Bind ngrams to list
        ngrams[names(corpora)[i]] <- data.frame(ngram.corpus)
    }

    # Return list of corpora ngrams
    return(ngrams)
}

train.bigram <- function() {

}

computer.perplexity <- function() {

}

# Executed statements
corpora <- setup() # Get local corpora and load required libraries
corpora <- normalize.corpora(corpora) # Create normalized corpora columns
corpora.ngrams <- create.ngrams(corpora, size = 2) # Create bigrams for each corpus