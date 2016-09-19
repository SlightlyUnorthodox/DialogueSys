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
    library(tm)
    library(RWeka)

    # Set default precision
    options(digits = 9)

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
        corpus <- corpora[i][[1]]

        # Remove special characters
        normalized.corpus <- iconv(as.character(corpus), "latin1", "ASCII", sub = "")
        normalized.corpus <- gsub("[-#\\(\\)\";'`,]", " ", normalized.corpus)
        
        # Create unique tokens for periods
        normalized.corpus <- gsub("\\.", " . ", normalized.corpus)

        # Conver to lower case
        normalized.corpus <- tolower(normalized.corpus)

        # Condense spacing
        normalized.corpus <- str_replace(gsub("\\s+", " ", str_trim(normalized.corpus)), "B", "b")

        # Bind normalized corpus back into corpora list
        print(names(corpora)[i])
        corpora[[i]] <- normalized.corpus
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

train.model <- function(training.corpora, ngram.size = 2) {

    # Bind corpora together
    training.data <- vector(mode = "character")
    training.data.prior <- vector(mode = "character")

    # Create necessary ngrams
    corpora.ngrams <- create.ngrams(training.corpora, size = ngram.size) # Create ngrams for each corpus
    corpora.leading.ngrams <- create.ngrams(training.corpora, size = (ngram.size - 1)) # Create leading ngrams for each corpus

    for (i in 1:length(training.corpora)) {
        training.data <- c(training.data, as.character(unlist(corpora.ngrams[i][[1]])))
        training.data.prior <- c(training.data.prior, as.character(unlist(corpora.leading.ngrams[i][[1]])))
    }

    # Get ngram frequencies
    freq <- sort(table(training.data), decreasing = TRUE)
    freq.prior <- sort(table(training.data.prior), decreasing = TRUE)

    # Calculate total tokens
    total.tokens <- sum(freq)
    total.tokens.prior <- sum(freq.prior)

    # Create data frame
    training.data <- data.frame(freq)
    names(training.data) <- c("ngram", "frequency")

    # Add leading segment information (ngrams size - 1)
    training.data$prior <- gsub(" [A-z0-9:?,\\. ]*", "", as.character(training.data$ngram))
    training.data$frequency.prior <- freq.prior[training.data$prior]

    # Calculate relative frequency
    training.data$relative.frequency <- training.data$frequency/total.tokens
    training.data$relative.frequency.prior <- training.data$frequency.prior / total.tokens.prior

    # Return ngram model
    return(training.data)
}

compute.perplexity <- function(training.set, test.set) {

    # Test tokens
    test.tokens <- unlist(strsplit(test.set, split = " "))

    # Tokens predicted is the length of the test set minus one
    tokens.predicted <- length(test.tokens) - 1

    # Check prediction for each token and sum inverse probabilities
    perplex <- 1
    for (i in 1:tokens.predicted) {
        match <- training.set[grep(paste(test.tokens[i], test.tokens[i + 1]), training.set$ngram, fixed = TRUE),]
        if (dim(match)[1] > 1) {
            match <- match[1,]
        }

       # print(match)
        if (dim(match)[1] == 0) {
            prob <- 1
        } else {
            prob <- (0.1 + as.double(match$frequency.prior)) / (0.1 + as.double(match$frequency))
        }
        
        perplex <- as.double(perplex * as.double(prob))
    }


    # Take the n-th root of the sum (n being number of tokens predicted)
    perplex <- 100 * (perplex ^ (1/tokens.predicted))

    # Return perplexity value
    return(perplex)
}

# Executed statements
corpora <- setup() # Get local corpora and load required libraries
corpora <- normalize.corpora(corpora) # Create normalized corpora columns

# Create training sets for bigram model
lb.train <- train.model(corpora[c("ga.address", "inaug.speech")], ngram.size = 2)
mb.train <- train.model(corpora[c("prepared.to.die", "freedom.award")], ngram.size = 2)

# Establish test sets
lb.test <- corpora$inaug.speech.two
mb.test <- corpora$anc.rally

# Calculate perplexities
lb.perplexity <- compute.perplexity(lb.train, lb.test)
mb.perplexity <- compute.perplexity(mb.train, mb.test)

lb.cross.perplexity <- compute.perplexity(lb.train, mb.test)
mb.cross.perplexity <- compute.perplexity(mb.train, lb.test)

print(lb.perplexity)
print(mb.perplexity)

print(lb.cross.perplexity)
print(mb.cross.perplexity)