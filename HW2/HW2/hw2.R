# Function Definitions

## 0. Setup
setup <- function(data.file = "godot.corpus.csv", speaker.col = 'speaker', dialogue.col = 'norm.dialogue') {
    # Load required libraries
    library(dplyr)
    library(plyr)
    library(ngramrr)
    library(NLP)
    #library(openNLP)

    # Load data extracted in hw1
    corpus.data <- read.csv(data.file, header = TRUE, sep = ",")

    if (data.file == "godot.corpus.csv") {
        # Remove rows with both speakers
        corpus.data <- godot.data[which(corpus.data$speaker %in% c("ESTRAGON", "VLADIMIR")),]
        corpus.data <- droplevels(corpus.data)
        corpus.data <- corpus.data[which(complete.cases(corpus.data) == TRUE),]
    } else {
        # If using alternate data source, ensure standard column names exist
        corpus.data$speaker <- corpus.data[, speaker.col]
        corpus.data$norm.dialogue <- corpus.data[, dialogue.col]
    }

    return(corpus.data)
}

## 1. Contingency Tables
contingency.table <- function(corpus.data, word, speaker.one, speaker.two) {

    # a)

    # To count 'utterances containin [word], we create a column that evaluates to TRUE if containing [word] and FALSE if not
    corpus.data$contains.word <- ifelse(grepl(word, corpus.data$norm.dialogue), TRUE, FALSE)

    # Create contingency table
    contingency.table <- table(corpus.data$speaker, corpus.data$contains.word)
    contingency.table <- addmargins(contingency.table)
    print(contingency.table)

    # Save contingency table values for part b)
    table.data <- data.frame(contingency.table)
    names(table.data) <- c("speaker", "contains.word", "freq")

    # b)

    # Extract row/column totals
    table.total <- table.data[which(table.data$speaker == "Sum" & table.data$contains.word == "Sum"), "freq"]
    speaker.one.total <- table.data[which(table.data$speaker == speaker.one & table.data$contains.word == "Sum"), "freq"]
    speaker.two.total <- table.data[which(table.data$speaker == speaker.two & table.data$contains.word == "Sum"), "freq"]
    contains.total <- table.data[which(table.data$speaker == "Sum" & table.data$contains.word == TRUE), "freq"]
    does.not.contain.total <- table.data[which(table.data$speaker == "Sum" & table.data$contains.word == FALSE), "freq"]

    # Calculate frequently used probabilities
    prob.contains <- (contains.total / table.total)
    prob.speaker.one <- (speaker.one.total / table.total)
    prob.speaker.two <- (speaker.two.total / table.total)

    # i. P(u is from Speaker 1)

    prob.i <- (prob.speaker.one)
    cat("\ni. P(u is from Speaker 1) =", prob.i, "\n")

    # ii. P(u is from Speaker 2)

    prob.ii <- (prob.speaker.two)
    cat("ii. P(u is from Speaker 2) =", prob.ii, "\n")

    # iii. P(u contains < choose - your - word > | u is from Speaker 1)

    prob.iii <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == TRUE), "freq"] / speaker.one.total))
    cat("iii. P(u contains < chose - your - word > | u is from Speaker 1) =", prob.iii, "\n")

    # iv. P(u contains < choose - your - word > | u is from Speaker 2)

    prob.iv <- ((table.data[which(table.data$speaker == speaker.two & table.data$contains.word == TRUE), "freq"] / speaker.two.total))
    cat("iv. P(u contains < choose - your - word > | u is from Speaker 2) =", prob.iv, "\n")

    # v. P(u being from Speaker 1 | u contains < choose - your - word > )

    prob.v <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == TRUE), "freq"] / contains.total))
    cat("v. P(u being from Speaker 1 | u contains < choose - your - word > ) =", prob.v, "\n")

    # vi. P(u being from Speaker 1 | u does not contain < choose - your - word > )

    prob.vi <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == FALSE), "freq"] / does.not.contain.total))
    cat("vi. P(u being from Speaker 1 | does not contain < choose - your - word >) =", prob.vi, "\n")

    # vii. P(u being from Speaker 2 | u contains < choose - your - word > )

    prob.vii <- ((table.data[which(table.data$speaker == speaker.two & table.data$contains.word == TRUE), "freq"] / contains.total))
    cat("vii. P(u being from Speaker 2 | u contains < choose - your - word >) =", prob.vii, "\n")

}

# 2. Sequence Analysis
sequence.analysis <- function(corpus.data, speaker.one, speaker.two) {

    # a) What is the most common token in your corpus ? The most common token for Speaker 1 ? Speaker 2 ?
    all.token.freq <- plyr::count(ngramrr(corpus.data$norm.dialogue, ngmin = 1, ngmax = 1))
    all.token.freq <- all.token.freq[order( - all.token.freq$freq),]

    speaker.one.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.one),]$norm.dialogue, ngmin = 1, ngmax = 1))
    speaker.one.token.freq <- speaker.one.token.freq[order( - speaker.one.token.freq$freq),]

    speaker.two.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.two),]$norm.dialogue, ngmin = 1, ngmax = 1))
    speaker.two.token.freq <- speaker.two.token.freq[order( - speaker.two.token.freq$freq),]

    cat("The most common token in", substitute(corpus.data), "is [", as.character(all.token.freq$x[1]), "]\n")
    cat("The most common token for", speaker.one, "is [", as.character(speaker.one.token.freq$x[1]), "]\n")
    cat("The most common token for", speaker.two, "is [", as.character(speaker.two.token.freq$x[1]), "]\n")

    # b) What is the most common bigram in your corpus ?
    all.token.freq <- plyr::count(ngramrr(corpus.data$norm.dialogue, ngmin = 2, ngmax = 2))
    all.token.freq <- all.token.freq[order( - all.token.freq$freq),]

    speaker.one.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.one),]$norm.dialogue, ngmin = 2, ngmax = 2))
    speaker.one.token.freq <- speaker.one.token.freq[order( - speaker.one.token.freq$freq),]

    speaker.two.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.two),]$norm.dialogue, ngmin = 2, ngmax = 2))
    speaker.two.token.freq <- speaker.two.token.freq[order( - speaker.two.token.freq$freq),]

    cat("The most common bigram in", substitute(corpus.data), "is [", as.character(all.token.freq$x[1]), "]\n")
    cat("The most common bigram for", speaker.one, "is [", as.character(speaker.one.token.freq$x[1]), "]\n")
    cat("The most common bigram for", speaker.two, "is [", as.character(speaker.two.token.freq$x[1]), "]\n")


    # c) What is the most common trigram ?
    all.token.freq <- plyr::count(ngramrr(corpus.data$norm.dialogue, ngmin = 3, ngmax = 3))
    all.token.freq <- all.token.freq[order( - all.token.freq$freq),]

    speaker.one.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.one),]$norm.dialogue, ngmin = 3, ngmax = 3))
    speaker.one.token.freq <- speaker.one.token.freq[order( - speaker.one.token.freq$freq),]

    speaker.two.token.freq <- plyr::count(ngramrr(corpus.data[which(corpus.data$speaker == speaker.two),]$norm.dialogue, ngmin = 3, ngmax = 3))
    speaker.two.token.freq <- speaker.two.token.freq[order( - speaker.two.token.freq$freq),]

    cat("The most common trigram in", substitute(corpus.data), "is [", as.character(all.token.freq$x[1]), "]\n")
    cat("The most common trigram for", speaker.one, "is [", as.character(speaker.one.token.freq$x[1]), "]\n")
    cat("The most common trigram for", speaker.two, "is [", as.character(speaker.two.token.freq$x[1]), "]\n")
    
    # d) From your corpus, given that Speaker 1 just finished a dialogue turn, what is the probability that the next dialogue turn will be Speaker 2 ?
    total.turns <- dim(corpus.data)[1]
    speaker.two.turns <- table.data[which(table.data$speaker == speaker.two & table.data$contains.word == "Sum"), "freq"]
    follows <- 0

    for (i in 1:(total.turns - 1)) {
        if ((corpus.data[i, "speaker"] == speaker.one) & (corpus.data[(i + 1), "speaker"] == speaker.two)) {
            follows <- follows + 1
        }
    }

    prob.follow <- ((follows/total.turns)/(speaker.two.turns/total.turns))
    cat("The probability that", speaker.two, "will follow", speaker.one, "is equal to", prob.follow, "\n")

}

# 3. Descriptive Statistics
descriptive.stats <- function() {

}

# 4. Hypothesis Testing
hypothesis.test <- function() {

}

# 5. Practice with Defintions
definition.practice <- function() {
    quote <- "Good night, good night! Parting is such sweet sorrow, that I shall say good night till it be morrow."

    # a) How many tokens are in this quote? (not including punctuation)

    # b)

    # c)

    # d)

    # e)
}

# Executed Statements
godot.data <- setup() # Load data from local source
contingency.table(corpus.data = godot.data, word = "godot", speaker.one = "ESTRAGON", speaker.two = "VLADIMIR") # Complete part 1) of HW2
sequence.analysis(corpus.data = godot.data, speaker.one = "ESTRAGON", speaker.two = "VLADIMIR") # Complare part 2) of HW2
