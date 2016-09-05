# Function Definitions

## 0. Setup
setup <- function(data.file = "godot.corpus.csv") {
    # Load required libraries
    library(dplyr)
    library(ngramrr)
    library(NLP)
    #library(openNLP)

    # Load data extracted in hw1
    corpus.data <- read.csv(data.file, header = TRUE, sep = ",")

    if (data.file == "godot.corpus.csv") {
        # Remove rows with both speakers
        corpus.data <- godot.data[which(corpus.data$speaker %in% c("ESTRAGON", "VLADIMIR")),]
        corpus.data <- droplevels(corpus.data)
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
    prob.contains <- (contains.total/table.total)
    prob.speaker.one <- (speaker.one.total/table.total)
    prob.speaker.two <- (speaker.two.total / table.total)

    # i. P(u is from Speaker 1)

    prob.i <- (prob.speaker.one)
    cat("\nP(u is from Speaker 1) =", prob.i, "\n")

    # ii. P(u is from Speaker 2)

    prob.ii <- (prob.speaker.two)
    cat("P(u is from Speaker 2) =", prob.ii, "\n")

    # iii. P(u contains < choose - your - word > | u is from Speaker 1)

    prob.iii <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == TRUE), "freq"]/speaker.one.total))
    cat("P(u contains < chose - your - word > | u is from Speaker 1) =", prob.iii, "\n")

    # iv. P(u contains < choose - your - word > | u is from Speaker 2)

    prob.iv <- ((table.data[which(table.data$speaker == speaker.two & table.data$contains.word == TRUE), "freq"]/speaker.two.total))
    cat("P(u contains < choose - your - word > | u is from Speaker 2) =", prob.iv, "\n")

    # v. P(u being from Speaker 1 | u contains < choose - your - word > )

    prob.v <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == TRUE), "freq"] / contains.total))
    cat("P(u being from Speaker 1 | u contains < choose - your - word > ) =", prob.v, "\n")

    # vi. P(u being from Speaker 1 | u does not contain < choose - your - word > )

    prob.vi <- ((table.data[which(table.data$speaker == speaker.one & table.data$contains.word == FALSE), "freq"] / does.not.contain.total))
    cat("P(u being from Speaker 1 | does not contain < choose - your - word >) =", prob.vi, "\n")

    # vii. P(u being from Speaker 2 | u contains < choose - your - word > )

    prob.vii <- ((table.data[which(table.data$speaker == speaker.two & table.data$contains.word == TRUE), "freq"] / contains.total))
    cat("P(u being from Speaker 2 | u contains < choose - your - word >) =", prob.vii, "\n")

}

# 2. Sequence Analysis
sequence.analysis <- function() {

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

