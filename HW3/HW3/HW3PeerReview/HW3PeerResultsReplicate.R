# Function Definitions

## 0. Setup
setup <- function(data.file = "godot.corpus.csv", speaker.col = 'speaker', dialogue.col = 'norm.dialogue') {
    # Load required libraries
    library(dplyr)
    library(plyr)
    library(ggplot2)
    library(ngramrr)
    library(NLP)
    library(openNLP)

    # Load data extracted in hw1
    corpus.data <- read.csv(data.file, header = TRUE, sep = ",")

    if (data.file == "godot.corpus.csv") {
        # Remove rows with both speakers
        corpus.data <- corpus.data[which(corpus.data$speaker %in% c("ESTRAGON", "VLADIMIR")),]
        corpus.data <- droplevels(corpus.data)
        corpus.data <- corpus.data[which(complete.cases(corpus.data) == TRUE),]
    } else {
        # If using alternate data source, ensure standard column names exist
        corpus.data$speaker <- corpus.data[, speaker.col]
        corpus.data$norm.dialogue <- corpus.data[, dialogue.col]
        corpus.data$length <- sapply(corpus.data$norm.dialogue, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
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
    print(addmargins(contingency.table))

    # Save contingency table values for part b)
    table.data <- data.frame(addmargins(contingency.table))
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

    # Return word counts for use in question 4
    return(contingency.table)
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
    speaker.two.turns <- dim(corpus.data[which(corpus.data$speaker == "ESTRAGON"),])[1]
    follows <- 0

    for (i in 1:(total.turns - 1)) {
        if ((corpus.data[i, "speaker"] == speaker.one) & (corpus.data[(i + 1), "speaker"] == speaker.two)) {
            follows <- follows + 1
        }
    }

    prob.follow <- ((follows / total.turns) / (speaker.two.turns / total.turns))
    cat("The probability that", speaker.two, "will follow", speaker.one, "is equal to", prob.follow, "\n")

}

# 3. Descriptive Statistics
descriptive.stats <- function(corpus.data, speaker.one, speaker.two) {

    speaker.one.length <- corpus.data[which(corpus.data$speaker == speaker.one), "length"]
    speaker.two.length <- corpus.data[which(corpus.data$speaker == speaker.two), "length"]

    # a) What is the median number of words per utterance for Speaker 1 in your data? For speaker 2?
    speaker.one.median <- median(speaker.one.length)
    speaker.two.median <- median(speaker.two.length)

    cat("Speaker one,", speaker.one, ", spoke a median of", speaker.one.median, "words per utterance\n")
    cat("Speaker two,", speaker.two, ", spoke a median of", speaker.two.median, "words per utterance\n")

    # b) What is the mode number of words per utterance for Speaker 1 and Speaker 2 ?
    speaker.one.mode <- unique(speaker.one.length)
    speaker.one.mode <- speaker.one.mode[which.max(tabulate(match(speaker.one.length, speaker.one.mode)))]
    speaker.two.mode <- unique(speaker.two.length)
    speaker.two.mode <- speaker.two.mode[which.max(tabulate(match(speaker.two.length, speaker.two.mode)))]

    cat("Speaker one,", speaker.one, ", spoke a mode of", speaker.one.mode, "words per utterance\n")
    cat("Speaker two,", speaker.two, ", spoke a mode of", speaker.two.mode, "words per utterance\n")

    # c) What is the standard deviation for the number of words per utterance for Speaker 1 and Speaker 2 ?
    speaker.one.sd <- sd(speaker.one.length)
    speaker.two.sd <- sd(speaker.two.length)

    cat("Speaker one,", speaker.one, ", spoke with a standard deviation of", speaker.one.sd, "words per utterance\n")
    cat("Speaker two,", speaker.two, ", spoke with a standard deviation of", speaker.two.sd, "words per utterance\n")

    # d) Create a histogram of number of words per utterance for Speaker 1 and for Speaker 2. Provide the histogram in your writeup along with discussion and interpretation
    ggplot(corpus.data, aes(length, fill = speaker)) +
    geom_histogram(alpha = 0.5, bins = 20, position = 'identity') +
    xlim(0, 50) +
    xlab("# of Words per Utterance") +
    ylab("Frequency") +
    ggtitle("# of Words per Utterance by Speaker")

}

# 4. Hypothesis Testing
hypothesis.test <- function(corpus.data, speaker.one, speaker.two, corpus.table) {
    # e) Using the counts from question 1 above, perform a statistical test to determine whether presence of 
    #    <choose-your-word> depends on Speaker. Choose either a Chi-square test or a Fisher’s exact test based
    #    upon your sample size. Explain your choice and tell how you ran the test. Provide your results and interpret 
    #    them. Interpret the p-value. For this statistical test, your alternative hypothesis HA is “The probability 
    #    of <choose-your-word> occurring in an utterance is different conditioned upon who the speaker was.” 
    #    The null hypothesis H0 is “The probability of <choose-your-word> is not significantly different conditioned
    #    upon who the speaker was.”
    #    Note: The word chosen is "Godot" and Fisher's exact test was chosen because of it's accuracy with a small sample 
    #    Size and in the case of our dataset, we assumed 1200 utterances to be "small"

    # fisher's test
    if (min(godot.table) < 5) {
        print(fisher.test(corpus.table))
    }
    # Chi-square test
    else if (min(godot.table) >= 5) {
        print(chisq.test(corpus.table))
    }
    else {
        cat("Something has gone wrong, please analyze your data set.")
    }



    # f) You are given the following HA: Speaker 1 said more words in this corpus than Speaker 2. Based on your data, 
    #    can you test this hypothesis with a statistical test? If so, explain the test and present your findings. If 
    #    not, explain why.

    # No, you can't test this hypothesis via a statistical test, because you can simply count which speaker spoke more words in this corpus.

    # g) You are asked to determine whether Speaker 1’s utterances contained more words on average than Speaker 2’s 
    #    utterances. Write the corresponding alternative and null hypotheses, perform the appropriate statistical test 
    #    (explain why it is appropriate), and interpret your finding.

    speaker.one.lengths <- corpus.data[which(corpus.data$speaker == speaker.one), "length"]
    speaker.two.lengths <- corpus.data[which(corpus.data$speaker == speaker.two), "length"]
    print(t.test(speaker.one.lengths, speaker.two.lengths, alternative = "greater"))
}

# 5. Practice with Defintions
definition.practice <- function() {

    # Define strings of interest, given and normalized (punctuation removed, all lower-case)
    quote <- "Good night, good night! Parting is such sweet sorrow, that I shall say good night till it be morrow."
    normalized.quote <- "good night good night parting is such sweet sorrow that i shall say good night till it be morrow"

    # a) How many tokens are in this quote? (not including punctuation)
    tokens <- ngramrr(normalized.quote, ngmin = 1, ngmax = 1)
    cat("a) There are", length(tokens), "tokens in this quote. They are as follows:\n", tokens)

    # b) How many distinct word types are in this quote?
    word.types <- unique(tokens)
    cat("b) There are", word.types, "distinct word types in this quote. They are as follows:\n", word.types)

    # c) Identify a verb in the quote which, when stemmed, remains unchanged.
    cat("c) A verb that will remain unchanged when stemmed is, 'is'.\n")

    # d) Identify a verb in the quote that is an example of inflectional morphology. Explain by providing its stem and noting how it is inflected.
    cat("d) 'Parting' is an example of inflectional morphology, it stems from 'to part', it is inflected in the present tense with the suffix, '-ing'. \n")

    # e) Identify a verb that is an example of derivational morphology (hint: the same verb appears twice in the above quote, in two different forms). Tell which one is the derived form.
    cat("e) The example of derivational morphology are the verbs 'is' and 'be'. The origin verb is 'be', and 'is' is the third person singular indicative of 'be'. \n")


}

# Executed Statements
set.speaker.one <- "MYLA"
set.speaker.two <- "RIM"
godot.data <- setup(data.file = "shared.corpus.csv") # Load data from local source
godot.table <- contingency.table(corpus.data = godot.data, word = "you", speaker.one = set.speaker.one, speaker.two = set.speaker.two) # Complete part 1) of HW2
sequence.analysis(corpus.data = godot.data, speaker.one = set.speaker.one, speaker.two = set.speaker.two) # Complete part 2) of HW2
descriptive.stats(corpus.data = godot.data, speaker.one = set.speaker.one, speaker.two = set.speaker.two) # Complete part 3) of HW2
hypothesis.test(corpus.data = godot.data, speaker.one = set.speaker.one, speaker.two = set.speaker.two, corpus.table = godot.table) # Complete part 4) of HW2
definition.practice() # Complete part 5) of HW2
