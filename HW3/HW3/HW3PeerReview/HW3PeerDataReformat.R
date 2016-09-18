# Load required libraries
library(RCurl)
library(stringr)
library(ngramrr)
library(wordcloud)
library(plyr)

# Link to text, 'dialog_script.txt' and convert to list of strings
con <- file('dialog_script.txt')
open(con)
lines.list <- list()
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    lines.list[current.line] <- as.character(line)[1]
    current.line <- current.line + 1
}
close(con)

# Initialize temporary vector for extracting data frame
temp <- vector(mode = "character")

for (i in 1:(length(lines.list) - 1)) {
    # Identify and set specific speaker
    if (is.na(strsplit(lines.list[i][[1]], split = " ")[[1]][1])) {
        dialg <- FALSE
    } else if (any("RIM" == strsplit(lines.list[i][[1]], split = " ")[[1]][1])) {
        speaker <- "RIM"
        dialg <- TRUE
    } else if (any("MYLA" == strsplit(lines.list[i][[1]], split = " ")[[1]][1])) {
        speaker <- "MYLA"
        dialg <- TRUE
    } else {
        dialg <- FALSE
    }

    # If speaker line, grab speaker's dialogue
    if (dialg == TRUE) {
        # Identify and set speaker's dialogue
        dialogue <- lines.list[i + 1][[1]]

        # Bind speaker and dialogue to dataframe
        temp <- rbind(temp, c(speaker, dialogue))
    }

}

# Create 'shared corpus' data frame
shared.corpus <- as.data.frame(temp)
names(shared.corpus) <- c("speaker", "dialogue")

# Create dialogue vector to be cleaned
dialogue <- shared.corpus$dialogue

# Clean dialogue strings
dialogue <- gsub("\\(.*?\\)", "\\1", dialogue) # Remove stage direction (in parentheses)
dialogue <- gsub("[\n]", " ", dialogue) # Remove newline characters
dialogue <- gsub("<i>(.*?)</i>", "", dialogue) # Remove italicized text (stage directions)
dialogue <- gsub("<img(.*?)>", "", dialogue) # Remove image tags
dialogue <- gsub(" \\.( *?)\\. \\.", "", dialogue) # Remove pauses
dialogue <- gsub("[-#\\(\\)]", "", dialogue) # Remove special characters
dialogue <- str_replace(gsub("\\s+", " ", str_trim(dialogue)), "B", "b") # Condense spacing
dialogue <- trimws(dialogue) # Remove leading/trailing whitespace

# Create normalized alternate dialogue (punctuation removed, all lowercase)
norm.dialogue <- gsub(pattern = "[[:punct:]]", "", dialogue)
norm.dialogue <- tolower(norm.dialogue)

# Get length of normalized dialogue utterances
length <- sapply(norm.dialogue, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

# Package data as data frame
shared.corpus <- data.frame(shared.corpus, norm.dialogue, length)

# Save output as csv file
write.csv(shared.corpus, "shared.corpus.csv", row.names = FALSE)