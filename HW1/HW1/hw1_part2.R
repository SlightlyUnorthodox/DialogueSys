# Load required libraries
library(RCurl)
library(stringr)
library(ngramrr)
library(wordcloud)
library(plyr)

# Link to text of 'Waiting for Godot'
link <- "http://www.lib.ru/PXESY/BEKETT/godo_engl.txt"

# Extract content from linked page
doc.html <- getURL(link)

# Parse and format text into potential rows
rows <- strsplit(doc.html, split = "<b>")

# Initialize dialogue object
speaker <- vector()
dialogue <- vector()
interlocutor <- TRUE # Bit for removing third-party dialogue

# Remove lines without "Estragon" or "Vladimir" as speakers and in which they respond to a third party
for (i in 1:length(rows[[1]])) {
    # If the speaker is Estragon or Vladimir continue
    if (grepl("(ESTRAGON)|(VLADIMIR)", rows[[1]][i])) {
        # If either is responding to the other, save the line
        if (interlocutor == TRUE) {
            speaker <- rbind(speaker, strsplit(rows[[1]][i], split = ":</b>")[[1]][1])
            dialogue <- rbind(dialogue, strsplit(rows[[1]][i], split = ":</b>\n")[[1]][2])
            # If either is responding to a third party, don't save, but flip 'interlocutor' bit
        } else {
            interlocutor = TRUE
        }
        # If speaker is neither Estragon or Vladimir, don't save the line and flip 'interlocutor' bit
    } else {
        interlocutor = FALSE
    }
}

# Clean dialogue strings
dialogue <- gsub("\\(.*?\\)", "\\1", dialogue) # Remove stage direction (in parentheses)
dialogue <- gsub("[\n]", " ", dialogue) # Remove newline characters
dialogue <- gsub("<i>(.*?)</i>", "", dialogue) # Remove italicized text (stage directions)
dialogue <- gsub("<img(.*?)>", "", dialogue) # Remove image tags
dialogue <- gsub(" \\.( *?)\\. \\.", "", dialogue) # Remove pauses
dialogue <- gsub("[-#\\(\\)]", "", dialogue) # Remove special characters
dialogue <- str_replace(gsub("\\s+", " ", str_trim(dialogue)), "B", "b") # Condense spacing
dialogue <- trimws(dialogue) # Remove leading/trailing whitespace

# Fix last line
dialogue[length(dialogue)] <- strsplit(dialogue[length(dialogue)], split = "Top Act 1")[[1]][1]

# Create normalized alternate dialogue (punctuation removed, all lowercase)
norm.dialogue <- gsub(pattern = "[[:punct:]]", "", dialogue)
norm.dialogue <- tolower(norm.dialogue)

# Package data as data frame
godot.corpus <- data.frame(speaker, dialogue, norm.dialogue)

# 'How many dialogue turns did each interlocutor make?'
estragon.turns <- length(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$speaker)
vladimir.turns <- length(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$speaker)

print(sprintf("Estragon took %d turns", estragon.turns))
print(sprintf("Vladimir took %d turns", vladimir.turns))

# 'How many total words did each interlocutor say?'
estragon.words <- length(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))
vladimir.words <- length(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))

estragon.unique.words <- length(unique(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1)))
vladimir.unique.words <- length(unique(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1)))

print(sprintf("Estragon spoke %d words and %d unique words", estragon.words, estragon.unique.words))
print(sprintf("Vladimir spoke %d words and %d unique words", vladimir.words, vladimir.unique.words))

# 'How many words per turn on average did each interlocutor make? (You may segment words based on white space and punctuation. There is no one perfect way to do it; just explain how you did it.)'
godot.corpus$length <- sapply(godot.corpus$norm.dialogue, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
estragon.avg.words <- mean(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")), "length"])
vladimir.avg.words <- mean(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")), "length"])

print(sprintf("Estragon averaged %f words per turn", estragon.avg.words))
print(sprintf("Vladimir averaged %f words per turn", vladimir.avg.words))

# 'What is the average length of word that each interlocutor made?'
estragon.avg.length <- mean(nchar(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1)))
vladimir.avg.length <- mean(nchar(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1)))

estragon.avg.unique.length <- mean(nchar(unique(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))))
vladimir.avg.unique.length <- mean(nchar(unique(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))))

print(sprintf("Estragon average word length is %f characters and average unique word length is %f characters", estragon.avg.length, estragon.avg.unique.length))
print(sprintf("Vladimir average word length is %f characters and average unique word length is %f characters", vladimir.avg.length, vladimir.avg.unique.length))

# 'If there are other fun things you want to compute about this data, go for it.'

# Frequency count of words overall word usage
all.word.freq <- plyr::count(ngramrr(godot.corpus$norm.dialogue, ngmin = 1, ngmax = 1))
all.word.freq <- all.word.freq[order( - all.word.freq$freq),]

wordcloud(head(all.word.freq$x, 50), head(all.word.freq$freq, 50), color = brewer.pal(9, "PuBu"))

# Comparative word freq
estragon.word.freq <- plyr::count(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("ESTRAGON", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))
estragon.word.freq <- estragon.word.freq[order( - estragon.word.freq$freq),]

vladimir.word.freq <- plyr::count(ngramrr(godot.corpus[which(godot.corpus$speaker %in% c("VLADIMIR", "VLADIMIR and ESTRAGON")),]$norm.dialogue, ngmin = 1, ngmax = 1))
vladimir.word.freq <- vladimir.word.freq[order( - vladimir.word.freq$freq),]

print(head(as.character(all.word.freq$x), 10))
print(head(as.character(estragon.word.freq$x), 10))
print(head(as.character(vladimir.word.freq$x), 10))