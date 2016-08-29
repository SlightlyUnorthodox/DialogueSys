# Load required libraries
library(RCurl)
library(stringr)

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

# Package data as data frame
godot.corpus <- data.frame(speaker, dialogue)

# 'How many dialogue turns did each interlocutor make?'
summary(godot.corpus$speaker)

# 'How many total words did each interlocutor say?'


# 'How many words per turn on average did each interlocutor make? (You may segment words based on white space and punctuation. There is no one perfect way to do it; just explain how you did it.)'


# 'What is the average length of word that each interlocutor made?'


# 'If there are other fun thingss you want to compute about this data, go for it.'