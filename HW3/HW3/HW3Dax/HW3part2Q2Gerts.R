# Title: CIS 4930 HW 3 part 2 Q2
# Author: Dax Gerts
# Date: September 18, 2016

# Load required packages
library(igraph)
library(plyr)

# Executed Statements

# Identify sequence
sequence <- unlist(strsplit("M,Q,A,S,S,S,S,S,S,S,S,S,Q,A,S,Q,A,S,ACK,Q,A,Q,Q,S,S,ACK,S,S,ACK,U,S,ACK,Q,A,S,S,S,S,S,Q,F,ACK,Q,Q,S,D,Q,U,Q,ACK,Q,A,S,M,Q,S,M,S,Q,S,A,U,ACK,S,S,Q,D,D,Q,S,S,Q,A,Q,M,S,ACK,D,S,S,SU,Q,S,S,S,S,S,Q,A,Q,A,S,Q,A,Q,Q,A,Q,ACK,S,S,U,ACK,D,F,Q,A,S,U", split = ","))

# Redefine sequence as data frame
state.sequence <- data.frame(sequence[1:(length(sequence) - 1)], sequence[2:length(sequence)])
names(state.sequence) <- c("current.step", "next.step")

# a) What are the state space and transition matrix for the model ?

# The state space
print(levels(state.sequence$current.step))

# The transition matrix
print(table(state.sequence))

# b) Draw a state diagram(nodes and directed edges) to accompany this model.

g <- graph.adjacency(as.matrix(prop.table(table(state.sequence))), weighted = TRUE)

# Created weighted nodes
temp <- data.frame(addmargins(table(state.sequence)))
temp <- temp[which(temp$current.step == "Sum" & temp$next.step != "Sum"), "Freq"]

node.size <- setNames(temp, c(names(V(g))))
tkplot(g, edge.arrow.size = 1, edge.width = round((E(g)$weight * 10) + 1), vertex.color = "gold", vertex.size = node.size * (5/2),
     vertex.frame.color = "gray", vertex.label.color = "black",
     vertex.label.cex = 0.8, vertex.label.dist = 2, edge.curved = 0)

# c) What is the highest transition probability of all ? Interpret this finding.

print(prop.table(table(state.sequence)))

# d) If you eliminate edges with less than 0.01 probability, how do you interpret the graph now ? Explain why you would or would not want to do this.

total <- length(state.sequence$current.step)
min.freq <- (108 * 0.1)
counts <- ddply(state.sequence, .(state.sequence$current.step, state.sequence$next.step), nrow)
names(counts) <- c("current.step", "next.step", "freq")

# Remove counts below minimum
counts <- counts[which(counts$freq >= min.freq),]
counts <- droplevels(counts)

# Present modified table
print(table(counts[,1:2]))