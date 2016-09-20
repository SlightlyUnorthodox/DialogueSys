import re
import nltk
from nltk.collocations import *

#open and read files
f = open('inaug.speech.two.txt')
ist = f.read()

#create tokens
tokens = nltk.word_tokenize(ist)

#create your bigrams
bgs = nltk.bigrams(tokens)

#compute frequency distribution for all the bigrams in the text
fdist = nltk.FreqDist(bgs)
for k,v in fdist.items():
    print k,v