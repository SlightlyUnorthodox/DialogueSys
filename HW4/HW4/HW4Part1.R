# Title: CIS4930 HW4 Part 1
# Author: Dax Gerts
# Date: September 26, 2016

## Function definitions

setup <- function() {

    # Load required packages
    library(psych)
    #library(dplyr)
    #library(plyr)
    #library(ggplot2)
    #library(ngramrr)
    #library(NLP)
    #library(openNLP)
    #library(stringr)
    #library(tm)
    #library(RWeka)

    # Set default precision
    options(digits = 9)

}

## Executed statements

# Run setup function
setup()

# Define annotators data
annotator.one <- c('MODIFIER ',
 'NONE ',
 'NONE ',
 'CATEGORY ',
 'NONE ',
 'MODIFIER ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'CATEGORY ',
 'NONE ',
 'NAME ',
 'CATEGORY ',
 'NONE ',
 'CATEGORY ',
 'NAME ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'NONE ',
 'VAR_TYPE ',
 'NONE ',
 'NONE ',
 'MODIFIER ',
 'NONE ',
 'MODIFIER '
)
annotator.two <- c('NAME',
 'NONE',
 'NONE',
 'CATEGORY',
 'NONE',
 'NAME',
 'CATEGORY',
 'NONE',
 'NONE',
 'NONE',
 'CATEGORY',
 'NONE',
 'NAME',
 'CATEGORY',
 'NONE',
 'NAME',
 'NAME',
 'NONE',
 'NONE',
 'NONE',
 'CATEGORY',
 'NONE',
 'NONE',
 'CATEGORY',
 'NONE',
 'CATEGORY',
 'VAR_TYPE',
 'MODIFIER',
 'NONE',
 'NAME',
 'NONE',
 'CATEGORY')
annotators <- data.frame(annotator.one, annotator.two)

# Show contingency table
table(annotators)

## Q1. a) What is the kappa between the two annotators?

# Calculate cohen's kappa
kappa <- cohen.kappa(table(annotators))
print(kappa)

# Exact kappa estimate
print(kappa$kappa)

# 2. 