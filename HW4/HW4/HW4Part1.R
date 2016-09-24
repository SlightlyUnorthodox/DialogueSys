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

# Q1. b) If you were planning to conduct research on these tagged data, would the kappa be sufficiently high for you to do so? Explain your reasoning. There is no absolute threshold that is “sufficiently high”, so you must contextualize your answer and give the reason(s) for it.

print("Q1. b)
Using the standards set by Landis and Koch, who stated that a kappa of 0.41 - 0.60 as 'moderate aggreement', it could be viewed that there is sufficient agreement to conduct research with these annotators. However this assessment is entirely contextual, with several other factors to consider. Primarily, it is known that kappa tends to score lower on sets with fewer codes, so it could be inferred that the kappa score is potentially underrating the aggreement between annotators. Further, the kappa was calculated on a confidence interval with alpha = 0.05, resulting in an interval of 0.29 to 0.71, the possible aggreement could be anywhere from 'slight' to 'substantial' . While a definitive answer cannot be provided, these results should be enough to proceed with research as long as the issues inherent in the problem are clearly addressed. ")

# Q1. c)

print("Q1. c) 
    We compute kappa in order to validate the use multiple annotators to discuss the same problems. If they have no aggreement, no inferences could be made from the combination or interaction of their results.
    
    Consequently, we use multiple annotators (rather than a single annotator) in order to account for the fact that no single annotator will ever give a definitive 'true' rendering of the data. We hope that by using multiple annotators, each with different methodologies for identifying codes, there will be sufficient cross-pollination to account for the potential codes that would be missed or by any single annotator.")