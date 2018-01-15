# Validations Data Analysis for JB/CH


# 1/12/2018 - Validations Data Analysis ----
library(readxl)
library(tidyverse)
library(tidytext)
library(stringr)

raw.data <- file.choose()
v.data <- readxl::read_excel(path = raw.data, sheet = "Data",trim_ws = TRUE)
ggplot(v.data, aes(Date)) + geom_bar()

# replace column names
names(v.data) <- str_replace_all(names(v.data), " |\\-", "\\.")

# unigrams
v.data %>% unnest_tokens(word, `POS.Readiness.Status`) %>% count(word, sort = TRUE)

# ngrams
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE)
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 3) %>% count(bigram, sort = TRUE)
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 4) %>% count(bigram, sort = TRUE)



# find most frequent phrases
# reference: https://stackoverflow.com/questions/36997147/understanding-anothers-text-mining-function-that-removes-similar-strings