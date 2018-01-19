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
names(v.data) <- str_replace_all(names(v.data), "[[:space:]]|[[:punct:]]", "\\.")

# unigrams
v.data %>% unnest_tokens(word, `POS.Readiness.Status`) %>% count(word, sort = TRUE)

# ngrams
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE)
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 3) %>% count(bigram, sort = TRUE)
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 4) %>% count(bigram, sort = TRUE)



# find most frequent phrases
# reference: https://stackoverflow.com/questions/36997147/understanding-anothers-text-mining-function-that-removes-similar-strings

# define documents
# loop through ngrams

# helper function: dictionary: ngram & frequency
# dic1: list all ngrams
# dic2: count each
# combine
# remove overlaps by seeing if a is in b or vis-versa...

# analysis function: return list for each document...



MasterList <- function(df, text.col, top.x.ngrams = 20, ngram.limit = 4){
  
  all.ngrams <- data_frame()
  
  for (i in 1:ngram.limit){
    slice <- df %>% unnest_tokens_("xgram", text.col, token = "ngrams", n = i) %>% count(xgram, sort = TRUE) %>% head(top.x.ngrams)
    all.ngrams <- bind_rows(all.ngrams, slice)
  }
  
  all.ngrams
}

MasterList(v.data, "POS.Readiness.Status")
master <- MasterList(v.data, "POS.Readiness.Status")
View(master)



# Failed Validations IRs Created ----

fv.data <- readxl::read_excel(path = "C:\\Work\\zRequests\\John Bumgardner\\IRs Created for Failed Validations.xlsx", 
                              sheet = "data",trim_ws = TRUE)
str(fv.data)
names(fv.data)
ggplot(fv.data, aes(CreatedDate)) + geom_bar()

fv.data %>% unnest_tokens(word, Title) %>% count(word, sort = T)
fv.data %>% unnest_tokens(bigram, Title, token = "ngrams", n = 2) %>% count(bigram, sort = T)
fv.data %>% unnest_tokens(bigram, Title, token = "ngrams", n = 3) %>% count(bigram, sort = T)
fv.data %>% unnest_tokens(bigram, Title, token = "ngrams", n = 4) %>% count(bigram, sort = T) %>% distinct()
fv.data %>% unnest_tokens(bigram, Title, token = "ngrams", n = 5) %>% count(bigram, sort = T) %>% distinct()

# confirming if 3_.1.4 is 3 1/2
fv.data %>% filter(grepl("(?i)failed.*3.*1.4.2", fv.data$Title)) %>% View()
fv.data %>% filter(grepl("(?i)failed.*3.{0,8}1.1", fv.data$Title))
fv.data %>% filter(grepl("(?i)failed.*3.*1.3", fv.data$Title))
fv.data %>% filter(grepl("(?i)3\\.5.{0,8}hf4\\.2", fv.data$Title)) 
fv.data %>% filter(grepl("(?i)3\\.5.*hf4\\.2", fv.data$Title), grepl("(?i)failed.*3.*1.1", fv.data$Title)) %>% View()