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






# -------------
# ONLINE EXAMPLE BELOW
# -------------


test <- v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE)
test
names(test) <- c("Words", "LenNorm")
names(test)
GetPrunedList <- function(wordfreqdf, prune_thru = 100) {
  #take only first n items in list
  tmp <- head(wordfreqdf, n = prune_thru) %>%
    select(ngrams = Words, tfidfXlength = LenNorm)
  #for each ngram in list:
  t <- (lapply(1:nrow(tmp), function(x) {
    #find overlap between ngram and all items in list (overlap = TRUE)
    idx <- overlap(tmp[x, "ngrams"], tmp$ngrams)
    #set overlap as false for itself and higher-scoring ngrams
    idx[1:x] <- FALSE
    idx
  }))
  
  #bind each ngram's overlap vector together to make a matrix
  t2 <- do.call(cbind, t)   
  
  #find rows(i.e. ngrams) that do not overlap with those below
  idx <- rowSums(t2) == 0
  pruned <- tmp[idx,]
  rownames(pruned) <- NULL
  pruned
}

#' overlap
#' OBJ: takes two ngrams (as strings) and to see if they overlap
#' INPUT: a,b ngrams as strings
#' OUTPUT: TRUE if overlap
overlap <- function(a, b) {
  max_overlap <- min(3, CountWords(a), CountWords(b))
  
  a.beg <- word(a, start = 1L, end = max_overlap)
  a.end <- word(a, start = -max_overlap, end = -1L)
  b.beg <- word(b, start = 1L, end = max_overlap)
  b.end <- word(b, start = -max_overlap, end = -1L)
  
  # b contains a's beginning
  w <- str_detect(b, coll(a.beg, TRUE))
  # b contains a's end
  x <- str_detect(b, coll(a.end, TRUE))
  # a contains b's beginning
  y <- str_detect(a, coll(b.beg, TRUE))
  # a contains b's end
  z <- str_detect(a, coll(b.end, TRUE))
  
  #return TRUE if any of above are true
  (w | x | y | z)
}


GetPrunedList(test)