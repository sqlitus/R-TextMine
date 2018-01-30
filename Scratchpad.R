# Scratchpad


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
v.data %>% unnest_tokens(word, `POS.Readiness.Status`) %>% count(word, sort = TRUE) %>% View()

# bigrams
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE)

# filtered bigrams vs unfiltered
test <- v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>%
  mutate(bigram.copy = bigram) %>%
  separate(bigram, c("word1","word2"), sep = " ")

test %>% count(word1, word2, sort = T)
test %>% count(bigram.copy, sort = T)


# 1/19/2018 test clipboard & text parsing ----

a <- readClipboard()
a
str(a)
grep("\\t", a) # metacharacters left in when data read from clipboard
a.t <- data_frame(text = a)
str(a.t)
a1 <- a.t %>% unnest_tokens(output = word, input = text) %>% count(word, sort = TRUE)
a2 <- a.t %>% unnest_tokens(output = word, input = text, format = c("text")) %>% count(word, sort = TRUE)
identical(a1, a2)

# custom words parser (n = 1 word) without filtering out single letter words
a3 <- a.t %>% unnest_tokens(word, text, token = "regex", pattern = "[ ]+") %>% count(word, sort = TRUE)
a3
a3$word[grep("(?i)source", a3$word)]

# r.1: replace entire cell w/ value; r.2: custom ngrams w/ filtering out metadata
a4 <- a.t %>% unnest_tokens(word, text, token = "regex", pattern = "[ ]+[^[:space:]]+[ ]+[^[:space:]]+") %>%
  mutate(word.r = replace(word, str_detect(word, "[[:cntrl:]]"), "")) %>%
  mutate(word.r.2 = str_replace_all(word, "[[:cntrl:]]", ""))
  count(word, sort = TRUE)
a4
View(a4)

# 1/29/2018 power point w/ ggplot & annotations ----
library(tidyverse)
library(officer)
library(rvg) # graphics into the ppt

setwd("C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017")
getwd()

myslide <- read_pptx("C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017\\01_31_2018 refresh.pptx")
layout_summary(myslide)
layout_properties(myslide)
slide_summary(myslide)

myslide %>%
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with_text(type = "title", str = "here is some Title text") %>%
    ph_with_vg(type = "body", code = print(aloha.top.down.p)) %>%
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with_text(type = "title", str = "phwithvgat") %>%
    ph_with_vg_at(code = print(aloha.top.down.p), left = .17, top = 1.2, width = 13, height = 6)

print(myslide, target = "myNewPowerPoint.pptx")

