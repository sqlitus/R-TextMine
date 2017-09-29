################################################
# Text mining - Extended Metrics CSV - TIDY TEXT
# word counts & trending up words for OnePOS, Aloha 
# 9/28/2017
# (REFACTORING EM TM - TRENDING ABOVE AVERAGE)
################################################

setwd("\\\\cewp1650\\Chris Jabr Reports\\Text Analysis")

#### Packages & Version Control ####

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# ipak("checkpoint")
# checkpoint("2017-08-24")
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","dplyr","stringr", "plotly","tidyverse",
       "wordcloud", "SnowballC"))



#### Global Variables ####

startweek.2017 <- as.Date("2017-01-02")
last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))
this.monday <- Sys.Date() + (1 - as.integer(format(Sys.Date(), format = "%u")))



#### IMPORT & CLEAN DATA ####

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics 2017.csv")
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")
em$Created_Week_Ending <- as.Date(em$Created_Week_Ending, "%m/%d/%Y")



#### SUBSET - ALL 2017 - ONEPOS IRS ####

num.weeks <- as.integer((this.monday - startweek.2017)/7)
stopwords.169 <- stop_words %>% filter(lexicon == "snowball", !word %in% c("down","above","below","again", "after"))

em.tidy <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  select(Id, Created_Date, Created_Week, Created_Week_Ending, Smart_Region, Smart_Location, Title, LAST_SUPPORT_GROUP)


# tidy text - distinct UNIGRAMS per IR w/ calcs, maybe stem/remove stopwords
em.tidy.unigrams <- em.tidy %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.169$word)
  distinct() %>%
  group_by(Created_Week_Ending, word.stem) %>%
    mutate(word.week.total = n()) %>%
  group_by(word.stem) %>%
    mutate(word.population.total = n()) %>%
  ungroup() %>%
    mutate(word.weekly.avg = word.week.total/num.weeks, 
           word.weekly.trend = word.week.total / word.weekly.avg) %>%
  ungroup()

# distinct BIGRAMS per IR w/ calcs - leaving in stopwords - stemming not working well
em.tidy.bigrams <- em.tidy %>%
  unnest_tokens(bigram, Title, token = "ngrams", n = 2, drop = FALSE) %>%
  distinct()


## unigrams & bigrams for aloha ...???



# scientific notation off
options(scipen = 999)




#### ANALYTICAL DATASETS FOR ANALYSIS & VISUALIZATION ####

## dataset - summary view ##
top.n.words.dataset <- em.tidy.words.all %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(20, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words summary view
top.n.words.dataset.p <- top.n.words.dataset %>%
  ggplot(aes(reorder(ord.term, wordorder), n, fill = term, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  # facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words - 9/27") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()

# save plot
top.n.words.dataset.p
write.csv(em,
          file = paste("IRs - 9-27", Sys.Date(), ".csv", sep = ""),
          row.names = FALSE)



## subset dataset - last 2 weeks ##
two.mondays.ago <- (Sys.Date() - 14) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
em.last.2.weeks <- em.tidy.words.all %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  # filter(word.week.total >= 5) %>%   ## filter out uncommon words?
  arrange(desc(word.trend)) %>%
  mutate(Created_Week_Ending = Created_Date + ( 7 - as.integer(format(Created_Date, format = "%u"))))

# analysis - top 10 words by count each week
top.10.words.weekly.count <- em.last.2.weeks %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words by count each week
top.10.words.weekly.count.p <- top.10.words.weekly.count %>%
  ggplot(aes(reorder(ord.term, wordorder), n, fill = term, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words by Week") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()

# save plot
top.10.words.weekly.count.p
ggsave(paste0("plot - Most Common Words - ", Sys.Date(), ".png"), width = 13, height = 6, units = ("in"))


### need to define this w/ Melissa & others ###
# analysis - top 10 words by trending above average 
top.10.words.trending.aa <- em.last.2.weeks %>%
  group_by(Created_Week_Ending, term) %>%
  summarize(word.trend = mean(word.trend), word.weekly.avg = mean(word.weekly.avg),
            word.week.total = mean(word.week.total), word.population.total = mean(word.population.total),
            word.population.total = mean(word.population.total), num.weeks = mean(num.weeks)) %>%
  arrange(desc(word.trend)) %>%
  top_n(10, wt = word.trend) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words by trending above average
top.10.words.trending.aa.p <- top.10.words.trending.aa %>% 
  ggplot(aes(reorder(ord.term, wordorder), word.trend, fill = term, label = round(word.trend,0)))+
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary to hav diff words 
  labs(x = "Word", y = "Spike in Frequency", title = "Words Spiking in Frequency") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()
top.10.words.trending.aa.p

# word cloud ^ ---- should make this for a month
wordcloud.data <- top.10.words.trending.aa %>% 
  filter(Created_Week_Ending == max(top.10.words.trending.aa$Created_Week_Ending))
wordcloud(wordcloud.data$term, wordcloud.data$word.week.total)
wordcloud(top.10.words.trending.aa$term, top.10.words.trending.aa$word.week.total)  





## Aloha Analysis Dataset ##
em.aloha.last.2.weeks <- em.tidy.words.all %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  arrange(desc(word.trend)) %>%
  mutate(Created_Week_Ending = Created_Date + ( 7 - as.integer(format(Created_Date, format = "%u"))))

# analysis - top 10 words by count each week
top.10.words.weekly.count <- em.last.2.weeks %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())   




# setwd("\\\\cewp1650\\Chris Jabr Reports\\Text Analysis")
# write.csv(em.last.2.weeks, 
#           file = paste("last 2 weeks - ", Sys.Date(), ".csv", sep = ""), 
#           row.names = FALSE)



#tidytext equivalent...
# consolidate stems
# mutate(word = if_else(word %in% c("emailing", "emails"), "email", word))
# test str replace ..
# handle *MSR from MSR ... ... regex gsub keeping certain punct.....

# plot - case when word is repeated, BOLD or something.
# outline color thicker based on freq of word...size, scale didn't work, ggplot aes vs geom aes...
