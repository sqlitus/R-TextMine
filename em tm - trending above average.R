################################################
# Text mining - Extended Metrics CSV - Long DTM - Title
# finding average word trend rates per week - and top trending words
# 9/2/2017
# (REFACTORING)
################################################



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
       "wordcloud"))

startweek.2017 <- as.Date("2017-01-02")
last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))


#### IMPORT & CLEAN DATA ####

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics 2017.csv")
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")


#### SUBSETTING / FILTERING ####

em <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  select(Id, Created_Date, Created_Week, Smart_Region, Smart_Location, Title)
  

#### Gsub df - title (takes too long in corpus) ####

em$Title <- gsub("[^a-zA-Z0-9 ]"," ",em$Title)


### Corpus ####

em.corpus <- VCorpus(DataframeSource(em), 
                     readerControl = list(reader = readTabular(mapping = 
                                                                 list(content = "Title", 
                                                                      id = "Id", 
                                                                      Location = "Smart_Location"))))

# lowercase
em.corpus <- tm_map(em.corpus, content_transformer(tolower))

# remove words (takes a while for larger datasets)
em.corpus <- tm_map(em.corpus, content_transformer(removeWords), stopwords(kind = "SMART"))
word.blacklist <- c("r10", "lane", "bus", "date", "ncr", "eod", "run", "lanes", "reg")
em.corpus <- tm_map(em.corpus, content_transformer(removeWords), word.blacklist)

# Whitespace + Stemming
em.corpus <- tm_map(em.corpus, content_transformer(stripWhitespace))
em.corpus <- tm_map(em.corpus, content_transformer(stemDocument))

##### ! need to consolidate similar stemmed words ! ###

# em.corpus <- tm_map(em.corpus, content_transformer(toupper)) # not working after DTM
em.dtm <- DocumentTermMatrix(em.corpus,
                             control = list(removePunctuation = TRUE))


#### DTM - LONG DATAFRAME #### 

em.tidy.dtm <- tidy(em.dtm)

# final calculations & transformations
em.tidy.dtm$flag <- ifelse(em.tidy.dtm$count > 0, 1, 0)
em.tidy.dtm$term <- toupper(em.tidy.dtm$term)

# join dtm to orig dataframe
em.tidy.dtm.full <- sqldf("select [em.tidy.dtm].term
                            ,[em.tidy.dtm].flag
                            ,[em].*
                            from [em.tidy.dtm] join [em] on [em.tidy.dtm].document = [em].Id")


# calculate word averages by week; scientific notation off
options(scipen = 999)

em.tidy.dtm.full <- em.tidy.dtm.full %>%
    mutate(num.weeks = n_distinct(Created_Week)) %>%
  group_by(Created_Week, term) %>%
    mutate(word.week.total = n()) %>%
  group_by(term) %>%
    mutate(word.population.total = n()) %>%
    mutate(word.weekly.avg = n()/num.weeks) %>%
    mutate(word.trend = word.week.total / word.weekly.avg)


#### ANALYTICAL DATASETS FOR ANALYSIS & VISUALIZATION ####

# subset dataset
two.mondays.ago <- (Sys.Date() - 14) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
em.last.2.weeks <- em.tidy.dtm.full %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  filter(word.week.total >= 5) %>%
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
  
    # word cloud ^
    wordcloud.data <- top.10.words.trending.aa %>% 
      filter(Created_Week_Ending == max(top.10.words.trending.aa$Created_Week_Ending))
    wordcloud(wordcloud.data$term, wordcloud.data$word.week.total)
    wordcloud(top.10.words.trending.aa$term, top.10.words.trending.aa$word.week.total)  
    
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
