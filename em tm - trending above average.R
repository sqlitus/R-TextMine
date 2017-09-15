################################################
# Text mining - Extended Metrics CSV - Long DTM - Title
# finding average word trend rates per week - and top trending words
# 9/2/2017
# (REFACTORING)
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
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","dplyr"))

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
  filter(Created_Date >= last.monday & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
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

# remove words
em.corpus <- tm_map(em.corpus, content_transformer(removeWords), stopwords(kind = "SMART"))
word.blacklist <- c("r10", "lane", "bus", "date", "ncr", "eod", "run", "lanes", "reg")
em.corpus <- tm_map(em.corpus, content_transformer(removeWords), word.blacklist)

# Whitespace + Stemming
em.corpus <- tm_map(em.corpus, content_transformer(stripWhitespace))
em.corpus <- tm_map(em.corpus, content_transformer(stemDocument))

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
