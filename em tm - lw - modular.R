################################################
# Text mining - Extended Metrics CSV - Long DTM - Title - quick script for 1 week's analysis
# 9/2/2017
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
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr"))


#### IMPORT & CLEAN DATA (test out readlines) #### 

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Analysis\\SP KMO 2017 IRs.csv")
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")


#### SUBSETTING / FILTERING ####

# slice - just OnePOS Incidents, then filter out columns for analysis
em <- sqldf("select * from [em] where ONEPOS_LIST = 1")
keep.columns <- c("Id", 	"STATUS", 	"LAST_SUPPORT_GROUP", 	"Created_Date", 	"Priority", 	"Title", 	
                  "Description", 	"Smart_Region", 	"Smart_Location", "StoreNumber", 	
                  "LaneAffected", 	"Created_Week", "LAST_ASSIGNED", 	"Incident_Type")
em <- em[,which(names(em) %in% keep.columns)]



#### Gsub dataframe ####

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

# export analysis and stopword list (add choice of naming file)
### or add filename to the output ###
write.csv(em.tidy.dtm.full, 
          file = paste("Text Analysis - SP KMO - ", Sys.Date(), ".csv", sep = ""), 
          row.names = FALSE)


# End