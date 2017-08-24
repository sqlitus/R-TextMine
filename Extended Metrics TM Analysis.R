################################################
# Text mining Extended Metrics CSV
# 8/23/2017
################################################



# import packages. Version control packages.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# ipak("checkpoint")
# checkpoint("2017-08-24")
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr"))



#### IMPORT & CLEAN DATA ####

# em <- read.csv("C:\\Work\\Analysis\\Text Mining\\sample ticket text mining dataset.csv")
em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics 2017 sample.csv")

# clean/transform / date calculations #
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")
em$Resolved_Date <- as.Date(em$Resolved_Date, "%m/%d/%Y")
em$Created_Week_Ending <- as.Date(em$Created_Week_Ending, "%m/%d/%Y")
em$Resolved_Week_Ending <- as.Date(em$Resolved_Week_Ending, "%m/%d/%Y")

# Created/Resolved Week / Month "variables" 
# em$Created_Week_R <- as.Date(cut(em$Created_Date, breaks = "week"))
# em$Created_Month_R <- as.Date(cut(em$Created_Date, breaks = "month"))
# em$Resolved_Week_R <- as.Date(cut(em$Resolved_Date, breaks = "week"))
# em$Resolved_Month_R <- as.Date(cut(em$Resolved_Date, breaks = "month"))


# grep / gsub / regex; further clean title
em$Title <- gsub("[^a-zA-Z0-9 ]"," ",em$Title)

# slice - just OnePOS Incidents, then filter out columns for analysis
em <- sqldf("select * from [em] where ONEPOS_LIST = 1")
exclude.columns <- c("Closed_date_cst","Created_Year","Resolved_Year", "Closed_Week","Closed_Week_Ending",
                     "L1_to_L2","L2_to_L3","L1_to_L3","L1_to_Hardware",
                     "CREATED_USER","Title_Region","Title_Location","First_Real_Assign_Date","ONEPOS_LIST"
                     ,"Time_to_Response__M_","Avg_Time_Per_Analyst__M_","Time_To_First_Assign"
                     ,"Open_To_Resolve_Time__M_", "multiple_resolve_flag", "NUM_DISTINCT_TEAMS", "NUM_DISTINCT_ASSIGNED",
                     "First_SG_Grouping", "Last_SG_Grouping")
em <- em[,-which(names(em) %in% exclude.columns)]

# write.csv(em, file = "em with transformations.csv", row.names = FALSE)



################# DTM LONG DATAFRAME #################

em.corpus <- VCorpus(DataframeSource(em), 
                   readerControl = list(reader = readTabular(mapping = 
                                                               list(content = "Title", id = "Id", Location = "Smart_Location"))))

em.dtm <- DocumentTermMatrix(em.corpus,
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          stripWhitespace = TRUE))

em.tidy.dtm <- tidy(em.dtm)
em.tidy.dtm$flag <- ifelse(em.tidy.dtm$count > 0, 1, 0)

# write.csv(em.tidy.dtm, file = "em tidy dtm.csv", row.names = FALSE)

# append reset of data to dtm. .Minor cleaning.
em.tidy.dtm.full <- sqldf("select [em.tidy.dtm].term
                          ,[em.tidy.dtm].flag
                          ,[em].*
                            from [em.tidy.dtm] join [em] on [em.tidy.dtm].document = [em].Id")



write.csv(em.tidy.dtm.full, file = "em tidy dtm full.csv", row.names = FALSE)


################# DTM WIDE DATAFRAME - APPEND DTM TO ORIGINAL DATAFRAME ################# 

# file size too big
# em.dtm.m <- as.matrix(em.dtm)
# em.plus <- cbind(em, em.dtm.m)
# write.csv(em.plus, file = "em plus.csv", row.names = FALSE)

#### End ####