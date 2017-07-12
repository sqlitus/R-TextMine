################################################
# text mine by row of dataset - csv - dataframe
################################################

install.packages("tm")
library(tm)

# read from sharefile and clean.
file_loc <- "\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics.csv"
x <- read.csv(file_loc, header = TRUE)
colnames(x)[1] <- "id"
head(x)
str(x)
x$Title <- as.character(x$Title)
x.1 <- x[,c("id","Title")]


corp <- Corpus(DataframeSource(x.1))
dtm <- DocumentTermMatrix(corp)




################################################
# FACET EXTENDED METRICS
# 7/10/2017
################################################

library(ggplot2)
library(sqldf)

x.2 <- sqldf("select * from [x] where ONEPOS_LIST = 1") # not null

ggplot(data=x, aes(x=Created_Week, y=Open_To_Resolve_Time__M_))+
  # stat_summary(geom="line", fun.y="mean") +
  geom_line()+
  facet_wrap(facets=~LAST_SUPPORT_GROUP)+ # rows ~ columns
  ggtitle("Mean Time Restore Service")+
  xlab("Week")+
  ylab("Avg Min Restore Service")






################################################
# analyzing text documents by row workflow
# 7/11/2017
################################################


packages <- c("qdap","ggplot2")

download.or.load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])] 
  if(length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE) 
  sapply(pkg, require, character.only = TRUE) 
  }

library(qdap)
download.or.load(packages)

DATA 
dput(head(DATA))

freqs <- t(wfm(DATA$state, 1:nrow(DATA))) 
df.txt1 <- data.frame(DATA, freqs, check.names = FALSE) 


ords <- rev(sort(colSums(freqs)))[1:9] #top 9 words 
top9 <- freqs[, names(ords)] #grab those columns from freqs
data.frame(DATA, top9, check.names = FALSE) #put it together
