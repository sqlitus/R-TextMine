################################################
# Import Extended Metrics
# 7/13/2017 - Plot Weekly or Monthly Totals
################################################

install.packages("tm")
library(tm)
library(ggplot2)
library(scales)

# read from sharefile, clean, transform, calculate columns
file_loc <- "\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics.csv"
x <- read.csv(file_loc, header = TRUE)

# clean/transform
colnames(x)[1] <- "id"
x$Title <- as.character(x$Title)
x$Created_Date <- as.Date(x$Created_Date, "%m/%d/%Y")
x$Resolved_Date <- as.Date(x$Resolved_Date, "%m/%d/%Y")

# Create Week / Month "variables" 
x$Created_Week_R <- as.Date(cut(x$Created_Date, breaks = "week"))
x$Created_Month_R <- as.Date(cut(x$Created_Date, breaks = "month"))

ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+ 
  stat_summary(fun.y = sum, geom = "bar")+
  scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")



# Count
ggplot(x, aes(Created_Month_R))+
  geom_bar()

# Sum
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")

# Avg
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "mean", geom = "bar")


ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")+
  scale_x_date(date_breaks = "4 month")

# text mine as corpus
# x.1 <- x[,c("id","Title")]
# corp <- Corpus(DataframeSource(x.1))
# dtm <- DocumentTermMatrix(corp)




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


