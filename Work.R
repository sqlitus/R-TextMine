################################################
# Import Extended Metrics
# 7/13/2017 - Plot Weekly or Monthly Totals
################################################

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak("checkpoint")
checkpoint("2017-07-12")
packages <- c("ggplot2", "tm", "sqldf", "qdap", "scales")
ipak(packages)


# read from sharefile, clean, transform, calculate columns
file_loc <- "C:\\Users\\2066074\\Documents\\Extended Metrics 2017.csv"
x <- read.csv(file_loc, header = TRUE)

# clean/transform
colnames(x)[1] <- "id"
x$Title <- as.character(x$Title)
x$Created_Date <- as.Date(x$Created_Date, "%m/%d/%Y")
x$Resolved_Date <- as.Date(x$Resolved_Date, "%m/%d/%Y")

# Created/Resolved Week / Month "variables" 
x$Created_Week_R <- as.Date(cut(x$Created_Date, breaks = "week"))
x$Created_Month_R <- as.Date(cut(x$Created_Date, breaks = "month"))
x$Resolved_Week_R <- as.Date(cut(x$Resolved_Date, breaks = "week"))
x$Resolved_Month_R <- as.Date(cut(x$Resolved_Date, breaks = "month"))

# month figure for coloring
x$created_month <- month


# slice - just OnePOS Incidents
x <- sqldf("select * from [x] where ONEPOS_LIST = 1")

function(not.working.date_format){
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+ 
  stat_summary(fun.y = "sum", geom = "bar")+
  scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")
}




#### Part 1 - Basic Bar Plots; Colors ####
# Count
ggplot(x, aes(Created_Month_R))+
  geom_bar()

# Sum
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")

# Avg
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "mean", geom = "bar")


# test - label intervals
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")+
  scale_x_date(date_breaks = "2 month")


## Custom colors

# border / fill
ggplot(x, aes(Created_Month_R))+
  geom_bar(color = "blue", fill = rgb(.1,.4,.5,.3))

# hue
ggplot(x, aes(Created_Month_R))+ # fill = as.factor(Created_Month_R)
  geom_bar()+scale_fill_hue(c = 11)


#### Part 2 - Stacked / Grouped bars ####
# Stacked Bar
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar()

# Stacked Percent
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "fill")

# Grouped Bar
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "dodge")

# Add RcolorBrewer
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "fill") + scale_fill_brewer(palette = "Pastel1")

# Faceting
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar()+facet_wrap(~Last_SG_Grouping)



# test - plot avg resolve time by month, facet by support group
ggplot(x, aes(x = Created_Month_R, y = Open_To_Resolve_Time__M_, color = Created_Month_R))+
  stat_summary(fun.y = "mean", geom = "bar")+
  facet_wrap(~Last_SG_Grouping)



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
ipak(packages)


DATA 
dput(head(DATA))

freqs <- t(wfm(DATA$state, 1:nrow(DATA))) 
# Data frame with all word counts
df.txt1 <- data.frame(DATA, freqs, check.names = FALSE) 

# Data frame with top 9 word counts
ords <- rev(sort(colSums(freqs)))[1:9] #top 9 words 
top9 <- freqs[, names(ords)] #grab those columns from freqs
DATA.top9 <- data.frame(DATA, top9, check.names = FALSE) #put it together




# word count by row attempt 1
function(firstattempt){
# turning word count columns into flag columns
df2 <- df.txt1

df2 <- as.data.frame(sapply(df2[-c(1:5)],function(i){
  ifelse(i>0,1,0)}))

# Data frame with all words & flags
df2 <- cbind(df.txt1[1:5],df2)

df2.ords <- rev(sort(colSums(df2[-c(1:5)])))[1:15] #top 9 words BY ROW (IR) OCCURENCE
df2.top15 <- df2[,names(df2.ords)] #grab columns names of the "top 15"
df2.wf <- data.frame(df2, df2.top15, check.names = F)
} 


#########
# EXTENDED METRICS WORD COUNT BY ROW - TITLE
# need to strip whitespace, etc.
# Reference: R text mining doc & https://stackoverflow.com/questions/30900229/performing-text-analytics-on-a-text-column-in-dataframe-in-r
#########

em <- x

em$Title <- tolower(em$Title)
# em$Title <- tm::removeNumbers(em$Title)
em$Title <- tm::removeWords()

em$Title

em.freqs <- t(wfm(em$Title, 1:nrow(em)))
