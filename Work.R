################################################
# Import Extended Metrics
# 7/13/2017 - Plot Weekly or Monthly Totals
################################################

#### LOG ####
  function(further_analysis_tasks){
    # Text mining
      # em tm - lightweight - change to last 2 weeks, plot words, color code words to see diff
      # IRs trending higher than their typical average
    
    # Stores IRs Lanes
      # scatterplot all stores, by IRs created / Lanes / IRs Lane. Shiny/markdown/plotly
  }

function(TM_PERSONALNOTES){
  # smarter import 
  # choose file
  # smart 1st column -> strip weird characters (substr(ï..))
  # choose what columns you want?
  # maybe convert all column names to lowercase
  # more consistent factoring...
  
  # smarter processing
  # gsub on corpus - not before
  # separate scripts -> source scripts for packages, import, processing (blacklist etc), etc
  # consolidate similar words!!!
  
  # choose export name + location
  
}
#### DATA - read em from sharefile, clean, transform, calculate columns ####
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

#### end data ####

#### 9/13/2017 - ggplotly hover over info ####

function(ggplotly_REFERENCE){ref = "http://www.r-graph-gallery.com/2017/06/07/get-the-best-from-ggplotly/"}

p <- stores.2017 %>% ggplot(aes(IRs.Lane.Week, IRs.Week, color = Region, size = Num.Lanes))+geom_point()
ggplotly(p)

mytext=paste("Region: ", stores.2017$Region, "\n" , "Store: ", stores.2017$Store, "\n", 
             "IRs.Lane.Week = ", stores.2017$IRs.Lane.Week, "\n","IRs.Week = ", stores.2017$IRs.Week, sep="")    
pp=plotly_build(p)   
style( pp, text=mytext, hoverinfo = "text", traces = c(1, 2, 3) )



#### 9/13/2017 - word stemming 2.0 - custom list of words to group together ####



# stemmed words list -> to combine to common term
test.words <- c("freeze","freezes","freezing","froze","frozed","frozen")
stemDocument(test.words)

freeze.synonyms <- list(
  list(word="freeze issues", syns=c("freez","froze","frozen"))
)

replaceSynonymsFreeze <- content_transformer(function(x,syn=NULL){
  Reduce(function(a,b){
    gsub(paste0("\\b(", paste(b$syns, collapse = "|"),")\\b"), b$word, a)
  }, syn, x)
})

test.tm <- Corpus(VectorSource(test.words))
test.tm <- tm_map(test.tm, stemDocument)
test.tm <- tm_map(test.tm, replaceSynonymsFreeze, freeze.synonyms)
inspect(test.tm)


# excellent stemming reference - including dictionary word corrections
# https://stackoverflow.com/questions/24443388/stemming-with-r-text-analysis




#### 9/13/2017 - file.choose; IRs/Store/Lane dataset  ####

filename <- file.choose()
stores.2017 <- read.csv(filename, header = TRUE)
### NOTE: NA REGION BEING READ AS "NA" NONVALUE. EITHER STRINGS.AS.FACTORS = F OR CUSTOM CONVERT NULLS
filename
basename(filename)
dirname(filename)

names(stores.2017)[3] <- "Count.IRs"
names(stores.2017)[4] <- "Region"
names(stores.2017)[6] <- "Num.Lanes"

#### 9/13/2017 - ggplot pt3 - Scatterplot stores by IRs/Lane ... ####
stores.2017 %>% ggplot(aes(IRs.Week, Num.Lanes))+geom_point()

# color palette for region
region.colors <- rainbow(length(levels(stores.2017$Region)))
names(region.colors) <- levels(stores.2017$Region)

# multivariate scatter plot
stores.2017 %>% ggplot(aes(IRs.Lane.Week, IRs.Week, color = Region, size = Num.Lanes))+geom_point()

# summary info
stores.summary <- stores.2017 %>%
  group_by(Region) %>%
  summarise(a.ilw = mean(IRs.Lane.Week)) %>%
  arrange(desc(a.ilw))

stores.miss.region <- stores.2017 %>%
  filter(is.na(Region) == T)



#### dynamic week formula ####

function(MORE_IMPROVEMENTS){
  
  # r formula to always get last business week
  last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
  last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))
  
  # FILENAME = Text Analysis - [min] to [max]
  paste("Text Analysis - ", last.monday, " to ", last.sunday, sep = "")
}
function(ggplot_references){
  # https://stackoverflow.com/questions/33613385/sort-bar-chart-by-sum-of-values-in-ggplot
  # stacked ordered bar - https://stackoverflow.com/questions/21596906/ggplot-how-to-limit-output-in-bar-plot-so-only-most-frequent-occurrences-are-sh
  
}


#### em tm plotting - sorting and top 10 ####


ggplot(data=em.tidy.dtm.full, aes(x=reorder(term, flag, function(x){sum(x)}), y=flag)) +
  geom_bar(stat="summary", fun.y=sum) +
  coord_flip()

ggplot(data=em.tidy.dtm.full, aes(x=reorder(term, flag, sum), y=flag)) +
  geom_bar(stat="summary", fun.y=sum) #+
# coord_flip()


# aggregate 1 col grouped by another
aggregate(em.tidy.dtm.full$flag, by = list(em.tidy.dtm.full$term), FUN = sum)

aggregate(em.tidy.dtm.full$flag, list(em.tidy.dtm.full$term), sum)


# plot top x words; ggplot order
topwords <- sqldf("select term, count(*) as 'count' from [em.tidy.dtm.full] 
                  group by term order by count(*) desc limit 20")
ggplot(topwords, aes(reorder(term, -count), count))+geom_bar(stat = 'identity')





#### stopword list ####

full.word.exclusion.list <- c(word.blacklist, stopwords(kind = "SMART"))
write.csv(full.word.exclusion.list, 
          file = paste("stopwords - ", Sys.Date(), ".csv", sep = ""), 
          row.names = FALSE)







#### ggplot Part 1 - Basic Bar Plots; Colors ####
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


#### ggplot Part 2 - Stacked / Grouped bars ####
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




