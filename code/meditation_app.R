###

# load packages

library(itunesr)
library(tidyverse)
library(highcharter)
library(lubridate)
library(sentimentr)
library(DataExplorer)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(textdata)
library(wesanderson)
library(tm)
library(radarchart)
library(data.table)
library(spacyr)
library(igraph)
library(ggraph)
library(widyr)

###

# We will analyze this meditation apps: Headspace, Calm, Insight Timer, Oak, Meditation studio, Shine

headspace_id <- c(493145008)
calm_id <- c(571800810)
insight_id <- c(337472899)
oak_id <- c(1210209691)
medstudio_id <- c(1066018502)
shine_id <- c(1293721589)



####

#number of review pages

#write the function
read_app_review <- function(id,country,name){
  # Each page has 50 reviews between each page and we want to load 500 reviews
  # so we will factor out the number of page we need to achieve that
  no_ratings <- 500
  no_reviews_per_page <- 49
  ratings <- no_ratings/no_reviews_per_page

  # Round up page & set variables for second and last page
  ratings <- floor(ratings) #round up to 10
  reviewStartPage <- 2
  reviewEndPage <- ratings

  df <- getReviews(id,country,1)

  # headspace
  for (i in reviewStartPage:reviewEndPage){
    df <- rbind(df,getReviews(id,country,i))
  }

  df$app_name <- name
  df$Date <- as.Date(df$Date)
  df$Rating <- as.numeric(df$Rating)    

  return(df)

}

#apply function to extract all apps reviews
headspace_review <- read_app_review(headspace_id,c('us'),c('headspace'))
calm_review <- read_app_review(calm_id,c('us'),c('calm'))
insight_review <- read_app_review(insight_id,c('us'),c('insight_timer'))
oak_review <- read_app_review(oak_id,c('us'),c('oak'))
medstudio_review <- read_app_review(medstudio_id,c('us'),c('meditation_studio'))
shine_review <- read_app_review(shine_id,c('us'),c('shine'))

#combine together
app_review <- rbind(headspace_review,calm_review,insight_review,oak_review,medstudio_review,shine_review)

#explore
plot_str(app_review)


#Number of review distribution by dates
qplot(headspace_review$Date,main='Headspace reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')
qplot(calm_review$Date,main='Calm reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')
qplot(insight_review$Date,main='Insight Timer reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')
qplot(oak_review$Date,main='Oak reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')
qplot(medstudio_review$Date,main='Meditation Studio reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')
qplot(shine_review$Date,main='Shine reviews distribution by dates',ylab = 'Counts', xlab = 'Dates')

#Review distribution 
qplot(headspace_review$Rating,main='Headspace reviews distribution',ylab = 'Reviews', xlab = 'Dates')
qplot(calm_review$Rating,main='Calm reviews distribution by',ylab = 'Reviews', xlab = 'Dates')
qplot(insight_review$Rating,main='Insight Timer reviews distribution',ylab = 'Reviews', xlab = 'Dates')
qplot(oak_review$Rating,main='Oak reviews distribution',ylab = 'Reviews', xlab = 'Dates')
qplot(medstudio_review$Rating,main='Meditation Studio reviews distribution',ylab = 'Reviews', xlab = 'Dates')
qplot(shine_review$Rating,main='Shine reviews distribution',ylab = 'Reviews', xlab = 'Dates')

#Ratings Trend

headspace_trend <- app_review %>% filter(app_name=='headspace') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='headspace')
calm_trend <- app_review %>% filter(app_name=='calm') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='calm')
insight_trend <- app_review %>% filter(app_name=='insight_timer') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='insight_timer')
oak_trend <- app_review %>% filter(app_name=='oak') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='oak')
medstudio_trend <- app_review %>% filter(app_name=='meditation_studio') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='meditation_studio')
shine_trend <- app_review %>% filter(app_name=='shine') %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2)) %>% mutate(app_name='shine')

review_trend <- rbind(headspace_trend,calm_trend,insight_trend,oak_trend,medstudio_trend,shine_trend)
review_trend <- review_trend %>% spread(app_name,Rating)

#interactive chart
highchart() %>% hc_add_series_times_values(headspace_trend$Date,headspace_trend$Rating, name = 'Average Rating')  %>%  hc_title(text = "Headspace review trend</b>",
                                                                                                                           margin = 20, align = "center",
                                                                                                                           style = list(useHTML = TRUE)) 
highchart() %>% hc_add_series_times_values(calm_trend$Date,calm_trend$Rating, name = 'Average Rating') %>%  hc_title(text = "Calm review trend</b>",
                                                                                                                     margin = 20, align = "center",
                                                                                                                     style = list(useHTML = TRUE)) 
highchart() %>% hc_add_series_times_values(insight_trend$Date,insight_trend$Rating, name = 'Average Rating')%>%  hc_title(text = "Insight Timer review trend</b>",
                                                                                                                          margin = 20, align = "center",
                                                                                                                          style = list(useHTML = TRUE)) 
highchart() %>% hc_add_series_times_values(oak_trend$Date,oak_trend$Rating, name = 'Average Rating')%>%  hc_title(text = "Oak review trend</b>",
                                                                                                                  margin = 20, align = "center",
                                                                                                                  style = list(useHTML = TRUE)) 
highchart() %>% hc_add_series_times_values(medstudio_trend$Date,medstudio_trend$Rating, name = 'Average Rating')%>%  hc_title(text = "Meditation Studio review trend</b>",
                                                                                                                              margin = 20, align = "center",
                                                                                                                              style = list(useHTML = TRUE)) 
highchart() %>% hc_add_series_times_values(shine_trend$Date,shine_trend$Rating, name = 'Average Rating')%>%  hc_title(text = "Shine review trend</b>",
                                                                                                                      margin = 20, align = "center",
                                                                                                                      style = list(useHTML = TRUE)) 
###

# rating according to each month


### Calculate average ratings for each day and change column names
rating_month <- app_review %>% group_by(Date) %>% summarise(mean_date=mean(Rating)) 

# Convert dates to day of  month
rating_month$Date <- unclass(as.POSIXlt(rating_month$Date))$mday

# Split Day of month and avg. rating to separate columns 
rating_month <- rating_month %>% group_by(Date) %>% summarise(mean_day = mean(mean_date))


# Round Ratings to 1 digit
rating_month$mean_day <- 
  round(rating_month$mean_day, digits = 2)
# Plot mean ratings for each day of month
ggplot(rating_month, aes(x = Date, y = mean_day, label = mean_day,angle = 90, hjust = -0.25))+
  geom_bar(fill = "#FF8C00", stat = "identity")+
  theme_bw() +
  geom_text(position = 'identity', stat = 'identity', size = 4, vjust = -0.4)+
  labs(title="Average ratings by day of month", size=60) +
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0)) +
  labs(x="Day of Month", y="Avg. Rating")+
  scale_x_discrete(limits=rating_month$Date)+
  scale_y_continuous(limits = c(0,5))


###

#average rating per app version


### Average ratings per App version


### headspace

version_mean <- function(df){

  version <- df %>% group_by(App_Version) %>% summarise(mean_rating=round(mean(Rating),2)) %>% arrange(desc(App_Version))
  version$App_Version <- factor(version$App_Version)
  
  return(version)
}

# Plot average ratings for each app version
version_plot <- function(df){
  ggplot(df, aes(x = App_Version, y = mean_rating, label=mean_rating)) +
    geom_bar(fill = '#FFCCCB', stat = "identity")+
    geom_text(position = 'identity', stat = 'identity', size = 4, angle=90, vjust = -0.4)+
    
    theme_bw() +
    labs(title="Average ratings for each App Version", size=60) +
    labs(x="App Version", y="Avg. Rating") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


#headspace
headspace_version <- version_mean(headspace_review)
version_plot(headspace_version)



### calm

calm_version <- version_mean(calm_review)
version_plot(calm_version)


### insight timer

insight_version <- version_mean(insight_review)
version_plot(insight_version)


### oak

oak_version <- version_mean(oak_review)
version_plot(oak_version)


### meditation studio

medstudio_version <- version_mean(medstudio_review)
version_plot(medstudio_version)


### shine
shine_version <- version_mean(shine_review)
version_plot(shine_version)


###

#Review length

### Count length of reviews and create a sorted df_App_allMarkets$Review <- as.character(df_App_allMarkets$Review)
app_review$Review <- as.character(app_review$Review)
app_review$ReviewLength <- nchar(app_review$Review)

# Count the average review lenght for each market
app_review_length <- app_review %>% group_by(app_name) %>% summarise(mean_length = round(mean(ReviewLength),2))
app_review_length1 <- merge(app_review,app_review_length, by = "app_name")

#visualiztion
ggplot(data=app_review_length1, aes(x=ReviewLength)) + 
  geom_density(aes(y = ..count..), color="#1F3161", fill = "#68E193", alpha=0.6) +
  geom_vline(aes(xintercept = app_review_length1$mean_length), linetype = "dashed", size = 0.5)+
  
  facet_wrap(~app_name, scales = 'free')+
  geom_text(data=app_review_length1, mapping=aes(x=mean_length, y=2, label=mean_length), check_overlap = TRUE, size=5, angle=0, vjust=1, hjust=-0.5)+
  ylim(0,5)+
  xlim(5,600)+
  theme_minimal()+
  labs(title="Review Character Length", subtitle = "The average length per review for each market", x="Review Length", y="")+
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Circular Std", color="black", face="bold", size=12)) +
  theme(plot.subtitle = element_text(family = "Helvetica", color="black", face="plain", size=14))+
  theme(strip.text = element_text(face="bold", size=12))  +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


###


### User feeling for each app
# Create a new df base on the app_name
app_review1 <- data.frame("Date" = app_review$Date, "Rating" = app_review$Rating, "AppVersion" = app_review$App_Version, "app_name" = app_review$app_name)
# Convert ratings to vector and replace ratings with text
app_review1$Rating <- as.character(app_review1$Rating)
# Replace 1-2 star ratings with text Negative, and 4-5 stars with text Positive, 3 stars with Neutral
app_review1$Rating[app_review1$Rating == '1'] <- 'Negative'
app_review1$Rating[app_review1$Rating == '2']  <- 'Negative'
app_review1$Rating[app_review1$Rating == '3']  <- 'Neutral'
app_review1$Rating[app_review1$Rating == '4']  <- 'Positive'
app_review1$Rating[app_review1$Rating == '5']  <- 'Positive'


# Plot user feelings for each app
ggplot(app_review1, aes(Rating, group = app_name)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), size = 4, stat= "count", vjust = -0.4) +
  theme_bw() +
  theme(legend.position="none")+
  scale_fill_manual("Ratings", values = c("1" = "#ED5540", "2" = "#F9D71C","3" = "#68E194"))+
  labs(y = "Rating", fill="Rating") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  ylab("relative frequencies") +
  xlab("Procent") +  labs(title="User feeling per app", x="Reviews", y="Amount")+
  labs(caption = "(Negative = 1-2 stars, Neutral = 3, Positive = 4-5 stars)")+
  facet_wrap(~app_name, scales = 'free_x')



###

# aggregated feeling per weekday

rating_week <- app_review1
rating_week$Date <- format(as.Date(rating_week$Date), '%A')
ggplot(rating_week, aes(x = as.factor(Date), fill = Rating, label = Rating)) +
  geom_bar(stat = "count")+
  theme_bw() +
  scale_fill_manual("Ratings", values = c("Positive" = "#68E194", "Neutral" = "#F9D71C", "Negative" = "#ED5540"))+
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0)) +
  ylab("relative frequencies")+
  xlab("Procent")+ 
  labs(title="User rating per weekday", x="Weekday", y="Ratings")+
  labs(caption = "(Negative = 1-2 stars, Neutral = 3, Positive = 4-5 stars)")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))



###


#NLP


tidy_review <- function(df){
  # Create a new data frame with only words
  reviews <- as.vector(df$Review)
  reviews <- data_frame(line = 1:nrow(df), text = reviews)

  # Split reviews to individual words - "Tokenization"
  tidy_df <- reviews %>% unnest_tokens(word, text)

  # Remove stop words
  data(stop_words)

  tidy_df <- tidy_df %>% anti_join(stop_words)

  tidy_df %>% count(word, sort = TRUE)

  tidy_df <- tidy_df %>% filter(!word %in% stop_words$word, # remove stop words
       !str_detect(word, "’") , # remove ’
       substr(word, 1, 1) != '#', # remove hashtags
       str_detect(word, "[a-z]")) # remove words containing ony numbers or symbols

  return(tidy_df)
}

###

#bing

#word count
review_word_count <- function(tidy_df){
  
  word_counts <- tidy_df %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

  return(word_counts)
}


#plot word counts
wordcount_plot <- function(df){
  df %>% count(word, sort = TRUE) %>%
    filter(n > 200) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, label=n)) +
    geom_col() +
    xlab(NULL)+
    coord_flip()
}

#plot sentimental
sentiment_plot <- function(df){
df %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL)+
  coord_flip()
  

}


wordcloud_plot <- function(df,palettes){
  wordcloud(words = df$word, freq = df$n,
            max.words=200, random.order=FALSE,
            col= palettes)
}

###
# headspace
tidy_headspace = tidy_review(headspace_review)
tidy_headspace <- tidy_headspace %>% mutate(app_name='headspace')
headspace_word_counts = review_word_count(tidy_headspace)
wordcount_plot(tidy_headspace)
sentiment_plot(headspace_word_counts)
wordcloud_plot(headspace_word_counts,c(wes_palettes$GrandBudapest1[1],wes_palettes$Darjeeling1[c(3,4)]))


# calm
tidy_calm = tidy_review(calm_review)
tidy_calm <- tidy_calm %>% mutate(app_name='calm')
calm_word_counts = review_word_count(tidy_calm)
wordcount_plot(tidy_calm)
sentiment_plot(calm_word_counts)
wordcloud_plot(calm_word_counts,lll_palette("Traffic", c(5,4,3)))

#insight time
tidy_insight = tidy_review(insight_review)
tidy_insight <- tidy_insight %>% mutate(app_name='insight_time')
insight_word_counts = review_word_count(tidy_insight)
wordcount_plot(tidy_insight)
sentiment_plot(insight_word_counts)
wordcloud_plot(insight_word_counts,lll_palette("Epilogue2", c(7,4,3)))


#oak
tidy_oak = tidy_review(oak_review)
tidy_oak <- tidy_oak %>% mutate(app_name='oak')
oak_word_counts = review_word_count(tidy_oak)
wordcount_plot(tidy_oak)
sentiment_plot(oak_word_counts)
wordcloud_plot(oak_word_counts,lll_palette("Summer", c(3,2,1)))


#meditation studio
tidy_medstudio = tidy_review(medstudio_review)
tidy_medstudio <- tidy_medstudio %>% mutate(app_name='meditation_studio')
medstudio_word_counts = review_word_count(tidy_medstudio)
wordcount_plot(tidy_medstudio)
sentiment_plot(medstudio_word_counts)
wordcloud_plot(medstudio_word_counts,lll_palette("CityOfStars2", c(2,3,4)))


#shine
tidy_shine = tidy_review(shine_review)
tidy_shine <- tidy_shine %>% mutate(app_name='shine')
shine_word_counts = review_word_count(tidy_shine)
wordcount_plot(tidy_shine)
sentiment_plot(shine_word_counts)
wordcloud_plot(shine_word_counts,lll_palette("California", c(5,6,7)))


#all

tidy_all = tidy_review(app_review)
all_word_counts = review_word_count(tidy_all)
wordcount_plot(tidy_all)
sentiment_plot(all_word_counts)
wordcloud_plot(all_word_counts,lll_palette("California", c(5,6,7)))


# combine all tidy review of all apps above
all_review_tidy <- rbind(tidy_headspace,tidy_calm,tidy_insight,tidy_oak,tidy_medstudio,tidy_shine)


#plot bing lexicon distribution for 6 apps

all_review_tidy_plot <- all_review_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(app_name, word, index = line %/% 7, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Put index on x-axis, sentiment on y-axis, and map comedy/tragedy to fill
all_review_tidy_plot %>% ggplot(aes(index, sentiment, fill=sentiment > 0)) +
  # Make a bar chart with geom_col()
  geom_col() +
  # Separate panels for each title with facet_wrap()
  facet_wrap(~app_name,scales = "free_x") +
  labs(title="App reviews sentiment score per app", 
       x="Reviews", 
       y="Reviews Sentiment Scores")


###

#afinn

appreview_AFINN <- tidy_all %>% inner_join(get_sentiments("afinn"),by='word')


# App reviews sentiment score
ggplot(appreview_AFINN, aes(x = Date, y = value, fill=app_name)) + 
  geom_smooth(colour="black", size=1) +
  theme_bw() +
  theme_minimal()+
  labs(title="App reviews sentiment score per app", 
       subtitle = "Time period differs due to the amount of reviews in the near future", 
       x="Date", 
       y="Reviews Sentiment Scores")+
  facet_wrap(~app_name, scales = "free_x")


#polar sentiment of reviews
all_review_tidy %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(value,app_name,word) %>%
  top_n(10) %>%
  summarise(n = n()) %>% filter(n>15) %>% 
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  ggplot(aes(reorder(word, contribution), n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Review words") +
  ylab("Sentiment value * Number of Occurrences") +
  ggtitle("Polar Sentiment of Review Top Words") +
  coord_flip()
###

#nrc

nrc <- read.csv('nrc.csv')
nrc <- nrc %>% rename('en'='English..en.')

nrc <- nrc %>% gather(lexicon, dummy, Positive:Trust)

lexicon_nrc <- rep(NA,nrow(nrc))
for(i in 1:length(lexicon_nrc)){
  if(nrc$dummy[i]==1) {
    lexicon_nrc[i] <- nrc$lexicon[i]
  }
}

nrc <- cbind(nrc,lexicon_nrc) %>% as.data.frame()
nrc <- nrc %>% drop_na() %>% select(en,lexicon_nrc) %>% rename('word'='en') 



tidy_all_nrc <- all_review_tidy %>%
  inner_join(nrc,by='word')

sentiment_nrc <- tidy_all_nrc %>%
  group_by(app_name, lexicon_nrc) %>%
  count(app_name, lexicon_nrc) %>% 
  select(app_name, lexicon_nrc, sentiment_total = n) %>% 
   filter(lexicon_nrc!='Positive') %>% filter(lexicon_nrc!='Negative')

appname_nrc <- tidy_all_nrc %>%
  count(app_name) %>% 
  select(app_name, app_total = n)

radar_chart <- sentiment_nrc %>% 
  inner_join(appname_nrc, by = 'app_name') %>% 
  mutate(percent = round((sentiment_total/app_total * 100), 3)) %>% 
  select(-sentiment_total, -app_total) %>%
  spread(app_name, percent)

radar_chart_general <- radar_chart[c(8,7,6,5,4,3,2,1),]
radar_chart1 <- radar_chart[,c(1,2,3)]
radar_chart2 <- radar_chart[,c(1,4,5)]
radar_chart3 <- radar_chart[,c(1,5,6)]

color <- grDevices::col2rgb(c("blue","orange","yellow","red","purple","green"))

color1 <- grDevices::col2rgb(c("blue","orange"))

color2 <- grDevices::col2rgb(c("yellow","red"))

color3 <- grDevices::col2rgb(c("purple","green"))

set.seed(123)
chartJSRadar(radar_chart1, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 20,
             colMatrix = color1, byrow = F, nrow = 3)


chartJSRadar(radar_chart2, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 20,
             colMatrix = color2, byrow = F, nrow = 3)


chartJSRadar(radar_chart3, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 20,
             colMatrix = color3, byrow = F, nrow = 3)

chartJSRadar(radar_chart_general, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 20,
               colMatrix = color, byrow = F, nrow = 3)


###

#Spacy

spacy_install(lang_models = "en")
spacy_initialize()

## spacy python option is already set, spacyr will use:
## condaenv = "spacy_condaenv"
## successfully initialized (spaCy Version: 2.0.18, language model: en)
## (python options: type = "condaenv", value = "spacy_condaenv")



#n-gram
ngram_convert <- function(df){
  df1 <- df %>%
    unnest_tokens(bigram, Review, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !str_detect(word1, "’"),
         !str_detect(word2, "’")) %>%
    count(app_name,word1, word2, sort = TRUE) 
  
  #POS and Tagging
  
  lemma1 <- spacy_parse(df1$word1)
  lemma1 <- lemma1 %>% select(token,lemma) %>% rename('word1'='token','lemma1'='lemma')
  df1 <- merge(x=df1,y=lemma1,by="word1",all.x=TRUE)

  lemma2 <- spacy_parse(df1$word2)
  lemma2 <- lemma2 %>% select(token,lemma) %>% rename('word2'='token','lemma2'='lemma')
  df1 <- merge(x=df1,y=lemma2,by="word2",all.x=TRUE)

  df1 <- df1 %>% select(app_name,lemma1,lemma2,n) %>% rename('word1'='lemma1','word2'='lemma2') %>% unique()


  df1 <- df1 %>% unite(bigram, word1, word2, sep = " ") %>% filter(bigram!='meditation app') %>% arrange(desc(n))

  return(df1)
}


headspace_ngram <- ngram_convert(headspace_review)
headspace_ngram <- headspace_ngram %>% 
  filter(bigram!='headspace' || bigram!='headspace app')

calm_ngram <- ngram_convert(calm_review)
calm_ngram <- calm_ngram %>% 
  filter(bigram!='calm' || bigram!='calm app')

insight_ngram <- ngram_convert(insight_review)
insight_ngram <- insight_ngram %>% 
  filter(bigram!='insight timer')

oak_ngram <- ngram_convert(oak_review)
oak_ngram <- oak_ngram %>% 
  filter(bigram!='oak' || bigram!='oak app')

medstudio_ngram <- ngram_convert(medstudio_review)
medstudio_ngram <- medstudio_ngram %>% 
  filter(bigram!='meditation studio' || bigram!='meditation studio app')

shine_ngram <- ngram_convert(shine_review)
shine_ngram <- shine_ngram %>% 
  filter(bigram!='shine' || bigram!='shine app')

all_ngram <- rbind(headspace_ngram,calm_ngram,insight_ngram,oak_ngram,medstudio_ngram,shine_ngram)

all_ngram %>% filter(n > 5) %>% mutate(bigram=reorder(bigram,n)) %>% 
  ggplot(aes(x=bigram, y=n, fill = app_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~app_name, scales = "free_y") +
  labs(y = "N-gram of words contribution",
       x = NULL)+
  coord_flip()



###


# Aggregated relationship between n-grams of words between 6 apps

#bigram
bigrams <- all_ngram %>% group_by(app_name) %>% filter(n > 5) %>% ungroup()


#tf_idf
#frequency–inverse document 

bigram_tf_idf <- bigrams %>%
  count(app_name, bigram) %>%
  bind_tf_idf(bigram, app_name, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf <- bigram_tf_idf %>% mutate(tf_idf =round(tf_idf,2))

#need to remove plural nouns
bigram_tf_idf %>%
  group_by(app_name) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, label=tf_idf, fill = app_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(position = 'identity', stat = 'identity',size=2,hjust=-0.5,vjust=-0.2)+
  facet_wrap(~app_name, scales = "free_y") +
  labs(y = "tf_idf of bigram Reviews",
       x = NULL)+
  coord_flip()

#Network of bigram with ggraph

bigram_graph <- bigram %>% graph_from_data_frame()

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###

#Uniqueness

# tokenized the text

#headspace

#POS tagging
pos_headspace <- spacy_parse(tidy_headspace$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_headspace <- pos_headspace %>% filter(lemma != 'headspace') %>% mutate(app_name='headspace')

#NER taging
ent_headspace <- spacy_extract_entity(tidy_headspace$word)
ent_headspace <- ent_headspace %>% mutate(app_name='headspace')


#calm

#POS
pos_calm <- spacy_parse(tidy_calm$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_calm <- pos_calm %>% filter(lemma != 'calm')  %>% mutate(app_name='calm')

#NER
ent_calm <- spacy_extract_entity(tidy_calm$word)
ent_calm <- ent_calm %>% mutate(app_name='calm')


#insight timer

#POS
pos_insight <- spacy_parse(tidy_insight$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_insight <- pos_insight %>% filter(lemma != 'insight' ||lemma != 'timer')  %>% mutate(app_name='insight_timer')

#NER
ent_insight <- spacy_extract_entity(tidy_insight$word)
ent_insight <- ent_insight %>% mutate(app_name='insight_timer')



#meditation studio

#POS
pos_medstudio <- spacy_parse(tidy_medstudio$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_medstudio <- pos_medstudio %>% filter(lemma != 'meditation' ||lemma != 'studio') %>% mutate(app_name='meditation_studio')

#NER
ent_medstudio <- spacy_extract_entity(tidy_medstudio$word)
ent_medstudio <- ent_medstudio %>% mutate(app_name='meditation_studio')



#oak

#POS
pos_oak <- spacy_parse(tidy_oak$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_oak <- pos_oak %>% filter(lemma != 'oak')  %>% mutate(app_name='oak')

#NER
ent_oak <- spacy_extract_entity(tidy_oak$word)
ent_oak <- ent_oak %>% mutate(app_name='oak')


#shine

#POS
pos_shine <- spacy_parse(tidy_shine$word,pos = TRUE, tag = FALSE, lemma = TRUE, entity = FALSE)
pos_shine <- pos_shine %>% filter(lemma != 'shine') %>% mutate(app_name='shine')

#NER
ent_shine <- spacy_extract_entity(tidy_shine$word)
ent_shine <- ent_shine %>% mutate(app_name='shine')

pos_all <- rbind(pos_headspace,pos_calm,pos_insight,pos_medstudio,pos_oak,pos_shine)
ent_all <- rbind(ent_headspace,ent_calm,ent_insight,ent_medstudio,ent_oak,ent_shine)


###


# Add the tf-idf for these words
pos_tf_idf <- pos_all %>% 
  count(app_name, lemma, pos, sort = TRUE) %>% 
  bind_tf_idf(lemma, app_name, n) %>% 
  arrange(desc(tf_idf))


###


#uniqueness function 
unique_pos <- function(df,pos_type,name){
  
  unique_pos_df <- df %>% 
  filter(pos == pos_type, app_name == name) %>% 
  mutate(rank = 1:n()) %>% arrange(rank)
  
}


#plotting uniqueness function
uniqueness_plot <- function(df,col,chart_name){
df %>% 
  top_n(10, tf_idf) %>% 
  mutate(lemma = fct_inorder(lemma)) %>% 
  ggplot(aes(x = fct_rev(lemma), y = tf_idf)) + 
  geom_col(fill = col) + 
  labs(x = NULL, y = "tf-idf",
       title = chart_name) +
  coord_flip() 
}

###

#UNIQUENESS

#headspace nouns

unique_nouns_headspace <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('headspace'))

uniqueness_plot(unique_nouns_headspace,col='orange',chart_name = c('Most unique nouns in headspace'))

wordcloud(words = unique_nouns_headspace$lemma, freq = unique_nouns_headspace$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$Royal1[c(1,3,2,4)])

#headspace verbs

unique_verbs_headspace <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('headspace'))
unique_verbs_headspace <- unique_verbs_headspace %>% filter(lemma!='pricey')

uniqueness_plot(unique_verbs_headspace,col='orange',chart_name = c('Most unique verbs in headspace'))

wordcloud(words = unique_verbs_headspace$lemma, freq = unique_verbs_headspace$tf_idf,
      max.words=1000,random.order=FALSE, col=wes_palettes$Royal1[c(3,2,4)])


#calm nouns

unique_nouns_calm <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('calm'))

uniqueness_plot(unique_nouns_calm,col=wes_palette("Moonrise3",c(1)),chart_name = c('Most unique nouns in calm'))

wordcloud(words = unique_nouns_calm$lemma, freq = unique_nouns_calm$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$Moonrise3[c(3,2,1)])

#calm verbs

unique_verbs_calm <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('calm'))

uniqueness_plot(unique_verbs_calm,col=wes_palette("Moonrise3",c(1)),chart_name = c('Most unique verbs in calm'))

wordcloud(words = unique_verbs_calm$lemma, freq = unique_verbs_calm$tf_idf,
      max.words=1000,random.order=FALSE, col=wes_palettes$Moonrise3[c(3,2,1)])


#insight timer nouns

unique_nouns_insight <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('insight_timer'))
unique_nouns_insight <- unique_nouns_insight %>% filter(lemma!='insight') %>% filter(lemma!='timer')

uniqueness_plot(unique_nouns_insight,col=wes_palettes$Chevalier1[2],chart_name = c('Most unique nouns in insight timer'))

wordcloud(words = unique_nouns_insight$lemma, freq = unique_nouns_insight$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$Chevalier1[c(4,2,1)])

#insight verbs

unique_verbs_insight <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('insight_timer'))

uniqueness_plot(unique_verbs_insight,col=wes_palettes$Chevalier1[2],chart_name = c('Most unique verbs in insight timer'))

wordcloud(words = unique_verbs_insight$lemma, freq = unique_verbs_insight$tf_idf,
      max.words=500,random.order=FALSE, col=wes_palettes$Chevalier1[c(4,2,1)])



#meditation studio nouns

unique_nouns_medstudio <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('meditation_studio'))
unique_nouns_medstudio <- unique_nouns_medstudio %>% filter(lemma!='studio')

uniqueness_plot(unique_nouns_medstudio,col=wes_palettes$GrandBudapest1[2],chart_name = c('Most unique nouns in meditation studio'))

wordcloud(words = unique_nouns_medstudio$lemma, freq = unique_nouns_medstudio$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$GrandBudapest1[c(1,2,3)])

#meditation studio verbs

unique_verbs_medstudio <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('meditation_studio'))

uniqueness_plot(unique_verbs_medstudio,col=wes_palettes$GrandBudapest1[2],chart_name = c('Most unique verbs in meditation studio'))

wordcloud(words = unique_verbs_medstudio$lemma, freq = unique_verbs_medstudio$tf_idf,
      max.words=500,random.order=FALSE, col=wes_palettes$GrandBudapest1[c(1,2,3)])


#oak nouns

unique_nouns_oak <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('oak'))

uniqueness_plot(unique_nouns_oak,col=wes_palettes$Darjeeling1[2],chart_name = c('Most unique nouns in oak'))

wordcloud(words = unique_nouns_oak$lemma, freq = unique_nouns_oak$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$Darjeeling1[c(2,3,4)])

#oak verbs

unique_verbs_oak <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('oak'))

uniqueness_plot(unique_verbs_oak,col=wes_palettes$Darjeeling1[2],chart_name = c('Most unique verbs in oak'))

wordcloud(words = unique_verbs_oak$lemma, freq = unique_verbs_oak$tf_idf,
      max.words=500,random.order=FALSE, col=wes_palettes$Darjeeling1[c(2,3,4)])


#shine nouns

unique_nouns_shine <- unique_pos(pos_tf_idf,pos_type = c('NOUN'), name= c('shine'))
unique_nouns_shine <- unique_nouns_shine %>% filter(lemma!='in')

uniqueness_plot(unique_nouns_shine,col=wes_palettes$BottleRocket2[3],chart_name = c('Most unique nouns in shine'))

wordcloud(words = unique_nouns_shine$lemma, freq = unique_nouns_shine$tf_idf,
      max.words=100,random.order=FALSE, col=wes_palettes$BottleRocket2[c(1,2,3)])

#shine verbs

unique_verbs_shine <- unique_pos(pos_tf_idf,pos_type = c('VERB'), name= c('shine'))

uniqueness_plot(unique_verbs_shine,col=wes_palettes$BottleRocket2[3],chart_name = c('Most unique verbs in shine'))

wordcloud(words = unique_verbs_shine$lemma, freq = unique_verbs_shine$tf_idf,
      max.words=500,random.order=FALSE, col=wes_palettes$BottleRocket2[c(1,2,3)])

###

#most unique nouns in app reviews

unique_nouns <- rbind(unique_nouns_headspace, unique_nouns_calm,
                      unique_nouns_insight, unique_nouns_oak,
                      unique_nouns_medstudio, unique_nouns_shine)

most_unique_nouns <- unique_nouns %>% 
  mutate(rank = 1:n()) %>% arrange(rank)


most_unique_nouns %>% group_by(app_name) %>% 
  top_n(3,  wt = tf_idf) %>%  top_n(3,  wt = -rank) %>% ungroup() %>% 
  mutate(lemma = fct_inorder(lemma)) %>% 
  ggplot(aes(fill=app_name, y=tf_idf, x=fct_rev(lemma))) + 
    geom_bar(position="dodge", stat="identity") +
  labs(x = NULL, y = "tf-idf",
       title = "Most unique nouns in 6 apps") +
  scale_fill_manual("app_name", 
                    values = c("headspace" = "#FF8C00", "calm" = "#87CEEB", "insight_timer" = "#D4AF37",
                               "meditation_studio" = "#FF7F7F","oak" = "#32CC32","shine" = "#FBC702")) +

  coord_flip() 


###


#most unique verbs app reviews

unique_verbs <- rbind(unique_verbs_headspace, unique_verbs_calm,
                      unique_verbs_insight, unique_verbs_medstudio,unique_verbs_oak,unique_verbs_shine)

most_unique_verbs <- unique_verbs %>% 
  mutate(rank = 1:n()) %>% arrange(rank) 


most_unique_verbs %>% group_by(app_name) %>% 
  top_n(3,  wt = tf_idf) %>%  top_n(3,  wt = -rank) %>% ungroup() %>% 
  mutate(lemma = fct_inorder(lemma)) %>% 
  ggplot(aes(fill=app_name, y=tf_idf, x=fct_rev(lemma))) + 
    geom_bar(position="dodge", stat="identity") +
  labs(x = NULL, y = "tf-idf",
       title = "Most unique verbs in 6 apps")+
  scale_fill_manual("app_name", 
                    values = c("headspace" = "#FF8C00", "calm" = "#87CEEB", "insight_timer" = "#D4AF37",
                               "meditation_studio" = "#FF7F7F","oak" = "#32CC32","shine" = "#FBC702")) +

  coord_flip() 



###




