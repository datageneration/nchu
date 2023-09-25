# Data Programming with R
# # Collecting YouTube data 
## tubeR package GitHub: https://github.com/gojiplus/tuber


Installed <- TRUE  # For checking if package is installed
toInstall <- c("tuber","quanteda", "quanteda.corpora", "stopwords", "showtext", "wordcloud", "devtools", "tidyverse", "lubridate", "stringi", "wordcloud", "gridExtra","httr")
if(Installed){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library


# install.packages("devtools")
#devtools::install_github("soodoku/tuber", build_vignettes = TRUE)

# Linux Ubuntu users only
httr::set_config(config( ssl_verifypeer = 0L ) ) # = Fixes some certificate problems on linux = #

## Be sure to get the correct credentials
## Create a project for web application
## https://console.developers.google.com
## 1. Enable APIs and services
## 2. Choose YouTube Data API v3
## 3. Go to APIs & Services --> + Create Credentials

# = Authentication = #
## Issue: when Error: HTTP failure: 401, delete .httr-oauth and run yt_oauth() again
yt_oauth()
yt_oauth("YOURCLIENTID","YOURCLIENTSECRET", token = "")


list_regions() # Hong Kong HK 104, Taiwan TW 105
# 104 HK              Hong Kong dPGz2QsPehBqvPiSigL1Dbg92vs
# 105 TW                 Taiwan i_HaeX1-HlrzaraiFMuZ0v1jK6I


# Search YouTube videos with keyword
yt_twelection = yt_search("台灣選舉")

tw50 = list_videos(part = "statistics",region_code = "TW") # Top 50 most popular videos

# = Download and prepare data = #

# Find the channel ID in the source page
# Alternatively, from get_video_details
# = Channel stats = #

setnews_stat = get_channel_stats("UC2TuODJhC03pLgd6MpWP0iw")
get_video_details(video_id = "DclUD0w3-Ic")
setnews_videos = yt_search(term="", type="video", channel_id = "UC2TuODJhC03pLgd6MpWP0iw")
setnews_channel = yt_search(term="", type="channel", channel_id = "UC2TuODJhC03pLgd6MpWP0iw")

setnews_videos = setnews_videos %>% 
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2015-01-01") %>%
  arrange(date) # Create date  variable 

setnews_videos = setnews_videos %>% 
  mutate(datetime = as.POSIXlt(publishedAt)) %>%
  filter(date > "2015-01-01") %>%
  arrange(datetime) # Create date + time variable 

class(setnews_videos$datetime)

setnews_comment01 = get_all_comments(video_id = "OTLCrfRw7m0")
setnews_comment02 = get_comment_threads(filter = c(video_id = "OTLCrfRw7m0"))
setnews_comment03 = get_all_comments(video_id = "HQOpbva7zyc")
setnews_comment04 = get_all_comments(video_id = "dakyuvKbzB4")


# read text files
# remotes::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)
library(quanteda)
library(stopwords)

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")
ch_stop1 = append(ch_stop, "br") # add custom stopword(s)

corp =setnews_comment03$textDisplay # create corpus

# Tokenize corpus
ch_toks <- corp %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(pattern = ch_stop1) 

# Construct a dfm
ch_dfm <- dfm(ch_toks)
topfeatures(ch_dfm)

# Plot a wordcloud using video's comments
set.seed(100)
library("quanteda.textplots")

# Font management in R
library(showtext)
showtext_auto()
textplot_wordcloud(ch_dfm, min_count = 10, random_order = FALSE,
                   rotation = .25, max_words = 200,
                   min_size = 0.5, max_size = 2.8,
                   font = if (Sys.info()['sysname'] == "Sonoma") "Kaiti TC" else NULL,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))




# Wordcloud for another video's comments
corp1 =setnews_comment04$textDisplay
# tokenize
ch_toks1 <- corp1 %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(pattern = ch_stop1)
# construct a dfm
ch_dfm1 <- dfm(ch_toks1)
topfeatures(ch_dfm1)
set.seed(100)
textplot_wordcloud(ch_dfm1, min_count = 10, random_order = FALSE,
                   rotation = .25, max_words = 250,
                   min_size = 0.5, max_size = 4,
                   font = if (Sys.info()['sysname'] == "Sonoma") "Heiti TC" else NULL,
                   color = RColorBrewer::brewer.pal(9, "Set1"))





# = Videos = #
curl::curl_version()
httr::set_config(httr::config(http_version = 0)) # Fix curl issue

# Comments in data frame
set_comments_all = lapply(as.character(setnews_videos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 101)
}) # Takes very long to get all
commentsstats = do.call(rbind.data.frame, set_comments_all)

# Video stats in data frame
videostats = lapply(as.character(setnews_videos$video_id), function(x){
  get_stats(video_id = x)
}) # Takes some time

x = get_all_channel_video_stats(channel_id="UC2TuODJhC03pLgd6MpWP0iw")  

videostats = do.call(rbind.data.frame, videostats)
class(videostats$viewCount) # What's wrong?
videostats$title = setnews_videos$title
videostats$date = setnews_videos$date

# Fix variable classes
videostats = select(videostats, video_id, date, title, viewCount, likeCount, favoriteCount, commentCount) %>%
  as_tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         favoriteCount = as.numeric(as.character(favoriteCount)),
         commentCount = as.numeric(as.character(commentCount)))

# = General Stat Table = #
# 
genstat = data.frame(Channel="UC2TuODJhC03pLgd6MpWP0iw", 
                     Subscriptions=set_stat$statistics$subscriberCount,
                     Views = set_stat$statistics$viewCount,
                     Videos = set_stat$statistics$videoCount, Likes = sum(videostats$likeCount),
                     Dislikes = sum(videostats$dislikeCount), Comments = sum(videostats$commentCount))

library(RColorBrewer)
# = videostats Plot = #
p1 = ggplot(data = videostats[-1, ], aes(x = date, y = likeCount))+ geom_point(color = "forestgreen", cex = 1) +
 theme_bw()  
p2 = ggplot(data = videostats[-1, ], aes(x = date, y = viewCount)) + geom_point(color = "firebrick1", cex = 1) +
  theme_bw()  
p3 = ggplot(data = videostats[-1, ], aes(x = date, y = commentCount) ) + geom_point(color = "steelblue", cex = 1) +
  theme_bw()  

grid.arrange(p1, p2, p3, ncol = 2)

# = Comments Time series = #
comments_ts = lapply(set_comments_all, function(x){
  as.Date(x$publishedAt)
})
comments_ts = tibble(date = as.Date(Reduce(c, comments_ts))) %>%
  group_by(date) %>% count()
ggplot(data = comments_ts) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2023-09-25")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-04")), linetype = 2,color = "red")+
  theme_bw()  

