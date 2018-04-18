require(quanteda, quietly = TRUE, warn.conflicts = FALSE)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#
# devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

# --- external files ---

source("01_globals.R")
prj_dir()



###########################################################
#   Twitter
###########################################################

# install.packages(c('twitteR', 'streamR', 'RCurl', 'ROAuth', 'httr',httpuv"))

# To register a twitter application and get your consumer keys:
# Go to https://apps.twitter.com in a web browser.
# Click on 'create new app'.
# Set the callback URL to http://127.0.0.1:1410. You might have to add a cellphone number your twitter account.
# copy your consumer key and consumer secret to the code below.
# (optional): For actions requiring write permissions, generate an access token and access secret .

# XglXRppOuMgdwdXdiuz7Kw 
# mLRwtIG2rOXkasNWFdCuXbj1e84VAP7PLfjiQx5Ehs

require(twitteR)
require(streamR)
require(ROAuth)

# consumerKey <- 'XglXRppOuMgdwdXdiuz7Kw'
# consumerSecret <- 'mLRwtIG2rOXkasNWFdCuXbj1e84VAP7PLfjiQx5Ehs'

# --- evtest_01
consumerKey <- "kympqRdDqzPONTgkjZtObRdYD"
consumerSecret <- "aX9iFthLU7TFQLvLzqUkw8uIjSBZV4D0IMEkNLUwFIAkFDT2FQ"
accessToken <- "	478515314-etXLscmShHsXiYv64uRumvT1lSGM0reqeEUkPXfV"
accessTokenSecret <- "NGLP9vDDi4F1adq8gtVGO8sIVF9gN04uNIGcVpAtn44i0"

# Try this first, to use twitteR
setup_twitter_oauth(consumerKey, consumerSecret)
results <- searchTwitter('#DemDebate')
df <- as.data.frame(t(sapply(results, as.data.frame)))
print(names(df))

# Then try these instructions, to use streamR: 
# https://github.com/pablobarbera/streamR#installation-and-authentication

# install.packages("streamR")  # from CRAN
# devtools::install_github("pablobarbera/streamR/streamR") # from GitHub

library(ROAuth)
# requestURL <- "https://api.twitter.com/oauth/request_token"
requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
authURL <- "https://api.twitter.com/oauth/authorize"


## RCurl/cURL used for persistent connection, for password authentication, also at times handles redirects
# download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem") 


# my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
#                              requestURL = requestURL, accessURL = accessURL, authURL = authURL)
# 
# 
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# # PIN 3488989
# save(my_oauth, file = "my_oauth.Rdata")

load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 120, 
             oauth = my_oauth)
tweets.df <- parseTweets("tweets.json", simplify = TRUE)
c( length(grep("obama", tweets.df$text, ignore.case = TRUE)),
   length(grep("biden", tweets.df$text, ignore.case = TRUE)) )


# Tweets can also be filtered by two additional parameters: follow, which can be used 
# to include tweets published by only a subset of Twitter users, and locations, 
# which will return geo-located tweets sent within bounding boxes defined by 
# a set of coordinates. Using these two options involves some additional complications
# – for example, the Twitter users need to be specified as a vector of user IDs and 
# not just screen names, and the locations filter is incremental to any keyword 
# in the track argument. For more information, I would suggest to check 
# Twitter's documentation for each parameter.
# Here's a quick example of how one would capture and visualize tweets sent from 
# the United States:

if (!file.exists("tweetsUS.json")) {
  filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50)
              , timeout = 90,  # was 300
             oauth = my_oauth)
}

tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)
library(ggplot2)
library(grid)
require(maps)
map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
p <- ggplot(map.data)
p <- p + geom_map(aes(map_id = region), map = map.data, fill = "white", 
      color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) 
p <- p +  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) 
p <- p + geom_point(data = points, aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")
print(p)


# ---------------------------------------------------------
# sampleStream
# ---------------------------------------------------------
# The function sampleStream allows the user to capture a small 
# random sample (around 1%) of all tweets that are being sent 
# at each moment. This can be useful for different purposes, 
# such as estimating variations in “global sentiment” or 
# describing the average Twitter user. 
# A quick analysis of the public statuses captured with this method shows, 
# for example, that the average (active) Twitter user follows 
# around 500 other accounts, that a very small proportion of tweets 
# are geo-located, and that Spanish is the second most common language 
# in which Twitter users set up their interface.

sampleStream("tweetsSample.json", timeout = 120, oauth = my_oauth, verbose = FALSE)
tweets.df <- parseTweets("tweetsSample.json", verbose = FALSE)
mean(as.numeric(tweets.df$friends_count))
table(is.na(tweets.df$lat))
round(sort(table(tweets.df$lang), decreasing = T)[1:5]/sum(table(tweets.df$lang)), 2)


# ---------------------------------------------------------
# userStream
# ---------------------------------------------------------
# Finally, I have also included the function userStream, 
# which allows the user to capture the tweets they would see 
# in their timeline on twitter.com. As was the case with filterStream, 
# this function allows to subset tweets by keyword and location, 
# and exclude replies across users who are not followed. 
# An example is shown below. Perhaps not surprisingly, many of the accounts 
# I follow use Twitter in Spanish.

userStream("mytweets.json", timeout = 120, oauth = my_oauth, verbose = FALSE)
tweets.df <- parseTweets("mytweets.json", verbose = FALSE)
round(sort(table(tweets.df$lang), decreasing = T)[1:3]/sum(table(tweets.df$lang)), 2)

# ---------------------------------------------------------
# More
# ---------------------------------------------------------
# In these examples I have used parseTweets to read the captured tweets 
# from the text file where they were saved in disk and store them 
# in a data frame in memory. The tweets can also be stored directly 
# in memory by leaving the file.name argument empty, but my personal 
# preference is to save the raw text, usually in different files, 
# one for each hour or day. Having the files means I can run UNIX 
# commands to quickly compute the number of tweets in each period, 
# since each tweet is saved in a different line:
  
system("wc -l 'tweetsSample.json'", intern = TRUE)

# ---------------------------------------------------------
prj_dir()
