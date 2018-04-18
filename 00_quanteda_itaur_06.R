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
#   Advanced
###########################################################

#
# setwd("C:\\Users\\enrico\\GDRIVE\\CAPSTONE\\Quanteda\\ITAUR\\6_advanced")
setwd(file.path(itaur_dir(),"6_advanced"))
# getwd()

load("tweetSample.RData")
str(tweetSample)

require(lubridate)
require(dplyr)

class(tweetSample); 
# [1] "data.frame"
names(tweetSample)
# [1] "created_at"           "geo_latitude"         "geo_longitude"       
# [4] "hashtags"             "id"                   "lang"                
# [7] "text"                 "type"                 "user_followers_count"
# [10] "user_friends_count"   "user_geo_enabled"     "user_id"             
# [13] "user_id_str"          "user_lang"            "user_listed_count"   
# [16] "user_location"        "user_name"            "user_screen_name"    
# [19] "user_statuses_count"  "user_time_zone"       "user_url"            
# [22] "user_created_at"      "user_geo_enabled.1"   "user_screen_nameL"   
# [25] "Party"                "Party.Code"           "Sitting_2009"        
# [28] "Sitting_2014"         "Name"                 "Twitter"             
# [31] "Facebook"             "gender"               "Country"             
# [34] "hasTwitter"           "candidate"
str(tweetSample$created_at)
#  chr [1:10000] "2014-05-28 15:53:33+00:00" "2014-05-30 08:32:13+00:00" ...

tweetSample <- mutate(tweetSample, day = yday(created_at))
tweetSample <- mutate(tweetSample, dayDate = as.Date(day-1, origin = "2014-01-01"))

juncker <- filter(tweetSample, grepl('juncker', text, ignore.case = TRUE)) %>% 
  mutate(kand = 'Juncker')


# ---------------------------------------------------------
prj_dir()
