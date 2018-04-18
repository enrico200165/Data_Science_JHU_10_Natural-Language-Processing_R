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
print(str(tweetSample$day))
tweetSample <- mutate(tweetSample, dayDate = as.Date(day-1, origin = "2014-01-01"))
print(str(tweetSample$dayDate))

juncker <- filter(tweetSample, grepl('juncker', text, ignore.case = TRUE)) %>% 
  mutate(kand = 'Juncker')
print(class(juncker))
head(juncker)
schulz <-  filter(tweetSample, grepl('schulz', text, ignore.case = TRUE)) %>% 
  mutate(kand = 'Schulz')
verhof <-  filter(tweetSample, grepl('verhofstadt', text, ignore.case = TRUE)) %>% 
  mutate(kand = 'Verhofstadt')
spitzAll <- bind_rows(juncker, schulz, verhof)


# Once the data is in the correct format, we can use ggplot to display the candidate mentions on the a single plot:

require(ggplot2)
require(scales)
plotDf <- count(spitzAll, kand, day=day) %>% 
  mutate(day = as.Date(day-1, origin = "2014-01-01"))

ggplot(data=plotDf, aes(x=day, y=n, colour=kand)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + geom_vline(xintercept=as.numeric(as.Date("2014-05-15")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2014-05-25")), linetype=4) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# use the keptFeatures argument to dfm() to analyse 
# only hashtags for each candidate’s text.
# Top hashtags for tweets that mention Juncker
dv <- data.frame(user = juncker$user_screen_name)
jCorp <- corpus(juncker$text, docvars = dv)
jd <- dfm(jCorp)
jd <- dfm_select(jd, "^#.+", "keep", valuetype = "regex") 
print(jd[1:3,1:3])
print(substr(juncker$text, 1, min(16,nchar(juncker$text))))

print(topfeatures(jd, nfeat(jd)))
print(str(topfeatures(jd, nfeat(jd))))


# ---------------------------------------------------------
# Further analysis examples
# ---------------------------------------------------------

data(data_corpus_amicus, package = "quanteda.corpora")

refs <- docvars(data_corpus_amicus, "trainclass")
refs <- (as.numeric(refs) - 1.5)*2
amicusDfm <- dfm(data_corpus_amicus)
wm <- textmodel_wordscores(amicusDfm, y = refs)
summary(wm)
preds <- predict(wm, newdata = amicusDfm)
summary(preds)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.20673  0.02659  0.07694  0.05667  0.09922  0.23308
plot(preds ~ docvars(amicusDfm, "testclass"),
     horizontal = TRUE, xlab = "Predicted document score",
     ylab = "Test class", las = 1)


# Correspondence analysis:

dfm(data_corpus_irishbudget2010) %>%
  textmodel_ca() %>% 
  textplot_scale1d

# Poisson scaling
ieWF <- dfm(data_corpus_irishbudget2010, remove_punct = TRUE) %>%
  textmodel_wordfish(dir = c(6,5))
summary(ieWF)
textplot_scale1d(ieWF)


# --- Topic models:
# LDA info https://www.quora.com/What-is-a-good-explanation-of-Latent-Dirichlet-Allocation
require(topicmodels)
## Loading required package: topicmodels
mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1950)
quantdfm <- dfm(mycorpus, verbose = FALSE, remove_punct = TRUE,
                remove = c(stopwords('english'), 'will', 'us', 'nation', 'can', 'peopl*', 'americ*'))
ldadfm <- convert(quantdfm, to = "topicmodels")
lda <- LDA(ldadfm, control = list(alpha = 0.1), k = 20)
terms(lda, 10)




# ---------------------------------------------------------
prj_dir()
