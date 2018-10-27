#tweets

library(data.table)
library(ggplot2)
library(tidytext)
library(dplyr)
library(plyr)
library(h2o)
library(lubridate)
library(tidyr)
library(forcats)
library(caret)
library(plyr)
library(stringr)


tw_train <- read.csv("Tweets/train.csv", stringsAsFactors = TRUE, na.strings = c(""," "))
tw_test <- read.csv("Tweets/test.csv", stringsAsFactors = TRUE, na.strings = c(""," "))

tw_sample <- fread('Tweets/sample_submission.csv')

summary(tw_train)
str(tw_train)

#tw_train[,.N,by=c("name","airline_sentiment","airline")][order(N,decreasing = TRUE),][1:50,]

summary(tw_train$retweet_count)

sapply(tw_train,function(x) sum(is.na(x)))

#now lets analyze the text
#max words in tweet is 200

tw_train$text[1:11]




### Exploratory Data Analysis


tw_train %>% ggplot(aes(airline_sentiment,fill = factor(airline_sentiment)))+geom_bar()
tw_train %>% ggplot(aes(airline_sentiment,fill = factor(airline)))+geom_bar() #united has more negative comment.

tw_train %>% ggplot(aes(negativereason,fill = factor(airline_sentiment)))+geom_bar()
#obvious that positive and neutral comments have no negative reasons to tweet.

#tw_train %>% ggplot(aes(airline_sentiment,fill = factor(tweet_location)))+geom_bar()


tw_train %>% ggplot(aes(airline_sentiment_confidence,fill=factor(airline_sentiment)))+
  geom_density(bw = 0.05, alpha = 0.3)
#the confidence of the negative word is high 0.8 and above
#confidence of the neural is between 0.5 to 0.85
#positive differs


tw_train %>% ggplot(aes(retweet_count,fill=factor(airline_sentiment)))+
  geom_density(bw = 0.05, alpha = 0.3)+scale_x_log10()

# negativereason vs nrconf with airline sentiment
tw_train %>% ggplot(aes(factor(negativereason),negativereason_confidence))+geom_boxplot()+
  geom_jitter(aes(colour=factor(airline_sentiment)))


#nr vs airline conf airline sentiment
tw_train %>% ggplot(aes(factor(negativereason),airline_sentiment_confidence))+geom_boxplot()+
  geom_jitter(aes(colour=factor(airline)))

#negative reason with airline sentiment
tw_train %>% ggplot(aes(negativereason,fill = factor(airline_sentiment)))+geom_bar()
#obvious that positive and neutral comments have no negative reasons to tweet.

#nr vs airline conf airline sentiment
tw_train %>% ggplot(aes(factor(negativereason),airline_sentiment_confidence))+geom_boxplot()+
  geom_jitter(aes(colour=factor(airline)))


tw_train %>% ggplot(aes(factor(negativereason),airline_sentiment_confidence))+geom_boxplot()+
  geom_jitter(aes(colour=factor(retweet_count)))

tw_train %>% ggplot(aes(factor(negativereason),negativereason_confidence))+geom_boxplot()+
  geom_jitter(aes(colour=factor(retweet_count)))


#combining the train test
tw_test$airline_sentiment <- NA
tw_train$is_train <- 1
tw_test$is_train <- 0

tw_full <- rbind(tw_train,tw_test)

summary(tw_full)
sapply(tw_full,function(x) sum(is.na(x)))
#####

fix_isse <- tw_full %>% select(tweet_created, tweet_location, user_timezone)
fix_isse$tweet_created <- gsub("[a-z*A-Z*\\[*\\]+",NA,fix_isse$tweet_created)
fix_iss <- as.data.frame(t(apply(fix_isse,1,function(x){return(c(x[!is.na(x)],x[is.na(x)]))})))


#both the files are corrupted.
tw_full$tweet_location <- NULL
tw_full$user_timezone <- NULL

tw_full$tweet_created_new <- fix_iss$V1

tw_full$tweet_created <- NULL

sapply(tw_full,function(x) sum(is.na(x)))

summary(tw_full$negativereason)

#replacing all NA with 0.0000
tw_full$negativereason_confidence[is.na(tw_full$negativereason_confidence)] <- 0.0000

tw_full$negativereason <- fct_explicit_na(tw_full$negativereason,"No Reason")

sapply(tw_full,function(x) sum(is.na(x)))

#remove common NA.
tw_full <- tw_full[!is.na(tw_full$airline),]


tw_full <- tw_full %>% separate(tweet_created_new,c("date","time","timezone"),sep=" ")

tw_full <- tw_full %>% separate(date,c("t_year","t_month","t_date"),sep="-")
tw_full <- tw_full %>% separate(time,c("t_hrs","t_min","t_sec"),sep=":")

tw_full <- tw_full[!is.na(tw_full$time),]
tw_full <- tw_full[!is.na(tw_full$t_min),]
tw_full <- tw_full[!is.na(tw_full$t_date),]

#tw_full$tweet_id <- NULL

#tw_full[,c('t_year','t_month','t_date','t_hrs','t_min','t_sec'):=lapply(.SD,factor),.SDcols=c('t_year','t_month','t_date','t_hrs','t_min','t_sec')]

tw_full <- tw_full %>% mutate_if(is.character,funs(factor(.)))

setDT(tw_full)
str(tw_full)


#single factor
tw_full$timezone <- NULL
tw_full$t_year <-NULL
tw_full$t_month <- NULL

future_use_col <- tw_full %>% select(text,is_train)

tw_full$text <- as.character(tw_full$text)

#a <- sapply(str_split(tw_full$text," "),length)

tw_full <- tw_full %>% mutate(word_count = sapply(str_split(tw_full$text," "),length))


replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z']|'(?![A-Za-z]))" #([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))

tweets <- tw_full %>% select(tweet_id,text) %>% filter(!str_detect(text,"^RT")) %>% 
  mutate(text = str_replace_all(text,replace_reg,"")) %>%
  unnest_tokens(word,text,token="ngrams",n=2) 
#unigram
wordseq <- tw_full %>% select(tweet_id,text) %>% filter(!str_detect(text,"^RT")) %>% 
  mutate(text = str_replace_all(text,replace_reg,"")) %>% unnest_tokens(word,text)

uni_remove_stop <-wordseq %>% 
  filter(!word %in% c("virginamerica","united","americanair","usairways")) %>% 
  filter(!word %in% stop_words$word)

uni_tf_idf <- uni_remove_stop %>% count(tweet_id,word) %>% bind_tf_idf(word,tweet_id,n)


#bigram
bigram <- tweets %>% separate(word,c('word1','word2'),sep=" ")

remove_stop <-bigram %>% 
  filter(!word1 %in% c("virginamerica","united","americanair","usairways")) %>% 
  filter(!word2 %in% c("virginamerica","united","americanair","usairways")) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)






######

split <- createDataPartition(tw_full[is_train==1,airline_sentiment],times = 1,list=FALSE,p=0.8)

tr <- tw_full[is_train==1,]
te <- tw_full[is_train==0,]
te$airline_sentiment <- NULL

va <- tr[-split,]
tr <- tr[split,]

X <- colnames(tr)[-c(1,6,8,9)]
Y <- colnames(tr)[1]

h2o.init()

##implementing automl

tr.x <- as.h2o(tr)
va.x <- as.h2o(va)
te.x <- as.h2o(te)


almname <- paste('ak_h2o_automl',format(Sys.time(),"%d%H%M%S"),sep = '_')
autoML <- h2o.automl(X,Y,training_frame = tr.x,validation_frame = va.x,seed=223, max_runtime_secs = 30,stopping_metric=c("misclassification"))
autoML


save(autoML, file="automlv1.rda")

aml_test_pred <- h2o.predict(autoML,te.x)
# roc.curve(lm_test,ntrain$stroke)
prediction <- as.vector(aml_test_pred['predict'])

submission <- data.frame('tweet_id'=tw_test$tweet_id,'airline_sentiment'=prediction)
colnames(submission) <- c('tweet_id','airline_sentiment')
filename <- paste('ak_h20_automl',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
