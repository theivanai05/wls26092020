library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(R.utils)
library(data.table)
library(scales)
library(recosystem)
library(doSNOW)
library(reshape2)
library("recommenderlab")
registerDoSNOW(makeCluster(3, type = "SOCK"))

# tags questions relationship 
streamToQuestionRel = readRDS("D:/NUS/FYP/data/streamToQuestionRel")

# load assesment data
# questions
dfAssesments = fread("D:/NUS/FYP/data/user_assessments.gz")

dfAssesments$country = as.factor(dfAssesments$country)
dfAssesments$question_id = as.factor(dfAssesments$question_id)
dfAssesments$masked_user_id = as.factor(dfAssesments$masked_user_id)
dfAssesments$question_tags = as.factor(dfAssesments$question_tags)
# convert timestamp into date time format
dfAssesments$eventDateTime = as_datetime(dfAssesments$submission_utc_ts,format="%Y-%m-%d %H:%M:%S.000")
# create new column  event date
dfAssesments$eventDate = as.Date(substr(dfAssesments$eventDateTime,1,10))
# create new column  event time
dfAssesments$eventTime = substr(dfAssesments$eventDateTime,12,22)

dfAssesments = dfAssesments %>%  unique()

saveRDS(dfAssesments,"D:/NUS/FYP/data/dfAssesments")
dfAssesments = readRDS("D:/NUS/FYP/data/dfAssesments")
unique(dfAssesments$country) 
#countries= c("IN","MX","PK","US","EG")

# stream recommendar model building
dfView = fread("D:/NUS/FYP/data/views_model-aug15.gz")
# convert user id ,deck_id,card_id all into factors
dfView$deck_id = as.factor(dfView$deck_id)
dfView$card_id = as.factor(dfView$card_id)
dfView$action = as.factor(dfView$action)
dfView$masked_user_id = as.factor(dfView$masked_user_id )
dfView$country = as.factor(dfView$country)
# convert timestamp into date time format
dfView$eventDateTime = as_datetime(dfView$user_action_timestamp,format="%Y-%m-%dT%H:%M:%S")
# create new column  event date
dfView$eventDate = as.Date(substr(dfView$eventDateTime,1,10))
# create new column  event time
dfView$eventTime = substr(dfView$eventDateTime,12,22)
# create year month field
dfView$EventMonth_Yr <- format(as.Date(dfView$eventDate), "%Y-%m")

# convert timestamp into date time format
dfView$user_since = as_datetime(dfView$user_since,format="%Y-%m-%dT%H:%M:%S")
dfView$user_since_date = as.Date(substr(dfView$user_since,1,10))

saveRDS(dfView,file = "D:/NUS/FYP/data/dfView")
dfView = readRDS(file = "D:/NUS/FYP/data/dfView")

# possible country loops

# select only specific ocuntry assesments

modeleval =  data.frame(user=character(),
                        viewedContent=character(), 
                        recommended=character(), 
                        viewcount=numeric(),
                        matchcount=numeric(),
                        matchpercentage=numeric(),
                        stringsAsFactors=FALSE)

QuestionModelEval =  data.frame(user=character(),
                                viewedContent=character(), 
                                recommended=character(), 
                                viewcount=numeric(),
                                matchcount=numeric(),
                                matchpercentage=numeric(),
                                stringsAsFactors=FALSE)


streamsRec =  data.frame(masked_user_id =character(),
                         deck_id =character(),
                         predicted=numeric(),
                         stringsAsFactors=FALSE)

questionsRec =  data.frame(masked_user_id =character(),
                           deck_id =character(),
                           predicted=numeric(),
                           stringsAsFactors=FALSE)



countries= c("IN")
country = "IN"

for(country in countries)
{

# select the assesment records for country in process
dfInAssesments = dfAssesments %>% filter(country == country) %>% select(masked_user_id,question_id,question_tags,eventDateTime,eventDate,no_of_trials,points_earned)
# select only India activity records
dfIndia = dfView %>% filter(country == country & eventDate >= "2019-04-01" & user_since_date <= "2020-01-01") %>% select("masked_user_id" ,"deck_id","card_id","action","eventDate","user_since","EventMonth_Yr")

# removed 1M duplicate records
dfIndia = unique(dfIndia)

#saveRDS(dfIndia,file = "D:/NUS/FYP/data/dfindia")
#dfIndia = readRDS(file = "D:/NUS/FYP/data/dfindia")


# aggregate event counts
dfIndiaEventCount = as.data.frame( dfIndia %>% group_by(masked_user_id,deck_id,action,eventDate) %>% count() )
# spread the action column
dfIndiaEventCountSpread = spread(dfIndiaEventCount,action,n,fill=0)



# create total viewed content on a day

dfIndiaTotalEventCount = as.data.frame( dfIndia %>% group_by(masked_user_id,action,eventDate) %>% count() )
dfIndiaTotalEventCountSpread = spread(dfIndiaTotalEventCount,action,n,fill=0)
dfIndiaTotalEventCountSpread$totalReadDay = dfIndiaTotalEventCountSpread$STREAM_COMPLETE + dfIndiaTotalEventCountSpread$STREAM_RECIEVED

colnames(dfIndiaTotalEventCountSpread) = c("masked_user_id","eventDate","TOTAL_STREAM_COMPLETE","TOTAL_STREAM_RECIEVED","totalCount")
# merge with spread data
dfIndiaEventCountSpread = inner_join(dfIndiaEventCountSpread , dfIndiaTotalEventCountSpread, by = c("masked_user_id","eventDate"))


# create user reading frequency per month

dfIndiaTotalEventFreq = as.data.frame( dfIndia %>% group_by(masked_user_id,EventMonth_Yr) %>% count() )
dfIndiaTotalEventFreq$n = dfIndiaTotalEventFreq$n / 30
dfIndiaTotalEventFreqSpread = spread(dfIndiaTotalEventFreq,EventMonth_Yr,n,fill=0)
dfIndiaTotalEventFreqSpread$avgReadRate = apply(dfIndiaTotalEventFreqSpread[,-1],1,sum)
dfIndiaTotalEventFreqSpread$avgReadRate = dfIndiaTotalEventFreqSpread$avgReadRate /12
summaryVal = summary(dfIndiaTotalEventFreqSpread$avgReadRate)
dfIndiaTotalEventFreqSpread$group = bin(dfIndiaTotalEventFreqSpread$avgReadRate,3, labels = c("low","medium","high"))

dfIndiaTotalEventFreqSpread = dfIndiaTotalEventFreqSpread %>%
   mutate(userGroupLamda = case_when(group == "low"~ 1/4,
                                     group ==  "medium" ~ 1/8,
                                     group == "high" ~  1/12 ))

str(dfIndiaTotalEventFreqSpread)
dfIndiaEventCountSpread = inner_join(dfIndiaEventCountSpread ,dfIndiaTotalEventFreqSpread, by = c("masked_user_id"))



# segement user groups and assign corresponding lamda value
#groupUsers = dfIndiaEventCountSpread  %>% group_by(masked_user_id)%>% count(totalRead = sum(totalCount)) %>% 
#     mutate(userGroupLamda = case_when(totalRead <= 2000 ~ 1/3,
#                                       totalRead < 3000 ~ 1/6,
#                                       totalRead < 4000 ~ 1/9,
#                                       totalRead >= 4000 ~ 1/12 ))

#groupUsers$n = NULL
# merge groupUsers informations
#dfIndiaEventCountSpread = inner_join(dfIndiaEventCountSpread , groupUsers, by = c("masked_user_id"))

 


# create age of event column

streams = dfIndia %>% select(deck_id) %>% unique()

recDate = as.Date("2020-04-30")
recData =readRDS("D:/NUS/FYP/data/recData")
recData = recData[rep(seq_len(nrow(recData)), each = nrow(streams)), ]
recData$deck_id = streams$deck_id




for ( i in 1:30)

{
   
recDate = recDate + 1
print(paste("running model for " , recDate))
# calculate age of the event in months
dfIndiaEventCountSpread$eventAge =  as.numeric( difftime( recDate, dfIndiaEventCountSpread$eventDate, units = "days" ))  


# make event age positive
#dfIndiaEventCountSpread$eventAge = ifelse(dfIndiaEventCountSpread$eventAge>0,dfIndiaEventCountSpread$eventAge,1)

# calculate interest score


#dfIndiaEventCountSpread$interestScore  = (100 * dfIndiaEventCountSpread$STREAM_COMPLETE/dfIndiaEventCountSpread$eventAge) + 
#   (50  * dfIndiaEventCountSpread$STREAM_RECIEVED/dfIndiaEventCountSpread$eventAge)


#dfIndiaEventCountSpread$interestScore  = (1 * dfIndiaEventCountSpread$STREAM_COMPLETE  ) + (0.5  * dfIndiaEventCountSpread$STREAM_RECIEVED  )

#dfIndiaEventCountSpread$interestScore  = ( (1 * dfIndiaEventCountSpread$STREAM_COMPLETE  ) / dfIndiaEventCountSpread$eventAge) + 
#                                          ((0.5  * dfIndiaEventCountSpread$STREAM_RECIEVED  ) /dfIndiaEventCountSpread$eventAge)
                                         

#dfIndiaEventCountSpread$interestScore  = ( (1 * dfIndiaEventCountSpread$STREAM_COMPLETE  ) * dfIndiaEventCountSpread$decay) + 
#   ((0.5  * dfIndiaEventCountSpread$STREAM_RECIEVED  ) * dfIndiaEventCountSpread$decay )

dfIndiaEventCountSpread$STREAM_COMPLETE_WEIGHTAGE = ifelse(dfIndiaEventCountSpread$TOTAL_STREAM_COMPLETE != 0, dfIndiaEventCountSpread$STREAM_COMPLETE / dfIndiaEventCountSpread$TOTAL_STREAM_COMPLETE,0)
dfIndiaEventCountSpread$STREAM_RECIEVED_WEIGHTAGE = ifelse(dfIndiaEventCountSpread$TOTAL_STREAM_RECIEVED != 0, dfIndiaEventCountSpread$STREAM_RECIEVED / dfIndiaEventCountSpread$TOTAL_STREAM_RECIEVED,0)

dfIndiaEventCountSpread$interestScore = ( exp(-0.5 * dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_COMPLETE )  +
                                      ( exp(-1 *  dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_RECIEVED)


#dfIndiaEventCountSpread$interestScore = ( exp(-0.5 * dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_COMPLETE_WEIGHTAGE )  +
#   ( exp(-1 *  dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_RECIEVED_WEIGHTAGE)

#dfIndiaEventCountSpread$interestScore = ( exp(-0.5 * dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_COMPLETE )  +
#                                      ( exp(-1 *  dfIndiaEventCountSpread$userGroupLamda *dfIndiaEventCountSpread$eventAge) * dfIndiaEventCountSpread$STREAM_RECIEVED )
#dfIndiaEventCountSpread$interestScore = NULL
print("interest score")



#dfIndiaEventCountSpread$interestScore =  ( 1 * dfIndiaEventCountSpread$STREAM_COMPLETE  *  2 / ( 1+ exp(lamda * dfIndiaEventCountSpread$eventAge))) + 
#   ( 0.5  * dfIndiaEventCountSpread$STREAM_RECIEVED *  2 / ( 1+ exp(lamda * dfIndiaEventCountSpread$eventAge)))  


#dfIndiaEventCountSpread$interestScore  =  lamda * 1 * dfIndiaEventCountSpread$STREAM_COMPLETE + lamda  * 0.5 * dfIndiaEventCountSpread$STREAM_RECIEVED 
#dfIndiaEventCountSpread$interestScore  = (1 * dfIndiaEventCountSpread$STREAM_COMPLETE + 1/dfIndiaEventCountSpread$eventAge) + 
#                                         (0.5  * dfIndiaEventCountSpread$STREAM_RECIEVED + 1/dfIndiaEventCountSpread$eventAge)

 
# scale event score
#dfIndiaEventCountSpread$interestScoreScaled = rescale(dfIndiaEventCountSpread$interestScore, to=c(0,10))
#dfIndiaEventCountSpread$interestScoreScaled = scale(dfIndiaEventCountSpread$interestScore)
 
#saveRDS(dfIndiaEventCountSpread,"D:/NUS/FYP/data/dfIndiaEventCountSpread")
#dfIndiaEventCountSpread = readRDS("D:/NUS/FYP/data/dfIndiaEventCountSpread")

# filter user id 's having < 30 records


t = dfIndiaEventCountSpread %>%  filter(dfIndiaEventCountSpread$eventDate >= "2019-01-01") %>% group_by(masked_user_id) %>%
   count(totalrec = n()) %>% filter(totalrec >50) 


#filter out records previous to year 2019
dfIndiaEventCountSpread = dfIndiaEventCountSpread %>% filter(masked_user_id %in% t$masked_user_id & eventDate >= "2019-01-01")

# create model data format
print("create model data format")
dfInTrain  = dfIndiaEventCountSpread  %>% filter(eventDate < recDate ) %>% 
   select(masked_user_id,deck_id,interestScore,eventDate) %>% group_by(masked_user_id,deck_id)%>% count(interestScore = sum(interestScore ))


# remove the count
dfInTrain$n = NULL



# removed scaling since not able to proper scaling betweeen test and train
#dfInTrain$interestScore = rescale(dfInTrain$interestScore, to=c(0,10))



#dfInTrain$interestScoreScaled  = rescale(dfInTrain$ , to=c(0,10))

dfInTest  = dfIndiaEventCountSpread  %>% filter(eventDate == recDate ) %>% 
   select(masked_user_id,deck_id,interestScore) %>% group_by(masked_user_id,deck_id)%>% count(interestScore = sum(interestScore ))

   
# remove the count
dfInTest$n = NULL

# removed scaling since not able to proper scaling betweeen test and train
#dfInTest$interestScore = rescale(dfInTest$interestScore, to=c(0,10))



# model building
print("model building")

set.seed(123) # This is a randomized algorithm
trainset = data_memory(dfInTrain$masked_user_id,dfInTrain$deck_id,dfInTrain$interestScore,index1 = T)
testset = data_memory(dfInTest$masked_user_id,dfInTest$deck_id,dfInTest$interestScore,index1 = T)
streamModel = Reco()
opts = streamModel$tune(trainset, opts = list(dim = c(30,40,50), lrate = c(0.05, 0.1, 0.2),
                                    costp_l1 = 0, costq_l1 = 0, 
                                    nthread = 4, niter = 10))

#saveRDS(opts, "D:/NUS/FYP/data/opts.rds")
#opts = readRDS("D:/NUS/FYP/data/opts.rds")

# train model

streamModel$train(trainset, opts = c(opts$min, nthread = 4, niter = 100))
 
#saveRDS(streamModel,"D:/NUS/FYP/data/streamModel.rds")
#streamModel=readRDS("D:/NUS/FYP/data/streamModel.rds")
#----------------#

# model testing logic
print("model testing")

trainUsers = dfInTrain[,1] %>% unique()
testUsers = dfInTest %>% filter(masked_user_id %in% trainUsers$masked_user_id) %>%  group_by(masked_user_id) %>% count() %>% filter(n>=5) 

# get user assessment data of recommended date
dfInAssesmentTest  = dfInAssesments %>% filter(eventDate == recDate ) %>% 
   select(masked_user_id,question_id) %>% group_by(masked_user_id,question_id)%>% count()
 
for (user in testUsers$masked_user_id)
{
   viewCount = testUsers [ testUsers$masked_user_id == user,2]
   recData[recData$masked_user_id != user,1] = user
   recDatatestset = data_memory(recData$masked_user_id,recData$deck_id,index1 = T)
   recData$predicted =  streamModel$predict(recDatatestset,out_memory())
   recStreams =  recData %>% arrange(desc(recData$predicted)) %>% head(50) %>% select("deck_id")  
   userViewedStreams = dfInTest %>% filter(masked_user_id == user) %>% select("deck_id")  
   
   match =   userViewedStreams %>% filter(deck_id %in% recStreams$deck_id)  
   matchper = (nrow(match) /viewCount) * 100
 
   streamsRec = rbind(streamsRec, recData %>% arrange(desc(recData$predicted)) %>% head(50) %>% select(masked_user_id,deck_id,predicted)  )
   rec1 = data.frame(user = user,
                     viewedContent=paste0(userViewedStreams$deck_id, collapse=","),
                     recommended=paste0(recStreams$deck_id, collapse=","),
                     viewcount=viewCount$n,
                     matchcount=nrow(match),
                     matchpercentage= matchper$n,
                     recdate = paste("O",as.character(recDate)))
 
   modeleval = rbind(modeleval,rec1)
   print(rec1)


   # recquestions using stream to question tags relationship
   recQuestions = streamToQuestionRel %>% filter(deck_id %in% recStreams$deck_id)%>% group_by(question_id) %>% 
      count(total =sum(rating)) %>% arrange(desc(total)) %>% head(50) %>% select(question_id,total)
   recQuestions$masked_user_id = user
   colnames(recQuestions) = c("deck_id","predicted","masked_user_id")
   # get user viewed questions
   userViewedQuestions= dfInAssesmentTest %>% filter(masked_user_id == user) %>% select("question_id")  
   matchQuestion =   userViewedQuestions %>% filter(question_id %in%  recQuestions$deck_id)  
   matchQuestionper = (nrow(matchQuestion) /nrow(userViewedQuestions)) * 100
   matchQuestionper  = ifelse(is.numeric(matchQuestionper), matchQuestionper,0)
   QuestionRecEval = data.frame(user = user,
                     viewedContent=paste0(userViewedQuestions$question_id, collapse=","),
                     recommended=paste0( recQuestions$deck_id, collapse=","),
                     viewcount=nrow(userViewedQuestions),
                     matchcount=nrow(matchQuestion ),
                     matchpercentage= matchQuestionper,
                     recdate = paste("",as.character(recDate)))
   
   QuestionModelEval= rbind(QuestionModelEval,QuestionRecEval)    
   questionsRec = rbind(questionsRec,recQuestions)
}

}
 
}
write.csv(recData$deck_id,"D:/NUS/FYP/data/r1.csv")

#modeleval %>% select(user,viewcount,matchcount, matchpercentage,recdate)
write.csv(modeleval,file="D:/NUS/FYP/data/modelevalexprecaug21.csv")
write.csv(QuestionModelEval,file="D:/NUS/FYP/data/QuestionModelEvalexprecaug21.csv") 


saveRDS(questionsRec,"D:/NUS/FYP/data/questionsRecIN")
saveRDS(questionsRec,"D:/NUS/FYP/data/streamsRecIN")

#streamrec = read.csv("D:/NUS/FYP/data/streamrec.csv")
#str(streamrec)
#streamrecUsers = streamrec %>% filter(recdate == " 2020-05-01") %>% select(user,recommended)

# current 





# old code

# questions
dfAssesments = fread("D:/NUS/FYP/data/user_assessments.gz")

dfAssesments$country = as.factor(dfAssesments$country)
dfAssesments$question_id = as.factor(dfAssesments$question_id)
dfAssesments$masked_user_id = as.factor(dfAssesments$masked_user_id)
dfAssesments$question_tags = as.factor(dfAssesments$question_tags)
# convert timestamp into date time format
dfAssesments$eventDateTime = as_datetime(dfAssesments$submission_utc_ts,format="%Y-%m-%d %H:%M:%S.000")
# create new column  event date
dfAssesments$eventDate = as.Date(substr(dfAssesments$eventDateTime,1,10))
# create new column  event time
dfAssesments$eventTime = substr(dfAssesments$eventDateTime,12,22)

dfAssesments = dfAssesments %>%  unique()

# remove duplicates based on specific column
dfAssesments = dfAssesments[!duplicated(dfAssesments[,c("masked_user_id","question_id","eventDateTime")]),]

saveRDS(dfAssesments,"D:/NUS/FYP/data/dfAssesments")
dfAssesments = readRDS("D:/NUS/FYP/data/dfAssesments")

summary(dfAssesments)

dfInAssesments = dfAssesments %>% filter(country == country) %>% select(masked_user_id,question_id,question_tags,eventDateTime,eventDate,no_of_trials,points_earned)
summary(dfInAssesments)

saveRDS(dfInAssesments,"D:/NUS/FYP/data/dfInAssesments")
dfInAssesments = readRDS("D:/NUS/FYP/data/dfInAssesments")



questions = dfInAssesments %>% select(question_id) %>% unique()
recAssesmentData = head(dfInAssesmentTrain ,921)
recAssesmentData$question_id = questions$question_id
saveRDS(recAssesmentData,"D:/NUS/FYP/data/recAssesmentData")
recAssesmentData =readRDS("D:/NUS/FYP/data/recAssesmentData")

modelAssesmenteval =  data.frame(user=character(),
                        viewedContent=character(), 
                        recommended=character(), 
                        viewcount=numeric(),
                        matchcount=numeric(),
                        matchpercentage=numeric(),
                        stringsAsFactors=FALSE)

recDate = as.Date("2020-04-30")

# model building

dfInAssesmentsSpread = dfInAssesments %>% select(masked_user_id,question_id,eventDate,no_of_trials) %>% 
                           group_by(masked_user_id,question_id,eventDate) %>% count(viewCount=sum(no_of_trials))


# filter user id 's having < 30 records
filterUser = dfInAssesmentsSpread %>%  filter(eventDate >= "2019-01-01") %>% group_by(masked_user_id) %>%
   count(totalrec = n()) %>% filter(totalrec >30) 


#filter out records previous to year 2019
dfInAssesmentsSpread = dfInAssesmentsSpread %>% filter(masked_user_id %in% filterUser $masked_user_id & eventDate >= "2019-01-01")

summary(dfInAssesmentsSpread)


for ( i in 1:30)
   
{
   
   recDate = recDate + 1
   print(paste("running model for " , recDate))

   dfInAssesmentsSpread$eventAge =  as.numeric( difftime( recDate, dfInAssesmentsSpread$eventDate, units = "days" ))  
   
   lamda = 1/120

   #dfInAssesmentsSpread$interestScores = exp(1 * lamda *  dfInAssesmentsSpread$viewCount*dfInAssesmentsSpread$eventAge)
   dfInAssesmentsSpread$interestScore  =  ( 2/(1+ exp( lamda * dfInAssesmentsSpread$eventAge)) ) * dfInAssesmentsSpread$viewCount
   
   summary(dfInAssesmentsSpread)
   saveRDS(dfInAssesmentsSpread,"D:/NUS/FYP/data/dfInAssesmentsSpread")
   dfInAssesmentsSpread = readRDS("D:/NUS/FYP/data/dfInAssesmentsSpread")

   # create model data format

   dfInAssesmentTrain  = dfInAssesmentsSpread  %>% filter(eventDate < recDate ) %>% 
   select(masked_user_id,question_id,interestScore,eventDate) %>% group_by(masked_user_id,question_id)%>% count(interestScore = sum(interestScore ))

   # remove the count
   dfInAssesmentTrain$n = NULL

   dfInAssesmentTrain[dfInAssesmentTrain$interestScore == Inf,3] = 1.21e251

   summary(dfInAssesmentTrain)
   
   
   dfInAssesmentTest  = dfInAssesmentsSpread  %>% filter(eventDate == recDate ) %>% 
      select(masked_user_id,question_id,interestScore) %>% group_by(masked_user_id,question_id)%>% count(interestScore = sum(interestScore ))
   

   # remove the count
   dfInAssesmentTest$n = NULL
   
   
   summary(dfInAssesmentTest)


   # model building
   
   
   set.seed(123) # This is a randomized algorithm
   trainAssesmentset = data_memory(dfInAssesmentTrain$masked_user_id,dfInAssesmentTrain$question_id,dfInAssesmentTrain$interestScore,index1 = T)
   testAssesmentset = data_memory(dfInAssesmentTest$masked_user_id,dfInAssesmentTest$question_id,dfInAssesmentTest$interestScore,index1 = T)
   rAssesment = Reco()
   optsAssesment = rAssesment$tune(trainset, opts = list(dim = c(30,40,50), lrate = c(0.05, 0.1, 0.2),
                                       costp_l1 = 0, costq_l1 = 0, 
                                       nthread = 4, niter = 10))
   optsAssesment$min
   
   saveRDS(optsAssesment, "D:/NUS/FYP/data/optsAssesment.rds")
   optsAssesment= readRDS("D:/NUS/FYP/data/optsAssesment.rds")
   
   # train model
   
   rAssesment$train(trainset, opts = c(optsAssesment$min, nthread = 4, niter = 100))
   
   
   #----------------#
   
   # model testing logic
   
   testAssesmentUsers = dfInAssesmentTest %>% group_by(masked_user_id) %>% count() %>% filter(n>=5) 
   
   for (user in testAssesmentUsers$masked_user_id)
   {
      viewCount = testAssesmentUsers [ testAssesmentUsers$masked_user_id == user,2]
      recAssesmentData[recAssesmentData$masked_user_id != user,1] = user
      recAssesmentDatatestset = data_memory(recAssesmentData$masked_user_id,recAssesmentData$question_id,index1 = T)
      recAssesmentData$predicted =  rAssesment$predict(recAssesmentDatatestset,out_memory())
      recQuestions =  recAssesmentData %>% arrange(desc(recAssesmentData$predicted)) %>% head(50) %>% select("question_id")
      userViewedQuestions = dfInAssesmentTest %>% filter(masked_user_id == user) %>% select("question_id")
      print(paste("user data for : ",user))
     
      match =     userViewedQuestions %>% filter(question_id %in% recQuestions$question_id)  
      matchper = (nrow(match) /viewCount) * 100
      
      rec1 = data.frame(user = user,
                        viewedContent=paste0(userViewedQuestions $question_id, collapse=","),
                        recommended=paste0(recQuestions$question_id, collapse=","),
                        viewcount=viewCount$n,
                        matchcount=nrow(match),
                        matchpercentage= matchper$n,
                        recdate = as.character(recDate))
      print(userViewedQuestions)
      print(rec1)
      
      modelAssesmenteval = rbind(modelAssesmenteval,rec1)
      
   }

}



write.csv( modelAssesmenteval,"D:/NUS/FYP/data/modelAssesmenteval.csv")



# recommendar lab
tagsDataMerged$rating = 1
dim(tagsDataMerged)
users <- as(tagsDataMerged, "binaryRatingMatrix")

View(users)
scheme <- evaluationScheme(data = users, method = "split", train = 0.9, given=-1)

# select one of UBCF or IBCF
st=Sys.time(); rec <- Recommender(getData(scheme,"train"),"UBCF",parameter=list(method="jaccard")); Sys.time()-st
st=Sys.time(); rec <- Recommender(getData(scheme,"train"),"IBCF",parameter=list(method="jaccard")); Sys.time()-st
# note : cannot do predict - no ratings to predict, so below is no good
st=Sys.time(); preds <- predict(rec, getData(scheme, "known"), type="ratingMatrix"); Sys.time()-st 

str(preds)
predictUser<- c(1)
x2 = predict(rec,predictUser,type="topNList", n=20)

as(x2 , "list")

as(users,"list")
str(x2)


preds["012b9349"]


# topic modelling
#create DTM
library(lda)
library(textmineR)
dtm <- CreateDtm(viewINDataTags$text, 
                 doc_names = viewINDataTags$deck_id , 
                 ngram_window = c(1, 2))


m <- FitLdaModel(dtm = dtm, k = 20, iterations = 500)
summary(m)
predict(m,"tag-e34b589d",iterations = 2)

library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# 
# clarification
# dfView %>% filter(eventDate == '1970-01-14') data needs to be removed
