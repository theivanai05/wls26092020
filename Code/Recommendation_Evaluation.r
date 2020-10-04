#============================================================================================================   
#Appending the Answers back to the Test Data Set 
#Creating Data Frame of multiple variable/column using Vector  
#============================================================================================================     

#New Day 1 Questions answered Data Set  
# colnames(ASSES_PS_EVAL_GB_28)
# [1] "question_id"       "country"           "org_id"            "role_id"           "submission_utc_ts" "no_of_trials"      "points_earned"    
# [8] "masked_user_id"    "tag1"              "tag2"              "tag3"              "tag4"              "tag5"              "tag6"             
# [15] "tag7"   
#View(Qns_Reco_Day1_2QPU)

require(tidyverse)

SYNTHETIC_activity_data <- create_activity_data_from_recomm_questions(Qns_Reco_Day5_2QPU,recDate)

names(SYNTHETIC_activity_data)[names(SYNTHETIC_activity_data) == "question_tags"] <- "tag1"
colnames_df <- t(t(colnames(ASSES_PS_EVAL_GB_28)))
setcolorder(SYNTHETIC_activity_data, colnames_df)
SYNTHETIC_activity_data[,c("rating","qns_ans")] <- NULL

#Adding observations using rbind() function  
#Df.day1assessment_w_ans<- rbind(te_assess_dt_7_GB_org28[,-c(16,17,18)], Df.day1assessment)   
#Df.day1assessment_w_ans_TR<- rbind(tr_assess_dt_7_GB_org28[,-c(16,17,18)], Df.day1assessment)
#Df.day1assessment_w_ans_TR<- rbind(ASSES_PS_EVAL_GB_28, Df.day1assessment)
#Df.day2assessment_w_ans_TR<- rbind(Df.day1assessment_w_ans_TR, Df.day2assessment)
#Df.day3assessment_w_ans_TR<- rbind(Df.day2assessment_w_ans_TR, Df.day3assessment)
#Df.day4assessment_w_ans_TR<- rbind(Df.day3assessment_w_ans_TR, Df.day4assessment)
#View(Df.day1assessment_w_ans_TR %>% filter( masked_user_id %in% Demo_UsersDay0))
Df.day5assessment_w_ans_TR<- rbind(Df.day4assessment_w_ans_TR,SYNTHETIC_activity_data)


# ==> Day1 END of Day Pulse Score Creation <=  

# model testing logic

recDate = as.Date("2020-05-03")

View(recAssesmentDatatestset %>% filter(eventDate == recDate))
View(SYNTHETIC_activity_data %>% filter(answer_date == recDate))

modelAssesmenteval =  data.frame(user=character(),
                                 viewedContent=character(), 
                                 recommended=character(), 
                                 viewcount=numeric(),
                                 matchcount=numeric(),
                                 matchpercentage=numeric(),
                                 stringsAsFactors=FALSE)

modelTageval =  data.frame(user=character(),
                           viewedContent=character(), 
                           recommended=character(), 
                           viewcount=numeric(),
                           matchcount=numeric(),
                           matchpercentage=numeric(),
                           stringsAsFactors=FALSE)

testAssesmentUsers =  te_assess_dt_7_GB_org28 %>% group_by(masked_user_id) %>% count() #%>% filter(n>=5) 
testAssesmentDataset = te_assess_dt_7_GB_org28
recAssesmentDatatestset = SYNTHETIC_activity_data

#recAssesmentDatatestset %>% filter(masked_user_id == "108ae76d") %>% select("question_id")
#testAssesmentDataset%>% filter(masked_user_id == "108ae76d") %>% stack(c("tag1","tag2","tag3","tag4")) %>% na.omit() %>% select("values") %>% rename("question_tags")

unique(recAssesmentDatatestset %>% arrange(desc(recAssesmentDatatestset$masked_user_id == "108ae76d" )) %>% select("question_tags") %>% na.omit())
View(recAssesmentDatatestset)

recAssesmentDatatestset[recAssesmentDatatestset$masked_user_id != user,1] = user

for (user in testAssesmentUsers$masked_user_id){
  viewCount = testAssesmentUsers [ testAssesmentUsers$masked_user_id == user,2]
  recAssesmentDatatestset[recAssesmentDatatestset$masked_user_id != user,1]
  #recAssesmentDatatestset = data_memory(recAssesmentData$masked_user_id,recAssesmentData$question_id,index1 = T)
  #recAssesmentData$predicted =  rAssesment$predict(recAssesmentDatatestset,out_memory())
  recQuestions =  unique(recAssesmentDatatestset %>% arrange(desc(recAssesmentDatatestset$masked_user_id == user )) %>% select("question_id"))
  userViewedQuestions = unique(testAssesmentDataset%>% filter(masked_user_id == user) %>% select("question_id"))
  recTags =  unique(recAssesmentDatatestset %>% arrange(desc(recAssesmentDatatestset$masked_user_id == user )) %>% select("question_tags") %>% na.omit())
  userViewedTags = unique(testAssesmentDataset%>% filter(masked_user_id == user) %>% stack(c("tag1","tag2","tag3","tag4")) %>% na.omit() %>% select("values") %>% rename("question_tags" = "values"))
  
  #print(paste("user data for : ",user))
  
  match_q =     userViewedQuestions %>% filter(question_id %in% recQuestions$question_id)  
  matchper_q = (nrow(match_q) /viewCount) * 100
  
  match_t =     userViewedTags %>% filter(question_tags%in% recTags$question_tags)  
  matchper_t = (nrow(match_t) /viewCount) * 100
  
  rec1 = data.frame(user = user,
                    viewedContent_q=paste0(userViewedQuestions $question_id, collapse=","),
                    recommended_q=paste0(recQuestions$question_id, collapse=","),
                    viewcount=viewCount$n,
                    matchcount_qns=nrow(match_q),
                    matchpercentage_qns= matchper_q$n,
                    recdate = as.character(recDate))
  
  rec2 = data.frame(user = user,
                    viewedContent_t=paste0(userViewedTags$question_tags, collapse=","),
                    recommended_t=paste0(recTags$question_tags, collapse=","),
                    viewcount=viewCount$n,
                    matchcount_tag=nrow(match_t),
                    matchpercentage_tag= matchper_t$n,
                    recdate = as.character(recDate))
  #print(userViewedQuestions)
  #print(rec1)
  
  modelAssesmenteval = rbind(modelAssesmenteval,rec1)
  modelTageval = rbind(modelTageval,rec2)
} 
print(paste("Completed Processing !!"))

write.csv( modelAssesmenteval,"D:/NUS/FYP/data/modelAssesmenteval.csv")



