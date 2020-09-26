#Looking at what to recommend to each user : 

#Input from Recosystem_V2.r 
#colnames(test)
#[1] "userid" "qtag" "pulsescore" "prediction" "MAE"   
test_pred_V2 = test_UI  #%>% filter(test$userid == "002c7b9d") 

#####3) Understanding the Master Users 
#Country_User_M = unique(master_dt[,c("country","masked_user_id")])
# Total Users in the system 
Users_M = unique(Country_User_M[,c("masked_user_id")])
# ==> Users_M and Country_User_M has the same list of users; Thus It gives evidence that all Users are Unique to 1 country only. 

# Pulling in the Bottom 100 recommendations based on prediction for each user ## Would not work as there are not too many Tags for each users 
#tag_recommend = top_n(test_pred_V2,-100,test_pred_V2$prediction)
#tag_recommend = tag_recommend["qtag"]


#have put in a value of 0.99 assuming that a user with that value will be a good performer
tag_recommend  = test_pred_V2 %>% filter(test_pred_V2$prediction <= '0.99') 
#rm(test_pred_V2)

#Given the Tags Recommended; Pull out the questions for the tags
## Pulling out Deck IDs for the Corresponding TAGS recommended.   

#### Left Join using merge function by Tags and USERID 
df = merge(x=assess_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.1","masked_user_id"),by.y=c("qtag","masked_user_id"))
df = df[,c("question_tags.1","question_id","country","prediction","masked_user_id")] 
# Rename column names
names(df)[names(df) == "question_tags.1"] <- "qtag"

df2 = merge(x=assess_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.2","masked_user_id"),by.y=c("qtag","masked_user_id"))
df2 = df2[,c("question_tags.2","question_id","country","prediction","masked_user_id")]
names(df2)[names(df2) == "question_tags.2"] <- "qtag"

df3 = merge(x=assess_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.3","masked_user_id"),by.y=c("qtag","masked_user_id"))
df3 = df3[,c("question_tags.3","question_id","country","prediction","masked_user_id")]
names(df3)[names(df3) == "question_tags.3"] <- "qtag"

df4 = merge(x=assess_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.4","masked_user_id"),by.y=c("qtag","masked_user_id"))
df4 = df4[,c("question_tags.4","question_id","country","prediction","masked_user_id")]
names(df4)[names(df4) == "question_tags.4"] <- "qtag"

#this is the set of questions recommended for a particular User for a set of 4 Tags... 
qns_recommended = data.table(rbind(df,df2,df3,df4))

## For the TAGS, Need to pick up Questions ANswered Wrongly or Not answered yet 
## Input Required What has the User ANswered and Wrongly Answered  -- DONE 
## for Reduction in Questions that is to be recommended...  -- DONE 

####1)  Questions ANswered by Users D.T = u_q_t_M

# ==> Pass1 : Questions not yet answered 
#u_q_t_M %>% filter( masked_user_id == "002c7b9d" & qns_ans == 0)
# ==> Pass2 : Questions already answered
  #qns_answered_by_users = u_q_t_M %>% filter( qns_ans == 1)
#qns_answered_by_user = u_q_t_M %>% filter( masked_user_id == "002c7b9d" & qns_ans == 1)
  #qns_answered_by_users = data.table(qns_answered_by_users[,c("masked_user_id","question_id")])
#    masked_user_id question_id no_of_trials points_earned qns_ans

# ==> Rate of Questions answered to not answered 
# ==> Not yet relevant Here


#Final User unanswered list of questions 
  #USER_Qns_Reco = qns_recommended[!qns_answered_by_users, on=c("masked_user_id","question_id")]   

#Users' Country of residence... 
#Country_User_M %>% filter( Country_User_M$masked_user_id == "002c7b9d")

#Recommendation Based on the Country for a Single User: 
#Change Column Names 
#names(USER_Qns_Reco)[names(USER_Qns_Reco) == "userid"] <- "masked_user_id"

USER_Qns_Reco = unique(merge(qns_recommended, Country_User_M,by = c("masked_user_id","country")) )


#Pull Out the TAG_STREAM Master table
#For the choosen Question Tags (QTAG), pull out the corresponding Stream Tags That has yet to be viewed 
#Output : User Id ==> (DeckID)
#=> Decks Viewed by user == u_d_a_M
####2)  Streams Viewed by Users D.T = u_d_a_M
#View(u_d_a_M)

# ==> Pass1 : STREAMS not yet Viewed 
#u_d_a_M %>% filter( masked_user_id == "002c7b9d" & completed == 0)
# ==> Pass2 : STREAMS already completed
  #streams_view_by_user = u_d_a_M %>% filter( completed == 1)
#streams_view_by_user = u_d_a_M %>% filter( masked_user_id == "002c7b9d" & completed == 1)
  #streams_view_by_user = data.table(streams_view_by_user [,c("masked_user_id","deck_id")])

## Pulling out Deck IDs for the Corresponding TAGS recommended. Using another Method, this does not filter by Users..
  # sel.col <- c("question_tags.1","question_tags.2","question_tags.3","question_tags.4")
  # out.col <- c("deck_id","country","masked_user_id")
  # # DeckIds- Streams : for Tags Recommended  
  # stream_recommended = views_sear_tags_dt[views_sear_tags_dt[, Reduce(`|`, lapply(.SD, `%in%`, unlist(tag_recommend[,c("qtag")]))),.SDcols = sel.col],..out.col]
  # stream_recommended = unique(stream_recommended)
  #rm(stream_recommended)


#### Left Join using merge function by Tags and USERID 
dfs = merge(x=views_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.1","masked_user_id"),by.y=c("qtag","masked_user_id"))
dfs = dfs[,c("question_tags.1","deck_id","country","prediction","masked_user_id")] 
names(dfs)[names(dfs) == "question_tags.1"] <- "qtag" # Rename column names

df2s = merge(x=views_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.2","masked_user_id"),by.y=c("qtag","masked_user_id"))
df2s = df2s[,c("question_tags.2","deck_id","country","prediction","masked_user_id")]
names(df2s)[names(df2s) == "question_tags.2"] <- "qtag"

df3s = merge(x=views_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.3","masked_user_id"),by.y=c("qtag","masked_user_id"))
df3s = df3s[,c("question_tags.3","deck_id","country","prediction","masked_user_id")]
names(df3s)[names(df3s) == "question_tags.3"] <- "qtag"

df4s = merge(x=views_sear_tags_dt,y=tag_recommend,by.x=c("question_tags.4","masked_user_id"),by.y=c("qtag","masked_user_id"))
df4s = df4s[,c("question_tags.4","deck_id","country","prediction","masked_user_id")]
names(df4s)[names(df4s) == "question_tags.4"] <- "qtag"

#this is the set of questions recommended for a particular User for a set of 4 Tags... 
Stream_recommended = data.table(rbind(dfs,df2s,df3s,df4s))

#Final User Unviewed list of STREAMS 
  #USER_Stream_Reco = Stream_recommended[!streams_view_by_user, on=c("deck_id","masked_user_id")]  

# ==> Streams Data : 

#Recommendation Based on the Country & User: 
USER_Stream_Reco = merge(Stream_recommended, Country_User_M,by = c("masked_user_id","country")) 

USER_Qns_Reco = unique(setcolorder(USER_Qns_Reco, c("country","masked_user_id", "question_id", "prediction")))
USER_Stream_Reco = unique(setcolorder(USER_Stream_Reco, c("country","masked_user_id", "deck_id", "prediction")))

#removeing the qtags columns 
USER_Qns_Reco [,c("qtags")] <- NULL
USER_Stream_Reco [,c("qtags")] <- NULL

#Making the Recommendations Unique again : 
USER_Qns_Reco  = unique(USER_Qns_Reco)
USER_Stream_Reco = unique(USER_Stream_Reco)


#Setting WD for saving the values for recommendation GUI
  # setwd("~/Documents/NUS_EBAC")
  # 
  # write.csv(USER_Stream_Reco,"Data/UpSkill_Stream_Recommendation.csv")
  # write.csv(USER_Qns_Reco,"Data/UpSkill_Question_Recommendation.csv")

#Removing all of the unwanted DTs : 
rm(dfs,df2s,df3s,df4s,df,df2,df3,df4) 

#Fields for the Streams : 
#country, masked_user_id , deck_id , prediction
#Fields for the questions : 
#country, masked_user_id , question_id , prediction