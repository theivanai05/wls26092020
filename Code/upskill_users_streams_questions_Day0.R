## Working on the GB Data alond from PULSEMASTER 
#install.packages("reshape")
#library(reshape)

#GB Users alone : 
          # GB_USERS = Country_User_M %>% filter(country == "GB")
          # c = GB_USERS[,2]
          # 
          # ## testing if Users are availabkle in dataset: 
          # #0100f873
          #  
          # filter(GB_USERS,masked_user_id == "0b0ddba9")             # 1 record
          # filter(pulsescore_Master_All,masked_user_id == "0b0ddba9") # 0 records 
          # filter(pulsescore_Master_GB,masked_user_id == "0b0ddba9") # 44 records 
          # 
          # 
          # ## Pulling out the PULSESCORE from ALL Scores, only for GB 
          # merge(pulsescore_Master_All,pulsescore_Master_GB, by=c("masked_user_id")) ## Not working - No Rows returned 
          # merge(pulsescore_Master_All,GB_USERS, by=c("masked_user_id"))  #==> # Giving me 0 records 
          # merge(pulsescore_Master_GB,GB_USERS, by=c("masked_user_id"))  #==> # Giving me 24553 records 
          # 
          # # Users sued to Test ==> 0100f873 / 011973bf / 0b0ddba9
          # 
          # # Combining Pulsescore data with Other User infomration 
          # PS_User_data = unique(merge(pulsescore_Master_GB, master_dt[,c("masked_user_id","org_id","lang_code","role_id","country")], by = c("masked_user_id")))
          # 
          # 
          # ####1)  To Create a new variable to find out the points earned.. 
          # new_var <- "qns_ans"
          # assess_dt[,(new_var):=dplyr::case_when(
          #   qns_ans = points_earned == 10 ~ 1,
          #   qns_ans = points_earned == 5 ~ 1,
          #   qns_ans = points_earned == 0 ~ 0)]
          # 
          # 
          # ####2)  To Create  a new variable to find out the stream viewed.. 
          # new_var <- "completed"
          # views_dt[,(new_var):=dplyr::case_when(
          #   completed = action == "STREAM_COMPLETE" ~ 1,
          #   completed = action == "STREAM_RECIEVED" ~ 0)]
          # 
          # ## Mergin PS User Data with ASSESS_DT 
          # new_Assess_dt = merge(assess_dt, PS_User_data, on = c("masked_user_id","question_tags"))
          # new_Assess_dt[,c("X")] <- NULL # without the User Count Field 
          # new_Assess_woTime_dt = unique(new_Assess_dt[,-c("submission_utc_ts")]) # Without the submission timestamp


# Creating Recommended Question output

## After Recommendation, need to pull out the Questions now: 
Questions_Recommended_0 = merge(C_Q_Tag_M, Recommended_Recolabs_Tag_ALL, by= c("question_tags"),allow.cartesian=TRUE)

## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)

filter(u_q_t_M, qns_ans == "1")
filter(pulsescore_Master_GB_O28_DO, score != "0")
Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)


    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day1 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0)
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day1, key="masked_user_id")
    Qns_Reco_Day1_2QPU = d[, head(.SD, 2), by=masked_user_id]
    
    
    # Appending the Answers back to the Test Data Set 
    #Creating Data Frame of multiple variable/column using Vector  
    
    #New Day 1 Questions answered Data Set  
      # colnames(ASSES_PS_EVAL_GB_28)
      # [1] "question_id"       "country"           "org_id"            "role_id"           "submission_utc_ts" "no_of_trials"      "points_earned"    
      # [8] "masked_user_id"    "tag1"              "tag2"              "tag3"              "tag4"              "tag5"              "tag6"             
      # [15] "tag7"   
    View(Qns_Reco_Day1_2QPU)
    
    require(tidyverse)
    ASSES_PS_EVAL_GB_28[,c("masked_user_id","org_id","role_id")] %>% filter(masked_user_id %in% Demo_UsersDay0) %>% unique()
    Df.day1assessment <- data.frame(c(5983,9645,14193,14407,4110,4111,9528,9529,12440,13030,5975,5978,5975,5978,7230,7939,5974,5980,7232,9528),  
                                          c("GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB"),  
                                          c(28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28),  
                                          c(31,31,1,1,10,10,1,1,1,1,156,156,1,1,1,1,1,1,1,1),  
                                          c("2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000","2020-09-22 22:06:17.000"),
                                          c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                                          c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10),
                                          c("035f412b","035f412b","108ae76d","108ae76d","22408aad","22408aad","4ffee38a","4ffee38a","560d7304","560d7304","8100aef3","8100aef3","8f7b79fd","8f7b79fd","9bffe329","9bffe329","abd51bd2","abd51bd2","fe5e359d","fe5e359d"),
                                          c("tag-7c89cbcc","tag-dea47eb3","tag-361795b2","tag-46f9a6ce","tag-458d763a","tag-458d763a","tag-70dc242d","tag-70dc242d","tag-afb1fea3","tag-c10034dd","tag-7d4ba1fb","tag-7d4ba1fb","tag-7d4ba1fb","tag-7d4ba1fb","tag-ff8eb424","tag-7b806310","tag-a59f678e","tag-a59f678e","tag-dffbd82e","tag-70dc242d"),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
                                          c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "))     
    
    
    
    #pulling out the columns names of Assess DS
    colnames_df <- t(t(colnames(ASSES_PS_EVAL_GB_28)))
    #Naming the above Data Frame  
    names(Df.day1assessment) <- colnames_df 
      
      #View(te_assess_dt_7_GB_org28)
    
      #Adding observations using rbind() function  
    Df.day1assessment_w_ans<- rbind(te_assess_dt_7_GB_org28[,-c(16,17,18)], Df.day1assessment)   
    Df.day1assessment_w_ans_TR<- rbind(tr_assess_dt_7_GB_org28[,-c(16,17,18)], Df.day1assessment)
      #View(Df.day1assessment_w_ans)
    
    # ==> Day1 END of Day Pulse Score Creation <=  
    
