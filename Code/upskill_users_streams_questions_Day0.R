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

## Creating the Question, COuntry, Tag Master
C_Q_Tag_M = unique(assess_dt %>% select("country","question_id","question_tags"))

#==============================================================================================================
#Day 1: Questions recommendation
#==============================================================================================================

## After Recommendation, need to pull out the Questions now: 
Questions_Recommended_1 = merge(C_Q_Tag_M, Recommended_Tags, by= c("question_tags"),allow.cartesian=TRUE)

## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
Questions_Recommended_1$masked_user_id <- as.factor(Questions_Recommended_1$masked_user_id)
Questions_Recommended_1$question_id <- as.factor(Questions_Recommended_1$question_id)
Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended_1,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)

#filter(u_q_t_M, qns_ans == "1")
#filter(pulsescore_Master_GB_O28_DO, score != "0")
Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)


    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day1 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    Qns_Reco_Day1$qns_ans[Qns_Reco_Day1$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day1$qns_ans[is.na(Qns_Reco_Day1$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day1$qns_ans[Qns_Reco_Day1$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day1 =   Qns_Reco_Day1 %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day1, key="masked_user_id")
    Qns_Reco_Day1_2QPU = d[, head(.SD, 2), by=masked_user_id]

    #==============================================================================================================
    #Day 2: Questions recommendation
    #==============================================================================================================
    
    ## After Recommendation, need to pull out the Questions now: 
    Questions_Recommended_2 = merge(C_Q_Tag_M, Recommended_Tags_Day1, by= c("question_tags"),allow.cartesian=TRUE)
    
    ## Updating questions that were answered 
    u_q_t_M = unique(Df.day1assessment_w_ans_TR[,c("masked_user_id","question_id", "no_of_trials","points_earned")])  #  "points_earned" 
    #View(u_q_t_M)
    
    ####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
    new_var <- "qns_ans"
    u_q_t_M[,(new_var):=dplyr::case_when(
      qns_ans = points_earned == 10 ~ 1,
      qns_ans = points_earned == 5 ~ 1,
      qns_ans = points_earned == 0 ~ 0)]
    
    
    ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
    Questions_Recommended_2$masked_user_id <- as.factor(Questions_Recommended_2$masked_user_id)
    Questions_Recommended_2$question_id <- as.factor(Questions_Recommended_2$question_id)
    Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended_2,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
    Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
    Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)
    
    #filter(u_q_t_M, qns_ans == "1")
    #filter(pulsescore_Master_GB_O28_DO, score != "0")
    Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
    setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)
    
    
    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day2 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    
    Qns_Reco_Day2$qns_ans[Qns_Reco_Day2$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day2$qns_ans[is.na(Qns_Reco_Day2$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day2$qns_ans[Qns_Reco_Day2$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day2 =   Qns_Reco_Day2 %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    
    unique(Qns_Reco_Day2$masked_user_id)
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day2, key="masked_user_id")
    Qns_Reco_Day2_2QPU = d[, head(.SD, 5), by=masked_user_id]

#==============================================================================================================
#Day 3: Questions recommendation
#==============================================================================================================
    
    ## After Recommendation, need to pull out the Questions now: 
    Questions_Recommended_3 = merge(C_Q_Tag_M, Recommended_Tags_Day2, by= c("question_tags"),allow.cartesian=TRUE)
    
    ## Updating questions that were answered 
    u_q_t_M = unique(Df.day2assessment_w_ans_TR[,c("masked_user_id","question_id", "no_of_trials","points_earned")])  #  "points_earned" 
    #View(u_q_t_M)
    
    ####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
    new_var <- "qns_ans"
    u_q_t_M[,(new_var):=dplyr::case_when(
      qns_ans = points_earned == 10 ~ 1,
      qns_ans = points_earned == 5 ~ 1,
      qns_ans = points_earned == 0 ~ 0)]
    
    
    ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
    Questions_Recommended_3$masked_user_id <- as.factor(Questions_Recommended_3$masked_user_id)
    Questions_Recommended_3$question_id <- as.factor(Questions_Recommended_3$question_id)
    Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended_3,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
    Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
    Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)
    
    #filter(u_q_t_M, qns_ans == "1")
    #filter(pulsescore_Master_GB_O28_DO, score != "0")
    Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
    setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)
    
    
    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day3 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    
    Qns_Reco_Day3$qns_ans[Qns_Reco_Day3$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day3$qns_ans[is.na(Qns_Reco_Day3$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day3$qns_ans[Qns_Reco_Day3$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day3 =   Qns_Reco_Day3 %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    
    unique(Qns_Reco_Day3$masked_user_id)
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day3, key="masked_user_id")
    Qns_Reco_Day3_2QPU = d[, head(.SD, 2), by=masked_user_id]
    
#==============================================================================================================
    #Day 4: Questions recommendation
#==============================================================================================================
    ## Creating the Question, COuntry, Tag Master
    df = melt(Df.day3assessment_w_ans_TR, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
    names(df)[10] = "question_tags"
    df = within(df, rm("variable"))
    
    C_Q_Tag_M = unique(df %>% select("country","question_id","question_tags"))
    
    ## After Recommendation, need to pull out the Questions now: 
    Questions_Recommended_4 = merge(C_Q_Tag_M, Recommended_Tags_Day3, by= c("question_tags"),allow.cartesian=TRUE)
    
    ## Updating questions that were answered 
    u_q_t_M = unique(Df.day3assessment_w_ans_TR[,c("masked_user_id","question_id", "no_of_trials","points_earned")])  #  "points_earned" 
    #View(u_q_t_M)
    
    ####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
    new_var <- "qns_ans"
    u_q_t_M[,(new_var):=dplyr::case_when(
      qns_ans = points_earned == 10 ~ 1,
      qns_ans = points_earned == 5 ~ 1,
      qns_ans = points_earned == 0 ~ 0)]
    
    
    ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
    Questions_Recommended_4$masked_user_id <- as.factor(Questions_Recommended_4$masked_user_id)
    Questions_Recommended_4$question_id <- as.factor(Questions_Recommended_4$question_id)
    Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended_4,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
    Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
    Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)
    
    #filter(u_q_t_M, qns_ans == "1")
    #filter(pulsescore_Master_GB_O28_DO, score != "0")
    Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
    setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)
    
    
    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day4 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    
    Qns_Reco_Day4$qns_ans[Qns_Reco_Day4$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day4$qns_ans[is.na(Qns_Reco_Day4$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day4$qns_ans[Qns_Reco_Day4$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day4 = Qns_Reco_Day4 %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    
    unique(Qns_Reco_Day4$masked_user_id)
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day4, key="masked_user_id")
    Qns_Reco_Day4_2QPU = d[, head(.SD, 50), by=masked_user_id]

#==============================================================================================================
    #Day 5: Questions recommendation
#==============================================================================================================
    ## Creating the Question, COuntry, Tag Master
    df = melt(Df.day4assessment_w_ans_TR, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
    names(df)[10] = "question_tags"
    df = within(df, rm("variable"))
    
    C_Q_Tag_M = unique(df %>% select("country","question_id","question_tags"))
    C_Q_Tag_M  = C_Q_Tag_M  %>% na_if(" ") %>% na.omit() 
    
    ## After Recommendation, need to pull out the Questions now: 
    Questions_Recommended_5 = merge(C_Q_Tag_M, Recommended_Tags_Day4, by= c("question_tags"),allow.cartesian=TRUE)
    
    ## Updating questions that were answered ; keeping on the the latest questions answered. 
    u_q_t_M = unique(Df.day4assessment_w_ans_TR[,c("masked_user_id","question_id", "no_of_trials","points_earned","submission_utc_ts")])  #  "points_earned" 
    u_q_t_M[, eval("answer_datetime"):=ymd_hms(u_q_t_M$submission_utc_ts)]
    u_q_t_M[, eval("answer_date"):=as_date(u_q_t_M$answer_datetime)]
    u_q_t_M[,c("submission_utc_ts")] <- NULL
    require(data.table)
    u_q_t_M <- data.table(u_q_t_M, key=c("masked_user_id","question_id"))
    setorder(u_q_t_M, masked_user_id,question_id,-answer_datetime)
    u_q_t_M = u_q_t_M[, head(.SD, 1), by=c("masked_user_id","question_id")]
    
    ####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
    new_var <- "qns_ans"
    u_q_t_M[,(new_var):=dplyr::case_when(
      qns_ans = points_earned == 10 ~ 1,
      qns_ans = points_earned == 5 ~ 1,
      qns_ans = points_earned == 0 ~ 0)]
    
    ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
    Questions_Recommended_5$masked_user_id <- as.factor(Questions_Recommended_5$masked_user_id)
    Questions_Recommended_5$question_id <- as.factor(Questions_Recommended_5$question_id)
    Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended_5,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
    Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned","answer_datetime","answer_date")] <- NULL
    Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)
    
    #filter(u_q_t_M, qns_ans == "1")
    #filter(pulsescore_Master_GB_O28_DO, score != "0")
    Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
    setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)
    
    
    #finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day5 = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    
    Qns_Reco_Day5$qns_ans[Qns_Reco_Day5$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day5$qns_ans[is.na(Qns_Reco_Day5$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day5$qns_ans[Qns_Reco_Day5$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day5 = Qns_Reco_Day5 %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    # Remove Duplicate Questions for same user 
    Qns_Reco_Day5 = Qns_Reco_Day5  %>% distinct(question_id, masked_user_id , .keep_all = TRUE)

    unique(Qns_Reco_Day5$masked_user_id)
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day5, key="masked_user_id")
    Qns_Reco_Day5_2QPU = d[, head(.SD, 50), by=masked_user_id]
    
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
    
    
    # #ASSES_PS_EVAL_GB_28[,c("masked_user_id","org_id","role_id")] %>% filter(masked_user_id %in% Demo_UsersDay0) %>% unique()
    # Df.day4assessment <- data.frame(c(2850,4112,12891,17831,16093,18580,14192,14585,12883,12897,15334,15334,12897,12887,22796,12891,2849,2850,14590,12881),  
    #                                       c("GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB"),  
    #                                       c(28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28),  
    #                                       c(31,31,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),  
    #                                       c("2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000","2020-09-30 16:21:01.000"),
    #                                       c(31,31,1,1,10,10,1,1,1,1,156,156,1,1,1,1,1,1,1,1),
    #                                       c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10),
    #                                       c("035f412b","035f412b","108ae76d","108ae76d","22408aad","22408aad","4ffee38a","4ffee38a","560d7304","560d7304","8100aef3","8100aef3","8f7b79fd","8f7b79fd","9bffe329","9bffe329","abd51bd2","abd51bd2","fe5e359d","fe5e359d"),
    #                                       c("sport","sport","kids","kids","environment","tools","accessories","auto","components","components","shoes","apparel","components","video","audio","kids","apparel","sport","kids","sport"),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "),
    #                                       c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "))     
    # 
    # Df.day3assessment <- data.frame(c(17847,17848,16453,16454,16063,16064,21648,21649,12890,12891,16461,16634),  
    #                                 c("GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB","GB"),  
    #                                 c(28,28,28,28,28,28,28,28,28,28,28,28),  
    #                                 c(31,31,1,1,1,1,1,1,1,1,1,1),  
    #                                 c("2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000","2020-09-29 16:21:01.000"),
    #                                 c(1,1,1,1,1,1,1,1,1,1,1,1),
    #                                 c(10,10,10,10,10,10,10,10,10,10,10,10),
    #                                 c("035f412b","035f412b","108ae76d","108ae76d","8f7b79fd","8f7b79fd","9bffe329","9bffe329","abd51bd2","abd51bd2","fe5e359d","fe5e359d"),
    #                                 c("tag-8277aa36","tag-8277aa36","tag-6bc64d6f","tag-6bc64d6f","tag-a22fadc8","tag-a22fadc8","tag-608fd563","tag-608fd563","tag-e442f041","tag-e442f041","tag-6bc64d6f","tag-6bc64d6f"),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "),
    #                                 c(" "," "," "," "," "," "," "," "," "," "," "," "))     
    # 
    # 
    
    #pulling out the columns names of Assess DS
      #colnames_df <- t(t(colnames(ASSES_PS_EVAL_GB_28)))
    #Naming the above Data Frame  
      #names(Df.day4assessment) <- colnames_df 
      
      #View(te_assess_dt_7_GB_org28)
    
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


    
