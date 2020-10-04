## Main File 

## Installing all of the Libraries required : 
  library(tidyverse)
  library(data.table)
  library(dplyr)
  library(reshape2)
  library(recommenderlab)
  library(devtools)
  library(lubridate)
  library(stringr)

  #Demo Users FOR GUI 
  Demo_UsersDay0 = c("fe5e359d","abd51bd2","9bffe329","8f7b79fd","8100aef3","560d7304","4ffee38a","22408aad","108ae76d","035f412b")
  #Test_USERS_GB
  
  
# Reading in the Master files from TeamStreamz
  # Date File Location 
  setwd("~/Documents/NUS_EBAC/Data") # data location of Mac
  #setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls20092020/Data") # location in Lenovo
  load("Base_DS_after_Filereads.RData")
  rm(pulsescore_Master_GB,pulsescore_Master_All) #assessment , views, 
  
# Coming back into current WD 
  setwd("~/wls26092020/wls26092020/")
  #setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls26092020")
  
# Sourcing the Underlying Functions 
  source("Code/underlyingfunctions.R")
  
# Sourcing and running the 
  source("Code/users_questions_marks_assessment_viewed.R")
   #u_q_t_M # Users, questions , Trial result master
   #u_d_a_m # Users, decks , actions master 
 
# Running the Pulse Score and Pre Processor 
  #Recommendation Date 
  recDate = as.Date("2020-05-01")
  source("Code/PS_Preprocessor.R")
  # o/p df_final_ps_score_set

# Churning out Recommendations
  source("Code/Recommendations_Latest.R")
  # o/p : Recommended_Tags
  
# Churning out the question and streams to be recommended 
  source("Code/upskill_users_streams_questions_Result.R")
  # o/p : Stream_Reco_Day_50S
  # o/p : Qns_Reco_Day_50Q
  
  # Output Evalution
  source("Code/Recommendation_Evaluation.r") 
  

  
  
  