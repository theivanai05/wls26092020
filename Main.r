## Main File 

## Installing all of the Libraries required : 

  #install.packages("tidyverse")
  #install.packages("data.table")
  #install.packages("dplyr")
  #install.packages("reshape")
  #install.packages("recommenderlab")
  #install_github("mhahsler/recommenderlab")

  # -- Not Required #install.packages("Hmisc")
  # -- Not Used now # install.packages("recosystem")


  library(tidyverse)
  library(data.table)
  library(dplyr)
  library(reshape2)
  library(recommenderlab)
  library(devtools)
  library(lubridate)
  library(stringr)

  # -- Not Used Now #library(recosystem)
  # -- Not Required #library(R.utils)
  # -- Not Required #library(Hmisc)
  
## Setting Working Directory 
  # Current 
  #setwd("~/wls")
  
  
  ## New Data Directory - mac
     # "~/wls/Data"
  ## Code Directory 
     # "~/wls/Code"
  
# Reading in the Master files from TeamStreamz
  # Date File Location 
  setwd("~/Documents/NUS_EBAC/Data") # data location of Mac
  setwd("~/wls26092020/wls26092020/Data/Day1") # data location of Mac
  #setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls20092020/Data") # location in Lenovo
  

## Reading in the full set of Data ==>
  
#   # Pulling in Data from Trade Source  -- Run if you need a fresh COPY 
#   
#     assess_dt = data.table(read_csv("user_assessments.gz")) # !!##!! - Work on Consuming the Tag Set with the 4 Variables  
#     master_dt= data.table(read_csv("user_master.gz"))
#     views_dt= data.table(read_csv("views_model.gz"))        # !!##!! - Work on Consuming the Tag Set with the 4 Variables  
  
   # setwd("~/Documents/NUS_EBAC/Data/Tagged_W_Dinakar/") # data location of 7 Tags DS in  Mac      
   # load("assessments_with_tags.RData")    # !!##!! - Work on Consuming the Full Tag Set with the 7 Variables  
   # load("streams_with_tags.RData")        # !!##!! - Work on Consuming the FULL Tag Set with the 7 Variables  
   # assess_dt_7 = question_data6
   # views_dt_7 = stream_data4
   # rm(question_data6,stream_data4)
#     
# # Pulling in Data from Pulse Score
#   setwd("~/Documents/NUS_EBAC/Data/") # data location of 7 Tags DS in  Mac     
#     #pulsescore_Master = data.table(read.csv("master.csv",header=TRUE, sep= ",")) # India Data Set - Pulse Score Alone 
#   
#   #setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/Data/PS") # location for Combined PS
#   
#     pulsescore_Master_All = read.csv("PS/pulse_score_c.csv",header=TRUE, sep= ",") # All Country Data Set - Pulse Score Alone 
#     pulsescore_Master_GB = read.csv("PS/GB_pulse_score.csv",header=TRUE, sep= ",") # GB Data Set - Pulse Score Alone     
    
## End of Reading in Data 
    
  #save.image("Base_DS_after_Filereads.RData")
  load("Base_DS_after_Filereads.RData")
  # ==> RM to 2009 RDATA
  rm(pulsescore_Master_GB,pulsescore_Master_All)
  save.image("workspace_29092020.RData")
  load("workspace_29092020.RData")
  
## Running the Serialization Code for Attaching Tags into Views Data
  # Coming back into current WD 
  setwd("~/wls26092020/wls26092020/")
  #setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls26092020")
  
  # Sourcing the Underlying Functions 
  source("Code/underlyingfunctions.R")
  
  # Sourcing the Tagged Deck/ Views data set with Serialized Tags 
  #source("Code/asses_view_seareaz.R")
  
  # Sourcing and running the 
  source("Code/users_questions_marks_assessment_viewed.R")
   #u_q_t_M # Users, questions , Trial result master
   #u_d_a_m # Users, decks , actions master 
  
  # Running the Tag Recommender 
   ## source("Code/CS_WLS_Recosystem_V2.R") # Was used for the initial Recosystem PluseScore Prediction 
  source("Code/CS_WLS_RecommenderLabs_V1.r")
  
  # Churning out the question and streams to be recommended 
   ##source("Code/upskill_qns_deck_reco.R") # Earlier RECOMMENDATION OUTPUT 
  #setwd("~/wls")
  setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls20092020")
  source("Code/upskill_users_streams_questions.R")
  
  # Output analysis 
  source("Code/Prediction_Validation.r") ## !!** to be uncommendted later on if required. 
  
            # # Demo Users FOR GUI 
            # Demo_Users = c("97d0a65c","b1459d23","c80bffb2","c930cc66","f10f490e","f810564e")
            Demo_UsersDay0 = c("fe5e359d","abd51bd2","9bffe329","8f7b79fd","8100aef3","560d7304","4ffee38a","22408aad","108ae76d","035f412b")
            # 
            # # setting WD back to Project Data Location 
            # #setwd("~/wls/Data")
            # setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/Data")
            # 
            # ## Checking if the Users exists 
            #       USER_Qns_Reco %>% filter(masked_user_id %in% Demo_Users)
            #       # #By Users ==> No Output 
            #       USER_Stream_Reco %>% filter(masked_user_id %in% Demo_Users)
            # 
            # 
            # saveRDS(USER_Stream_Reco,file = "UpSkill_Stream_Recommendation.RDS")
            # saveRDS(USER_Qns_Reco,file = "UpSkill_Question_Recommendation.RDS")
  
  # Saving working directory to Local Directory 
    # setwd("~/Documents/NUS_EBAC")   -- To Uncomment when saving the Rdata Source
    # save.image("Data/WLS_22082020_V2.RData")
    # load("Data/WLS_22082020_V2.RData")
  
  
  
  
  