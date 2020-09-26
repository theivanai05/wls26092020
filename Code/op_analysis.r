### ** Output Analysis ** ###
#1) Next Action Steps 
#2) 


# Demo_Users_dt = data.table(c("97d0a65c","b1459d23","c80bffb2","c930cc66","f10f490e","f810564e"))
# names(Demo_Users_dt)[names(Demo_Users_dt) == "V1"] <- "masked_user_id"
# 
# train_UI = pulsescore_Master[!Demo_Users_dt, on=c("masked_user_id")]  
# test_UI = pulsescore_Master[Demo_Users_dt, on=c("masked_user_id")]  

## **!! ** 3 of the Test Users do not have any Pulse Score 

# Shows the N?A pulse Score Users rows where pulsescore is missing.
na.omit(test_UI, cols=c("pulsescore"),invert=TRUE)


#Checking on the USers Assessment Data.
Demo_Users_Assess = assess_sear_tags_dt[Demo_Users_dt, on=c("masked_user_id")]  

#Checking on the users Streams Data. 
Demo_Users_Views = views_sear_tags_dt[Demo_Users_dt, on=c("masked_user_id")]  


setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/Data")
save.image("WLS_25082020_V3.RData")
load("WLS_25082020_V3.RData")
