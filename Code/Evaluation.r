# Working on and analyzing the Test and Validation Data Sets 
#1) Test is May, Jun , Jul 2020 
#2) Training : April 2019 - April 2020 


# Mutating Time fields for processing in assess_dt_7. 

assess_dt_7$country = as.factor(assess_dt_7$country)
assess_dt_7$question_id = as.factor(assess_dt_7$question_id)
assess_dt_7$masked_user_id = as.factor(assess_dt_7$masked_user_id)
# convert timestamp into date time format
assess_dt_7$eventDateTime = as_datetime(assess_dt_7$submission_utc_ts,format="%Y-%m-%d %H:%M:%S.000")
# create new column  event date
assess_dt_7$eventDate = as.Date(substr(assess_dt_7$eventDateTime,1,10))
# create new column  event time
assess_dt_7$eventTime = substr(assess_dt_7$eventDateTime,12,22)


#Unique Role ID 
unique(assess_dt_7$role_id)
 #[1]  35  42  NA  39  72   1  10  31   3 156  73   8   5   2   9  68 154  34 155 159 157

#Unique  Org id
unique(assess_dt_7$org_id) 
 #[1]  35  42  NA  39  72   1  10  31   3 156  73   8   5   2   9  68 154  34 155 159 157

#Unique Country
unique(assess_dt_7$country) 

  # [1] AO OM PE PH PK SG US IN KE LK MY RU TZ UG ZA AE ID SA HK VN FR MA SN TN TH NG    AU BD BG DE ES GH MX PL PT SK TR HU IT RO RS UA EG FI MU NO CL CO JO GR GB JE CZ IE SE IR BE AR BH KW QA NL GT
  # [65] HN SV IQ NP TW LB UY PY CR NI PR
  # 75 Levels:  AE AO AR AU BD BE BG BH CL CO CR CZ DE EG ES FI FR GB GH GR GT HK HN HU ID IE IN IQ IR IT JE JO KE KW LB LK MA MU MX MY NG NI NL NO NP OM PE PH PK PL PR PT PY QA RO RS RU SA SE ... ZA

## Assessments served to GB 
assess_dt_7_GB = unique(assess_dt_7 %>% filter (assess_dt_7$country == "GB"))

unique(assess_dt_7_GB$role_id)
  # [1]   1  NA   2   3  10  31 156   8   9   5
#Unique  Org id
unique(assess_dt_7_GB$org_id) 
  # [1] 28 36


## Assessments served to GB Org ID 28 alone 
assess_dt_7_GB_org28 = unique(assess_dt_7 %>% filter (assess_dt_7$country == "GB") %>% filter(assess_dt_7_GB$org_id == "28"))

unique(assess_dt_7_GB_org28$role_id)
# [1]   1  NA   2   3  10  31 156   8   9   5
#Unique  Org id
unique(assess_dt_7_GB_org28$org_id) 
# [1] 28

#Train and Test data will be divided out form this set: 
#1) Test is May, Jun , Jul 2020 
#2) Training : April 2019 - April 2020 

## ==> get a count view visual of the assessment Tags used each day.. ## !! ?? TO DO ??!! 

#1) code : Test is May, Jun , Jul 2020 
te_assess_dt_7_GB_org28 = assess_dt_7_GB_org28 %>% filter(eventDate > "2020-05-01")
#> count(te_assess_dt_7_GB_org28)
#n 1: 376
gb_te_users = data.frame(unique(te_assess_dt_7_GB_org28$masked_user_id))

#2) code : Training : April 2019 - April 2020  == 
tr_assess_dt_7_GB_org28 = assess_dt_7_GB_org28 %>% filter(eventDate > "2019-03-31", eventDate < "2020-05-01") #comma same as &
#> count(tr_assess_dt_7_GB_org28)
#n 1: 83206
gb_tr_users = data.frame(unique(tr_assess_dt_7_GB_org28$masked_user_id))

## checking if Test users are in the Training users set 
names(gb_tr_users)[names(gb_tr_users) == "unique.tr_assess_dt_7_GB_org28.masked_user_id."] <- "masked_user_id"
names(gb_te_users)[names(gb_te_users) == "unique.te_assess_dt_7_GB_org28.masked_user_id."] <- "masked_user_id"

Test_USERS_GB = data.frame(gb_tr_users %>% filter (masked_user_id %in% gb_te_users$masked_user_id))


#=============================================================================================================================
        # ====> GENERATING SCORE for the Training PERIOD < ============
#=============================================================================================================================

## Preparation of Data to generate Pulse Score for GB train users 

ASSES_PS_EVAL_GB_28 = tr_assess_dt_7_GB_org28[,-c("eventDate","eventTime","eventDateTime")]


## Pulse Score has been evlauated using file "PulseScoreCalculator.R"

##Reading in Day 0 Pulse Score 
#setwd("~/wls/Data/") # location for Combined PS
setwd("~/wls26092020/wls26092020/Data") # data location of Mac
pulsescore_Master_GB_O28_DO = read.csv("pulse_score_GB_28.csv",header=TRUE, sep= ",") # All Country Data Set - Pulse Score Alone 
temp_all <- as(pulsescore_Master_GB_O28_DO,"realRatingMatrix")

#Day 1 Pulse SCORE 
setwd("~/wls26092020/wls26092020/Data/Day1") # data location of Mac
pulsescore_Master_GB_O28_D1 = read.csv("pulse_score_Day1_27092020.csv",header=TRUE, sep= ",",na.strings=" ") # All Country Data Set - Pulse Score Alone 
pulsescore_Master_GB_O28_D1 = pulsescore_Master_GB_O28_D1 %>% na.omit()

temp_all_D1 <- as(pulsescore_Master_GB_O28_D1,"realRatingMatrix")

#Day 2 Pulse SCORE 
setwd("~/wls26092020/wls26092020/Data/Day1") # data location of Mac
pulsescore_Master_GB_O28_D2 = read.csv("pulse_score_Day2_28092020.csv",header=TRUE, sep= ",",na.strings=" ") # All Country Data Set - Pulse Score Alone 
pulsescore_Master_GB_O28_D2 = pulsescore_Master_GB_O28_D2 %>% na.omit()

temp_all_D2 <- as(pulsescore_Master_GB_O28_D2,"realRatingMatrix")

#Day 3 Pulse SCORE 
setwd("~/wls26092020/wls26092020/Data/Day1") # data location of Mac
pulsescore_Master_GB_O28_D3 = read.csv("pulse_score_Day3_29092020.csv",header=TRUE, sep= ",",na.strings=" ") # All Country Data Set - Pulse Score Alone 
pulsescore_Master_GB_O28_D3 = pulsescore_Master_GB_O28_D3 %>% na.omit()

temp_all_D3 <- as(pulsescore_Master_GB_O28_D3,"realRatingMatrix")

#Day 4 Pulse SCORE 
setwd("~/wls26092020/wls26092020/Data/Day1") # data location of Mac
pulsescore_Master_GB_O28_D4 = read.csv("pulse_score_Day4_30092020.csv",header=TRUE, sep= ",",na.strings=" ") # All Country Data Set - Pulse Score Alone 
pulsescore_Master_GB_O28_D4 = pulsescore_Master_GB_O28_D4 %>% na.omit()

temp_all_D4 <- as(pulsescore_Master_GB_O28_D4,"realRatingMatrix")

require(recommenderlab)
require(devtools)
# Training Pulse Score, removing the test users ==> This ensures that the dimensions are all the same 
# for the training and Prediction... 
test_all  = temp_all[Demo_UsersDay0,]
test_all_D1  = temp_all_D1[Demo_UsersDay0,]
test_all_D2  = temp_all_D2[Demo_UsersDay0,]
test_all_D3  = temp_all_D3[Demo_UsersDay0,]
test_all_D4  = temp_all_D4[Demo_UsersDay0,]
train_all = temp_all[] #%notin% c(Demo_UsersDay0) 

## THe Matrix is a Very Very Sparse Matrix  to go and use it as part of UBCF or IBCF; Using Random and Popular might be the best bet  

## Training of the Data Day 0 
    # rec <- Recommender(train_all, method = "UBCF")
    # #rec
    # rec_i <- Recommender(train_all, method = "IBCF")
    # #rec_i
    # rec_svd <- Recommender(train_all, method = "SVD")
    # #rec_svd
    # rec_libmf <- Recommender(train_all, method = "LIBMF")
#rec_libmf
rec_pop <- Recommender(train_all, method = "POPULAR")
#rec_pop
rec_rereco <- Recommender(train_all, method = "RERECOMMEND")
#rec_rereco

## after this have to use the CS_WLS_RecommenderLabs_alltags.R

    # rec
    # rec_i
    # rec_svd
    # rec_limbf
    # rec_pop
    # rec_rereco

#26/SEP ==> TESTING  on the Predictions with Test datasets
#Create predictions for new test users
    # require(recommenderlab)
    # pre <- predict(rec, test_all ,type = "ratings")               # U-2_U
    # # pre
    # # as(pre, "list")
    # pre_i <- predict(rec_i, test_all ,type = "ratings")           # I-2_I
    # pre_svd <- predict(rec_svd,test_all ,type = "ratings")        # SVD
    # pre_libmf <- predict(rec_libmf,test_all ,type = "ratings")    # libmf
pre_pop<- predict(rec_pop, test_all ,type = "ratings")        #popular
pre_rereco <- predict(rec_rereco, test_all ,type = "ratings") #rerecommender

pre_pop_D1<- predict(rec_pop, test_all_D1 ,type = "ratings")        #popular
pre_rereco_D1 <- predict(rec_rereco, test_all_D1 ,type = "ratings") #rerecommender

pre_pop_D2<- predict(rec_pop, test_all_D2 ,type = "ratings")        #popular
pre_rereco_D2 <- predict(rec_rereco, test_all_D2 ,type = "ratings") #rerecommender

pre_pop_D3<- predict(rec_pop, test_all_D3 ,type = "ratings")        #popular
pre_rereco_D3 <- predict(rec_rereco, test_all_D3 ,type = "ratings") #rerecommender

pre_pop_D4<- predict(rec_pop, test_all_D4 ,type = "ratings")        #popular
pre_rereco_D4 <- predict(rec_rereco, test_all_D4 ,type = "ratings") #rerecommender

# View(as(pre_rereco, "matrix"))
# View(as(test_all,"matrix"))
# View(as(pre_libmf,"matrix"))
# View(as(pre_svd,"matrix"))
# View(t(as(pre,"matrix")))
# View(t(as(pre_i,"matrix")))

# Day 0 : 
predit_Rereco_op = as(pre_rereco,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

# Day 1 :   
predit_Rereco_op = as(pre_rereco_D1,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop_D1,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

# Day 2 :   
predit_Rereco_op = as(pre_rereco_D2,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop_D2,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

# Day 3 :   
predit_Rereco_op = as(pre_rereco_D3,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop_D3,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

# Day 4 :   
predit_Rereco_op = as(pre_rereco_D4,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop_D4,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

## Generating 10 taged Recommendations with Rerecommender  + Popular Recommendations. 
ten_tag_reco = PulseScore_Bottom_10_TAGS %>% group_by(user) %>% count()
t=1
x=0
Recommended_Tags = data.frame()
filler = data.frame()
for (t in 1:nrow(ten_tag_reco)) 
{ if (ten_tag_reco[t,]$n != 10)
  {
    x=10-ten_tag_reco[t,]$n
    filler = Popular_Top_10 %>% filter(user == ten_tag_reco[t,]$user) %>% slice(1:x)
  }
  Recommended_Tags = rbind(PulseScore_Bottom_10_TAGS,filler)
  t <- t + 1 
}

# Setting the field names to match with the rest of the code. 
names(Recommended_Tags)[names(Recommended_Tags) == "user"] <- "masked_user_id"
names(Recommended_Tags)[names(Recommended_Tags) == "item"] <- "question_tags"
Recommended_Tags = Recommended_Tags %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)

#Recommended_Tags_Day0 = Recommended_Tags
#Recommended_Tags_Day1 = Recommended_Tags
#Recommended_Tags_Day2 = Recommended_Tags
#Recommended_Tags_Day3 = Recommended_Tags
Recommended_Tags_Day4 = Recommended_Tags
# HybridRecommender()

#test_all = temp_all[c(Demo_UsersDay0),]
#train_all = temp_all[ i %notin% Demo_UsersDay0 ,]

## mix popular movies with a random recommendations for diversity and
## rerecommend some movies the user liked. rerecommend is not working in the HYBRID Model 
    # 
    # recom_hyb <- HybridRecommender(
    #   Recommender(train_all, method = "RERECOMMEND"),
    #   Recommender(train_all, method = "RANDOM"),
    #   Recommender(train_all, method = "POPULAR"),
    #   weights = c(.9,.1,.0)
    # )
    # 
    #   # recom_hyb
    #   # getModel(recom_hyb)
    #   # View(as(predict(recom_hyb, test_all,type = "ratings"), "matrix"))
    # predict_hyb = predict(recom_hyb, test_all,type = "ratings")
    # require(dplyr)
    # df = as(predict_hyb,"matrix")
    # df = data.frame(df)
    # require(reshape2)
    # df$id <- rownames(df) 
    # df= melt(df)
    # df= df[complete.cases(df),]
    # df= as(df,"data.frame")
    # x5 = df[order(df$id),]
    # #Changes to field names 
    # names(x5)[names(x5) == "id"] <- "masked_user_id"
    # names(x5)[names(x5) == "variable"] <- "question_tags"
    # write_csv(x5, "predict_hyb.csv")

