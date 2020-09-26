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
te_assess_dt_7_GB_org28 = assess_dt_7_GB_org28 %>% filter(eventDate > "2020-04-30")
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
setwd("~/wls/Data/") # location for Combined PS
pulsescore_Master_GB_O28_DO = read.csv("pulse_score.csv",header=TRUE, sep= ",") # All Country Data Set - Pulse Score Alone 

temp_all <- as(pulsescore_Master_GB_O28_DO,"realRatingMatrix")



## THe Matrix is a Very Very Sparse Matrix  to go and use it as part of UBCF or IBCF; Using Random and Popular might be the best bet  

## Training of the Data Day 0 
train <- temp_all[]
rec <- Recommender(train, method = "UBCF")
rec
rec_i <- Recommender(train, method = "IBCF")
rec_i
rec_svd <- Recommender(train, method = "SVD")
rec_svd
rec_libmf <- Recommender(train, method = "LIBMF")
rec_libmf
rec_pop <- Recommender(train, method = "POPULAR")
rec_pop

## after this have to use the CS_WLS_RecommenderLabs_alltags.R




