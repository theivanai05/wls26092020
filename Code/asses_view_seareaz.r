## Attaching the Streams/ Deck Ids with Question ID Tags from VIEW model master 

## Viewing field names of the data sets 
  # colnames(assess_dt)
  # colnames(master_dt)
  # colnames(views_dt)
  # colnames(pulsescore_Master)

    # > colnames(assess_dt)
    # [1] "country"           "org_id"            "role_id"           "question_id"       "submission_utc_ts" "no_of_trials"      "points_earned"     "masked_user_id"    "question_tags"    
    # > colnames(master_dt)
    # [1] "user_since"     "org_id"         "client_type"    "lang_code"      "role_id"        "country"        "location"       "city"           "masked_user_id"
    # > colnames(views_dt)
    # [1] "deck_id"               "card_id"               "action"                "user_action_timestamp" "app_version_id"        "user_since"            "client_type"          
    # [8] "city"                  "country"               "login_handle_type"     "lang_code"             "role_id"               "masked_user_id"       
    # > colnames(pulsescore_Master)
    # [1] "X"          "userid"     "qtag"       "pulsescore"

#Unique List of Countries for the Different files
#Creating the Country Masters for the different data.tables
Ctry_View_M = data.table(unique(views_dt$country))    #22
Ctry_assess_M = data.table(unique(assess_dt$country)) #75
Ctry_master_M = data.table(unique(master_dt$country)) #103

## Creating the Question, COuntry, Tag Master
C_Q_Tag_M = unique(assess_dt %>% select("country","question_id","question_tags"))

## Preparing for Searealization 
sereliz = unique(C_Q_Tag_M[,c("question_id","question_tags")])


#***********************************#
##Serializing the values of the column = question_tags into 4 columns max
#***********************************#
Q_TAG_SER =  unique(reshape(transform(sereliz, time=ave(question_tags, question_id, FUN=seq_along)), idvar="question_id", direction="wide"))

  # Removing unwanted D.Ts 
  rm(sereliz)


#Country_Question_Master
C_Q_M = unique(C_Q_Tag_M[,c("country","question_id")])

#Binding the Serialized Data set of Views wih counties 
C_Q_TAG_SER= merge(Q_TAG_SER, C_Q_M,by = c("question_id")) 

#Converging the Data Sets across Assess_dt and views_dt for moving it out to the rest of the teams
  
  #1) There are Question ID Duplicates in the C_Q_TAQ_SER.data.table
C_TAG_SER_4_VIEWDATA = unique(C_Q_TAG_SER[,2:6])
#View(C_TAQ_SER_4_VIEWDATA)

  #2)#Creating the Country Masters for the different data.tables
  #View(Ctry_View_M)

  #3)Looking up with Views(Streams) ID & Country Master Data_Table 
#Enrich unique stream at country level data with tags from question data - randomize at country level. 

#Country_Deck Master from Views tables
CountryDeck_M = views_dt %>% select("country", "deck_id") %>% unique() %>% data.table()

#created the country list of values for Iteration
cntry_lst = unique(CountryDeck_M$country)

#Initiating the data before processing 
stream_data5 = data.table()

for ( c in cntry_lst){
  stream_data5 = rbind(stream_data5,cbind(CountryDeck_M[CountryDeck_M$country == c,],C_TAG_SER_4_VIEWDATA[C_TAG_SER_4_VIEWDATA$country == c,c("question_tags.1","question_tags.2","question_tags.3","question_tags.4")])
  )}


#4) Merging with the views_dt
#rm(views_sear_tags_dt)

views_sear_tags_dt = merge(views_dt,stream_data5[,c("country","deck_id","question_tags.1","question_tags.2","question_tags.3","question_tags.4")
],by = c("country","deck_id"))

#5) Merging with the assess_dt
assess_sear_tags_dt = merge(assess_dt,C_Q_TAG_SER[,c("country","question_id","question_tags.1","question_tags.2","question_tags.3","question_tags.4")
],by = c("country","question_id"))

# Removing the Duplicate after Searealization 
views_sear_tags_dt = unique(views_sear_tags_dt)
assess_sear_tags_dt = unique(assess_sear_tags_dt)

#Removing Stream Data 5 that was used in the Searealization Evaluation 
rm(stream_data5)

