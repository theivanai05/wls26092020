# Working on and analyzing the Test and Validation Data Sets 
#1) Test is May, Jun , Jul 2020 
#2) Training : April 2019 - April 2020 


# Only Possible to do on the Assessment Data Set as Stream Tag did not originally have any TAGGING... 
# ==> questions to ask KUSHAL, can we have real Tags for a very small TOY Data Set 
# ==> agreed advised team that Tag Recommendation Evaluation on Stream data, as Tag information is synthetic.  

# Questions and Analysis to work on 
# Done a) GB Vs rest of the countries
# b) What was the TAG to User Id to Question Recommended Trend 
# c) What was the TAG to User Id to Stream Recommended Trend 
# d) Tag Usage Trend per Role Id 
# Done e) Tag Usage Trend per country
# e) Tag Usage Trend per country & Role Id 

#=====================================================================================================================
# a) GB Vs rest of the countries
#=====================================================================================================================
# TAGs in GB Vs Other Countries 
tble = C_Q_Tag_M %>% group_by(country)%>% distinct(question_tags) %>% summarise(count=n()) 
tble = data.frame(tble)

#Basic Numbers
tble %>% ggplot(aes(x= country,y = count)) + geom_boxplot()

# TAG COUNT IN EACH  country  ==> # e) Tag Usage Trend per country
tble %>% ggplot(aes(x = reorder(country, -count),y =count)) + geom_bar(stat='identity')

# GB_Master_Tags = C_Q_Tag_M %>% filter(country == "GB")
# C_Q_Tag_M %>% filter(country == "GB") %>% distinct(question_id) %>% summarise(count=n()) 
# C_Q_Tag_M %>% filter(country == "GB") %>% distinct(question_tags) %>% summarise(count=n()) 


## How many questions per tag
Qns_per_tag = C_Q_Tag_M %>% distinct() %>% group_by(question_tags) %>% summarise(count=n()) 
Qns_per_tag %>% ggplot(aes(x =count, y=question_tags)) + geom_jitter(stat='identity')

ggplot(Qns_per_tag, aes(x = question_tags,y =count)) + geom_point(fill = "red")+
  scale_x_discrete("Question Tags", breaks= seq(0,1200, by=50))+
  labs(title = "Arranging the Question Per Tags ", x = "QuestionTags")

ggplot(Qns_per_tag, aes(x = question_tags,y =count)) + geom_histogram(stat='identity')+
  scale_x_discrete("Question Tags", breaks= seq(0,1200, by=50))+
  labs(title = "Arranging the Question Per Tags ", x = "QuestionTags")
#Qns_per_tag %>% filter(question_tags == "tag-01aa4bbe")


## how many questions per Tags in Each country 
Qns_per_country_tag =C_Q_Tag_M %>% group_by(question_tags,country) %>% summarise(count=n()) 
ggplot(Qns_per_country_tag, aes(x=question_tags, y=country, size = count)) + geom_point(alpha=0.7)+
scale_x_discrete("Question Tags", breaks= seq(0,1200, by=100))

# e) Tag Usage Trend per country
C_Q_Tag_M %>% group_by(country) %>% summarise(count=unique(question_tags))
distinct(C_Q_Tag_M[,-"question_id"]) %>% group_by(country) %>% summarise(count=n()) %>%
ggplot(aes(x = reorder(country,-count),y =count)) + geom_histogram(stat='identity')+
labs(title = "Arranging the TAGS PER COUNTRY ", x = "Country")

#Qns_per_country_tag %>% filter(question_tags == "tag-01aa4bbe")
# C_Q_Tag_M %>% filter(question_tags == "tag-004f0c68") # -- Test 


#=====================================================================================================================
# b) What was the TAG to User Id to Question Recommended Trend 
#=====================================================================================================================
# Total values for the ORG_ID ==> 
unlist(distinct(assess_dt[,"org_id"])) #      28      14      35      30      36      31      23      21 
assess_dt %>% group_by(org_id,role_id,country,question_tags) %>% summarise(count=n()) #%>% plot() # "org_id"[order("org_id", decreasing = TRUE)]
assess_dt %>% group_by(org_id,role_id,country,masked_user_id) %>% summarise(count=n()) #%>% plot() # "org_id"[order("org_id", decreasing = TRUE)]
assess_dt %>% group_by(org_id,role_id,country) %>% summarise(count=n()) #%>% plot() # "org_id"[order("org_id", decreasing = TRUE)]


