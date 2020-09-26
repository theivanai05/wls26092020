
#install.packages("recommenderlab")
#Current development version: Download package from AppVeyor or install from GitHub (needs devtools).
#install_github("mhahsler/recommenderlab")
#Usage
#Load the package and prepare a dataset (included in the package).

#library("recommenderlab")
#library("devtools")

# ==> Converting pulsescore_Master_GB into a real rating matrix ## !! to remove later on swhere applicable 
# pulsescore_Master_GB[,c("X")] <- NULL
# temp <- as(pulsescore_Master_GB,"realRatingMatrix")

temp_all <- as(pulsescore_Master_All,"realRatingMatrix")

## THe Matrix is a Very Very Sparse Matrix  to go and use it as part of UBCF or IBCF; Using Random and Popular might be the best bet  

temp_matrix = as(temp,"matrix")
temp_matrix_all = as(temp_all,"matrix")


train <- temp_all[]
rec <- Recommender(train, method = "UBCF")
rec

rec_i <- Recommender(train, method = "IBCF")
rec_i

rec_svd <- Recommender(train, method = "SVD")
rec_svd

rec_libmf <- Recommender(train, method = "LIBMF")
rec_libmf

rec_rnd <- Recommender(train, method = "RANDOM")
rec_rnd

rec_pop <- Recommender(train, method = "POPULAR")
rec_pop

        # #Create top-N recommendations for new users (users 101 and 102)
        # 
        # pre <- predict(rec, temp_all[201:202], n = 10)
        # pre
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre, "list")
        # 
        # #Create top-N recommendations for new users (users 101 and 102)
        # 
        # pre_i <- predict(rec_i, temp_all[201:202], n = 10)
        # pre_i
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre_i, "list")
        # 
        # #Create top-N recommendations for new users (users 101 and 102)
        # 
        # pre_svd <- predict(rec_svd, temp_all[201:202], n = 10)
        # pre_svd
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre_svd, "list")
        # 
        # #Create top-N recommendations for new users (users 101 and 102)
        # 
        # pre_libmf <- predict(rec_libmf, temp_all[201:202], n = 10)
        # pre_libmf
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre_libmf, "list")
        # 
        # #Create top-N recommendations for new users (users 101 and 102)
        # 
        # pre_rnd <- predict(rec_rnd, temp_all[201:202], n = 10)
        # pre_rnd
        # 
        # 
        # pre_rnd_ratings <- predict(rec_rnd, temp_all[201:202], type = "ratings")
        # pre_rnd_ratings
        # as(pre_rnd_ratings, "matrix")[,1:235]
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre_rnd, "list")
        # 
        # 
        # ## Predictions for the popular Tags 
        # pre_pop<- predict(rec_pop, temp_all[201:202], n = 10)
        # pre_pop
        # 
        # pre_pop_ratings <- predict(rec_pop, temp_all[201:202], type = "ratings")
        # pre_pop_ratings
        # as(pre_pop_ratings, "matrix")[,1:235]
        # #Recommendations as ‘topNList’ with n = 10 for 2 users. 
        # as(pre_pop, "list")

#  ==> For Day 1 Recommendations 
# temp_all_D1 <- as(pulsescore_Master_GB_O28_D1,"realRatingMatrix")
#temp_all_D1_te <- as(pulsescore_Master_GB_O28_D1_te,"realRatingMatrix")
#temp_all_D0 <- as(pulsescore_Master_GB_O28_DO,"realRatingMatrix")
#

temp_all = temp_all_D1

## Random Recommendation 

Recommended_Recolabs_Tag_ALL = predict(rec_rnd, temp_all[], type = "ratings")
# Recommended_Recolabs_Tag_ALL = as(Recommended_Recolabs_Tag_ALL,"list")
# #Reordering the predicted Tags Output
# Recommended_Recolabs_Tag_ALL = gather(bind_rows(Recommended_Recolabs_Tag_ALL))
# #Changes to field names 
# names(Recommended_Recolabs_Tag_ALL)[names(Recommended_Recolabs_Tag_ALL) == "key"] <- "masked_user_id"
# names(Recommended_Recolabs_Tag_ALL)[names(Recommended_Recolabs_Tag_ALL) == "value"] <- "question_tags"
x1 = as(Recommended_Recolabs_Tag_ALL,"matrix")
x2 = data.frame(x1)
require(reshape2)
x2$id <- rownames(x2) 
x3 = melt(x2)
x4= x3[complete.cases(x3),]
x5 = as(x4,"data.frame")
x5 = x4[order(x4$id),]
#Changes to field names 
names(x5)[names(x5) == "id"] <- "masked_user_id"
names(x5)[names(x5) == "variable"] <- "question_tags"
Recommended_Recolabs_Tag_ALL = x5
Recommended_Recolabs_Tag_ALL$value <= NULL
rm(x1,x2,x3,x4,x5)


#rm(Recommended_Recolabs_Tag,Recommended_Recolabs_Tags,reco_dt,reco_dt_long)

#Changing directory for file movement 
setwd("~/wls/Data/Day0")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag_ALL,file = "Recommended_Recolabs_Tag_ALL_Random.RDS")
#Recommended_Recolabs_Tag["22f08364"]

## SVD Recommendation 
setwd("~/wls/Code") # moving into the code directory 

Recommended_Recolabs_Tag_ALL_SVD = predict(rec_svd, temp_all[], type = "ratings")
Recommended_Recolabs_Tag_ALL_SVD = as(Recommended_Recolabs_Tag_ALL_SVD,"list")
#Reordering the predicted Tags Output
Recommended_Recolabs_Tag_ALL_SVD = gather(bind_rows(Recommended_Recolabs_Tag_ALL_SVD))
#Changes to field names 
names(Recommended_Recolabs_Tag_ALL_SVD)[names(Recommended_Recolabs_Tag_ALL_SVD) == "key"] <- "masked_user_id"
names(Recommended_Recolabs_Tag_ALL_SVD)[names(Recommended_Recolabs_Tag_ALL_SVD) == "value"] <- "question_tags"

#rm(Recommended_Recolabs_Tag,Recommended_Recolabs_Tags,reco_dt,reco_dt_long)

#Changing directory for file movement 
setwd("~/wls/Data/Day0")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag_ALL_SVD,file = "Recommended_Recolabs_Tag_ALL_SVD.RDS")
#Recommended_Recolabs_Tag["22f08364"]

#======================================================================================================================
## POP Recommendation 
setwd("~/wls/Code") # moving into the code directory 

Recommended_Recolabs_Tag_ALL_POP = predict(rec_pop, temp_all[], type = "ratings")
Recommended_Recolabs_Tag_ALL_POP = as(Recommended_Recolabs_Tag_ALL_POP,"list")
#Reordering the predicted Tags Output
Recommended_Recolabs_Tag_ALL_POP = gather(bind_rows(Recommended_Recolabs_Tag_ALL_POP))
#Changes to field names 
names(Recommended_Recolabs_Tag_ALL_POP)[names(Recommended_Recolabs_Tag_ALL_POP) == "key"] <- "masked_user_id"
names(Recommended_Recolabs_Tag_ALL_POP)[names(Recommended_Recolabs_Tag_ALL_POP) == "value"] <- "question_tags"

#rm(Recommended_Recolabs_Tag,Recommended_Recolabs_Tags,reco_dt,reco_dt_long)

#Changing directory for file movement 
setwd("~/wls/Data/Day0")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag_ALL_POP,file = "Recommended_Recolabs_Tag_ALL_POP.RDS")
#Recommended_Recolabs_Tag["22f08364"]

#======================================================================================================================
#======================================================================================================================
## User to User  Recommendation 
setwd("~/wls/Code") # moving into the code directory 

Recommended_Recolabs_Tag_ALL_U2U = predict(rec, temp_all[], type = "ratings")
x1 = as(Recommended_Recolabs_Tag_ALL_U2U,"matrix")
x2 = data.frame(x1)
require(reshape2)
x2$id <- rownames(x2) 
x3 = melt(x2)
x4= x3[complete.cases(x3),]
x5 = as(x4,"data.frame")
x5 = x4[order(x4$id),]
#Changes to field names 
names(x5)[names(x5) == "id"] <- "masked_user_id"
names(x5)[names(x5) == "variable"] <- "question_tags"
Recommended_Recolabs_Tag_ALL_U2U = x5
Recommended_Recolabs_Tag_ALL_U2U$value <= NULL
rm(x1,x2,x3,x4,x5)

Recommended_Recolabs_Tag_ALL_U2U = as(Recommended_Recolabs_Tag_ALL_U2U,"list")
#Reordering the predicted Tags Output
Recommended_Recolabs_Tag_ALL_U2U = gather(bind_rows(Recommended_Recolabs_Tag_ALL_U2U))
#Changes to field names 
names(Recommended_Recolabs_Tag_ALL_U2U)[names(Recommended_Recolabs_Tag_ALL_U2U) == "key"] <- "masked_user_id"
names(Recommended_Recolabs_Tag_ALL_U2U)[names(Recommended_Recolabs_Tag_ALL_U2U) == "value"] <- "question_tags"

#rm(Recommended_Recolabs_Tag,Recommended_Recolabs_Tags,reco_dt,reco_dt_long)

#Changing directory for file movement 
setwd("~/wls/Data/Day0")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag_ALL_U2U,file = "Recommended_Recolabs_Tag_ALL_U2U.RDS")
#Recommended_Recolabs_Tag["22f08364"]

#======================================================================================================================
## Removed Rnd  & SVD as there were no similarity on them 
#rm(Recommended_Recolabs_Tag_ALL,Recommended_Recolabs_Tag_ALL_SVD)
#rm(Recommended_Recolabs_Tag_ALL)
#======================================================================================================================

## Item to Item Recommendation 
setwd("~/wls/Code") # moving into the code directory 

Recommended_Recolabs_Tag_ALL_ITEM = predict(rec_i, temp_all[], type = "ratings")
x1 = as(Recommended_Recolabs_Tag_ALL_ITEM,"matrix")
x2 = data.frame(x1)
require(reshape2)
x2$id <- rownames(x2) 
x3 = melt(x2)
x4= x3[complete.cases(x3),]
x5 = as(x4,"data.frame")
x5 = x4[order(x4$id),]
#Changes to field names 
names(x5)[names(x5) == "id"] <- "masked_user_id"
names(x5)[names(x5) == "variable"] <- "question_tags"
Recommended_Recolabs_Tag_ALL_ITEM = x5
Recommended_Recolabs_Tag_ALL_ITEM$value <= NULL
rm(x1,x2,x3,x4,x5)

    # Recommended_Recolabs_Tag_ALL_ITEM = as(Recommended_Recolabs_Tag_ALL_ITEM,"list")
    # #Reordering the predicted Tags Output
    # Recommended_Recolabs_Tag_ALL_ITEM = gather(bind_rows(Recommended_Recolabs_Tag_ALL_ITEM))
    #Changes to field names 
    # names(Recommended_Recolabs_Tag_ALL_ITEM)[names(Recommended_Recolabs_Tag_ALL_ITEM) == "key"] <- "masked_user_id"
    # names(Recommended_Recolabs_Tag_ALL_ITEM)[names(Recommended_Recolabs_Tag_ALL_ITEM) == "value"] <- "question_tags"

#Changing directory for file movement 
setwd("~/wls/Data/Day0")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag_ALL_ITEM,file = "Recommended_Recolabs_Tag_ALL_ITEM_Raw.RDS")
#Recommended_Recolabs_Tag["22f08364"]

#================================================================================================================
## Checking the Tags the users below have been using 
# Users to test 029bd201, 029befc7 
#================================================================================================================
# Demo Users FOR GUI
Demo_Users =c('029bd201','029befc7') # c("97d0a65c","b1459d23","c80bffb2","c930cc66","f10f490e","f810564e")
Demo_UsersDay0 = c("fe5e359d","abd51bd2","9bffe329","8f7b79fd","8100aef3","560d7304","4ffee38a","22408aad","108ae76d","035f412b")

      # ## Checking Recommendations for Users exists
      # 
      # checking_users = assess_dt_7 %>% filter(masked_user_id %in% Demo_Users) 
      # checking_users = unique(checking_users[,c("question_id","masked_user_id","tag1","tag2","tag3","tag4","tag5","tag6","tag7")])
      # 
      # saveRDS(checking_users,file = "checking_users.RDS")
      # write_csv(checking_users,"checking_users.csv")
      # 
      # Recommended_Recolabs_Tag_ALL_SVD %>% filter(masked_user_id %in% Demo_Users) 
      # Recommended_Recolabs_Tag_ALL %>% filter(masked_user_id %in% Demo_Users) 
      # Recommended_Recolabs_Tag_ALL_POP %>% filter(masked_user_id %in% Demo_Users)
      # Recommended_Recolabs_Tag_ALL_ITEM %>% filter(masked_user_id %in% Demo_Users)
      # Recommended_Recolabs_Tag_ALL_U2U %>% filter(masked_user_id %in% Demo_Users)

## Checking Recommendations for Day 0 Users exists

checking_users = ASSES_PS_EVAL_GB_28 %>% filter(masked_user_id %in% Demo_UsersDay0) 
checking_users = unique(checking_users[,c("question_id","masked_user_id","tag1","tag2","tag3","tag4","tag5","tag6","tag7")])


checking_users = melt(checking_users, id.vars = c("question_id","masked_user_id"))
names(checking_users)[4] = "question_tags"
checking_users = within(checking_users, rm("variable"))
checking_users = unique(checking_users[order(checking_users$masked_user_id),])
checking_users = checking_users[complete.cases(checking_users),]

setwd("~/wls/Data/Day0")
saveRDS(checking_users,file = "checking_users_Day0.RDS")
write_csv(checking_users,"checking_users_Day0.csv")


RECO_OP_CHK = Recommended_Recolabs_Tag_ALL_SVD %>% filter(masked_user_id %in% Demo_UsersDay0)

RECO_OP_CHK = Recommended_Recolabs_Tag_ALL %>% filter(masked_user_id %in% Demo_UsersDay0)  ## for Random 

RECO_OP_CHK = RECO_OP_CHK [with(RECO_OP_CHK , order(masked_user_id, value)),]
#RECO_OP_CHK = by(RECO_OP_CHK, RECO_OP_CHK["masked_user_id"], head, n=10)
require(data.table)
d <- data.table(RECO_OP_CHK, key="masked_user_id")
RECO_OP_CHK = d[, head(.SD, 10), by=masked_user_id]

#RECO_OP_CHK %>% str_replace(".", "-")
RECO_OP_CHK$question_tags <- lapply(RECO_OP_CHK$question_tags , function(x){
  str_replace(x,"tag.","tag-")})



RECO_OP_CHK = Recommended_Recolabs_Tag_ALL_POP %>% filter(masked_user_id %in% Demo_UsersDay0)
RECO_OP_CHK = Recommended_Recolabs_Tag_ALL_ITEM %>% filter(masked_user_id %in% Demo_UsersDay0)
#Recommended_Recolabs_Tag_ALL_U2U %>% filter(masked_user_id %in% Demo_UsersDay0)
