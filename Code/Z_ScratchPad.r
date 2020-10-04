## Scratch Pad 

Recommended_Recolabs_Tag_ALL_ITEM_bottom10 = predict(rec_i, temp_all[], n = -10)
chk_bottom10 = as(Recommended_Recolabs_Tag_ALL_ITEM_bottom10, "matrix")
user_035f412b = chk_bottom10["035f412b",]
user_035f412b[c("Tools")]

chk_top10 = tibble(Recommended_Recolabs_Tag_ALL_ITEM)
user_035f412b_top10 = chk_top10 %>% filter(masked_user_id == "035f412b")
View(user_035f412b_top10)

pulsescore_Master_GB_O28_DO  %>% filter(masked_user_id == "035f412b")

rm(Recommended_Recolabs_Tag_ALL_ITEM_bottom10,chk_bottom10,user_035f412b,chk_top10,user_035f412b_top10  )

Demo_UsersDay0
#[1] "fe5e359d" "abd51bd2" "9bffe329" "8f7b79fd" "8100aef3" "560d7304" "4ffee38a" "22408aad" "108ae76d" "035f412b"

View(pulsescore_Master_GB_O28_DO  %>% filter(masked_user_id == "035f412b"))
View(pulsescore_Master_GB_O28_D1  %>% filter(masked_user_id == "035f412b"))

View(pulsescore_Master_GB_O28_DO  %>% filter(masked_user_id == "108ae76d",question_tags == "tag-e34b589d")) 
View(pulsescore_Master_GB_O28_D1  %>% filter(masked_user_id == "108ae76d",question_tags == "tag-e34b589d"))

View(pulsescore_Master_GB_O28_DO  %>% filter(masked_user_id == "22408aad",question_tags == "tag-6bc64d6f")) 
View(pulsescore_Master_GB_O28_D1  %>% filter(masked_user_id == "22408aad",question_tags == "tag-6bc64d6f"))

u_q_t_M %>% filter(masked_user_id == "108ae76d")


# 28th September Duplicate Question & Duplictae Streams are beong recommended ==> Need to sort it out.. 
u_q_t_M %>% filter(question_id == "14192",masked_user_id == "4ffee38a")
# masked_user_id question_id no_of_trials points_earned qns_ans
# 1:       4ffee38a       14192            4             0       0
# 2:       4ffee38a       14192            1            10       1
Questions_Recommended_w_qns_ans_GB_Users %>% filter(question_id == "14192",masked_user_id == "4ffee38a")
# masked_user_id question_id question_tags country rating qns_ans
# 1248       4ffee38a       14192   accessories      GB 0.0063       1
# 1247       4ffee38a       14192   accessories      GB 0.0063       0


u_q_t_M %>% filter(question_id == "14590",masked_user_id == "fe5e359d")
df %>% filter(question_id == "14590",masked_user_id == "fe5e359d")
# masked_user_id question_id no_of_trials points_earned qns_ans
# 1:       4ffee38a       14192            4             0       0
# 2:       4ffee38a       14192            1            10       1
Questions_Recommended_w_qns_ans_GB_Users %>% filter(question_id == "14590",masked_user_id == "fe5e359d")
df %>% filter(question_id == "14590",masked_user_id == "fe5e359d")

View(Qns_Reco_Day5_2QPU %>% filter(masked_user_id == "035f412b"))
View(Qns_Reco_Day5 %>% filter(masked_user_id == "035f412b"))

# Remove Duplicate Questions for same user 
# Removal of items 
df <- Qns_Reco_Day5 %>% group_by(masked_user_id,question_id) %>% mutate(Duplicated = case_when(length(masked_user_id)>1 ~ "Yes",TRUE ~ "No")) %>%ungroup()

df %>% filter(question_id == "7235",masked_user_id == "035f412b")

df_uq <- unique(Qns_Reco_Day5)
# Remove duplicated rows based on 
# Sepal.Length and Petal.Width
df_uq = Qns_Reco_Day5  %>% distinct(question_id, masked_user_id , .keep_all = TRUE)
View(df_uq %>% filter(question_id == "7235",masked_user_id == "035f412b"))


df_uq[,"org_id" := 28]
df_uq[,role_id := 28]


