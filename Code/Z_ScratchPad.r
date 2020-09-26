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
