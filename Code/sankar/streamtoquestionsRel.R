library(tidyverse)
library(magrittr)
library(lubridate)
library(R.utils)
library(data.table)

# model with tags building

load("D:/NUS/FYP/data/streams_with_tags.RData")
load("D:/NUS/FYP/data/assessments_with_tags.RData")

#initialize countries to run
countries= c("IN","MX","PK","US","EG")

# initial dataframe to store country level stream to question relationship
streamToQuestionRel = data.frame()

for (country in countries) {

viewINDataTags = stream_data4 %>% filter(country == country ) %>% select(deck_id ,tag1,tag2,tag3,tag4,tag5,tag6,tag7)  
viewINDataTags = unique(viewINDataTags)
viewINDataTags %>% select(deck_id) %>% unique()

assesmentINDataTags = question_data6 %>% filter(country == country) %>% select(question_id,tag1,tag2,tag3,tag4,tag5,tag6,tag7) 
assesmentINDataTags = unique(assesmentINDataTags)


viewINDataTags = melt(viewINDataTags, id.vars=c("deck_id"))
viewINDataTags$variable = NULL

names(viewINDataTags)[2] = "tags"
viewINDataTags = viewINDataTags %>% filter( !is.na(tags))
viewINDataTags$rating = 1



# convert questiontags

names(assesmentINDataTags)[1] = "deck_id"
assesmentINDataTags = melt(assesmentINDataTags, id.vars=c("deck_id"))
assesmentINDataTags$variable = NULL

names(assesmentINDataTags)[2] = "tags"
assesmentINDataTags = assesmentINDataTags %>% filter( !is.na(tags))
assesmentINDataTags$rating = 1

# find if any missing tags between assessment and view data

assesmentCols = unique(assesmentINDataTags$tags)
viewCols      = unique(viewINDataTags$tags)

# find any aassment cols not there in streams
missingAssesmentCols = assesmentCols [!(assesmentCols %in% viewCols)]
missingViewCols =viewCols [!(viewCols %in% assesmentCols )]

# remove records where tags not in streams
assesmentINDataTags  =assesmentINDataTags [!(assesmentINDataTags$tags  %in% missingAssesmentCols),]


# create matrix 
viewINDataTagsSpread = spread(viewINDataTags, tags,rating,fill = 0)
assesmentINDataTagsSpread = spread(assesmentINDataTags, tags,rating,fill = 0)


dim(viewINDataTagsSpread )
dim(assesmentINDataTagsSpread)
# create dot product
viewINDataTagsSpreadM = as.matrix(viewINDataTagsSpread[,2:ncol(viewINDataTagsSpread)])
dim(viewINDataTagsSpreadM)
rownames(viewINDataTagsSpreadM)= viewINDataTagsSpread$deck_id


assesmentINDataTagsSpreadM = as.matrix(assesmentINDataTagsSpread[,2:ncol(viewINDataTagsSpread)])
rownames(assesmentINDataTagsSpreadM)= assesmentINDataTagsSpread$deck_id

for(i in 1:nrow(viewINDataTagsSpread))
{
  dotP = viewINDataTagsSpreadM[i,] %*% t(assesmentINDataTagsSpreadM)
  dotPDF = data.frame(question_id = colnames(dotP)[which(dotP>=1)],rating=dotP[which(dotP>=1)] )
  dotPDF = dotPDF %>% arrange(desc(rating)) %>% head(10)
  dotPDF$deck_id = rownames(viewINDataTagsSpreadM)[i]
  dotPDF$country = country
  streamToQuestionRel = rbind(streamToQuestionRel,dotPDF)
}



}



saveRDS(streamToQuestionRel,"D:/NUS/FYP/data/streamToQuestionRel")