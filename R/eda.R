library(tidyverse)
library(googlesheets4)

if(!sheets_has_token()){
  sheets_auth(email = 'schneeman@gmail.com')
}

id <- NULL
if(sheets_has_token()) {
  sheet <- sheets_find(pattern = '2019 Fall Annual Survey Data_Breakdown by Scale')
  id <- sheet$id
}

survey <- sheets_get(id)

survey_df <- sheets_read(id, sheet=survey$sheet$name[1], skip = 2)

survey_df %>% group_by(`Name of School`, `Grade`) %>% summarize(ct = n()) %>%
  mutate(ct = as.factor(ct)) %>%
  ggplot(aes(x=`Name of School`, y=`Grade`, fill=ct)) + geom_tile() +
  scale_fill_discrete()


survey_df %>% mutate(`Do you receive free/reduced lunch?` = if_else(`Do you receive free/reduced lunch?` %in% c('Yes', 'No'), `Do you receive free/reduced lunch?`, "I don't know")) %>%
  group_by(`Do you receive free/reduced lunch?`) %>% summarize(ct = n()) %>%
  ggplot(aes(x=`Do you receive free/reduced lunch?`, y=ct)) +
  geom_col() + theme_ipsum() +
  theme(legend.position = "none")

