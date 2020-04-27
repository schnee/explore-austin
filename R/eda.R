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

social_cap <- survey_df %>% select(name, knows_how_to_fix_a_car:owns_a_car)

cap_long <- social_cap %>%
  pivot_longer(knows_how_to_fix_a_car:owns_a_car, names_to = "cap_measure", values_to = "value") %>%
  mutate(value = na_if(value, "Yes | No"),
         value = na_if(value,  "IDK"),
         value = na_if(value, "I don't know"),
         value = na_if(value, "`Yes"))

cap_long %>% count(cap_measure,value) %>%
  pivot_wider(id_cols = "cap_measure", names_from = "value", values_from = "n") %>%
  arrange(desc(No))


cap_long %>%
  mutate(numeric_value = if_else(value == "No", 0, 1)) %>%
  group_by(name) %>% tally(numeric_value)

cap_density <- cap_long %>%
  mutate(numeric_value = if_else(value == "No", 0, 1)) %>%
  group_by(name) %>% tally(numeric_value) %>%
  ggplot(aes(x=n)) +
  geom_boxplot(fill = base_color, color=base_color, alpha=0.5) +
  theme_ipsum()

