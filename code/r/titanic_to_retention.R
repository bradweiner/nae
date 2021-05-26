##turn the titanic data into retention data##

library(tidyverse)
library(magrittr)
library(janitor)
library(scales)
library(tidy)

d <- read.csv("https://gist.githubusercontent.com/michhar/2dfd2de0d4f8727f873422c5d959fff5/raw/fa71405126017e6a37bea592440b4bee94bf7b9e/titanic.csv",stringsAsFactors = F)

d %<>% clean_names(.)
head(d)

d.agg <- d %>%
    select(-ticket,-name,-cabin) %>%
    rename(.,student_id = passenger_id,
              retained = survived,
              income_group = pclass,
              siblings_enrolled = sib_sp,
              peers_from_hs = parch,
              net_tuition = fare,
              #residence_hall = cabin,
              residency = embarked) %>%
    mutate(net_tuition = round(rescale(net_tuition,to = c(0,20000)),digits = 0)) %>%
    rowwise() %>%
    mutate(total_peer_group = sum(c(siblings_enrolled,peers_from_hs),na.rm = T)) %>%
    ungroup() %>%
    mutate(income_group = case_when(income_group == 1 ~ "No Aid",
                                    income_group == 2 ~ "State Grant Eligible",
                                    income_group == 3 ~ "Pell Eligible"),
           residency = case_when(residency == "S" ~ "Resident",
                                 residency == "Q" ~ "International",
                                 residency == "C" ~ "Non-Resident")) %>%
    as.data.frame(.)

head(d.agg)

write.csv(d.agg,"data/retention_file_raw.csv",row.names = F)


