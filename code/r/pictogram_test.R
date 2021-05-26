library(waffle)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(forcats)

cugoldtet <- c("#CFB87C",
               "#7CCF8F",
               "#7C93CF",
               "#CF7CBD")

retn.agg <- retn_test %>%
    group_by(quartile) %>%
    summarise(values = n()) %>%
    mutate(policy_label = case_when(quartile == 1 ~ "High",
                                    quartile == 2 ~ "Medium",
                                    quartile ==3 ~ "Low"),
           policy_label = factor(policy_label,levels = c("Low","Medium","High"))) %>% 
    as.data.frame(.)

table(retn.agg$policy_label)

ggplot(retn.agg, aes(label = policy_label, values = values,color=policy_label)) +
    geom_pictogram(n_rows = 10, make_proportional = T,inherit.aes = T,flip = T) +
    scale_color_manual(
        name = NULL,
        values = cugoldtet) +
    scale_label_pictogram(
        name = NULL,
        values = c(
            Low  = "user-graduate", 
            Medium  = "user-graduate",
            High = "user-graduate")) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() +
    theme(legend.key.height = unit(2.25, "line")) +
    theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) 
    #ggeasy::easy_remove_legend()

library(scales)
s <- read.csv("data/ipeds_Data_5-24-2021.csv",stringsAsFactors = F)

s %<>% clean_names(.)
head(s)

table(s$x)

s.agg <- s %>%
    rename(applicants = applicants_total_adm2018_rv,
           admits = admissions_total_adm2018_rv) %>%
    select(unit_id,applicants,admits) %>%
    filter(!is.na(applicants) & !is.na(admits)) %>%
    mutate(selectivity = admits/applicants) %>%
    as.data.frame(.)

head(s.agg)

ggplot(s.agg, aes(selectivity)) + stat_ecdf(geom = "step") +
    ggtitle(label = "Cumulative Distribution of Selectivity\n",subtitle = "") +
    theme_clean() +
    scale_y_continuous(breaks = seq(0,1,.05)) + 
    scale_x_continuous(breaks = seq(0,1,.05)) +
    ylab("Cumulative Proportion of Institutions\n\n") +
    xlab("\nAdmit Rate 2018") +
    geom_vline(xintercept = .35)
    
