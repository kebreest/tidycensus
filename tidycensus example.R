#----install-packages----------------------------------------------------
# install.packages(c("tidycensus", "tidyverse", "geofacet", "ggridges","openxlsx"))

########################## Setup Code ####################################### 

library(tidycensus)
library(tidyverse)
library(foreign)
library(openxlsx)
library(writexl)
library(flextable)
library(officer)
library(readr)
library(readxl)
library(purrr)
library(janitor)
library(dplyr)
library(ggplot2)
library(scales)
library(tools)
library(stringr)
library(extrafont)

# Variable lookup
vars <- load_variables(2021, "acs5",cache =TRUE)

TownToWDA_Xwalk <- read.csv("J:/EPPR/Active Studies/Northeast Mass Innovation Ecosystem_FY23/Data/Crosswalks/Old/TownToWDA_Xwalk.csv")


WDA_df <- filter(TownToWDA_Xwalk,
                 T_WDA_City == "Greater Lowell WDA" 
                 | T_WDA_City == "Lower Merrimack Valley WDA" 
                 | T_WDA_City == "North Shore WDA")

View(WDA_df)

GEOID_list <- as.character(WDA_df$GEOID)

geographys <- "County Subdivision"
years <- 2021
states <- "MA"
outputs <- "tidy"
tables <- c("C15002A")
setwd("J:/EPPR/Active Studies/Northeast Mass Innovation Ecosystem_FY23/Data Work")
getwd()

#creating filtered dataframe of WDA towns
MA_df <- data.frame(select(TownToWDA_Xwalk, GEOID, NAMELSAD, T_WDA_City))

updated_df <- MA_df %>%
  mutate(NEMA = if_else(T_WDA_City == "Greater Lowell WDA" | T_WDA_City == "Lower Merrimack Valley WDA" | T_WDA_City == "North Shore WDA", "TRUE" , "FALSE"))
View(updated_df)

WDA_df <- filter(MA_df, T_WDA_City == "Greater Lowell WDA" | T_WDA_City == "Lower Merrimack Valley WDA" | T_WDA_City == "North Shore WDA") %>%
  rename(MCD = NAMELSAD) %>%
  mutate(MCD = case_when(MCD == "Manchester-by-the-Sea town" ~ "Manchester",
                         T ~ substr(MCD, 1, nchar(MCD)-5)))
View(WDA_df)

#List of GEOIDs as characters (for filtering)
GEOID_list <- as.character(WDA_df$GEOID)
MCD_list <- pull(WDA_df, MCD)

Ed_Attainment_25plus_White <-   get_acs(
  geography = geographys,
  table = "C15002A",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "White") 

Ed_Attainment_25plus_Black <-   get_acs(
  geography = geographys,
  table = "C15002B",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Black")

Ed_Attainment_25plus_AIAN <-   get_acs(
  geography = geographys,
  table = "C15002C",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Other")

Ed_Attainment_25plus_Asian <-   get_acs(
  geography = geographys,
  table = "C15002D",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Asian")

Ed_Attainment_25plus_NHPI <-   get_acs(
  geography = geographys,
  table = "C15002E",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Other")

Ed_Attainment_25plus_Other <-   get_acs(
  geography = geographys,
  table = "C15002F",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Other")

Ed_Attainment_25plus_TwoorMore <-   get_acs(
  geography = geographys,
  table = "C15002G",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Other")

Ed_Attainment_25plus_Hispanic <-   get_acs(
  geography = geographys,
  table = "C15002I",
  year= years,
  geometry = FALSE,
  state = states,
  output= outputs,
  cache_table = TRUE) %>%
  mutate(race = "Hispanic")

#then bind everything together
Ed_Attainment_25plus <- rbind(Ed_Attainment_25plus_White,
                              Ed_Attainment_25plus_Black,
                              Ed_Attainment_25plus_AIAN,
                              Ed_Attainment_25plus_Asian,
                              Ed_Attainment_25plus_NHPI,
                              Ed_Attainment_25plus_Other,
                              Ed_Attainment_25plus_TwoorMore,
                              Ed_Attainment_25plus_Hispanic) %>% 
  mutate(varnumber = substr(variable,nchar(variable)-2,nchar(variable))) %>%
  mutate (edlevel = case_when(varnumber =="001"~"Total",
                              varnumber =="002"~"Male and Female Total",
                              varnumber =="003"~"Less than HS",
                              varnumber =="004"~"HS or Equal",
                              varnumber =="005"~"Some College",
                              varnumber =="006"~"Bachelors or more",
                              varnumber =="007"~"Male and Female Total",
                              varnumber =="008"~"Less than HS",
                              varnumber =="009"~"HS or Equal",
                              varnumber =="010"~"Some College",
                              varnumber =="011"~"Bachelors or more"
  ))

#Filtering Edu Attainment by Race
filtered_Ed_Attainment_25plus <- filter(Ed_Attainment_25plus, GEOID %in% GEOID_list) %>%
  group_by(edlevel, race) %>%
  filter(edlevel != "Total") %>%
  filter(edlevel != "Male and Female Total") %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  group_by(race) %>%
  mutate(edu_share = estimate/sum(estimate))

#ggplot for Edu Attainment by Race
#ggplot for Edu Attainment by Race
edu_attainment_chart <- ggplot(filtered_Ed_Attainment_25plus, 
                               aes(x = race, 
                                   y = edu_share, 
                                   fill = factor(edlevel, levels=c('Bachelors or more', 'Some College', 'HS or Equal', 'Less than HS')), 
                                   label = scales::percent(edu_share))) +
  geom_bar(stat = "identity", position = "stack", width = .8)  +
  geom_text(colour = "white", size = 3, position = position_stack(vjust = 0.5), aes(label = sprintf("%1.1f%%", 100*edu_share))) +
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(limits = c("White", "Black", "Asian", "Hispanic", "Other")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  ggtitle(toTitleCase("Educational Attainment by Race")) +
  theme(axis.title.x = element_text(face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 12), 
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
        axis.text.x = element_text(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(title = toTitleCase("Legend"))) +
  labs(x = str_to_title("Race"), y = str_to_title("Percent"))

edu_attainment_chart

ggsave("J:/EPPR/Active Studies/Northeast Mass Innovation Ecosystem_FY23/Data Work/edu_attainment_chart.png", width = 10, height = 4)
