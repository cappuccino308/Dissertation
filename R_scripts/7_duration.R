# STUDY DURATION BY MANAGEMENT TYPE
library(readr)
metadata <- read_csv("metadata_final.csv", skip=1)
library(tidyverse) 
library(ggplot2)


max_duration_col <-metadata%>%
  select(`Study duration`, `Covidence ID`, `Study focus`)%>%
  rename(duration_clean = `Study duration`) %>%
  mutate(duration_clean = case_when(
    duration_clean == "<1" ~ 1,
    duration_clean %in% c("N/A", "unknown", "NA") ~ NA_real_,
    TRUE ~ as.numeric(duration_clean)
  ))%>%
  group_by(`Covidence ID`)%>%
  mutate(max_dur = max(duration_clean, na.rm = TRUE))%>%
  distinct(`Covidence ID`, `Study focus`, max_dur)%>%
  filter(!is.na(`Covidence ID`))%>%
  ungroup()


site_duration <- max_duration_col %>%
  left_join(
    grouped_management %>% select(`Covidence ID`, site),
    by = "Covidence ID"
  )


ggplot(site_duration, aes(x = max_dur, fill = site)) +
  geom_histogram(binwidth = 10, boundary = 0, color = "white", position="stack") +
  theme_classic() +
  labs(x = "Study duration (years)", y = "No. of articles")+
  scale_y_continuous(breaks = seq(0, 130, by = 10), expand = c(0,0))+
  scale_x_continuous(breaks = seq(0, 45, by = 10), expand = c(0,0))+
  theme(
    legend.text = element_text(size = 9.5),
    legend.title = element_text(size = 10)
  )+
  labs(fill = "Management type")+
  scale_fill_manual(
    name = "Management type",
    values = c("#01082D", "steelblue", "#DEF5E5FF"),
    labels = c("MPA", "TURF", "TURF+MPA"))
#ggsave("duration_plot.png", dpi=500)


