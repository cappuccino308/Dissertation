

library(readr)
metadata <- read_csv("metadata_final.csv", skip=1)
library(tidyverse)

## Methods by management type

methods_site <- metadata %>%
  left_join(
    grouped_management %>% select(`Covidence ID`, site),
    by = "Covidence ID"
  )%>%
  select(site, `Study focus`, `Primary data collection method...8`, `Primary data collection method...9`, `Primary data collection method...10`, `Covidence ID`)

method_1<-methods_site%>%
  distinct(site, `Primary data collection method...8`, `Covidence ID`)%>%
  count(site, `Primary data collection method...8`)

method_2<-methods_site%>%
  distinct(site, `Primary data collection method...9`, `Covidence ID`)%>%
  count(site, `Primary data collection method...9`)

method_3<-methods_site%>%
  distinct(site, `Primary data collection method...10`, `Covidence ID`)%>%
  count(site, `Primary data collection method...10`)

methods_data <- bind_rows(
  method_1 %>% rename(method = `Primary data collection method...8`),
  method_2 %>% rename(method = `Primary data collection method...9`),
  method_3 %>% rename(method = `Primary data collection method...10`)
) %>%
  filter(method != "N/A") %>%
  group_by(site, method) %>%
  summarise(total_frequency = sum(n), .groups = "drop")

methods_data$method <- factor(
  methods_data$method,
  levels = c(
    "Artificial collectors","Biological sample","Capture and tagging", "Catch analysis", "Census", "Cohort study", "Direct extractive sampling", "Direct non-extractive sampling", "Laboratory analysis", "Mark and recapture", "Observation", " ",
    "Aerial census","Camera traps","Photo-identification","Underwater photography","Underwater video cameras (RUVs)", "Video analysis",
    "Litter collection", "Sediment sample", "",
    "Ethnography", "Focus group", "Interview","Participant observation", "Participatory Rural Appraisal (PRA)", "Participatory workshops","Questionnaire", "Survey","Workshop",
    "Bidding game","Best-worst case scaling","Contingent valuation","Dynamic common pool resource experiment", "Randomised response technique (RRT)"
  )
)

ggplot(methods_data, aes(y = method, x = site, fill = total_frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(total_frequency==1, "",total_frequency)), color = "white", size = 5) +
  scale_fill_gradientn(colours=c(
    "lightgrey",
    "steelblue",
    "darkblue",
    "#0F2573",
    "#03045E",
    "#01082D"
  )) +
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Management type", y = "Method", fill = "No. of articles")+
  scale_y_discrete(limits = rev(levels(methods_data$method)))
# ggsave("methods.png", dpi=500)



# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

### Methods per discipline

methods_data<-metadata%>%
  select(`Study focus`, `Primary data collection method...8`, `Primary data collection method...9`, `Primary data collection method...10`, `Covidence ID`)

method_1<-methods_data%>%
  distinct(`Study focus`, `Primary data collection method...8`, `Covidence ID`)%>%
  count(`Study focus`, `Primary data collection method...8`)

method_2<-methods_data%>%
  distinct(`Study focus`, `Primary data collection method...9`, `Covidence ID`)%>%
  count(`Study focus`, `Primary data collection method...9`)

method_3<-methods_data%>%
  distinct(`Study focus`, `Primary data collection method...10`, `Covidence ID`)%>%
  count(`Study focus`, `Primary data collection method...10`)

methods_data <- bind_rows(
  method_1 %>% rename(method = `Primary data collection method...8`),
  method_2 %>% rename(method = `Primary data collection method...9`),
  method_3 %>% rename(method = `Primary data collection method...10`)
) %>%
  filter(method != "N/A") %>%
  mutate(`Study focus`=case_when(
    `Study focus`=="Social-economic"~"Interdisciplinary",
    `Study focus`=="Social-environmental"~"Interdisciplinary",
    `Study focus`=="Economic-environmental"~"Interdisciplinary",
    `Study focus`=="Social-economic-environmental"~"Interdisciplinary",
    TRUE~`Study focus`))%>%
  group_by(`Study focus`, method) %>%
  summarise(total_frequency = sum(n), .groups = "drop")

methods_data$method <- factor(
  methods_data$method,
  levels = c(
    "Artificial collectors","Biological sample","Capture and tagging", "Catch analysis", "Census", "Cohort study", "Direct extractive sampling", "Direct non-extractive sampling", "Laboratory analysis", "Mark and recapture", "Observation", " ",
    "Aerial census","Camera traps","Photo-identification","Underwater photography","Underwater video cameras (RUVs)", "Video analysis",
    "Litter collection", "Sediment sample", "",
    "Ethnography", "Focus group", "Interview","Participant observation", "Participatory Rural Appraisal (PRA)", "Participatory workshops","Questionnaire", "Survey","Workshop",
    "Bidding game","Best-worst case scaling","Contingent valuation","Dynamic common pool resource experiment", "Randomised response technique (RRT)"
  )
)

methods_data$`Study focus`<-factor(
  methods_data$`Study focus`,
  levels= c("Economic", "Environmental", "Social", "Interdisciplinary")
)

ggplot(methods_data, aes(y = method, x = `Study focus`, fill = total_frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(total_frequency==1, "",total_frequency)), color = "white", size = 5) +
  scale_fill_gradientn(colours=c(
    "lightgrey",
    "steelblue",
    "darkblue",
    "#0F2573",
    "#03045E",
    "#01082D"
  )) +
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Focus", y = "Method", fill = "No. of articles")+
  scale_y_discrete(limits = rev(levels(methods_data$method)))





