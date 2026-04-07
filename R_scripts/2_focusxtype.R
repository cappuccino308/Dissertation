## STUDY FOCUS x MPA type

library(readr)
metadata <- read_csv("metadata_coded.csv", skip=1)
library(tidyverse)
library(ggplot2)

grouped_management <- metadata %>%
  rename(
    OA = `Number of comparator sites (OA)`,
    TURF = `Number of reference sites (TURF)`,
    MEABR = `Number of reference sites (MEABR)`,
    MPA = `Number of reference sites (MPA)`,
  ) %>%
mutate(
  across(c(OA, TURF, MEABR, MPA),
         ~ as.numeric(if_else(. == "Multiple", "1", as.character(.))))
) %>%
  group_by(`Covidence ID`, `Study focus`) %>%
  summarise(
    OA = max(OA, na.rm = TRUE),
    TURF = max(TURF, na.rm = TRUE),
    MEABR = max(MEABR, na.rm = TRUE),
    MPA = max(MPA, na.rm = TRUE),
    max_TURFs = max(TURF, na.rm = TRUE) + max(MEABR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    site = case_when(
      MPA == 0 & max_TURFs >= 1 ~ "TURF",
      MPA >= 1 & max_TURFs == 0 ~ "MPA",
      max_TURFs >= 1 & MPA >= 1 ~ "TURF+MPA",
      TRUE ~ "Unknown"
    )
  )%>% ## comparator
  mutate (
    comparator = case_when(
      OA >=1 ~ "OA",
    #  OA == ~ "N/A", 
      TRUE ~ "Unknown"
    ))


summary_sites<-grouped_management%>%
  filter(!is.na(`Study focus`))%>%
  mutate(`Study focus` = factor(`Study focus`,
                           levels = c("Social", "Economic", "Environmental", "Social-environmental", "Social-economic", "Economic-environmental", "Social-economic-environmental"))) %>%
  count(site, `Study focus`) %>%
  ungroup()


type_focus<-ggplot(summary_sites, aes(y = `Study focus`, x = site, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", size = 5) +
  scale_fill_gradientn(colours=c(
    "lightblue",
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
  labs(x = "Management type", y = "Study focus", fill = "No. of articles")+
  scale_y_discrete(limits = rev)
type_focus
#ggsave("type_focus.png", plot=type_focus, dpi=500)


