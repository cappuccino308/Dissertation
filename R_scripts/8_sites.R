library(readr)
metadata <- read_csv("metadata_coded.csv", skip=1)
library(tidyverse)

safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

sites_no <- metadata %>%
  filter(!is.na(`Covidence ID`)) %>%
  rename(
    OA = `Number of comparator sites (OA)`,
    TURF = `Number of reference sites (TURF)`,
    MEABR = `Number of reference sites (MEABR)`,
    MPA = `Number of reference sites (MPA)`
  ) %>%
  mutate(across(c(OA, TURF, MEABR, MPA),
                ~ suppressWarnings(as.numeric(.)))) %>%
  group_by(`Covidence ID`, `Study focus`) %>%
  summarise(
    across(c(OA, TURF, MEABR, MPA), safe_max),
    TURFs = safe_max(TURF) + safe_max(MEABR),
    .groups = "drop"
  )


box_data<-sites_no %>%
  pivot_longer(cols = c(TURFs, MPA, OA), names_to = "variable", values_to = "value")%>%
  filter(value != 0)


ggplot(box_data, aes(x = value, y = variable)) +
  geom_boxplot(fill="lightblue") +
  labs(y = "Management type", x = "No. of sites")+
  coord_flip()+
  theme_classic()

