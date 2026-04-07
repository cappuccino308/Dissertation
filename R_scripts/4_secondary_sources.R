# SECONDARY SOURCES 
library(readr)
metadata <- read_csv("metadata_final.csv", skip=1)
library(tidyverse)

secondary_methods<-metadata%>%
  select(`Outcome measure focus`, `Covidence ID`, `Secondary data type`, `Secondary data source...12`, `Secondary data source...13`, `Study focus`)


source_1 <- secondary_methods %>%
  distinct(`Covidence ID`, `Secondary data type`, `Study focus`) %>%
  rename(secondary = `Secondary data type`)

source_2 <- secondary_methods %>%
  distinct(`Covidence ID`, `Secondary data source...12`, `Study focus`) %>%
  rename(secondary = `Secondary data source...12`)

source_3 <- secondary_methods %>%
  distinct(`Covidence ID`, `Secondary data source...13`, `Study focus`) %>%
  rename(secondary = `Secondary data source...13`)

# standardise values 
secondary_tally <- bind_rows(source_1, source_2, source_3) %>%
  filter(!is.na(secondary), secondary != "N/A") %>%
  mutate(secondary = case_when(
    secondary %in% c("Catch data (SERNAPESCA)", "Landings (SERNAPESCA)", "Landings data", "Landings data (SERNAPESCA)", "National Fisheries Service",
                     "National Fisheries Service (SERNAPESCA)", "Official reports (SERNAPESCA)", "Landing data", "Stranding records (SERNAPESCA)",
                     "Official reports", "Landing data (SERNAPESCA)") ~ "SERNAPESCA",
    secondary %in% c("Monitoring reports (SUBPESCA)", "Quotas (SUBPESCA)", 
                     "Technical reports (SUBPESCA)", "National Fisheries Service, SUBPESCA", "Technical reports", 
                     "Reports (Undersecretariat of Fisheries and Aquaculture)", "Undersecretariat for Fisheries") ~ "SUBPESCA",
    secondary %in% "Published studies" ~ "Published literature",
    secondary %in% c("Databases (OBIS, WORMS, museums, GBIF)", "Global Biodiversity Information Facility", "Survey (CEQUA and INACH)") ~ "Occurrence databases",
    secondary %in% c("Global Fishing Watch (Vessel Monitoring System)", "Global fishing watch") ~ "Global Fishing Watch",
    secondary %in% c("Geographical Information Systems (GIS)", "NASA Ocean Colour", "GIS (Google Earth)") ~ "GIS",
    secondary %in% "Gray literature" ~ "Grey literature",
    secondary %in% c("Historical documentation", "Legal documents", "Official documents") ~ "Historical documents",
    TRUE ~ secondary
  )) %>%
  distinct(`Covidence ID`, secondary, `Study focus`) %>%   # deduplicate after standardising names
  count(secondary)   %>%                       # now tally
  slice_max(n, n=8)


ggplot(secondary_tally, aes(x = secondary, y = n)) +
  geom_col(fill="steelblue") +
  #  coord_polar(theta = "y") +
  theme_classic()+
  ylab("No. of articles")+
  xlab("Secondary data source")+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))
# ggsave("secondary.png", dpi=500)

