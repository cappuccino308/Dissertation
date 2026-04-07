#### SOCIAL OUTCOMES and TAXONOMIC GROUPS

## User perception categories

library(readr)
metadata <- read_csv("metadata_final.csv", skip=1)
library(tidyverse) 
library(ggplot2)


# No of papers per user perception category
social_data<-metadata%>%
  filter(`Outcome category`=="User perception")%>%
  group_by(`Outcome measure`, `Covidence ID`)%>%
  distinct(`Outcome measure`, `Covidence ID`)%>%
  ungroup()%>%
  count(`Outcome measure`)%>%
  filter(n>1)

social_plot<-ggplot(social_data, aes(x=reorder(`Outcome measure`, n), y=n))+  
  geom_bar(stat = "identity")+
  coord_flip() +               
  theme_classic()+
  geom_col(fill = "#666699")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  labs(y="No. of articles", x="Social outcome category")
social_plot
#ggsave("social_plot.png", plot=social_plot, dpi=600)

unique(social_data$`Outcome measure`)


# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# Demographic groups
demo_groups<-metadata%>%
  filter(!`Demographic groups`=="N/A")%>%
  separate_rows(`Demographic groups`, sep=",")%>%
  mutate(demo_clean = str_remove(`Demographic groups`, "\\s*\\(.*\\)")) %>%
  mutate(demo_clean = str_trim(demo_clean))%>%
  mutate(demo_clean=case_when(
    demo_clean == "Artisanal fishermen" ~ "Fishers",
    demo_clean == "Artisanal fishers" ~ "Fishers",
    demo_clean == "Fishing Leaders" ~ "Fishers' union leaders",
    demo_clean == "Fishing leaders" ~ "Fishers' union leaders",
    demo_clean == "Fishing union Directorates" ~ "Fishers' union leaders",
    demo_clean == "Leaders of fisher's organisations" ~ "Fishers' union leaders",
    demo_clean == "Syndicate members" ~ "Fishers",
    demo_clean == "Government agencies / representatives" ~ "Government officials",
    demo_clean == "Government representative" ~ "Government officials",
    demo_clean == "State representatives" ~ "Government officials",
    demo_clean == "Government institutions" ~ "Government officials",
    demo_clean == "Government official" ~ "Government officials",
    demo_clean == "Union leaders" ~ "Fishers' union leaders",
    demo_clean == "Seaweed farmer unions" ~ "Fishers",
    demo_clean == "Fishers associations representatives" ~ "Fishers' union leaders",
    demo_clean == "Fishers' leaders" ~ "Fishers' union leaders",
    demo_clean == "Fishers' organisation leaders" ~ "Fishers' union leaders",
    demo_clean == "Academics" ~ "Researchers",
    demo_clean == "resident" ~ "Residents",
    demo_clean == "Local people" ~ "Residents",
    demo_clean == "Syndicates' commission" ~ "Fishers' union leaders",
    demo_clean == "Commissioners" ~ "Fishers' union leaders",
    demo_clean == "SUBPESCA directors" ~ "Government officials",
    demo_clean == "SERNAPESCA director" ~ "Government officials",
    demo_clean == "Leader of sport divers organisation" ~ "Sport divers",
    demo_clean == "Divers" ~ "Sport divers",
    demo_clean == "Indigenous Peoples" ~ "Indigenous community leaders",
    demo_clean == "Indigenous community leader" ~ "Indigenous community leaders",
    demo_clean == "NGOs" ~ "NGO staff",
    demo_clean == "Technical consultant" ~ "Consultants",
    demo_clean == "Experts" ~ "Consultants",
    demo_clean == "aquaculture workers" ~ "Fishers",
    demo_clean == "TURF members" ~ "Fishers",
    TRUE ~ demo_clean  # keep other values unchanged
  ))%>%
  distinct(`Covidence ID`, demo_clean)

# Demographic groups of participants (from user perception measures only)

only_demo<-demo_groups%>%
  filter(!`Covidence ID`%in% c( "#191", "#86" , "#288", "#213" ,"#112"))%>%
  distinct(`Covidence ID`, demo_clean)%>%
  count(demo_clean)%>%
  filter(n>1)


new_demo<-ggplot(only_demo, aes(x=reorder(demo_clean, n), y=n))+
  geom_bar(stat = "identity")+
  coord_flip() +               
  theme_classic()+
  geom_col(fill = "#666699")+
  labs(y="No. of articles", x="Demographic group")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))
new_demo
#ggsave("demo_plot.png", plot=new_demo, dpi=500)


# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

## Species data
  

## TAXONOMIC GROUP BY MANAGEMENT TYPE
taxon_site <- metadata %>%
  left_join(
    grouped_management %>% select(`Covidence ID`, site),
    by = "Covidence ID"
  )%>%
  select(`Covidence ID`, Taxon, site) %>%
  filter(Taxon != "N/A") %>%
  mutate(new_taxon = case_when(
    Taxon %in% c("Benthic assemblages", "Benthic communities") ~ "Benthic fauna",
    Taxon == "Invertebrate" ~ "Invertebrates",
    Taxon == "Keyhole limpet, Trematode parasite" ~ "Keyhole limpet",
    Taxon == "Fish" ~ "Reef fish",
    TRUE ~ Taxon
  )) %>%
  group_by(`Covidence ID`, new_taxon) %>%
  distinct() %>%
  ungroup()

length(unique(taxon_site$`Covidence ID`))

top_taxa_site <- taxon_site %>%
  count(new_taxon) %>%
  slice_max(n, n = 10) %>%
  pull(new_taxon)


taxon_site <- taxon_site %>%
  filter(new_taxon %in% top_taxa) %>%
  count(new_taxon, site) %>%
  mutate(new_taxon = fct_reorder(new_taxon, -n, .fun = sum))%>%
  filter(!site=="N/A")

ggplot(taxon_site,
       aes(x = new_taxon, y = n, fill = site)) +
  geom_col() +
  # coord_flip() +
  theme_classic() +
  # scale_fill_viridis_d(option="G") +
  labs(y = "No. of articles", x = "Taxonomic group")+
  theme(
    legend.text = element_text(size = 9.5),
    legend.title = element_text(size = 10)
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(
    name = "Management type",
    values = c("#01082D", "steelblue", "#DEF5E5FF"),
    labels = c("MPA", "TURF", "TURF+MPA"))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))
# ggsave("taxon_bar.png", dpi=500)


# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
## No of papers per species
species_data<-metadata%>%
  select(`Covidence ID`, Species)%>%
  filter(!Species=="N/A")%>%
  mutate(new_species = case_when(
    Species == "Fissurella latimarginata" ~ "Fissurella spp",
    Species == "Fissurella Latimarginata" ~ "Fissurella spp",    
    Species == "Fissurella crassa" ~ "Fissurella spp",
    Species == "Fissurella limbata" ~ "Fissurella spp",
    Species == "Fissurella picta" ~ "Fissurella spp",
    TRUE ~ Species  # keep other values unchanged
  ))%>%
  group_by(`Covidence ID`, new_species)%>%
  distinct()%>%
  ungroup()%>%
  count(new_species)%>%
  slice_max(n, n=10)

ggplot(species_data, aes(x=reorder(new_species, n), y=n))+
  geom_bar(stat = "identity")+
  coord_flip() +               
  theme_classic()+
  geom_col(fill = "darkblue")+
  labs(y="Number of publications", x="Taxa")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))

