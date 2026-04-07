## OUTCOME PER MANAGEMENT TYPE

outcome_site <- metadata %>%
  left_join(
    grouped_management %>% select(`Covidence ID`, site),
    by = "Covidence ID"
  )%>%
  select(`Outcome measure focus`,`Outcome category`, `Covidence ID`, site)%>%
  filter(!is.na(`Outcome category`))%>%
  distinct(`Outcome category`, `Covidence ID`, site)%>%
  count(`Outcome category`, site)

outcome_site$`Outcome category`<- factor(
  outcome_site$`Outcome category`,
  levels = c(
    "Abundance","Area","Behaviour","Biomass","Buoyancy","Chemical composition","Community composition","Contamination",
    "Density","Diversity","Ecosystem functioning","Keystoneness","Parasitism","Population change","Predation",
    "Reproductive output","Richness","Size", " ",
    "Catch per unit effort","Fishing pressure","Income","Landings","Landings (illegal)","Management",
    "User perception","Willingness to pay"   
  )
)

unique(outcome_site$`Outcome category`)



ggplot(outcome_site, aes(y =  `Outcome category`, x = site, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(n==1, "",n)), color = "white", size = 5) +
  scale_fill_gradientn(colours=c(
    "#dddddd",
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
  labs(x = "Management type", y = "Outcome measured", fill = "No. of articles")+
  scale_y_discrete(limits = rev(levels(outcome_site$`Outcome category`)))
#ggsave("outcomes.png", width = 6, height = 6, units = "in", dpi = 500)





# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

## OUTCOME PER DISCIPLINE
outcome_data<-metadata%>%
  select(`Outcome measure focus`,`Outcome category`, `Covidence ID`)%>%
  filter(!is.na(`Outcome category`))%>%
  count(`Outcome category`, `Outcome measure focus`)

## per paper 
outcome_data<-metadata%>%
  select(`Study focus`,`Outcome category`, `Covidence ID`)%>%
  filter(!is.na(`Outcome category`))%>%
  distinct(`Outcome category`, `Covidence ID`, `Study focus`)%>%
  count(`Outcome category`, `Study focus`)

outcome_data<-metadata%>%
  select(`Outcome measure focus`,`Outcome category`, `Covidence ID`)%>%
  filter(!is.na(`Outcome category`))%>%
  distinct(`Outcome category`, `Covidence ID`, `Outcome measure focus`)%>%
  count(`Outcome category`, `Outcome measure focus`)


outcome_data$`Outcome category`<- factor(
  outcome_data$`Outcome category`,
  levels = c(
    "Abundance","Area","Behaviour","Biomass","Buoyancy","Chemical composition","Community composition","Contamination",
    "Density","Diversity","Ecosystem functioning","Keystoneness","Parasitism","Population change","Predation",
    "Reproductive output","Richness","Size", " ",
    "Catch per unit effort","Fishing pressure","Income","Landings","Landings (illegal)","Management",
    "User perception","Willingness to pay"   
  )
)


unique(outcome_data$`Outcome category`)

heatmap_2<-ggplot(outcome_data, aes(y =  `Outcome category`, x = `Outcome measure focus`, fill = n)) +
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
  labs(x = "Study focus", y = "Outcome measured", fill = "Frequency")+
  scale_y_discrete(limits = rev(levels(outcome_data$`Outcome category`)))
# heatmap_2



