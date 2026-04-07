# BIBLIOGRAPHY CHART

library(readr)
library(tidyverse)
bibliography <- read_csv("bibliography_final.csv", 
                           skip = 1)


bibliography<-bibliography%>%
  select(`Author / Year`, Year)%>%
  count(Year)

new_bibliography<- bibliography%>%
  complete(Year = 1986:2026) %>%
  mutate(n = replace_na(n, 0))

library(ggplot2)

bib_year<-ggplot(data=new_bibliography, aes(x=Year, y=n))+
  geom_line(linewidth=1.3, color="lightblue")+
  theme_bw()+
  geom_vline(xintercept=1989, linetype="dashed", color="darkblue", linewidth=0.6)+
  geom_vline(xintercept=2004, linetype="dashed", color="darkblue", linewidth=0.6)+
  geom_vline(xintercept=2022, linetype="dashed", color="darkblue", linewidth=0.6)+
  theme(
    panel.grid=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(color="black"))+
  ylab("No. of publications")+
  scale_y_continuous(
    limits = c(0, 11),
    breaks = seq(0, 11, by = 1),
    expand = c(0, 0)
  )
bib_year
#ggsave("bibliography_year.jpg", plot = bib_year, width = 6, height = 4, units = "in", dpi = 500)


