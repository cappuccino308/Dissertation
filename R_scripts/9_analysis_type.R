
### Type of analysis used per discipline
analysis_type<-metadata%>%
  distinct(`Covidence ID`, Analysis, `Study focus`, `Outcome measure`)%>% # remove outcome measure for per paper
  mutate(`Study focus`=case_when(
    `Study focus`=="Social-economic"~"Interdisciplinary",
    `Study focus`=="Social-environmental"~"Interdisciplinary",
    `Study focus`=="Economic-environmental"~"Interdisciplinary",
    `Study focus`=="Social-economic-environmental"~"Interdisciplinary",
    TRUE~`Study focus`))

#  count(Analysis, `Study focus`)
       

library(ggplot2)
ggplot(analysis_type, aes(x=`Study focus`, fill=Analysis))+
  geom_bar(position="stack")+
#  scale_y_continuous(labels=scales::percent)+
  theme_classic()+
  scale_fill_viridis_d(option="G")+
  ylab("Total no. of reported measures")



