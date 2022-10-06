library(tidyverse)
library(ggrepel)
library(patchwork)


#colors
manual_colors <-  c("OT-2" = "dodgerblue4","manual"= "orange","incubation_time" = "grey")


#part1
part1<- tribble(~item, ~activity,~manual_OT_2,~start,~end,
        1,"sample dilution","OT-2",0,58,
        2,"reduction","OT-2",58,63,
        3,"reduction","manual",63,64,
        4,"reduction","incubation_time",64,94,
        5,"reduction","manual",94,95,
        6,"alkylation","OT-2",95,100,
        7,"alkylation","manual",100,101,
        8,"alkylation","incubation_time",101,116,
        9,"SP3 beads dispensing","OT-2",116,131
        )


# part2
part2<- tribble(~item, ~activity,~manual_OT_2,~start,~end,
                1,"SP3","OT-2",0,118,
                2,"empty thrash","manual",30,31,
                3,"empty thrash","manual",60,61,
                4,"replace tips","manual",71,73
)


# part3 EvoTip
part3_EvoTip<- tribble(~item, ~activity,~manual_OT_2,~start,~end,
                1,"sample elution & dilution","OT-2",0,21,#sample prep
                2,"EvoTip preparation","OT-2",21,23,#Solvent B
                3,"centrifugation","manual",23,25,
                4,"EvoTip preparation","OT-2",25,27,#Solvent A
                5,"centrifugation","manual",27,30,
                6,"EvoTip preparation","OT-2",30,35,#sample loading
                7,"centrifugation","manual",35,38,
                8,"EvoTip preparation","OT-2",38,39,#Solvent A
                9,"centrifugation","manual",39,42,
                10,"EvoTip preparation","OT-2",42,45,#Solvent A
                11,"centrifugation","manual",45,47
)


# part4 post Evotip elution
part4_EvoTip<- tribble(~item, ~activity,~manual_OT_2,~start,~end,
                1,"elution","OT-2",0,18
)

# part3 MSvial
part3_MSvial<- tribble(~item, ~activity,~manual_OT_2,~start,~end,
                       1,"elution","OT-2",0,100
)


# tidy data ---------------------------------------------------------------
  
#tidy part1
part1_fact <- unique(part1$activity)
part1_tidy <- part1 %>% 
  pivot_longer(c("start","end"),names_to = "start_end",values_to = "minutes") %>% 
  mutate(activity = factor(activity,levels = rev(part1_fact)))

#tidy part2
part2_fact <- unique(part2$activity)
part2_tidy <- part2 %>% 
  pivot_longer(c("start","end"),names_to = "start_end",values_to = "minutes") %>% 
  mutate(activity = factor(activity,levels = rev(part2_fact)))

#tidy part3 EvoTip
part3_EvoTip_fact <- unique(part3_EvoTip$activity)
part3_EvoTip_tidy <- part3_EvoTip %>% 
  pivot_longer(c("start","end"),names_to = "start_end",values_to = "minutes") %>% 
  mutate(activity = factor(activity,levels = rev(part3_EvoTip_fact)))

#tidy part4
part4_EvoTip_fact <- unique(part4_EvoTip$activity)
part4_EvoTip_tidy <- part4_EvoTip %>% 
  pivot_longer(c("start","end"),names_to = "start_end",values_to = "minutes") %>% 
  mutate(activity = factor(activity,levels = rev(part4_EvoTip_fact)))


#tidy part3 MSvial
part3_MSvial_fact <- unique(part3_MSvial$activity)
part3_MSvial_tidy <- part3_MSvial %>% 
  pivot_longer(c("start","end"),names_to = "start_end",values_to = "minutes") %>% 
  mutate(activity = factor(activity,levels = rev(part3_MSvial_fact)))


# plots -------------------------------------------------------------------

part1_plot<- ggplot(part1_tidy, aes(minutes, activity, color = manual_OT_2, group=item)) +
  geom_line(size = 10) +
  labs(x="time [min]", y=NULL, title="Mass Spec Preppy - Part 1 - time chart", subtitle = "sample dilution + reduction and alkylation of 96 samples")+
  theme_light()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,140,10))+
  scale_color_manual(values = manual_colors)+
  geom_label_repel(mapping = aes(label = paste(minutes,"'",sep="")),size = 2,
                   nudge_y = rep(c(-0.5,0.5),nrow(part1_tidy)/2),show.legend = F,segment.color = "black",min.segment.length = 0.01)+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(0,140))+
  guides(color = "none")


part2_plot<- ggplot(part2_tidy, aes(minutes, activity, color = manual_OT_2, group=item)) +
  geom_line(size = 10) +
  labs(x="time [min]", y=NULL, title="Mass Spec Preppy - Part 2 - time chart", subtitle = "SP3 of 96 samples")+
  theme_light()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,140,10))+
  scale_color_manual(values = manual_colors)+
  geom_label_repel(mapping = aes(label = paste(minutes,"'",sep="")),size = 2,
                   nudge_y = rep(c(-0.5,0.5),nrow(part2_tidy)/2),show.legend = F,segment.color = "black",min.segment.length = 0.01)+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(0,140))+
  guides(color = "none")


part3_EvoTip_plot<- ggplot(part3_EvoTip_tidy, aes(minutes, activity, color = manual_OT_2, group=item)) +
  geom_line(size = 10) +
  labs(x="time [min]", y=NULL, title="Mass Spec Preppy - Part 3 - time chart", subtitle = "Evotip Pure loading of 96 samples")+
  theme_light()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,140,10))+
  scale_color_manual(values = manual_colors)+
  geom_label_repel(mapping = aes(label = paste(minutes,"'",sep="")),size = 2,
                   nudge_y = rep(c(-0.5,0.5),nrow(part3_EvoTip_tidy)/2),show.legend = F,segment.color = "black",min.segment.length = 0.01)+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(0,140))+
  guides(color = "none")


part4_EvoTip_plot<- ggplot(part4_EvoTip_tidy, aes(minutes, activity, color = manual_OT_2, group=item)) +
  geom_line(size = 10) +
  labs(x="time [min]", color = "", y=NULL, title="Mass Spec Preppy - Part 4 - time chart", subtitle = "elution to plate of 96 samples")+
  theme_light()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,140,10))+
  scale_color_manual(values = manual_colors)+
  geom_label_repel(mapping = aes(label = paste(minutes,"'",sep="")),size = 2,
                   nudge_y = rep(c(-0.5,0.5),nrow(part4_EvoTip_tidy)/2),show.legend = F,segment.color = "black",min.segment.length = 0.01)+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(0,140))

part3_MSvial_plot<- ggplot(part3_MSvial_tidy, aes(minutes, activity, color = manual_OT_2, group=item)) +
  geom_line(size = 10) +
  labs(x="time [min]", color = "", y=NULL, title="Mass Spec Preppy - Part 3 - time chart", subtitle = "MS vial elution to plate of 96 samples")+
  theme_light()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,140,10))+
  scale_color_manual(values = manual_colors)+
  geom_label_repel(mapping = aes(label = paste(minutes,"'",sep="")),size = 2,
                   nudge_y = rep(c(-0.5,0.5),nrow(part3_MSvial_tidy)/2),show.legend = F,segment.color = "black",min.segment.length = 0.01)+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(0,140))



# combine charts ----------------------------------------------------------



evotip_timechart_OT2<- part1_plot/part2_plot/part3_EvoTip_plot/part4_EvoTip_plot
ggsave(plot = evotip_timechart_OT2,filename = "www/OT2_Evotip_timechart.png",device = "png",width = 11,height = 9)

MSvial_timechart_OT2<- part1_plot/part2_plot/part3_MSvial_plot
ggsave(plot = MSvial_timechart_OT2,filename = "www/OT2_MSvial_timechart.png",device = "png",width = 11,height = 7)
