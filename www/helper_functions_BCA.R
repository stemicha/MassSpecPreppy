#____________________________________________________________________#
## Project:
## Script:
## Author:
## Version:
## Notes:
#____________________________________________________________________#



# tip rack plot -----------------------------------------------------------

BCA_tip_rack_plot <- function(label = "20ul tips",
                          OT_slot = "1",
                          number_of_used_tips = 80,
                          text_color = "black"){
  

slot_template_96 <- read_excel("www/slot_template_96well.xlsx")

slot_template_96$used_tip <- FALSE
if(number_of_used_tips!=0){slot_template_96$used_tip[1:number_of_used_tips] <- TRUE}


ggplot(slot_template_96,aes(col_pos,row_pos))+
  geom_tile()+
  geom_point(size = 5, mapping = aes(color = used_tip))+
  scale_color_manual(values = c("TRUE" = "darkgrey", "FALSE"= "grey30"))+
  labs(subtitle = paste("OT-2 slot:",OT_slot), title = label)+
  theme_void()+
  theme(text = element_text(color = text_color),plot.title = element_text(face = "bold"))+
  #theme(axis.text = element_text())+
  scale_x_continuous(breaks = c(1:12),position = "top")+
  guides(color = "none")+
  scale_y_discrete(limits=rev)

}

# preparation plate plot -----------------------------------------------------------

BCA_prep_plate_plot <- function(label = "preparation plate (96 NEST 100ul plate)",
                          OT_slot = "6",
                          point_size = 5,
                          number_of_samples = 12,
                          text_color = "black"){
  
  
  slot_template_96 <- read_excel("www/slot_template_96well.xlsx")
  
  slot_template_96$samples <- FALSE
  if(number_of_samples!=0){slot_template_96$samples[1:number_of_samples] <- TRUE}
  
  
  ggplot(slot_template_96,aes(col_pos,row_pos))+
    geom_tile()+
    geom_point(size = point_size, mapping = aes(color = samples))+
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE"= "#00897B"))+
    labs(subtitle = paste("OT-2 slot:",OT_slot), title = label)+
    theme_void()+
    theme(text = element_text(color = text_color),plot.title = element_text(face = "bold"))+
    #theme(axis.text = element_text())+
    scale_x_continuous(breaks = c(1:12),position = "top")+
    guides(color = "none")+
    geom_text(mapping = aes(x= col_pos, y = row_pos,
                            label = position),
              size = 2,angle = 0, color = "white")+
    scale_y_discrete(limits=rev)
  
}

# samples 1.5ml tube plot -----------------------------------------------------------

BCA_sample_rack_plot <- function(label = "samples_1 (24 x 1.5ml tube rack)",
                            OT_slot = "4",
                            BSA_standard = T,
                            number_of_samples = 12,
                            text_color = "black"){
  
  
  slot_template_24 <- read_excel("www/slot_template_24well.xlsx")
  
  slot_template_24$samples <- FALSE
  if(number_of_samples!=0){slot_template_24$samples[1:number_of_samples] <- TRUE}

  slot_template_24 <- slot_template_24 %>% mutate(samples=as.character(samples))
  
  
  if(BSA_standard==TRUE){
    slot_template_24 <- bind_rows(slot_template_24,tibble(running_number=24,sample="400µl BSA std. 1mg/ml",position= "D6",row_pos="D",col_pos=6,samples="BSA"))
  }
  
  plot_samples_tmp<- ggplot(slot_template_24,aes(col_pos,row_pos))+
    geom_tile()+
    geom_point(size = 10, mapping = aes(color = samples))+
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE"= "steelblue3","BSA"="firebrick3"))+
    labs(subtitle = paste("OT-2 slot:",OT_slot), title = label)+
    theme_void()+
    theme(text = element_text(color = text_color),plot.title = element_text(face = "bold"))+
    #theme(axis.text = element_text())+
    scale_x_continuous(breaks = c(1:12),position = "top")+
    guides(color = "none")+
    geom_text(mapping = aes(x= col_pos, y = row_pos,
                            label = position),
              size = 4,angle = 0, color = "white")+
    scale_y_discrete(limits=rev)
  
  if(BSA_standard==TRUE){
    
    plot_samples_tmp <- plot_samples_tmp+geom_label_repel(data = slot_template_24 %>% 
                                                            filter(sample=="400µl BSA std. 1mg/ml"),
                                                          mapping = aes(label=sample),nudge_y = 0.5)
  }
  plot_samples_tmp
}




# reagent plate plot 12well -----------------------------------------------

BCA_nest_12well_reagent_plot <- function(input_df=input_df,
                                     text_color = "black",
                                     label = "reagent plate (NEST 12)",
                                     OT_slot = "1"){
  
  #colors
  colorsN12 <- c("#1B5E20","#C62828","#0D47A1","#4E342E","#E65100","#000000","#6A1B9A","#4F8F00","#89694E","#00695C","#827717","#5E5E5E")
  
  #input example
  #input_df <- tibble(postion = c("A1","A2","A3","A4","A5"),
  #                   solution = c("95% acetonitril","water","wash buffer","80% ethanol","95% acetonitril1"),
  #                   volume_ml = c(6,10,11,12,6)
  #)
  
  #setup nest layout
  nest_12_plate_layout <- tibble(postion = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"),
                                 xmin = seq(0,11),
                                 xmax = seq(1,12),
                                 ymin = rep(0,12),
                                 ymax = rep(1,12)) |> 
    rowwise() |> 
    mutate(xmean = mean(c(xmin,xmax)), ymean = mean(c(ymin,ymax)))
  
  #merge data
  nest_12_plate_data <- left_join(nest_12_plate_layout,input_df,by = "postion")
  nest_12_plate_data <- nest_12_plate_data |> 
    mutate(label = paste(solution," (",volume_ml," ml)",sep = "")) |> 
    mutate(label = if_else(condition = is.na(solution),true = "",false = label))
  
  ggplot(nest_12_plate_data)+
    geom_rect(mapping = aes(xmin = xmin,xmax = xmax, ymin = ymin, ymax= ymax, fill = solution), color="black")+
    theme_void()+
    scale_fill_manual(values = colorsN12, na.value = "lightgrey")+
    geom_text(mapping = aes(x= xmean, y = ymean,
                            label = label),
              size = 3,angle = 90, color = "white")+
    guides(fill = "none")+
    labs(subtitle = paste("OT-2 slot:",OT_slot), title = label)+
    scale_x_continuous(breaks = c(0.5:11.5),position = "top",labels = c(1:12))+
    theme(#axis.text.x = element_text(vjust = -3, size = 18, face = "bold",color = text_color),
      text = element_text(color = text_color),
      plot.title = element_text(face = "bold"))
  
}



BCA_pipette_plot <- function(left = "20µl single channel",right = "20µl multi channel",text_color = "black"){
  
  #pipette images tibble
  images_pipettes<- tibble(
    label = c("20µl single channel","20µl multi channel","300µl multi channel"),
    IMAGE = c("www/pipette_pics_20_ul_single_channel.png","www/pipette_pics_20_ul_8_channel.png","www/pipette_pics_300_ul_8_channel.png"),
    size_factor = c(0.7,1,1)
  )

  #tibble for plotting
  pipette_tibble<- tibble(position = c("left","right"),
                          label =c(left,right),
                          x = c(0.65,2),y = c(0.5,0.5))                  
  #add images paths
  pipette_tibble <- left_join(pipette_tibble,images_pipettes,by = "label")
  
  ggplot(data = pipette_tibble,mapping = aes(x= x, y = y))+
    theme_void()+
    geom_image(aes(image = IMAGE),size = 0.2*pipette_tibble$size_factor) +
    geom_label(mapping = aes(y = 0.7,label = paste(paste(position,"postion:"),"\n",label)))+
    guides(fill = "none")+
    labs(title = "pipette setup")+
    coord_cartesian(xlim = c(0,3),ylim = c(0,1))+
    theme(#axis.text.x = element_text(vjust = -3, size = 18, face = "bold",color = text_color),
        plot.title = element_text(face = "bold", color = text_color))
  
  
  
}

# decklayout step 1 -------------------------------------------------------

plot_deck_layout_BCA <- function(number_of_samples = 20, text_color = "black"){
  
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- BCA_pipette_plot(left = "20µl single channel",right = "300µl multi channel")
  
    #NEST 12ml reagent plate step1
    input_df_NEST_12ml <- tibble(postion = c("A1","A2","A3","A4"),
                                  solution = c("water","1x sample buffer","2x sample buffer","BCA working reagent"),
                                  volume_ml = c(3,plate_columns_used*8*0.260+2,3,(plate_columns_used*2+2)*8*0.075+2)
    )
    
    #regents
    Slot3 <- BCA_nest_12well_reagent_plot(input_df = input_df_NEST_12ml,text_color = text_color,label = "reagent plate (NEST 12 column reservoir)",OT_slot = 3)
    
    #tips
    tips_20ul <- 96*2
    tips_300ul <- 96
    
    
    Slot10 <- plot_spacer()
    Slot11 <- plot_spacer()
    Slot6 <- plot_spacer()
    
    Slot7 <- BCA_tip_rack_plot(label = "20ul_tips_1",text_color = text_color,OT_slot = 7,number_of_used_tips = ifelse((tips_20ul-96)>=96,96,ifelse((tips_20ul-96)<0,0,(tips_20ul-96))))
    Slot8 <- BCA_tip_rack_plot(label = "20ul_tips_2",text_color = text_color,OT_slot = 8,number_of_used_tips = ifelse((tips_20ul-96)>=96,96,ifelse((tips_20ul-96)<0,0,(tips_20ul-96))))
    Slot9 <- BCA_tip_rack_plot(label = "300ul_tips",text_color = text_color,OT_slot = 9,number_of_used_tips = tips_300ul)
    
    #preparation plate
    Slot2<- BCA_prep_plate_plot(label = "preparation plate plate (full area plate)",point_size = 8,text_color = text_color,OT_slot = 2,number_of_samples = 0)
    Slot1<- BCA_prep_plate_plot(label = "BCA plate (half area plate)",point_size = 5,text_color = text_color,OT_slot = 1,number_of_samples = 0)
    
    #samples plot
    Slot4 <- BCA_sample_rack_plot(label = "samples_1 (24x 1.5ml tube rack)",text_color = text_color,OT_slot = 4,BSA_standard = F,number_of_samples = ifelse(number_of_samples>24,24,number_of_samples))
    Slot5 <- BCA_sample_rack_plot(label = "samples_2 (24x 1.5ml tube rack)",text_color = text_color,OT_slot = 5,BSA_standard = T,number_of_samples = ifelse((number_of_samples-24)>=24,24,ifelse((number_of_samples-24)<0,0,(number_of_samples-24))))
    
    
    BCA_deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
      (Slot7+Slot8+Slot9)/
      (Slot4+Slot5+Slot6)/
      (Slot1+Slot2+Slot3)
  
    BCA_deck_layout_plot_out
}




