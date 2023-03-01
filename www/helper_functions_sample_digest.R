#____________________________________________________________________#
## Project:
## Script:
## Author:
## Version:
## Notes:
#____________________________________________________________________#



# tip rack plot -----------------------------------------------------------

tip_rack_plot <- function(label = "20ul tips",
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

prep_plate_plot <- function(label = "preparation plate (96 NEST 100ul plate)",
                          OT_slot = "6",
                          number_of_samples = 12,
                          text_color = "black", outline = F, outline_color = "orangered"){
  
  
  slot_template_96 <- read_excel("www/slot_template_96well.xlsx")
  
  slot_template_96$samples <- FALSE
  if(number_of_samples!=0){slot_template_96$samples[1:number_of_samples] <- TRUE}
  
  
  prep_tmp_p<- ggplot(slot_template_96,aes(col_pos,row_pos))+
    geom_tile()+
    geom_point(size = 5, mapping = aes(color = samples))+
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
  
  if(outline==T){
    prep_tmp_p <- prep_tmp_p+
      theme(panel.border = element_rect(colour = outline_color, fill=NA, size=5))
  }
  prep_tmp_p
}

# samples 1.5ml tube plot -----------------------------------------------------------

sample_rack_plot <- function(label = "samples_1 (24 x 1.5ml tube rack)",
                            OT_slot = "4",
                            slot_meta_table = "samples1",
                            meta_table = meta_table,
                            text_color = "black"){
  
  
  slot_template_24 <- read_excel("www/slot_template_24well.xlsx")
  
  
  # clean meta table
  meta_table_cleaned <- meta_table %>% 
    filter(`OT-2 slot` == slot_meta_table) %>% 
    mutate(samples = ifelse(!is.na(sample),yes = TRUE,no = FALSE))

  
  slot_template_24 <- left_join(slot_template_24,
                                meta_table_cleaned %>% 
                                  filter(samples==TRUE) %>% 
                                  distinct(`OT-2 position`,samples), by = c("position"="OT-2 position")) %>% 
    rowwise() %>% 
    mutate(samples = ifelse(is.na(samples),FALSE,TRUE)) %>% 
    ungroup()
  
  slot_template_24 <- slot_template_24 %>% mutate(samples=as.character(samples))
  
  ggplot(slot_template_24,aes(col_pos,row_pos))+
    geom_tile()+
    geom_point(size = 10, mapping = aes(color = samples))+
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE"= "steelblue3"))+
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
  
}

# MS vial plot -----------------------------------------------------------

MSvial_plot <- function(label = "MSvials_1",
                             OT_slot = "6",
                             number_of_samples = 48,
                             text_color = "black"){
  
  
  slot_template_48 <- read_excel("www/slot_template_48well.xlsx")
  
  slot_template_48$samples <- FALSE
  if(number_of_samples!=0){slot_template_48$samples[1:number_of_samples] <- TRUE}
  
  ggplot(slot_template_48,aes(col_pos,row_pos))+
    geom_tile()+
    geom_point(size = 10, mapping = aes(color = samples))+
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE"= "steelblue3"))+
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
  
}



# reagent plate plot 12well -----------------------------------------------

nest_12well_reagent_plot <- function(input_df=input_df,
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
  nest_12_plate_data <- nest_12_plate_data %>% 
    mutate(label = paste(solution," (",volume_ml," ml)",sep = "")) %>% 
    mutate(label = if_else(is.na(volume_ml),true =  paste(solution),false = label)) %>% 
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


# reagent plate plot 96well -----------------------------------------------

nest_96well_reagent_plot <- function(input_df=input_df, 
                                     text_color = "black",
                                     label = "reagent plate (NEST 12)",
                                     OT_slot = "1"){
  
  #colors
  colorsN12 <- c("#1B5E20","#C62828","#0D47A1","#4E342E","#E65100","#000000","#6A1B9A","#4F8F00","#89694E","#00695C","#827717","#5E5E5E")
  
  #input example
  #input_df <- tibble(postion = c("A1","A2","A3","A4","A5"),
  #                   solution = c("95% acetonitril","water","wash buffer","80% ethanol","95% acetonitril1"),
  #                   volume_ul = c(6,10,11,12,6)
  #)
  
  #setup nest layout
  nest_12_plate_layout <- tibble(postion = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"),
                                 xmin = seq(0,11),
                                 xmax = seq(1,12),
                                 ymin = rep(0,12),
                                 ymax = rep(1,12)) |> 
    rowwise() |> 
    mutate(xmean = mean(c(xmin,xmax)), ymean = mean(c(ymin,ymax)))
  

  
  #generate circle sequence
  radius=0.1
  circle_seq<- seq(radius,1-radius,length.out=8)
  pos_unique <- unique(nest_12_plate_layout$postion)
  circle_pos_tibble <- tibble()
  for(i in 1:length(pos_unique)){
    tmp <- nest_12_plate_layout |> filter(postion==pos_unique[i])
    
    circle_pos_tibble <- bind_rows(circle_pos_tibble,
                                   tibble(position = pos_unique[i],
           circle_mid_y = circle_seq,
           circle_mid_x =  tmp$xmean, 
           radius = radius)
    )
  }
  
  #merge data
  nest_12_plate_data <- left_join(nest_12_plate_layout,input_df,by = "postion")
  nest_12_plate_data <- nest_12_plate_data |> 
    mutate(label = paste(solution," (",volume_ul," µl each well)",sep = "")) |> 
    mutate(label = if_else(condition = is.na(solution),true = "",false = label))
  
  ggplot(nest_12_plate_data)+
    geom_rect(mapping = aes(xmin = xmin,xmax = xmax, ymin = ymin, ymax= ymax, fill = solution), color="black")+
    theme_void()+
    geom_point(data = circle_pos_tibble,mapping = aes(x = circle_mid_x,y = circle_mid_y),
               inherit.aes = F,
               size = 5, shape = 1, alpha = 0.3, color = "grey")+
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

pipette_plot <- function(left = "20µl single channel",right = "20µl multi channel",text_color = "black"){
  
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

plot_deck_layout_step1 <- function(red_alk = TRUE,
                                   meta_table = meta_table,
                                   text_color = "black"){
  
  #number of samples
  number_of_samples <- nrow(meta_table %>% filter(!is.na(sample)))
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- pipette_plot(left = "20µl single channel",right = "20µl multi channel", text_color = text_color)
  
  #red alk = TRUE
  if(red_alk == TRUE){
    
    #NEST PCR reagent plate step1
    input_df_NEST_100ul <- tibble(postion = c("A1","A2","A3","A4","A5"),
                                  solution = c("1x sample buffer","1x sample buffer","12.5mM DTT","50mM IAA","50 µg/µl SP3 beads"),
                                  volume_ul = c(150,150,2.5*plate_columns_used+10,3.125*plate_columns_used+10,5*plate_columns_used+20)
    )
    
    
    #regents
    Slot3 <- nest_96well_reagent_plot(input_df = input_df_NEST_100ul,label = "reagent plate (96well NEST plate)",OT_slot = 3,text_color = text_color)
    
    #tips
    sample_dil_tips <- number_of_samples+1
    prep__tips <- 8*plate_columns_used*3
    
    
    Slot10 <- tip_rack_plot(label = "20ul_tips_1",text_color = text_color, OT_slot = 10,number_of_used_tips = ifelse(sample_dil_tips>96,96,sample_dil_tips))
    Slot8 <- tip_rack_plot(label = "20ul_tips_2",text_color = text_color,OT_slot = 8,number_of_used_tips = ifelse((sample_dil_tips-96)>=96,96,ifelse((sample_dil_tips-96)<0,0,(sample_dil_tips-96))))
    
    Slot11 <- tip_rack_plot(label = "20ul_tips_3",text_color = text_color,OT_slot = 11,number_of_used_tips = ifelse(prep__tips>96,96,prep__tips))
    Slot9 <- tip_rack_plot(label = "20ul_tips_4",text_color = text_color,OT_slot = 9,number_of_used_tips = ifelse((prep__tips-96)>=96,96,ifelse((prep__tips-96)<0,0,(prep__tips-96))))
    Slot6 <- tip_rack_plot(label = "20ul_tips_5",text_color = text_color,OT_slot = 6,number_of_used_tips = ifelse((prep__tips-192)>=192,192,ifelse((prep__tips-192)<0,0,(prep__tips-192))))

    #preparation plate
    Slot7<- prep_plate_plot(label = "HEAT/SHAKER MODULE +\nsample plate (96well NEST plate)",
                            text_color = text_color,
                            OT_slot = 7,
                            outline = T,
                            outline_color = "orangered",
                            number_of_samples = number_of_samples)
    #samples plot
    Slot4 <- sample_rack_plot(label = "samples_1 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 4,
                              slot_meta_table = "samples1",
                              meta_table = meta_table)
    Slot1 <- sample_rack_plot(label = "samples_2 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 1,
                              slot_meta_table = "samples2",
                              meta_table = meta_table)
    Slot5 <- sample_rack_plot(label = "samples_3 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 5,
                              slot_meta_table = "samples3",
                              meta_table = meta_table)
    Slot2 <- sample_rack_plot(label = "samples_4 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 2,
                              slot_meta_table = "samples4",
                              meta_table = meta_table)
    
    deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
      (Slot7+Slot8+Slot9)/
      (Slot4+Slot5+Slot6)/
      (Slot1+Slot2+Slot3)
    
  }
  
  #red alk = FALSE
  if(red_alk == FALSE){
    
    #NEST 12ml reagent plate step1
    input_df_NEST_100ul <- tibble(postion = c("A1","A2","A3"),
                                  solution = c("1x sample buffer","1x sample buffer","50 µg/µl SP3 beads"),
                                  volume_ul = c(150,150,5*plate_columns_used+20)
    )
    
    
    #regents
    Slot3 <- nest_96well_reagent_plot(input_df = input_df_NEST_100ul,text_color = text_color,label = "reagent plate (96well NEST plate)",OT_slot = 3)
    
    #tips
    sample_dil_tips <- number_of_samples*1+1
    prep__tips <- 8*plate_columns_used
    
    
    Slot10 <- tip_rack_plot(label = "20ul_tips_1",text_color = text_color,OT_slot = 10,number_of_used_tips = ifelse(sample_dil_tips>96,96,sample_dil_tips))
    Slot7 <- tip_rack_plot(label = "20ul_tips_2",text_color = text_color,OT_slot = 7,number_of_used_tips = ifelse((sample_dil_tips-96)>=96,96,ifelse((sample_dil_tips-96)<0,0,(sample_dil_tips-96))))
    
    Slot11 <- tip_rack_plot(label = "20ul_tips_3",text_color = text_color,OT_slot = 11,number_of_used_tips = ifelse(prep__tips>96,96,prep__tips))
    Slot8 <- plot_spacer()
    Slot9 <- plot_spacer()
    
    #preparation plate
    Slot6<- prep_plate_plot(label = "sample plate (96 NEST plate)",text_color = text_color,OT_slot = 6,number_of_samples = number_of_samples)
    #samples plot
    Slot4 <- sample_rack_plot(label = "samples_1 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 4,
                              slot_meta_table = "samples1",
                              meta_table = meta_table)
    Slot1 <- sample_rack_plot(label = "samples_2 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 1,
                              slot_meta_table = "samples2",
                              meta_table = meta_table)
    Slot5 <- sample_rack_plot(label = "samples_3 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 5,
                              slot_meta_table = "samples3",
                              meta_table = meta_table)
    Slot2 <- sample_rack_plot(label = "samples_4 (24x 1.5ml tube rack)",
                              text_color = text_color,
                              OT_slot = 2,
                              slot_meta_table = "samples4",
                              meta_table = meta_table)
    
    deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
      (Slot7+Slot8+Slot9)/
      (Slot4+Slot5+Slot6)/
      (Slot1+Slot2+Slot3)
  
    
  }
  deck_layout_plot_out
}





# decklayout step 2 -------------------------------------------------------

plot_deck_layout_step2_SP3 <- function(number_of_samples = 96,
                                       text_color = "black",
                                       trypsin_conc = 40,
                                       trypsin_ratio=50,
                                       LysC_conc = 20,
                                       LysC_ratio = 100,
                                       trypsin_LysC_mix_used = T,
                                       trypsin_LysC_mix_ratio = 50,
                                       trypsin_LysC_mix_conc = 40,
                                       sample_amount = 4){
  
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- pipette_plot(left = "300µl multi channel",right = "20µl multi channel",text_color = text_color)
  
  plate_columns_used*8*150
    #NEST 12ml reagent plate step1
    input_df_NEST_12ml <- tibble(postion = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A11","A12"),
                                  solution = c("95% ACN","95% ACN","95% ACN","95% ACN","80% EtOH","80% EtOH","80% EtOH","80% EtOH","digest buffer","leave empty: ACN WASTE","leave empty: ACN WASTE"),
                                  volume_ml= c(ifelse(plate_columns_used>=6,8,plate_columns_used*8*0.15+1),#A1 ACN
                                               ifelse((plate_columns_used-6)>=6,8,ifelse((plate_columns_used-6)<=0,0,(plate_columns_used-6)*8*0.15+1)),#A2 ACN
                                               ifelse(plate_columns_used>=6,8,plate_columns_used*8*0.15+1),#A3 ACN
                                               ifelse((plate_columns_used-6)>=6,8,ifelse((plate_columns_used-6)<=0,0,(plate_columns_used-6)*8*0.15+1)),#A4 ACN
                                               ifelse(plate_columns_used>=6,8,plate_columns_used*8*0.15+1),#A5 EtOH
                                               ifelse((plate_columns_used-6)>=6,8,ifelse((plate_columns_used-6)<=0,0,(plate_columns_used-6)*8*0.15+1)),#A6 EtOH
                                               ifelse(plate_columns_used>=6,8,plate_columns_used*8*0.15+1),#A7 EtOH
                                               ifelse((plate_columns_used-6)>=6,8,ifelse((plate_columns_used-6)<=0,0,(plate_columns_used-6)*8*0.15+1)),#A8 EtOH
                                               ifelse(plate_columns_used==12,4,plate_columns_used*8*0.02+2),#A9 digest buffer,
                                               NA,
                                               NA
                                                )
    )
    
    if(trypsin_LysC_mix_used == T){
      #NEST PCR reagent plate step1; LysC/Trypsin Mix
      input_df_NEST_100ul <- tibble(postion = c("A1"),
                                    solution = c(paste("Trypsin/LysC Mix",trypsin_LysC_mix_conc,"ng/µl\n")),
                                    volume_ul = c((sample_amount*1000/trypsin_LysC_mix_ratio)/trypsin_LysC_mix_conc*plate_columns_used+10
                                                  )
      )
      
    }else{
      #NEST PCR reagent plate step1; trypsin + LysC sequential
      input_df_NEST_100ul <- tibble(postion = c("A1","A2"),
                                    solution = c(paste("LysC",LysC_conc,"ng/µl\n"),paste("Trypsin",trypsin_conc,"ng/µl\n")),
                                    volume_ul = c((sample_amount*1000/LysC_ratio)/LysC_conc*plate_columns_used+10,
                                                  (sample_amount*1000/trypsin_ratio)/trypsin_conc*plate_columns_used+10)
      )
    }
 
    
    
    #regents
    Slot2 <- nest_12well_reagent_plot(input_df = input_df_NEST_12ml,label = "reagent plate (12well NEST plate)",OT_slot = 2,text_color = text_color)
    
    Slot3 <- nest_96well_reagent_plot(input_df = input_df_NEST_100ul,label = "reagent plate (96well NEST plate)",OT_slot = 3,text_color = text_color)
    
    #tips
    digest_tips <- ifelse(test = trypsin_LysC_mix_used == T,plate_columns_used*8*2,plate_columns_used*8*3)
    prep__tips <- 8*plate_columns_used*8
    
    
    Slot10 <- tip_rack_plot(label = "300ul_tips_1",text_color = text_color, OT_slot = 10,number_of_used_tips = ifelse(prep__tips>96,96,prep__tips))
    Slot7 <- tip_rack_plot(label = "300ul_tips_2",text_color = text_color,OT_slot = 7,number_of_used_tips = ifelse((prep__tips-96)>=96,96,ifelse((prep__tips-96)<0,0,(prep__tips-96))))
    Slot4 <- tip_rack_plot(label = "300ul_tips_3",text_color = text_color,OT_slot = 4,number_of_used_tips = ifelse((prep__tips-192)>=96,96,ifelse((prep__tips-192)<0,0,(prep__tips-192))))
    Slot11 <- tip_rack_plot(label = "300ul_tips_4",text_color = text_color,OT_slot = 11,number_of_used_tips = ifelse((prep__tips-288)>=96,96,ifelse((prep__tips-288)<0,0,(prep__tips-288))))
    Slot8 <- tip_rack_plot(label = "300ul_tips_5",text_color = text_color,OT_slot = 8,number_of_used_tips = ifelse((prep__tips-384)>=96,96,ifelse((prep__tips-384)<0,0,(prep__tips-384))))
    Slot5 <- tip_rack_plot(label = "300ul_tips_6",text_color = text_color,OT_slot = 5,number_of_used_tips = ifelse((prep__tips-480)>=96,96,ifelse((prep__tips-480)<0,0,(prep__tips-480))))
    Slot9 <- tip_rack_plot(label = "20ul_tips_1",text_color = text_color, OT_slot = 9,number_of_used_tips = ifelse(digest_tips>96,96,digest_tips))
    Slot6 <- tip_rack_plot(label = "20ul_tips_2",text_color = text_color, OT_slot = 6,number_of_used_tips = ifelse((digest_tips-96)>=96,96,ifelse((digest_tips-96)<0,0,(digest_tips-96))))
    
    #preparation plate
    Slot1<- prep_plate_plot(label = "MAGNET MODULE +\nsample plate (96well NEST plate)",
                            text_color = text_color,OT_slot = 1,
                            number_of_samples = number_of_samples,
                            outline = T,
                            outline_color = "dodgerblue")
      
    
    deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
      (Slot7+Slot8+Slot9)/
      (Slot4+Slot5+Slot6)/
      (Slot1+Slot2+Slot3)
    
  
  
  deck_layout_plot_out
}

# decklayout step 3 MS vials -------------------------------------------------------

plot_deck_layout_step3_MSvial <- function(number_of_samples = 96,
                                          text_color = "black"){
  
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- pipette_plot(left = "20µl single channel",right = "20µl multi channel", text_color = text_color)
  
  plate_columns_used*8*150
  #NEST 12ml reagent plate step1
  input_df_NEST_12ml <- tibble(postion = c("A1"),
                               solution = c("5% TFA"),
                               volume_ml= c(4)
  )
  

  #regents
  Slot2 <- nest_12well_reagent_plot(input_df = input_df_NEST_12ml,label = "reagent plate (12well NEST plate)",OT_slot = 2,text_color = text_color)
  
  Slot6 <- MSvial_plot(label = "MSvial_1",OT_slot = 6,number_of_samples = ifelse(number_of_samples>=48,48,number_of_samples),text_color = text_color)
  Slot3 <- MSvial_plot(label = "MSvial_2",OT_slot = 3,number_of_samples = ifelse((number_of_samples-48)>=48,48,ifelse((number_of_samples-48)<=0,0,number_of_samples-48)),text_color = text_color)
  
  
  Slot10 <-  plot_spacer()
  Slot7 <- plot_spacer()
  Slot4 <- tip_rack_plot(label = "20ul_tips_1",text_color = text_color, OT_slot = 9,number_of_used_tips = plate_columns_used*8)
  Slot11 <- plot_spacer()
  Slot8 <- plot_spacer()
  Slot5 <- tip_rack_plot(label = "20ul_tips_2",text_color = text_color, OT_slot = 9,number_of_used_tips = number_of_samples)
  Slot9 <- plot_spacer()

  #preparation plate
  Slot1<- prep_plate_plot(label = "MAGNET MODULE +\nsample plate (96well NEST plate)",
                          text_color = text_color,OT_slot = 1,number_of_samples = number_of_samples,
                          outline = T,
                          outline_color = "dodgerblue")
  
  
  deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
    (Slot7+Slot8+Slot9)/
    (Slot4+Slot5+Slot6)/
    (Slot1+Slot2+Slot3)
  
  
  
  deck_layout_plot_out
}

# decklayout step 3 EvoTips -------------------------------------------------------

plot_deck_layout_step3_EvoTips <- function(number_of_samples = 96,
                                          text_color = "black"){
  
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- pipette_plot(left = "300µl multi channel",right = "20µl multi channel", text_color = text_color)
  
  #NEST 12ml reagent plate step1
  input_df_NEST_12ml <- tibble(postion = c("A1","A2","A3","A4"),
                               solution = c("5% TFA","solvent B","solvent A","solvent A"),
                               volume_ml= c(plate_columns_used*8*0.00244+3,#A1 1% TFA
                                            plate_columns_used*8*0.02+2,#A2 solvent B
                                            (plate_columns_used*8*0.02*2)+(plate_columns_used*8*0.03)+2,#A3 solvent A
                                            plate_columns_used*8*0.1+2#A4 solvent A
                                            
                               )
  )
  
  
  #regents
  Slot2 <- nest_12well_reagent_plot(input_df = input_df_NEST_12ml,label = "reagent plate (12well NEST plate)",OT_slot = 2,text_color = text_color)
  
  Slot3 <- tip_rack_plot(label = "EvoTips",OT_slot = 3,number_of_used_tips = number_of_samples,text_color = text_color)
  
  #tips
  tips_20ul <- (plate_columns_used*8)*2
  tips_300ul <- 8*4
  
  
  Slot10 <- plot_spacer()
  Slot7 <- tip_rack_plot(label = "20ul_tips_1",text_color = text_color,OT_slot = 7,number_of_used_tips = ifelse((tips_20ul)>=96,96,ifelse((tips_20ul)<0,0,(tips_20ul))))
  Slot8 <- tip_rack_plot(label = "20ul_tips_2",text_color = text_color,OT_slot = 8,number_of_used_tips = ifelse((tips_20ul-96)>=96,96,ifelse((tips_20ul-96)<0,0,(tips_20ul-96))))
  Slot11 <- plot_spacer()
  
  Slot6 <- plot_spacer()
  Slot4 <- plot_spacer()
  Slot5 <- plot_spacer()
  Slot9 <- tip_rack_plot(label = "300ul_tips_1",text_color = text_color, OT_slot = 9,number_of_used_tips = ifelse(tips_300ul>96,96,tips_300ul))
  
  #preparation plate
  Slot1<- prep_plate_plot(label = "MAGNET MODULE +\nsample plate (96well NEST plate)",
                          text_color = text_color,OT_slot = 1,number_of_samples = number_of_samples,
                          outline = T,
                          outline_color = "dodgerblue")
  
  
  deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
    (Slot7+Slot8+Slot9)/
    (Slot4+Slot5+Slot6)/
    (Slot1+Slot2+Slot3)
  
  
  
  deck_layout_plot_out
}


# decklayout step 4 EvoTips -------------------------------------------------------

plot_deck_layout_step4_EvoTips <- function(number_of_samples = 96,
                                           text_color = "black"){
  
  
  plate_columns_used<- ceiling(number_of_samples/8)
  pipettes <- pipette_plot(left = "300µl multi channel",right = "20µl multi channel", text_color = text_color)
  
  
  
  
  #tips
  tips_20ul <- plate_columns_used*8

  
  Slot10 <- plot_spacer()
  Slot7 <- plot_spacer()
  Slot11 <- plot_spacer()
  Slot8 <- plot_spacer()
  
  
  Slot6 <- plot_spacer()
  Slot4 <- plot_spacer()
  Slot5 <- plot_spacer()
  Slot9 <- plot_spacer()
  
  #preparation plate
  Slot1<- prep_plate_plot(label = "MAGNET MODULE +\nsample plate (96well NEST plate)",
                          text_color = text_color,OT_slot = 1,number_of_samples = number_of_samples,
                          outline = T,
                          outline_color = "dodgerblue")
  #regents
  Slot2 <- prep_plate_plot(label = "elution plate (96well NEST plate)",text_color = text_color,OT_slot = 2,number_of_samples = number_of_samples)
  
  Slot3 <- tip_rack_plot(label = "20ul_tips",text_color = text_color, OT_slot = 3,number_of_used_tips = tips_20ul)
  
  
  deck_layout_plot_out<- (Slot10+Slot11+pipettes)/
    (Slot7+Slot8+Slot9)/
    (Slot4+Slot5+Slot6)/
    (Slot1+Slot2+Slot3)
  
  
  deck_layout_plot_out
}
