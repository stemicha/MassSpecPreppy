
# sample digest OT-2 template generation ----------------------------------


# template OT-2 download --------------------------------------------------
OT2_template_generation <- eventReactive(input$inputButton_generate_OT2_template, {
  # progressbar
  withProgress(message = "generating protocols for the OT-2", style = "notification", value = 0, {
    # Progress: loading file
    incProgress(0.2, detail = "loading file")


    # error vector generation
    error <- c()

    # error no input file selected
    if (is.null(input$input_sample_file)) {
      error <- c(error, no_input_sample_file = 1)
      shinyalert(
        title = "no input sample file selcted",
        text = paste("please select a sample file"),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE
      )
    }

    # read input sample file -------------------------------------------------------
    OT2_template <- read_excel(
      input$input_sample_file$datapath,
      skip = 0, sheet = 1
    )
    # OT2_template <- read_excel("www/DEMO_MSpreppy.xlsx")


    # remove rows without sample name -----------------------
    OT2_template <- OT2_template %>% filter(!is.na(sample))


    # check for input sample errors -------------------------------------------------
    # Progress: check for errors in file
    incProgress(0.4, detail = "check for errors in file")

    input_amount <- input$input_sample_amount


    # sample volume needed calculation for digest -----------------------------

    OT2_template_tmp <- OT2_template %>%
      rowwise() %>%
      mutate(volume_needed = input_amount / `protein concentration (µg/µl)`) 
    
    
    # add position data ------------------------------------------
    
    OT2_template_tmp <- OT2_template_tmp %>% 
      mutate(row_pos = factor(substr(`preparation plate position`,
                                     start = 1,
                                     stop = 1),
                              levels = rev(LETTERS[1:8])),
             col_pos = as.integer(substr(`preparation plate position`,
                                         start = 2,
                                         stop = nchar(`preparation plate position`))))
    
    
    # add coloring legend -----------------------------------------------------
    
    max_volume <-  10
    max_volume_EVOTIP <- 20
    min_volume <-  2
    
    OT2_template_tmp <- OT2_template_tmp %>% 
      rowwise() %>% 
      mutate(pipetting_quality = ifelse(volume_needed>max_volume,
                                        yes = paste0("above max. volume"),
                                        no = ifelse(volume_needed<min_volume,
                                                    yes = ifelse(volume_needed>=1,
                                                                 yes = "≥ 1µl & < 2µl",
                                                                 no = "<1µl"),
                                                    no = paste0("≥ 2µl & ≤ max vol."))
      )
      )
    #EVOTIPs table calculations
    if(input$EvoTips_vials=="EvoTips"){
      OT2_template_tmp <- OT2_template_tmp %>%
          rowwise() %>% 
          mutate(EVOTIP_volume_needed = input$EvoTips_amount/(input_amount*1000/24.44))
      
      OT2_template_tmp <- OT2_template_tmp %>% 
        rowwise() %>% 
        mutate(EVOTIP_pipetting_quality = ifelse(EVOTIP_volume_needed>max_volume_EVOTIP,
                                                 yes = paste0("above max. volume"),
                                                 no = ifelse(EVOTIP_volume_needed<min_volume,
                                                             yes = ifelse(EVOTIP_volume_needed>=1,
                                                                          yes = "≥ 1µl & < 2µl",
                                                                          no = "<1µl"),
                                                             no = paste0("≥ 2µl & ≤ max vol."))
        )
        )
    }   
    
    
   

    # error: above 10µl for amount of red/alk or 15.625µl nor NON red/alk! --------------------------
    
    if (input$logical_red_alk == TRUE) {
      OT2_template_tmp_low_conc <- OT2_template_tmp %>%
        filter(volume_needed > 10)
    }
    if (input$logical_red_alk == FALSE) {
      OT2_template_tmp_low_conc <- OT2_template_tmp %>%
        filter(volume_needed > 15.625)
    }
 


    if (dim(OT2_template_tmp_low_conc)[1] != 0) {
      error <- c(error, low_conc = 1)
      shinyalert(
        title = "concentration of samples is to low (prep with higher conc. or lower protein amount for digest!!!)",
        text = paste("sample with too low concentration are:\n", paste(OT2_template_tmp_low_conc$sample, collapse = "\n")),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE
      )
    }

    # error: volume left is below 10µl > air can be aspirated ------------------------------------
    OT2_template_tmp_low_volume <- OT2_template_tmp %>%
      rowwise() %>%
      mutate(volume_left = volume - volume_needed) %>%
      filter(volume_left < 10)

    if (dim(OT2_template_tmp_low_volume)[1] != 0) {
      error <- c(error, low_volume = 1)
      shinyalert(
        title = "volume of sample is to low (volume left is below 10µl); please use a higher volume of sample",
        text = paste("sample with too low concentration are:\n", paste(OT2_template_tmp_low_volume$sample, collapse = "\n")),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE
      )
    }

    # error: below 2µl for amount! --------------------------------------------------------------
    OT2_template_tmp_to_high_conc <- OT2_template_tmp %>%
      filter(volume_needed < 2)

    if (dim(OT2_template_tmp_to_high_conc)[1] != 0) {
      error <- c(error, too_high_conc = 1)
      shinyalert(
        title = "concentration of samples is to high; pipetting less than 2µl of sample (dilute samples or increase protein amount for digest!!!)",
        text = paste("sample with too high concentration are:\n", paste(OT2_template_tmp_to_high_conc$sample, collapse = "\n")),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE
      )
    }

    # error: only 96 samples allowed ---------------------------------------------------------
    if (dim(OT2_template)[1] > 96) {
      error <- c(error, N_above_96_samples = 1)
      shinyalert(
        title = "N of samples > 96",
        text = "this protocol only allows the simultaneous preparation of 96 samples!!!!",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE
      )
    }


    # load OT-2 python protocol max. 96 samples -------------------------------
    # generate file name
    inFile <- input$input_sample_file
    file_out_tmp <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")

    # Progress: loading protocol templates
    incProgress(0.6, detail = "loading protocol templates")

    OT2_protocol_part1 <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part1_sample_preparation_in_96well.py")
    OT2_protocol_part1_wo_alk_red <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part1_sample_preparation_in_96well_without_red_alk.py")
    OT2_protocol_part2_sequential <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part2_SP3_in_96well_enzyme_sequential.py")
    OT2_protocol_part2_mix <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part2_SP3_in_96well_enzyme_mix.py")
    OT2_protocol_part3_vial <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part3_vial_elution.py")
    OT2_protocol_part3_EvoTip <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part3_EvoTip_loading.py")
    OT2_protocol_part4_post_EvoTip <- read_lines(file = "OT2_protocols_template/Mass_Spec_Preppy__OT2_part4_post_EvoTip_loading.py")

    # JSON implementation of files
    # genrate python JSON input edited file -----------------------------------

    input_JSON <- "    _all_values = json.loads(\"\"\"{\"csv_sample\":\"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position"


    for (i in 1:nrow(OT2_template)) {
      input_JSON <- paste(input_JSON, paste(OT2_template[i, ], collapse = ","), sep = "\\\\n")
    }

    # add sample amount / Enzymes ... -----------------------------------------
    input_JSON <- paste(input_JSON, "\"", ",", "\"sample_amount\"", ":", "\"", input$input_sample_amount, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"trypsin_ratio\"", ":", "\"", input$trypsin_ratio, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"trypsin_stock_concentration\"", ":", "\"", input$trypsin_stock_conc, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"LysC_ratio\"", ":", "\"", input$LysC_ratio, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"LysC_stock_concentration\"", ":", "\"", input$LysC_stock_conc, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"EvoTips_amount_ng\"", ":", "\"", input$EvoTips_amount, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"LysC_Trypsin_Mix_stock_concentration\"", ":", "\"", input$Trypsin_LysC_Mix_stock_conc, sep = "")
    input_JSON <- paste(input_JSON, "\"", ",", "\"LysC_Trypsin_Mix_ratio\"", ":", "\"", input$Trypsin_LysC_Mix_ratio, "\"", sep = "")
    input_JSON <- paste(input_JSON, "}\"\"\")", sep = "")

    # Progress: adding sample information
    incProgress(0.8, detail = "adding sample information")

    # generate OT-2 protocol output  -----------------------------------------
    # part 1
    OT2_protocol_part1_out <- OT2_protocol_part1
    OT2_protocol_part1_out[16] <- input_JSON
    OT2_protocol_part1_out[23] <- paste(
      substr(
        x = OT2_protocol_part1_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part1_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    # part 1 w/o alk red
    OT2_protocol_part1_wo_alk_red_out <- OT2_protocol_part1_wo_alk_red
    OT2_protocol_part1_wo_alk_red_out[16] <- input_JSON
    OT2_protocol_part1_wo_alk_red_out[23] <- paste(
      substr(
        x = OT2_protocol_part1_wo_alk_red_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part1_wo_alk_red_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )

    # part 2 (enzyme sequential)
    OT2_protocol_part2_sequential_out <- OT2_protocol_part2_sequential
    OT2_protocol_part2_sequential_out[16] <- input_JSON
    OT2_protocol_part2_sequential_out[23] <- paste(
      substr(
        x = OT2_protocol_part2_sequential_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part2_sequential_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    # part 2 (enzyme Mix)
    OT2_protocol_part2_mix_out <- OT2_protocol_part2_mix
    OT2_protocol_part2_mix_out[16] <- input_JSON
    OT2_protocol_part2_mix_out[23] <- paste(
      substr(
        x = OT2_protocol_part2_mix_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part2_mix_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    # part 3 MSvial
    OT2_protocol_part3_vial_out <- OT2_protocol_part3_vial
    OT2_protocol_part3_vial_out[16] <- input_JSON
    OT2_protocol_part3_vial_out[23] <- paste(
      substr(
        x = OT2_protocol_part3_vial_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part3_vial_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    # part 3 EvoTip
    OT2_protocol_part3_EvoTip_out <- OT2_protocol_part3_EvoTip
    OT2_protocol_part3_EvoTip_out[16] <- input_JSON
    OT2_protocol_part3_EvoTip_out[23] <- paste(
      substr(
        x = OT2_protocol_part3_EvoTip_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part3_EvoTip_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    # part 4 EvoTip
    OT2_protocol_part4_post_EvoTip_out <- OT2_protocol_part4_post_EvoTip
    OT2_protocol_part4_post_EvoTip_out[16] <- input_JSON
    OT2_protocol_part4_post_EvoTip_out[23] <- paste(
      substr(
        x = OT2_protocol_part4_post_EvoTip_out[23],
        start = 1,
        stop = nchar(OT2_protocol_part4_post_EvoTip_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )

    # Progress: generate output
    incProgress(1, detail = "generate output")

    
    
    # plot volume needed plot -------------------------------------------------
    
    volume_needed_plot<- ggplot(OT2_template_tmp,aes(col_pos,row_pos,fill=pipetting_quality, color = (volume-volume_needed)<10))+
      geom_tile(size = 1)+
      scale_fill_manual(values = c("above max. volume" = "#D84738",
                                   "≥ 1µl & < 2µl" = "#EB9711",
                                   "<1µl" = "#D84738",
                                   "≥ 2µl & ≤ max vol." = "seagreen"))+
      scale_color_manual(values = c("TRUE" = "orangered2","FALSE"="white"))+
      geom_text(mapping = aes(label = round(volume_needed,digits = 2)),color= "white",show.legend = F, fontface = "bold")+
      #geom_hline(yintercept = seq(1.5,7.5,by = 1), color = "white")+
      #geom_vline(xintercept = seq(1.5,11.5,by = 1), color = "white")+
      theme_void(base_size = 18)+
      scale_x_continuous(breaks = c(1:12),position = "top")+
      theme(axis.text = element_text(),legend.position = "bottom",legend.key.width = unit(1.5,"cm"), title = element_text(face = "bold"))+
      geom_rect(mapping = aes(xmin=0.5,xmax=12.5,ymin=0.5,ymax = 8.5),fill=NA,color="grey")+
      labs(title = "Pipetted sample volume with settings", subtitle = "preparation plate",fill = "",caption = "red line around well means volume left is below 10µl = critical" )+
      theme(text = element_text(color = "white"),
            plot.title = element_text(face = "bold"))+
      coord_cartesian(xlim = c(1,12),ylim = c(1,8))+
      guides(color = "none")
    
    if(input$EvoTips_vials=="EvoTips"){
    EVOTIP_volume_needed_plot<- ggplot(OT2_template_tmp,aes(col_pos,row_pos,color=EVOTIP_pipetting_quality))+
      geom_point(size = 16)+
      scale_color_manual(values = c("above max. volume" = "#D84738",
                                   "≥ 1µl & < 2µl" = "#EB9711",
                                   "<1µl" = "#D84738",
                                   "≥ 2µl & ≤ max vol." = "seagreen"))+
      geom_text(mapping = aes(label = round(EVOTIP_volume_needed,digits = 2)),color= "white",show.legend = F, fontface = "bold")+
      theme_void(base_size = 18)+
      scale_x_continuous(breaks = c(1:12),position = "top")+
      theme(axis.text = element_text(),legend.position = "bottom",legend.key.width = unit(1.5,"cm"), title = element_text(face = "bold"))+
      geom_rect(mapping = aes(xmin=0.5,xmax=12.5,ymin=0.5,ymax = 8.5),fill=NA,color="grey")+
      labs(title = "Pipetted digest volume for EVOTIP loading with settings",color = "", subtitle = "EVOTIP dilution plate",fill = "" )+
      theme(text = element_text(color = "white"),
            plot.title = element_text(face = "bold"))+
      coord_cartesian(xlim = c(1,12),ylim = c(1,8))
    
    #merge plot with patchwork
    volume_needed_plot <- volume_needed_plot/EVOTIP_volume_needed_plot

    }
    
    
    
      # list output
    list(
      number_of_samples = dim(OT2_template)[1],
      input_sample_list = OT2_template,
      OT2_template_tmp = OT2_template_tmp,
      error = error,
      volume_needed_plot = volume_needed_plot,
      volume_needed_plot_N = ifelse(test = input$EvoTips_vials=="EvoTips", yes = 2, no = 1),
      OT2_protocol_part1_out = OT2_protocol_part1_out,
      OT2_protocol_part2_sequential_out = OT2_protocol_part2_sequential_out,
      OT2_protocol_part2_mix_out = OT2_protocol_part2_mix_out,
      OT2_protocol_part3_vial_out = OT2_protocol_part3_vial_out,
      OT2_protocol_part3_EvoTip_out = OT2_protocol_part3_EvoTip_out,
      OT2_protocol_part1_wo_alk_red_out = OT2_protocol_part1_wo_alk_red_out,
      OT2_protocol_part4_post_EvoTip_out = OT2_protocol_part4_post_EvoTip_out,
      file_out_short = file_out_tmp,
      file_name = paste(str_replace_all(file_out_tmp, ".xlsx", ""), sep = ""),
      file_output_part1 = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep.py", sep = ""),
      file_output_part1_wo_alk_red = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep__without_red_alk.py", sep = ""),
      file_output_part2_sequential = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_sequential.py", sep = ""),
      file_output_part2_mix = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_mix.py", sep = ""),
      file_output_part3_vial_out = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_3_MSvial_elution.py", sep = ""),
      file_output_part3_EvoTip_out = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_3_EvoTip_loading.py", sep = ""),
      file_output_part4_post_EvoTip_out = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_Mass_Spec_Preppy_protocol_part_4_post_EvoTip_loading.py", sep = "")
    )
  }) # end progressbar
}) # end OT2_template_generation



# render volume needed plot -----------------------------------------------

output$volume_needed_plot <- renderUI(
  renderPlot(OT2_template_generation()$volume_needed_plot,
             width = 950,
             height = 500*OT2_template_generation()$volume_needed_plot_N)
)

# render volume needed table -----------------------------------------------

output$volume_needed_table <- DT::renderDataTable({
  rendered_table <- DT::datatable(
    OT2_template_generation()$OT2_template_tmp %>% 
      select(-row_pos,-col_pos),
    filter = "top",
    options = list(
      pageLength = 100,
      autoWidth = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE
    )
  ) %>%
    DT::formatStyle("pipetting_quality",
                    backgroundColor = styleEqual(
                      c("above max. volume",
                        "≥ 1µl & < 2µl" ,
                        "<1µl" ,
                        "≥ 2µl & ≤ max vol." ),
                      c("above max. volume" = "#D84738",
                        "≥ 1µl & < 2µl" = "#EB9711",
                        "<1µl" = "#D84738",
                        "≥ 2µl & ≤ max vol." = "seagreen")
                    )
    )
  if(input$EvoTips_vials=="EvoTips"){
  rendered_table <- rendered_table %>%
    DT::formatStyle("EVOTIP_pipetting_quality",
                    backgroundColor = styleEqual(
                      c("above max. volume",
                        "≥ 1µl & < 2µl" ,
                        "<1µl" ,
                        "≥ 2µl & ≤ max vol." ),
                      c("above max. volume" = "#D84738",
                        "≥ 1µl & < 2µl" = "#EB9711",
                        "<1µl" = "#D84738",
                        "≥ 2µl & ≤ max vol." = "seagreen")
                    )
    )
  }
  # output render table volume pipetting quality
  rendered_table
})

