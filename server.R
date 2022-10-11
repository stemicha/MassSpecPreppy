#____________________________________________________________________#
## Script: shiny app: Mass Spec Preppy
## Author: Stephan Michalik
## part: server.R
## Notes:
#____________________________________________________________________#

# define some credentials
credentials <- data.frame(
  user = c("admin"), # mandatory
  password = c("Protein!112"), # mandatory
  expire = c(NA),
  admin = c(TRUE),
  comment = "please login ...",
  stringsAsFactors = FALSE
)

# server function of Mass Spec Preppy
shinyServer(function(input, output) {
  #check credentials
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # EvoTip UI reactive amount ---------------------------------------------
  output$evosep_amount_ui<-renderUI({
    if(input$EvoTips_vials=="EvoTips"){
      sliderInput(inputId = "EvoTips_amount",label = "EvoTip amount (ng)",min = 50,max = 1000,step = 50,value = 400)
    }
  })
  
  # stock solution suggestion -----------------------------------------------
  #trypsin
  output$trypsin_stock_suggestion_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    trypsin_cross_table_filtered <- trypsin_cross_table %>%  filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$trypsin_ratio )
    tags$p(paste("optimal trypsin stock is (2µl added to digest):",trypsin_cross_table_filtered$trypsin_stock,"ng/µl"), style = "color:#84B135; align:center; margin-left: 5px")
  })
  #LysC
  output$LysC_stock_suggestion_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    LysC_cross_table_filtered <- trypsin_cross_table  %>%  filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$LysC_ratio )
    tags$p(paste("optimal LysC stock is (2µl added to digest):",LysC_cross_table_filtered$trypsin_stock,"ng/µl"), style = "color:#FF9300; align:center; margin-left: 5px")
  })
  #Trypsin_LysC_mix
  output$Trypsin_LysC_Mix_stock_suggestion_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    Trypsin_LysC_cross_table_filtered <- trypsin_cross_table  %>%  filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$Trypsin_LysC_Mix_ratio )
    tags$p(paste("optimal Trypsin/LysC Mix stock is (2µl added to digest):",Trypsin_LysC_cross_table_filtered$trypsin_stock,"ng/µl"), style = "color:#299FD6; align:center; margin-left: 5px")
  })

  # enzyme stock slider -----------------------------------------------------

  #Trypsin slider
  output$trypsin_stock_slider_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    trypsin_cross_table_filtered <- trypsin_cross_table %>% filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$trypsin_ratio )
    sliderInput(inputId = "trypsin_stock_conc",label = "trypsin stock concentration (ng/µl) in 50mM acetic acid",
                min = 5,
                max = 200,
                value = trypsin_cross_table_filtered$trypsin_stock,
                step = 5)
  })
  #LysC slider
  output$LysC_stock_slider_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    LysC_cross_table_filtered <- trypsin_cross_table %>% filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$LysC_ratio )
    sliderInput(inputId = "LysC_stock_conc",label = "LysC stock concentration (ng/µl) in water",
                min = 5,
                max = 200,
                value = LysC_cross_table_filtered$trypsin_stock,
                step = 5)
  })
  #Trypsin/LysC mix slider
  output$Trypsin_LysC_Mix_stock_slider_ui<-renderUI({
    trypsin_cross_table <- read_excel("www/trypsin_stock_cross_table.xlsx")
    Trypsin_LysC_cross_table_filtered <- trypsin_cross_table %>% filter(input_sample_amount ==input$input_sample_amount & trypsin_ratio==input$Trypsin_LysC_Mix_ratio )
    sliderInput(inputId = "Trypsin_LysC_Mix_stock_conc",label = "Trypsin/LysC Mix stock concentration (ng/µl) in water",
                min = 5,
                max = 200,
                value = Trypsin_LysC_cross_table_filtered$trypsin_stock,
                step = 5)
  })
  
  
  
  # template OT-2 download --------------------------------------------------
  OT2_template_generation<- eventReactive(input$inputButton_generate_OT2_template,{
    
    #progressbar
    withProgress(message = "generating protocols for the OT-2", style = "notification", value = 0, { 
      
      # Progress: loading file
      incProgress(0.2, detail = "loading file")
      
      
      # error vector generation
      error <- c()
      
      # error no input file selected
      if(is.null(input$input_sample_file)){
        error <- c(error,no_input_sample_file = 1)
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
        skip = 0,sheet = 1
      ) 
      #OT2_template <- read_excel("www/DEMO_MSpreppy.xlsx")
      
      
      # remove rows without sample name -----------------------
      OT2_template <- OT2_template %>% filter(!is.na(sample))
      
      
      # check for input sample errors -------------------------------------------------
      # Progress: check for errors in file
      incProgress(0.4, detail = "check for errors in file")
      
      input_amount = input$input_sample_amount
      
      OT2_template_tmp <- OT2_template %>% 
        rowwise() %>%  
        mutate(volume_needed = input_amount/`protein concentration (µg/µl)`)
      
      # errror: above 10µl for amount! --------------------------------------------------
      OT2_template_tmp_low_conc <- OT2_template_tmp %>% 
        filter(volume_needed>10)
      
      
      if(dim(OT2_template_tmp_low_conc)[1]!=0){
        error <- c(error,low_conc = 1)
        shinyalert(
          title = "concentration of samples is to low (prep with higher conc. or lower protein amount for digest!!!)",
          text = paste("sample with too low concentration are:\n",paste(OT2_template_tmp_low_conc$sample,collapse = "\n")),
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
        mutate(volume_left = volume-volume_needed) %>% 
        filter(volume_left<10)
      
      if(dim(OT2_template_tmp_low_volume)[1]!=0){
        error <- c(error,low_volume = 1)
        shinyalert(
          title = "volume of sample is to low (volume left is below 10µl); please use a higher volume of sample",
          text = paste("sample with too low concentration are:\n",paste(OT2_template_tmp_low_volume$sample,collapse = "\n")),
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
      
      #error: below 2µl for amount! --------------------------------------------------------------
      OT2_template_tmp_to_high_conc <- OT2_template_tmp %>% 
        filter(volume_needed<2)
      
      if(dim(OT2_template_tmp_to_high_conc)[1]!=0){
        error <- c(error,too_high_conc = 1)
        shinyalert(
          title = "concentration of samples is to high; pipetting less than 2µl of sample (dilute samples or increase protein amount for digest!!!)",
          text = paste("sample with too high concentration are:\n",paste(OT2_template_tmp_to_high_conc$sample,collapse = "\n")),
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
      if(dim(OT2_template)[1]>96){
        error <- c(error,N_above_96_samples = 1)
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
      #generate file name
      inFile <- input$input_sample_file
      file_out_tmp <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")
      
      # Progress: loading protocol templates
      incProgress(0.6, detail = "loading protocol templates")
      
      OT2_protocol_part1 <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part1_sample_preparation_in_96well.py")
      OT2_protocol_part1_wo_alk_red <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part1_sample_preparation_in_96well_without_red_alk.py")
      OT2_protocol_part2_sequential <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part2_SP3_in_96well_enzyme_sequential.py")
      OT2_protocol_part2_mix <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part2_SP3_in_96well_enzyme_mix.py")
      OT2_protocol_part3_vial <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part3_vial_elution.py")
      OT2_protocol_part3_EvoTip <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part3_EvoTip_loading.py")
      OT2_protocol_part4_post_EvoTip <- read_lines(file = "www/Mass_Spec_Preppy__OT2_part4_post_EvoTip_loading.py")
      
      #JSON implementation of files
      # genrate python JSON input edited file -----------------------------------
      
      input_JSON <- "    _all_values = json.loads(\"\"\"{\"csv_sample\":\"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position"
      
      
      for(i in 1:nrow(OT2_template)){
        input_JSON <- paste(input_JSON,paste(OT2_template[i,],collapse = ","),sep = "\\\\n")
      }
      
      # add sample amount / Enzymes ... -----------------------------------------
      input_JSON <- paste(input_JSON,"\"",",","\"sample_amount\"",":","\"",input$input_sample_amount,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"trypsin_ratio\"",":","\"",input$trypsin_ratio,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"trypsin_stock_concentration\"",":","\"",input$trypsin_stock_conc,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"LysC_ratio\"",":","\"",input$LysC_ratio,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"LysC_stock_concentration\"",":","\"",input$LysC_stock_conc,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"EvoTips_amount_ng\"",":","\"",input$EvoTips_amount,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"LysC_Trypsin_Mix_stock_concentration\"",":","\"",input$Trypsin_LysC_Mix_stock_conc,sep="")
      input_JSON <- paste(input_JSON,"\"",",","\"LysC_Trypsin_Mix_ratio\"",":","\"",input$Trypsin_LysC_Mix_ratio,"\"",sep="")
      input_JSON <- paste(input_JSON,"}\"\"\")",sep="")
      
      # Progress: adding sample information
      incProgress(0.8, detail = "adding sample information")
      
      # generate OT-2 protocol output  -----------------------------------------
      # part 1
      OT2_protocol_part1_out <- OT2_protocol_part1
      OT2_protocol_part1_out[16] <- input_JSON
      OT2_protocol_part1_out[23] <- paste(substr(x = OT2_protocol_part1_out[23],
                                                 start = 1,
                                                 stop = nchar(OT2_protocol_part1_out[23])-2),
                                          " | ",file_out_tmp,
                                          "\",",
                                          sep="",
                                          collapse = "")
      # part 1 w/o alk red
      OT2_protocol_part1_wo_alk_red_out <- OT2_protocol_part1_wo_alk_red
      OT2_protocol_part1_wo_alk_red_out[16] <- input_JSON
      OT2_protocol_part1_wo_alk_red_out[23] <- paste(substr(x = OT2_protocol_part1_wo_alk_red_out[23],
                                                 start = 1,
                                                 stop = nchar(OT2_protocol_part1_wo_alk_red_out[23])-2),
                                          " | ",file_out_tmp,
                                          "\",",
                                          sep="",
                                          collapse = "")
      
      # part 2 (enzyme sequential)
      OT2_protocol_part2_sequential_out <- OT2_protocol_part2_sequential
      OT2_protocol_part2_sequential_out[16] <- input_JSON
      OT2_protocol_part2_sequential_out[23] <- paste(substr(x = OT2_protocol_part2_sequential_out[23],
                                                            start = 1,
                                                            stop = nchar(OT2_protocol_part2_sequential_out[23])-2),
                                                     " | ",file_out_tmp,
                                                     "\",",
                                                     sep="",
                                                     collapse = "")
      # part 2 (enzyme Mix)
      OT2_protocol_part2_mix_out <- OT2_protocol_part2_mix
      OT2_protocol_part2_mix_out[16] <- input_JSON
      OT2_protocol_part2_mix_out[23] <- paste(substr(x = OT2_protocol_part2_mix_out[23],
                                                            start = 1,
                                                            stop = nchar(OT2_protocol_part2_mix_out[23])-2),
                                                     " | ",file_out_tmp,
                                                     "\",",
                                                     sep="",
                                                     collapse = "")
      # part 3 MSvial
      OT2_protocol_part3_vial_out <- OT2_protocol_part3_vial
      OT2_protocol_part3_vial_out[16] <- input_JSON
      OT2_protocol_part3_vial_out[23] <- paste(substr(x = OT2_protocol_part3_vial_out[23],
                                                     start = 1,
                                                     stop = nchar(OT2_protocol_part3_vial_out[23])-2),
                                              " | ",file_out_tmp,
                                              "\",",
                                              sep="",
                                              collapse = "")
      # part 3 EvoTip
      OT2_protocol_part3_EvoTip_out <- OT2_protocol_part3_EvoTip
      OT2_protocol_part3_EvoTip_out[16] <- input_JSON
      OT2_protocol_part3_EvoTip_out[23] <- paste(substr(x = OT2_protocol_part3_EvoTip_out[23],
                                                      start = 1,
                                                      stop = nchar(OT2_protocol_part3_EvoTip_out[23])-2),
                                               " | ",file_out_tmp,
                                               "\",",
                                               sep="",
                                               collapse = "")
      # part 4 EvoTip
      OT2_protocol_part4_post_EvoTip_out <- OT2_protocol_part4_post_EvoTip
      OT2_protocol_part4_post_EvoTip_out[16] <- input_JSON
      OT2_protocol_part4_post_EvoTip_out[23] <- paste(substr(x = OT2_protocol_part4_post_EvoTip_out[23],
                                                        start = 1,
                                                        stop = nchar(OT2_protocol_part4_post_EvoTip_out[23])-2),
                                                 " | ",file_out_tmp,
                                                 "\",",
                                                 sep="",
                                                 collapse = "")
     
      # Progress: generate output
      incProgress(1, detail = "generate output")
      
      # list output
      list(number_of_samples = dim(OT2_template)[1],
           input_sample_list = OT2_template,
           error = error,
           OT2_protocol_part1_out = OT2_protocol_part1_out,
           OT2_protocol_part2_sequential_out = OT2_protocol_part2_sequential_out,
           OT2_protocol_part2_mix_out = OT2_protocol_part2_mix_out,
           OT2_protocol_part3_vial_out = OT2_protocol_part3_vial_out,
           OT2_protocol_part3_EvoTip_out = OT2_protocol_part3_EvoTip_out,
           OT2_protocol_part1_wo_alk_red_out = OT2_protocol_part1_wo_alk_red_out,
           OT2_protocol_part4_post_EvoTip_out = OT2_protocol_part4_post_EvoTip_out,
           file_out_short = file_out_tmp,
           file_name = paste(str_replace_all(file_out_tmp,".xlsx",""),sep=""),
           file_output_part1 = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep.py",sep=""),
           file_output_part1_wo_alk_red = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep__without_red_alk.py",sep=""),
           file_output_part2_sequential = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_sequential.py",sep=""),
           file_output_part2_mix = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_mix.py",sep=""),
           file_output_part3_vial_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_3_MSvial_elution.py",sep=""),
           file_output_part3_EvoTip_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_3_EvoTip_loading.py",sep=""),
           file_output_part4_post_EvoTip_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_4_post_EvoTip_loading.py",sep="")
      )
    })#end progressbar
    
  })#end OT2_template_generation
  
  
  # generate decklayout plots ------------------------------------------------
  
  decklayout_plots <- eventReactive(input$inputButton_generate_OT2_template,{
    #progress decklayouts
    withProgress(message = "generate deck layout plots for the OT-2", style = "notification", value = 0, { 
      
      incProgress(0.3, detail = "load sample list")
      
      #load sample file
      OT2_template <- read_excel(
        input$input_sample_file$datapath,
        skip = 0,sheet = 1
      ) 
      #OT2_template <- read_excel("www/DEMO_MSpreppy.xlsx")
      #remove rows without sample name
      OT2_template <- OT2_template %>% 
        filter(!is.na(sample))
      
      incProgress(0.6, detail = "generate plots")
      
      # step 1 red & alk == TRUE ------------------------------------------------
      if(input$logical_red_alk==TRUE){
        deck_plot1<- plot_deck_layout_step1(red_alk = T,number_of_samples = dim(OT2_template %>% distinct(`OT-2 slot`,`OT-2 position`))[1],prep_plate_number_of_samples = dim(OT2_template)[1],text_color = "white")
      }
      if(input$logical_red_alk==FALSE){
        deck_plot1<- plot_deck_layout_step1(red_alk = F,number_of_samples = dim(OT2_template %>% distinct(`OT-2 slot`,`OT-2 position`))[1],prep_plate_number_of_samples = dim(OT2_template)[1],text_color = "white")
      }
      
      deck_plot2<-plot_deck_layout_step2_SP3(number_of_samples = dim(OT2_template)[1],
                                             text_color = "white",
                                             trypsin_LysC_mix_used = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = T,no = F),
                                             trypsin_LysC_mix_ratio = as.numeric(input$Trypsin_LysC_Mix_ratio),
                                             trypsin_LysC_mix_conc = as.numeric(input$Trypsin_LysC_Mix_stock_conc),
                                             trypsin_conc = as.numeric(input$trypsin_stock_conc),
                                             LysC_conc = as.numeric(input$LysC_stock_conc),
                                             sample_amount = as.numeric(input$input_sample_amount),
                                             trypsin_ratio = as.numeric(input$trypsin_ratio),
                                             LysC_ratio = as.numeric(input$LysC_ratio))
      
      # EvoTip / Vial
      if(input$EvoTips_vials=="Vial"){
        deck_plot3<-plot_deck_layout_step3_MSvial(number_of_samples = dim(OT2_template)[1],text_color = "white")
        deck_plot4 <- plot_spacer()
      }
      if(input$EvoTips_vials=="EvoTips"){
        deck_plot3<-plot_deck_layout_step3_EvoTips(number_of_samples = dim(OT2_template)[1],text_color = "white")
        deck_plot4<-plot_deck_layout_step4_EvoTips(number_of_samples = dim(OT2_template)[1],text_color = "white")
      }
      
      incProgress(1, detail = "return output")
      
      #return plots
      list(deck_plot1 = deck_plot1,
           deck_plot2 = deck_plot2,
           deck_plot3 = deck_plot3,
           deck_plot4 = deck_plot4)
    })#end progressbar
  })#end decklayout_plots  
  
  
  # render deck plot output ------------------------------------------------------
  
  # progressbar plots
  withProgress(message = "render deck layout plots for the OT-2", style = "notification", value = 0, { 
    incProgress(0.3, detail = "deck layout step 1")
    output$step1_plot <- renderPlot(decklayout_plots()$deck_plot1)
    incProgress(0.6, detail = "deck layout step 2")
    output$step2_plot <- renderPlot(decklayout_plots()$deck_plot2)
    incProgress(0.9, detail = "deck layout step 3")
    output$step3_plot <- renderPlot(decklayout_plots()$deck_plot3)
    output$step4_plot <- renderPlot(decklayout_plots()$deck_plot4)
    
  })#end progressbar
  
  # generate OT2_template download -------------------------------------------
  
  # download OT-2 template
  output$dlOT2 <- downloadHandler(
    filename = function() {
      paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.zip",sep="")
    },
    content = function(fname) {
      #tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
      #tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
      #setwd(tmpdir)
      #print(tmpdir)
      
      #progressbar
      withProgress(message = "generate download", style = "notification", value = 0, { 
        incProgress(0.2, detail = "saving deck layout plots and protocols")
        # write protocol / decklayout files ----------------------------------------------
        if(input$logical_red_alk==F & input$EvoTips_vials=="Vial"){
          writeLines(text = OT2_template_generation()$OT2_protocol_part1_wo_alk_red_out,
                     con = OT2_template_generation()$file_output_part1_wo_alk_red)
          # SP3 part2 mix or sequential
          if(input$enzyme_or_mix=="Trypsin/LysC Mix"){
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out,con = OT2_template_generation()$file_output_part2_mix)
          }else{
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out,con = OT2_template_generation()$file_output_part2_sequential)
          }
          writeLines(text = OT2_template_generation()$OT2_protocol_part3_vial_out,
                     con = OT2_template_generation()$file_output_part3_vial_out)
          
          # save decklayoutplots for output
          ggsave(plot = decklayout_plots()$deck_plot1,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot2,device = "png",
                 filename = paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                         yes = OT2_template_generation()$file_output_part2_mix,
                                         no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot3,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          # merge files for zipping
          fs <- c(OT2_template_generation()$file_output_part1_wo_alk_red,
                  ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                         yes = OT2_template_generation()$file_output_part2_mix,
                         no = OT2_template_generation()$file_output_part2_sequential),
                  OT2_template_generation()$file_output_part3_vial_out,
                  paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep=""),
                  paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                               yes = OT2_template_generation()$file_output_part2_mix,
                               no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep="")
          )
        }
        
        if(input$logical_red_alk==T & input$EvoTips_vials=="Vial"){
          writeLines(text = OT2_template_generation()$OT2_protocol_part1_out,con = OT2_template_generation()$file_output_part1)
          # SP3 part2 mix or sequential
          if(input$enzyme_or_mix=="Trypsin/LysC Mix"){
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out,con = OT2_template_generation()$file_output_part2_mix)
          }else{
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out,con = OT2_template_generation()$file_output_part2_sequential)
          }
          writeLines(text = OT2_template_generation()$OT2_protocol_part3_vial_out,con = OT2_template_generation()$file_output_part3_vial_out)
          
          # save decklayoutplots for output ----------------------------------------------
          ggsave(plot = decklayout_plots()$deck_plot1,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot2,device = "png",
                 filename = paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                        yes = OT2_template_generation()$file_output_part2_mix,
                                        no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot3,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          # merge files for zipping ----------------------------------------------
          fs <- c(OT2_template_generation()$file_output_part1,
                  ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                         yes = OT2_template_generation()$file_output_part2_mix,
                         no = OT2_template_generation()$file_output_part2_sequential),
                  OT2_template_generation()$file_output_part3_vial_out,
                  paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep=""),
                  paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                               yes = OT2_template_generation()$file_output_part2_mix,
                               no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep="")
          )
        }
        
        # write protocol / decklayout files ----------------------------------------------
        if(input$logical_red_alk==F & input$EvoTips_vials=="EvoTips"){
          writeLines(text = OT2_template_generation()$OT2_protocol_part1_wo_alk_red_out,con = OT2_template_generation()$file_output_part1_wo_alk_red)
          # SP3 part2 mix or sequential
          if(input$enzyme_or_mix=="Trypsin/LysC Mix"){
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out,con = OT2_template_generation()$file_output_part2_mix)
          }else{
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out,con = OT2_template_generation()$file_output_part2_sequential)
          }
          writeLines(text = OT2_template_generation()$OT2_protocol_part3_EvoTip_out,con = OT2_template_generation()$file_output_part3_EvoTip_out)
          writeLines(text = OT2_template_generation()$OT2_protocol_part4_post_EvoTip_out,con = OT2_template_generation()$file_output_part4_post_EvoTip_out)
          
          # save decklayoutplots for output ----------------------------------------------
          ggsave(plot = decklayout_plots()$deck_plot1,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot2,device = "png",
                 filename = paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                        yes = OT2_template_generation()$file_output_part2_mix,
                                        no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot3,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot4,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          # merge files for zipping ----------------------------------------------
          fs <- c(OT2_template_generation()$file_output_part1_wo_alk_red,
                  ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                         yes = OT2_template_generation()$file_output_part2_mix,
                         no = OT2_template_generation()$file_output_part2_sequential),
                  OT2_template_generation()$file_output_part3_EvoTip_out,
                  OT2_template_generation()$file_output_part4_post_EvoTip_out,
                  paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep=""),
                  paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                               yes = OT2_template_generation()$file_output_part2_mix,
                               no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep="")
          )
        }
        # EvoTip == T & red&alk == F
        if(input$logical_red_alk==T & input$EvoTips_vials=="EvoTips"){
          writeLines(text = OT2_template_generation()$OT2_protocol_part1_out,
                     con = OT2_template_generation()$file_output_part1)
          # SP3 part2 mix or sequential
          if(input$enzyme_or_mix=="Trypsin/LysC Mix"){
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out,con = OT2_template_generation()$file_output_part2_mix)
          }else{
            writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out,con = OT2_template_generation()$file_output_part2_sequential)
          }
          writeLines(text = OT2_template_generation()$OT2_protocol_part3_EvoTip_out,
                     con = OT2_template_generation()$file_output_part3_EvoTip_out)
          writeLines(text = OT2_template_generation()$OT2_protocol_part4_post_EvoTip_out,
                     con = OT2_template_generation()$file_output_part4_post_EvoTip_out)
          
          # save decklayoutplots for output
          ggsave(plot = decklayout_plots()$deck_plot1,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot2,device = "png",
                 filename = paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                        yes = OT2_template_generation()$file_output_part2_mix,
                                        no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot3,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          ggsave(plot = decklayout_plots()$deck_plot4,device = "png",
                 filename = paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep=""),
                 width = 13,
                 height = 11)
          # merge files for zipping
          fs <- c(OT2_template_generation()$file_output_part1,
                  ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                         yes = OT2_template_generation()$file_output_part2_mix,
                         no = OT2_template_generation()$file_output_part2_sequential),
                  OT2_template_generation()$file_output_part3_EvoTip_out,
                  OT2_template_generation()$file_output_part4_post_EvoTip_out,
                  paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep=""),
                  paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                               yes = OT2_template_generation()$file_output_part2_mix,
                               no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep=""),
                  paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep="")
          )
        }
        
        incProgress(0.5, detail = "copy files")

        # generate HTML report ----------------------------------------------------
        # report file
        # copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # file
        # copy markdown
        if(input$EvoTips_vials=="Vial"){
          file.copy("www/Mass_spec_Preppy_MASTER.Rmd",
                    file.path(tempdir(), "Mass_spec_Preppy_MASTER.Rmd"),
                    overwrite = TRUE)
        }
        if(input$EvoTips_vials=="EvoTips"){
          file.copy("www/Mass_spec_Preppy_EvoTip_MASTER.Rmd",
                    file.path(tempdir(), "Mass_spec_Preppy_MASTER.Rmd"),
                    overwrite = TRUE)
        }
        
        # copy logo and other pics
        file.copy("www/Mass_Spec_Preppy_hexbin_small.png",
                  file.path(tempdir(), "Mass_Spec_Preppy_hexbin_small.png"),
                  overwrite = TRUE)
        # copy Evotip pure quick guide
        file.copy("www/Evotip_pure_loading.png",
                  file.path(tempdir(), "Evotip_pure_loading.png"),
                  overwrite = TRUE)
        
        # copy timecharts
        file.copy("www/OT2_Evotip_timechart.png",
                  file.path(tempdir(), "OT2_Evotip_timechart.png"),
                  overwrite = TRUE)
        file.copy("www/OT2_MSvial_timechart.png",
                  file.path(tempdir(), "OT2_MSvial_timechart.png"),
                  overwrite = TRUE)
        
        # red/alk == F
        if(as.logical(input$logical_red_alk)==FALSE){
          file.copy(paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep="")),
                    overwrite = TRUE)
          deck1_plot_tmp <- paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep="")
        }
        # red/alk == T
        if(as.logical(input$logical_red_alk)==TRUE){
          file.copy(paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep="")),
                    overwrite = TRUE)
          deck1_plot_tmp <- paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep="")
        }
        
        # SP3 step enzymes mix or sequential
          file.copy(paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                           yes = OT2_template_generation()$file_output_part2_mix,
                           no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                                      yes = OT2_template_generation()$file_output_part2_mix,
                                                      no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep="")),
                    overwrite = TRUE)
          
          deck2_plot_tmp <- paste(ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                                         yes = OT2_template_generation()$file_output_part2_mix,
                                         no = OT2_template_generation()$file_output_part2_sequential),"__decklayout.png",sep="")
        # MS vial 
        if(input$EvoTips_vials=="Vial"){
          file.copy(paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep="")),
                    overwrite = TRUE)
        }
        # EvoTip 
        if(input$EvoTips_vials=="EvoTips"){
          file.copy(paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep="")),
                    overwrite = TRUE)
          file.copy(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep=""),
                    file.path(tempdir(), paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep="")),
                    overwrite = TRUE)
        }
        
        incProgress(0.7, detail = "generate report parameters")
        
        # Set up parameters to pass to Rmd document ----------------------------------------------
        if(input$EvoTips_vials=="EvoTips"){
          params <- list(number_of_samples = OT2_template_generation()$number_of_samples,
                         reduction_and_alkylation = input$logical_red_alk,
                         sample_amount = input$input_sample_amount,
                         file_name = OT2_template_generation()$file_out_short,
                         LysC_Trypsin_Mix_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = input$Trypsin_LysC_Mix_ratio,no = NA),
                         LysC_Trypsin_Mix_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = input$Trypsin_LysC_Mix_stock_conc,no = NA),
                         trypsin_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$trypsin_ratio),
                         trypsin_stock_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$trypsin_stock_conc),
                         LysC_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$LysC_ratio),
                         LysC_stock_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$LysC_stock_conc),
                         EvoTips_loading = input$EvoTips_amount,
                         EvoTips_vials = input$EvoTips_vials,
                         sample_list = OT2_template_generation()$input_sample_list,
                         deck_plot1 = file.path(deck1_plot_tmp),
                         deck_plot2 = file.path(deck2_plot_tmp),
                         deck_plot3 = file.path(paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep="")),
                         deck_plot4 = file.path(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep=""))
                         
          )
          
        }
        if(input$EvoTips_vials=="Vial"){
          params <- list(number_of_samples = OT2_template_generation()$number_of_samples,
                         reduction_and_alkylation = input$logical_red_alk,
                         sample_amount = input$input_sample_amount,
                         file_name = OT2_template_generation()$file_out_short,
                         LysC_Trypsin_Mix_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = input$Trypsin_LysC_Mix_ratio,no = NA),
                         LysC_Trypsin_Mix_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = input$Trypsin_LysC_Mix_stock_conc,no = NA),
                         trypsin_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$trypsin_ratio),
                         trypsin_stock_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$trypsin_stock_conc),
                         LysC_ratio = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$LysC_ratio),
                         LysC_stock_concentration = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",yes = NA,no = input$LysC_stock_conc),
                         EvoTips_vials = input$EvoTips_vials,
                         sample_list = OT2_template_generation()$input_sample_list,
                         deck_plot1 = file.path(deck1_plot_tmp),
                         deck_plot2 = file.path(deck2_plot_tmp),
                         deck_plot3 = file.path(paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep=""))
          )
          
        }
        
        incProgress(0.9, detail = "render report")
        
        # render report HTML report ----------------------------------------------
        rmarkdown::render(file.path(tempdir(), "Mass_spec_Preppy_MASTER.Rmd"), 
                          output_file = file.path(tempdir(),paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.html",sep="")),
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
        # copy HTML report ----------------------------------------------
        file.copy(file.path(tempdir(),paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.html",sep="")),
                  file.path(paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.html",sep="")),
                  overwrite = TRUE)
        
        # append HTML report to file list ----------------------------------------------
        fs <- c(fs,file.path(paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.html",sep="")))
        
        incProgress(1, detail = "zip files and generate download link")
        # zip files ----------------------------------------------
        zip(zipfile=fname, files=fs)
        
        
        # remove files ----------------------------------------------
        file.remove(file.path(paste(format(Sys.Date(),"%Y_%m_%d_"),"__",OT2_template_generation()$file_name,"__Mass_Spec_Preppy.html",sep="")))

        # EvoTips / Vials
        if(input$EvoTips_vials=="EvoTips"){
          file.remove(file.path(paste(OT2_template_generation()$file_output_part3_EvoTip_out,"__decklayout.png",sep="")))
          file.remove(file.path(OT2_template_generation()$file_output_part3_EvoTip_out))
          file.remove(file.path(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out,"__decklayout.png",sep="")))
          file.remove(file.path(OT2_template_generation()$file_output_part4_post_EvoTip_out))
        }
        if(input$EvoTips_vials=="Vial"){
          file.remove(file.path(paste(OT2_template_generation()$file_output_part3_vial_out,"__decklayout.png",sep="")))
          file.remove(file.path(OT2_template_generation()$file_output_part3_vial_out))
        }
        # red/alk == F
        if(as.logical(input$logical_red_alk)==FALSE){
          file.remove(file.path(OT2_template_generation()$file_output_part1_wo_alk_red))
          file.remove(file.path(paste(OT2_template_generation()$file_output_part1_wo_alk_red,"__decklayout.png",sep="")))
        }
        # red/alk == T
        if(as.logical(input$logical_red_alk)==TRUE){
          file.remove(file.path(OT2_template_generation()$file_output_part1))
          file.remove(file.path(paste(OT2_template_generation()$file_output_part1,"__decklayout.png",sep="")))
        }
        
        # enzyme Mix == T
        if(input$enzyme_or_mix=="Trypsin/LysC Mix"){
          file.remove(file.path(OT2_template_generation()$file_output_part2_mix))
          file.remove(file.path(deck2_plot_tmp))
        }else{
          file.remove(file.path(OT2_template_generation()$file_output_part2_sequential))
          file.remove(file.path(deck2_plot_tmp))
        }
      })
    }
  )
  
  
  # render conditional output download ------------------------------------
  
  output$download_OT2_template<-renderUI({
    if(!is.null(OT2_template_generation()$file_name) #& sum(OT2_template_generation()$error)==0
    ){
      tagList(
        hr(),
        p("download OT-2 template:", style = "color:#84B135;margin-left: 5px"),
        downloadButton(outputId = "dlOT2",
                       label = "OT-2 protocol (.py)",
                       style="color:#FFFFFF; background-color: #060606; border-color: #84B135; margin-left: 5px;width:100%")
      )
    }
  })
  

# render static excel input template --------------------------------------

  output$input_template_OT2 <- downloadHandler(
    filename="Mass_Spec_Preppy_sample_meta_template.xlsx",  # desired file name on client 
    content=function(con) {
      file.copy("www/Mass_Spec_Preppy_sample_meta_template.xlsx", con)
    }
  )  
  
  
})
