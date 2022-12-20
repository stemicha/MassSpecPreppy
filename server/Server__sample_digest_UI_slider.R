
# sample digest UI elemets -----------------------------------------------


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