
# deck layout plot code ---------------------------------------------------


decklayout_plots <- eventReactive(input$inputButton_generate_OT2_template, {
  # progress decklayouts
  withProgress(message = "generate deck layout plots for the OT-2", style = "notification", value = 0, {
    incProgress(0.3, detail = "load sample list")

    # load sample file
    OT2_template_raw <- read_excel(
      input$input_sample_file$datapath,
      skip = 0, sheet = 1
    )
    # OT2_template <- read_excel("www/DEMO_MSpreppy.xlsx")
    OT2_template <- OT2_template_raw %>%
      filter(!is.na(sample))

    incProgress(0.6, detail = "generate plots")

    # step 1 red & alk == TRUE ------------------------------------------------
    if (input$logical_red_alk == TRUE) {
      deck_plot1 <- plot_deck_layout_step1(red_alk = T, meta_table = OT2_template_raw, text_color = "white")
    }
    if (input$logical_red_alk == FALSE) {
    }

    deck_plot2 <- plot_deck_layout_step2_SP3(
      number_of_samples = dim(OT2_template)[1],
      text_color = "white",
      trypsin_LysC_mix_used = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = T, no = F),
      trypsin_LysC_mix_ratio = as.numeric(input$Trypsin_LysC_Mix_ratio),
      trypsin_LysC_mix_conc = as.numeric(input$Trypsin_LysC_Mix_stock_conc),
      trypsin_conc = as.numeric(input$trypsin_stock_conc),
      LysC_conc = as.numeric(input$LysC_stock_conc),
      sample_amount = as.numeric(input$input_sample_amount),
      trypsin_ratio = as.numeric(input$trypsin_ratio),
      LysC_ratio = as.numeric(input$LysC_ratio)
    )

    # EvoTip / Vial
    if (input$EvoTips_vials == "Vial") {
      deck_plot3 <- plot_deck_layout_step3_MSvial(number_of_samples = dim(OT2_template)[1], text_color = "white")
      deck_plot4 <- plot_spacer()
    }
    if (input$EvoTips_vials == "EvoTips") {
      deck_plot3 <- plot_deck_layout_step3_EvoTips(number_of_samples = dim(OT2_template)[1], text_color = "white")
      deck_plot4 <- plot_deck_layout_step4_EvoTips(number_of_samples = dim(OT2_template)[1], text_color = "white")
    }

    incProgress(1, detail = "return output")

    # return plots
    list(
      deck_plot1 = deck_plot1,
      deck_plot2 = deck_plot2,
      deck_plot3 = deck_plot3,
      deck_plot4 = deck_plot4
    )
  }) # end progressbar
}) # end decklayout_plots

# render deck plot output ------------------------------------------------------

# progressbar render deck plots
withProgress(message = "render deck layout plots for the OT-2", style = "notification", value = 0, {
  incProgress(0.3, detail = "deck layout step 1")
  output$step1_plot <- renderPlot(decklayout_plots()$deck_plot1)
  incProgress(0.6, detail = "deck layout step 2")
  output$step2_plot <- renderPlot(decklayout_plots()$deck_plot2)
  incProgress(0.9, detail = "deck layout step 3")
  output$step3_plot <- renderPlot(decklayout_plots()$deck_plot3)
  output$step4_plot <- renderPlot(decklayout_plots()$deck_plot4)
}) # end progressbar render deck plots
