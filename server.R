# ____________________________________________________________________#
## Script: shiny app: Mass Spec Preppy
## Author: Stephan Michalik
## part: server.R
## Notes:
# ____________________________________________________________________#

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
  # check credentials
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )


  # source UI sample digest -------------------------------------------------
  source("server/Server__sample_digest_UI_slider.R", local = TRUE)$value

  # OT-2 template generation ------------------------------------------------
  source("server/Server__sample_digest_OT2_template_generation.R", local = TRUE)$value

  # generate decklayout plots ------------------------------------------------
  source("server/Server__sample_digest_deck_layout_plots.R", local = TRUE)$value


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

  # generate OT2_template download -------------------------------------------
  source("server/Server__sample_digest_OT2_generate_protocol_download.R", local = TRUE)$value


  # static template download ------------------------------------------------
  source("server/Server__static_template_downloads.R", local = TRUE)$value


  # render conditional output sample digest download ------------------------------------

  output$download_OT2_template <- renderUI({
    if (!is.null(OT2_template_generation()$file_name) # & sum(OT2_template_generation()$error)==0
    ) {
      tagList(
        hr(),
        p("download OT-2 template:", style = "color:#84B135;margin-left: 5px"),
        downloadButton(
          outputId = "dlOT2",
          label = "OT-2 protocol (.py)",
          style = "color:#FFFFFF; background-color: #060606; border-color: #84B135; margin-left: 5px;width:100%"
        )
      )
    }
  })

  # BCA assay server --------------------------------------------------------
  source("server/Server__BCA_assay.R", local = TRUE)$value
})
