
# generate sample digest OT-2 protocol R code -----------------------------



# download OT-2 template
output$dlOT2 <- downloadHandler(
  filename = function() {
    paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.zip", sep = "")
  },
  content = function(fname) {
    # tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
    # tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
    # setwd(tmpdir)
    # print(tmpdir)

    # progressbar
    withProgress(message = "generate download", style = "notification", value = 0, {
      incProgress(0.2, detail = "saving deck layout plots and protocols")
      # write protocol / decklayout files ----------------------------------------------
      
      # save volume needed plot
      ggsave(
        plot = OT2_template_generation()$volume_needed_plot &
          theme(text = element_text(color = "black"),
                panel.background = element_rect(fill = "white")) & 
          plot_annotation(theme = theme(plot.background = element_rect(fill  = "white", color = NA))),
        device = "png",
        filename = "sample_volume_plot.png",
        width = 15,
        height = 7*OT2_template_generation()$volume_needed_plot_N
      )
      
      
    if (input$logical_red_alk == F & input$EvoTips_vials == "Vial") {
        writeLines(
          text = OT2_template_generation()$OT2_protocol_part1_wo_alk_red_out,
          con = OT2_template_generation()$file_output_part1_wo_alk_red
        )
        # SP3 part2 mix or sequential
        if (input$enzyme_or_mix == "Trypsin/LysC Mix") {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out, con = OT2_template_generation()$file_output_part2_mix)
        } else {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out, con = OT2_template_generation()$file_output_part2_sequential)
        }
        writeLines(
          text = OT2_template_generation()$OT2_protocol_part3_vial_out,
          con = OT2_template_generation()$file_output_part3_vial_out
        )

        # save decklayoutplots for output
        ggsave(
          plot = decklayout_plots()$deck_plot1, device = "png",
          filename = paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot2, device = "png",
          filename = paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot3, device = "png",
          filename = paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        # merge files for zipping
        fs <- c(
          OT2_template_generation()$file_output_part1_wo_alk_red,
          ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ),
          OT2_template_generation()$file_output_part3_vial_out,
          paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = ""),
          paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = "")
        )
      } #end vial w/o red. alk.

    if (input$logical_red_alk == T & input$EvoTips_vials == "Vial") {
        writeLines(text = OT2_template_generation()$OT2_protocol_part1_out, con = OT2_template_generation()$file_output_part1)
        # SP3 part2 mix or sequential
        if (input$enzyme_or_mix == "Trypsin/LysC Mix") {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out, con = OT2_template_generation()$file_output_part2_mix)
        } else {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out, con = OT2_template_generation()$file_output_part2_sequential)
        }
        writeLines(text = OT2_template_generation()$OT2_protocol_part3_vial_out, con = OT2_template_generation()$file_output_part3_vial_out)

        # save decklayoutplots for output ----------------------------------------------
        ggsave(
          plot = decklayout_plots()$deck_plot1, device = "png",
          filename = paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot2, device = "png",
          filename = paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot3, device = "png",
          filename = paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        # merge files for zipping ----------------------------------------------
        fs <- c(
          OT2_template_generation()$file_output_part1,
          ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ),
          OT2_template_generation()$file_output_part3_vial_out,
          paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = ""),
          paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = "")
        )
      } #end vial with red. alk.

      # write protocol / decklayout files ----------------------------------------------
      if (input$logical_red_alk == F & input$EvoTips_vials == "EvoTips") {
        writeLines(text = OT2_template_generation()$OT2_protocol_part1_wo_alk_red_out, con = OT2_template_generation()$file_output_part1_wo_alk_red)
        # SP3 part2 mix or sequential
        if (input$enzyme_or_mix == "Trypsin/LysC Mix") {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out, con = OT2_template_generation()$file_output_part2_mix)
        } else {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out, con = OT2_template_generation()$file_output_part2_sequential)
        }
        writeLines(text = OT2_template_generation()$OT2_protocol_part3_EvoTip_out, con = OT2_template_generation()$file_output_part3_EvoTip_out)
        writeLines(text = OT2_template_generation()$OT2_protocol_part4_post_EvoTip_out, con = OT2_template_generation()$file_output_part4_post_EvoTip_out)

        # save decklayoutplots for output ----------------------------------------------
        ggsave(
          plot = decklayout_plots()$deck_plot1, device = "png",
          filename = paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot2, device = "png",
          filename = paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot3, device = "png",
          filename = paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot4, device = "png",
          filename = paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        # merge files for zipping ----------------------------------------------
        fs <- c(
          OT2_template_generation()$file_output_part1_wo_alk_red,
          ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ),
          OT2_template_generation()$file_output_part3_EvoTip_out,
          OT2_template_generation()$file_output_part4_post_EvoTip_out,
          paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = ""),
          paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = "")
        )
      } #end EVOTIP w/o red. alk.
      
      # EvoTip == T & red&alk == F
    if (input$logical_red_alk == T & input$EvoTips_vials == "EvoTips") {
        writeLines(
          text = OT2_template_generation()$OT2_protocol_part1_out,
          con = OT2_template_generation()$file_output_part1
        )
        # SP3 part2 mix or sequential
        if (input$enzyme_or_mix == "Trypsin/LysC Mix") {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out, con = OT2_template_generation()$file_output_part2_mix)
        } else {
          writeLines(text = OT2_template_generation()$OT2_protocol_part2_sequential_out, con = OT2_template_generation()$file_output_part2_sequential)
        }
        writeLines(
          text = OT2_template_generation()$OT2_protocol_part3_EvoTip_out,
          con = OT2_template_generation()$file_output_part3_EvoTip_out
        )
        writeLines(
          text = OT2_template_generation()$OT2_protocol_part4_post_EvoTip_out,
          con = OT2_template_generation()$file_output_part4_post_EvoTip_out
        )

        # save decklayoutplots for output
        ggsave(
          plot = decklayout_plots()$deck_plot1, device = "png",
          filename = paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot2, device = "png",
          filename = paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot3, device = "png",
          filename = paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )
        ggsave(
          plot = decklayout_plots()$deck_plot4, device = "png",
          filename = paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = ""),
          width = 13,
          height = 11
        )

        
        # merge files for zipping
        fs <- c(
          OT2_template_generation()$file_output_part1,
          ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ),
          OT2_template_generation()$file_output_part3_EvoTip_out,
          OT2_template_generation()$file_output_part4_post_EvoTip_out,
          paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = ""),
          paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
            yes = OT2_template_generation()$file_output_part2_mix,
            no = OT2_template_generation()$file_output_part2_sequential
          ), "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = ""),
          paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = "")
        )
      } #end EVOTIP with red. alk.

      incProgress(0.5, detail = "copy files")

      # generate HTML report ----------------------------------------------------
      # report file
      # copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # file
      # copy quarto
      file.copy("www/styles.css",
                file.path(tempdir(), "styles.css"),
                overwrite = TRUE
      )
      
      if (input$EvoTips_vials == "Vial") {
        file.copy("reprot_template/Mass_spec_Preppy_MASTER.qmd",
          file.path(tempdir(), "Mass_spec_Preppy_MASTER.qmd"),
          overwrite = TRUE
        )
      }
      if (input$EvoTips_vials == "EvoTips") {
        file.copy("reprot_template/Mass_spec_Preppy_EvoTip_MASTER.qmd",
          file.path(tempdir(), "Mass_spec_Preppy_MASTER.qmd"),
          overwrite = TRUE
        )
      }

      # copy logo and other pics
      file.copy("www/Mass_Spec_Preppy_hexbin_small.png",
        file.path(tempdir(), "Mass_Spec_Preppy_hexbin_small.png"),
        overwrite = TRUE
      )
      # copy Evotip pure quick guide
      file.copy("www/Evotip_pure_loading.png",
        file.path(tempdir(), "Evotip_pure_loading.png"),
        overwrite = TRUE
      )

      # copy timecharts
      file.copy("www/OT2_Evotip_timechart.png",
        file.path(tempdir(), "OT2_Evotip_timechart.png"),
        overwrite = TRUE
      )
      
      file.copy("www/OT2_MSvial_timechart.png",
        file.path(tempdir(), "OT2_MSvial_timechart.png"),
        overwrite = TRUE
      )

      
      # red/alk == F
      if (as.logical(input$logical_red_alk) == FALSE) {
        file.copy(paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = ""),
          file.path(tempdir(), paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = "")),
          overwrite = TRUE
        )
        deck1_plot_tmp <- paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = "")
      }
      # red/alk == T
      if (as.logical(input$logical_red_alk) == TRUE) {
        file.copy(paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = ""),
          file.path(tempdir(), paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = "")),
          overwrite = TRUE
        )
        deck1_plot_tmp <- paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = "")
      }

      # SP3 step enzymes mix or sequential
      file.copy(
        paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
          yes = OT2_template_generation()$file_output_part2_mix,
          no = OT2_template_generation()$file_output_part2_sequential
        ), "__decklayout.png", sep = ""),
        file.path(tempdir(), paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
          yes = OT2_template_generation()$file_output_part2_mix,
          no = OT2_template_generation()$file_output_part2_sequential
        ), "__decklayout.png", sep = "")),
        overwrite = TRUE
      )

      deck2_plot_tmp <- paste(ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix",
        yes = OT2_template_generation()$file_output_part2_mix,
        no = OT2_template_generation()$file_output_part2_sequential
      ), "__decklayout.png", sep = "")
      # MS vial
      if (input$EvoTips_vials == "Vial") {
        file.copy(paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = ""),
          file.path(tempdir(), paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = "")),
          overwrite = TRUE
        )
      }
      # EvoTip
      if (input$EvoTips_vials == "EvoTips") {
        file.copy(paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = ""),
          file.path(tempdir(), paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = "")),
          overwrite = TRUE
        )
        file.copy(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = ""),
          file.path(tempdir(), paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = "")),
          overwrite = TRUE
        )
      }
      
      #copy volume needed plot
      file.copy("sample_volume_plot.png",
                file.path(tempdir(), paste("sample_volume_plot.png", sep = "")),
                overwrite = TRUE
      )
      
      incProgress(0.7, detail = "generate report parameters")

      # Set up parameters to pass to Rmd document ----------------------------------------------
      if (input$EvoTips_vials == "EvoTips") {
        params <- list(
          number_of_samples = OT2_template_generation()$number_of_samples,
          reduction_and_alkylation = input$logical_red_alk,
          volume_needed_table = OT2_template_generation()$OT2_template_tmp %>% select(-row_pos,-col_pos),
          volume_needed_plot = file.path("sample_volume_plot.png"),
          sample_amount = input$input_sample_amount,
          file_name = OT2_template_generation()$file_out_short,
          LysC_Trypsin_Mix_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = input$Trypsin_LysC_Mix_ratio, no = NA),
          LysC_Trypsin_Mix_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = input$Trypsin_LysC_Mix_stock_conc, no = NA),
          trypsin_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$trypsin_ratio),
          trypsin_stock_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$trypsin_stock_conc),
          LysC_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$LysC_ratio),
          LysC_stock_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$LysC_stock_conc),
          EvoTips_loading = input$EvoTips_amount,
          EvoTips_vials = input$EvoTips_vials,
          sample_list = OT2_template_generation()$input_sample_list,
          deck_plot1 = file.path(deck1_plot_tmp),
          deck_plot2 = file.path(deck2_plot_tmp),
          deck_plot3 = file.path(paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = "")),
          deck_plot4 = file.path(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = ""))
        )
      }
      if (input$EvoTips_vials == "Vial") {
        params <- list(
          number_of_samples = OT2_template_generation()$number_of_samples,
          reduction_and_alkylation = input$logical_red_alk,
          volume_needed_table = OT2_template_generation()$OT2_template_tmp %>% select(-row_pos,-col_pos),
          volume_needed_plot = file.path(paste("sample_volume_plot.png", sep = "")),
          sample_amount = input$input_sample_amount,
          file_name = OT2_template_generation()$file_out_short,
          LysC_Trypsin_Mix_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = input$Trypsin_LysC_Mix_ratio, no = NA),
          LysC_Trypsin_Mix_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = input$Trypsin_LysC_Mix_stock_conc, no = NA),
          trypsin_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$trypsin_ratio),
          trypsin_stock_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$trypsin_stock_conc),
          LysC_ratio = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$LysC_ratio),
          LysC_stock_concentration = ifelse(test = input$enzyme_or_mix == "Trypsin/LysC Mix", yes = NA, no = input$LysC_stock_conc),
          EvoTips_vials = input$EvoTips_vials,
          sample_list = OT2_template_generation()$input_sample_list,
          deck_plot1 = file.path(deck1_plot_tmp),
          deck_plot2 = file.path(deck2_plot_tmp),
          deck_plot3 = file.path(paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = ""))
        )
      }

      incProgress(0.9, detail = "render report")

      # render report HTML report ----------------------------------------------
      # rmarkdown::render(file.path(tempdir(), "Mass_spec_Preppy_MASTER.Rmd"),
      #   output_file = file.path(tempdir(), paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")),
      #   params = params,
      #   envir = new.env(parent = globalenv())
      # )
      #render quarto
      quarto::quarto_render(input = file.path(tempdir(), "Mass_spec_Preppy_MASTER.qmd"),
                        execute_params = params
                        )
      #rename file
      file.rename(from = file.path(tempdir(), "Mass_spec_Preppy_MASTER.html"),to = file.path(tempdir(),paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")))

      # copy HTML report ----------------------------------------------
      file.copy(file.path(tempdir(), paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")),
        file.path(paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")),
        overwrite = TRUE
      )

      # append HTML report to file list ----------------------------------------------
      fs <- c(fs, file.path(paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")))

      incProgress(1, detail = "zip files and generate download link")
      # zip files ----------------------------------------------
      zip(zipfile = fname, files = fs)


      # remove files ----------------------------------------------
      file.remove(file.path(paste(format(Sys.Date(), "%Y_%m_%d_"), "__", OT2_template_generation()$file_name, "__Mass_Spec_Preppy.html", sep = "")))
      file.remove(file.path("sample_volume_plot.png"))
      
      
      # EvoTips / Vials
      if (input$EvoTips_vials == "EvoTips") {
        file.remove(file.path(paste(OT2_template_generation()$file_output_part3_EvoTip_out, "__decklayout.png", sep = "")))
        file.remove(file.path(OT2_template_generation()$file_output_part3_EvoTip_out))
        file.remove(file.path(paste(OT2_template_generation()$file_output_part4_post_EvoTip_out, "__decklayout.png", sep = "")))
        file.remove(file.path(OT2_template_generation()$file_output_part4_post_EvoTip_out))
      }
      if (input$EvoTips_vials == "Vial") {
        file.remove(file.path(paste(OT2_template_generation()$file_output_part3_vial_out, "__decklayout.png", sep = "")))
        file.remove(file.path(OT2_template_generation()$file_output_part3_vial_out))
      }
      # red/alk == F
      if (as.logical(input$logical_red_alk) == FALSE) {
        file.remove(file.path(OT2_template_generation()$file_output_part1_wo_alk_red))
        file.remove(file.path(paste(OT2_template_generation()$file_output_part1_wo_alk_red, "__decklayout.png", sep = "")))
      }
      # red/alk == T
      if (as.logical(input$logical_red_alk) == TRUE) {
        file.remove(file.path(OT2_template_generation()$file_output_part1))
        file.remove(file.path(paste(OT2_template_generation()$file_output_part1, "__decklayout.png", sep = "")))
      }

      # enzyme Mix == T
      if (input$enzyme_or_mix == "Trypsin/LysC Mix") {
        file.remove(file.path(OT2_template_generation()$file_output_part2_mix))
        file.remove(file.path(deck2_plot_tmp))
      } else {
        file.remove(file.path(OT2_template_generation()$file_output_part2_sequential))
        file.remove(file.path(deck2_plot_tmp))
      }
    })
  }
)


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