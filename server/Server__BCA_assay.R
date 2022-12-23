# BCA assay R code -------------------------------------------------------------

# processing data
observeEvent(input$BCA_inputButton_data_processing, {
  if (!is.null(input$BCA_xlsx_raw_96well) & !is.null(input$BCA_xlsx_meta)) {
    # Show a modal when the button is pressed
    shinyalert("processing...",
      "This might take while",
      imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
      type = "success",
      timer = 2000
    )
  }
})

# generating template
observeEvent(input$inputButton_generate_BCA_OT2_template, {
  # Show a modal when the button is pressed
  shinyalert("generating OT-2 protocol ...", "This might take while",
    imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
    type = "success",
    timer = 2000
  )
})


# no 96well input file selected
observeEvent(input$BCA_inputButton_data_processing, {
  if (is.null(input$BCA_xlsx_raw_96well)) {
    shinyalert("missing file",
      "no 96well file input file selected",
      imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
      type = "error",
      timer = 0
    )
  }
})

# no meta input file selected
observeEvent(input$BCA_inputButton_data_processing, {
  if (is.null(input$BCA_xlsx_meta)) {
    shinyalert("missing file",
      "no meta file input file selected",
      imageUrl = "Mass_Spec_Preppy_hexbin_small.png",
      type = "error",
      timer = 0
    )
  }
})



# comment colors ----------------------------------------------------------

comments_colors <- c(
  "Error: Abs below std. curve" = "#DB4437", # red
  "Error: Abs. above std. curve" = "#DB4437", # red
  "Warning: Abs. in lower range of std. curve" = "#F4B400", # yellow
  "Warning: Abs. in upper range of std. curve" = "#F4B400", # yellow
  "OK" = "#0F9D58" # green
)



# BCA template OT-2 download --------------------------------------------------
BCA_OT2_template_generation <- eventReactive(input$inputButton_generate_BCA_OT2_template, {
  withProgress(message = "generate BCA protocol for the OT-2", style = "notification", value = 0, {
    incProgress(0.2, detail = "load data")

    BCA_OT2_template <- read_excel(
      input$BCA_OT2_template_upload_file$datapath,
      skip = 0, sheet = 1
    )
    BCA_OT2_template <- BCA_OT2_template |> filter(!is.na(sample))
    BCA_OT2_template$sample <- as.character(BCA_OT2_template$sample)

    # load shinyBCA meta file template for 1 plate and adjust it
    shiny_plate1_template <- read_excel(path = "www/Meta_file_template_for__1_plates.xlsx")
    index <- 17
    for (i in 1:nrow(BCA_OT2_template)) {
      shiny_plate1_template[index, "Sample"] <- BCA_OT2_template[i, "sample"]
      shiny_plate1_template[index, "Dilution"] <- BCA_OT2_template[i, "dilution"]
      shiny_plate1_template[index + 1, "Sample"] <- BCA_OT2_template[i, "sample"]
      shiny_plate1_template[index + 1, "Dilution"] <- BCA_OT2_template[i, "dilution"]
      index <- index + 2
    }
    # load colors for meta template
    meta_template <- read_excel("www/meta_data_template_colors.xlsx")

    incProgress(0.4, detail = "create meta workbook")

    BCA_excel_output_template_OT2 <- createWorkbook()

    addWorksheet(wb = BCA_excel_output_template_OT2, sheetName = "Sheet1")

    # write data into workbook
    writeDataTable(
      wb = BCA_excel_output_template_OT2,
      sheet = 1,
      x = shiny_plate1_template,
      startRow = 1,
      startCol = 1,
      tableStyle = "TableStyleMedium1"
    )


    # format cells
    for (i in 1:nrow(shiny_plate1_template)) {
      meta_template_tmp <- meta_template %>% filter(Well == shiny_plate1_template$Well[i])
      colorStyle <- createStyle(fgFill = meta_template_tmp$colors)
      addStyle(
        wb = BCA_excel_output_template_OT2,
        sheet = 1,
        style = colorStyle,
        rows = i + 1,
        cols = 1:5,
        gridExpand = FALSE
      )
    }

    # generate file name
    inFile <- input$BCA_OT2_template_upload_file
    file_out_tmp <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")

    incProgress(0.6, detail = "generate BCA assay OT-2 protocol")

    # load OT-2 python protocol max. 40 samples
    BCA_OT2_protocol <- read_lines(file = "www/BCAassay_OT2_C_FunGene_version1.py")

    # implement files
    # genrate python JSON input edited file -----------------------------------

    input_JSON <- "    _all_values = json.loads(\"\"\"{\"csv_sample\":\"Sample,Dilution,OT2_samples,OT-2 position,sample_volume"



    for (i in 1:nrow(BCA_OT2_template)) {
      input_JSON <- paste(input_JSON, paste(BCA_OT2_template[i, ], collapse = ","), sep = "\\\\n")
    }
    input_JSON <- paste(input_JSON, "\"}\"\"\")", sep = "")

    BCA_OT2_protocol_out <- BCA_OT2_protocol
    BCA_OT2_protocol_out[16] <- input_JSON
    BCA_OT2_protocol_out[23] <- paste(
      substr(
        x = BCA_OT2_protocol_out[23],
        start = 1,
        stop = nchar(BCA_OT2_protocol_out[23]) - 2
      ),
      " | ", file_out_tmp,
      "\",",
      sep = "",
      collapse = ""
    )
    incProgress(0.8, detail = "BCA assay...writing output")

    list(
      OT2_protocol_out = BCA_OT2_protocol_out,
      OT2_template = BCA_OT2_template,
      excel_output_template_OT2 = BCA_excel_output_template_OT2,
      file_output_short = file_out_tmp,
      file_output = paste(str_replace_all(file_out_tmp, ".xlsx", ""), "__OT2_BCA_half_area_plate_protocol.py", sep = "")
    )
  }) # end progressbar
}) # end BCA_OT2_template_generation


# generate BCA OT2_template download -------------------------------------------



# download BCA OT-2 template
output$dlOT2_BCA <- downloadHandler(
  filename = function() {
    paste(format(Sys.Date(), "%Y_%m_%d_"), "__", BCA_OT2_template_generation()$file_output_short, "__BCA_assay.zip", sep = "")
  },
  content = function(fname) {
    # tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
    # tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
    # setwd(tmpdir)
    # print(tmpdir)
    withProgress(message = "generate BCA assay output download", style = "notification", value = 0, {
      incProgress(0.2, detail = "format data")

      # OT-2 file
      writeLines(text = BCA_OT2_template_generation()$OT2_protocol_out, con = BCA_OT2_template_generation()$file_output)
      # shinyBCA template
      saveWorkbook(BCA_OT2_template_generation()$excel_output_template_OT2, file = paste0(BCA_OT2_template_generation()$file_output_short, "_BCA_assay_meta_file.xlsx"), overwrite = TRUE)

      incProgress(0.4, detail = "copy files")

      # Rmd file
      file.copy("www/OT2_BCA_MASTER.Rmd",
        file.path(tempdir(), "OT2_BCA_MASTER.Rmd"),
        overwrite = TRUE
      )
      # copy logo and other pics
      file.copy("www/ShinyBCA_hexbin_small.png",
        file.path(tempdir(), "ShinyBCA_hexbin_small.png"),
        overwrite = TRUE
      )

      ggsave(
        plot = plot_deck_layout_BCA(meta_table = BCA_OT2_template_generation()$OT2_template, text_color = "black"), device = "png",
        filename = paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = ""),
        width = 13,
        height = 11
      )
      file.copy(paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = ""),
        file.path(tempdir(), paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = "")),
        overwrite = TRUE
      )

      params <- list(
        number_of_samples = nrow(BCA_OT2_template_generation()$OT2_template),
        sample_list = BCA_OT2_template_generation()$OT2_template,
        file_name = BCA_OT2_template_generation()$file_output_short,
        deck_plot = paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = "")
      )

      incProgress(0.6, detail = "render report")

      # render report
      rmarkdown::render(file.path(tempdir(), "OT2_BCA_MASTER.Rmd"),
        output_file = file.path(tempdir(), paste(format(Sys.Date(), "%Y_%m_%d_"), "__", BCA_OT2_template_generation()$file_output_short, "__BCA_assay_description.html", sep = "")),
        params = params,
        envir = new.env(parent = globalenv())
      )

      # copy report
      file.copy(file.path(tempdir(), paste(format(Sys.Date(), "%Y_%m_%d_"), "__", BCA_OT2_template_generation()$file_output_short, "__BCA_assay_description.html", sep = "")),
        file.path(paste(format(Sys.Date(), "%Y_%m_%d_"), "__", BCA_OT2_template_generation()$file_output_short, "__BCA_assay_description.html", sep = "")),
        overwrite = TRUE
      )



      # merge files for zipping
      fs <- c(
        BCA_OT2_template_generation()$file_output, # OT-2 protocol
        paste0(BCA_OT2_template_generation()$file_output_short, "_BCA_assay_meta_file.xlsx")
      )
      # append report to file list
      fs <- c(
        fs,
        file.path(paste(format(Sys.Date(), "%Y_%m_%d_"), "__", BCA_OT2_template_generation()$file_output_short,
          "__BCA_assay_description.html",
          sep = ""
        ))
      )

      incProgress(0.6, detail = "compress output")

      # zip files
      zip(zipfile = fname, files = fs)

      # remove temp. files
      file.remove(fs)
      file.remove(file.path(tempdir(), "OT2_BCA_MASTER.Rmd"))
      file.remove(file.path(tempdir(), "ShinyBCA_hexbin_small.png"))
      file.remove(file.path(tempdir(), paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = "")))
      file.remove(file.path(paste(BCA_OT2_template_generation()$file_output_short, "__decklayout.png", sep = "")))
    }) # end progress
  }
) # end download BCA OT-2 template




# render conditional download template
output$download_BCA_OT2_template <- renderUI({
  if (!is.null(BCA_OT2_template_generation()$file_output)) {
    tagList(
      h6("download OT-2 BCA assay protocol folder:", style = "color:#C0C0C0;margin-left: 10px"),
      downloadButton(
        outputId = "dlOT2_BCA",
        label = "OT-2 protocol zip folder",
        width = "100%",
        style = "color:#FFFFFF; background-color: #689EC8; border-color: #689EC8; margin-left: 5px;width:100%"
      )
    )
  }
})


# BCA assay load data ---------------------------------------------------------------
input_data <- eventReactive(input$BCA_inputButton_data_processing, {
  df <- read_excel(
    input$BCA_xlsx_raw_96well$datapath,
    skip = 1, sheet = 1
  ) # skip first 4 rows

  # load meta file -------------------------------------------------------------
  meta_in <- read_excel(
    input$BCA_xlsx_meta$datapath,
    skip = 0, sheet = 1
  )

  # shinyalert error pop ups if plate well count is above 96
  if (wells96_per_plate_check(meta_input = meta_in) == TRUE) {
    shinyalert(
      title = "one plate in meta data have more than 96 wells",
      text = "correct the error in the meta file and try again",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "ShinyBCA_hexbin_small.png",
      imageWidth = 100,
      imageHeight = 100,
      animation = TRUE
    )
  }

  # df cleaning -------------------------------------------------------------
  if (wells96_per_plate_check(meta_input = meta_in) == FALSE) {
    df_filtered <- synergy_tidy(df)

    list(
      df_filtered = df_filtered,
      meta = meta_in
    )
  } else {
    NULL
  }
})



file_name <- eventReactive(input$BCA_inputButton_data_processing, {
  inFile <- input$BCA_xlsx_raw_96well

  if (is.null(inFile)) {
    return(NULL)
  }

  return(stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
})

output$myFileName <- renderText({
  file_name()
})


# split standard and sample measurements ----------------------------------

calculations <- reactive({
  withProgress(
    message = "processing data...",
    style = "notification",
    value = 0.1,
    {
      incProgress(amount = 1 / 10, message = "get data")
      # remove white space
      raw_data <- input_data()$df_filtered
      meta <- input_data()$meta
      # raw_data <- df_filtered
      # meta <- meta_in

      # add meta data -----------------------------------------------------------
      incProgress(amount = 2 / 10, message = "add meta data")

      # 96 well plate
      raw_data <- left_join(raw_data, meta, by = c("Plate Number", "Well"))
      # plate positions to add missing sample and plate information
      well_position_name_adding <- c(
        paste(LETTERS[seq(from = 1, to = 8)], 1, sep = ""),
        paste(LETTERS[seq(from = 1, to = 8)], 2, sep = "")
      )

      for (i in 1:nrow(raw_data)) {
        # check if well is in Std or Black well position range
        if (raw_data$Well[i] %in% well_position_name_adding) {
          if (is.na(raw_data$`Plate Number`[i])) {
            raw_data$`Plate Number`[i] <- raw_data$`Plate Number`[i - 1]
            raw_data$Sample[i] <- raw_data$Sample[i - 1]
          }
        }
      }
      # remove empty samples
      raw_data <- raw_data %>% filter(!is.na(Sample))

      # end 96 well meta file adding

      incProgress(amount = 3 / 10, message = "extract standard")

      # get standard ------------------------------------------------------------
      std_tidy <- raw_data %>%
        filter(grepl(pattern = "BLK|STD", x = Sample))



      std_conc_tibble <- tibble(
        Sample = c(
          "STD160",
          "STD120",
          "STD80",
          "STD60",
          "STD40",
          "STD20",
          "STD10",
          "BLK"
        ),
        conc_mg_per_ml = as.numeric(c(0.16, 0.12, 0.08, 0.06, 0.04, 0.02, 0.01, 0))
      )

      std_tidy <- left_join(std_tidy, std_conc_tibble, by = "Sample")

      # mean over technical replicates
      std_tidy_summary <- std_tidy %>%
        rename(Abs = `562`) %>%
        group_by(Sample, `Plate Number`, conc_mg_per_ml) %>%
        group_by(conc_mg_per_ml, `Plate Number`) %>%
        summarise(
          meanAbs = mean(Abs, na.rm = T),
          SD_Abs = sd(Abs, na.rm = T),
          CV_Abs = sd(Abs, na.rm = T) / mean(Abs, na.rm = T)
        ) %>%
        ungroup()



      incProgress(amount = 4 / 10, message = "summarizing samples")


      # sample sorting vector
      sample_sorting_vector <- unlist(raw_data %>%
        filter(!grepl(pattern = "STD", x = Sample) & Sample != "BLK") %>%
        distinct(Sample))

      # samples / mean over technical replicates / take 3 workaround
      data_samples <- raw_data %>%
        filter(!grepl(pattern = "STD", x = Sample) & Sample != "BLK") %>%
        group_by(Sample, Well, Dilution, Replicate, `Plate Number`) %>%
        summarise(Abs = mean(`562`, na.rm = T), tech_Abs_CV = sd(`562`, na.rm = T) / mean(`562`, na.rm = T)) %>%
        ungroup() %>%
        arrange(match(x = Sample, table = sample_sorting_vector))

      # summarize raw data
      data_samples_summary <- data_samples %>%
        group_by(Sample, Dilution, `Plate Number`) %>%
        summarise(
          meanAbs = mean(as.numeric(unlist(Abs)), na.rm = T),
          SD_Abs = sd(as.numeric(unlist(Abs)), na.rm = T),
          CV_Abs = sd(as.numeric(unlist(Abs)), na.rm = T) / mean(as.numeric(unlist(Abs)), na.rm = T)
        ) %>%
        ungroup() %>%
        mutate(dilution_factor = as.numeric(Dilution)) %>%
        arrange(match(x = Sample, table = sample_sorting_vector))


      incProgress(amount = 5 / 10, message = "generate standard plots")

      # CV plot
      std_cv_plot <- ggplot(std_tidy_summary, aes(as.factor(conc_mg_per_ml), CV_Abs, fill = CV_Abs > 0.2)) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = 18) +
        scale_fill_manual(values = c("TRUE" = "orangered3", "FALSE" = "grey")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(title = "Standard CV plot", x = "conc_mg_per_ml") +
        geom_hline(yintercept = 0.1) +
        geom_hline(yintercept = 0.2, linetype = "dotted") +
        ylim(0, 1) +
        facet_wrap(~`Plate Number`) +
        theme(
          strip.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = "white")
        )

      data_cv_plot <- ggplot(data_samples_summary, aes(Sample, CV_Abs, fill = CV_Abs > 0.2)) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = 18) +
        scale_fill_manual(values = c("TRUE" = "orangered3", "FALSE" = "grey")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(title = "CV plot") +
        geom_hline(yintercept = 0.1) +
        geom_hline(yintercept = 0.2, linetype = "dotted") +
        ylim(0, 1) +
        facet_wrap(~Dilution, labeller = label_both, ncol = 1) +
        theme(
          strip.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = "white")
        )


      # perform loess and linear fit / exchange axis for predict ----
      incProgress(amount = 6 / 10, message = "generate curve fitting")
      # plate-wise fit
      # loess fit
      fits <- std_tidy %>%
        rename(Abs = `562`) %>%
        group_by(`Plate Number`) %>%
        select(`Plate Number`, conc_mg_per_ml, Abs) %>%
        nest(data = c(conc_mg_per_ml, Abs)) %>%
        mutate(loess_fit = map(data, ~ loess(formula = conc_mg_per_ml ~ Abs, data = .x, control = loess.control(surface = "direct")))) %>%
        mutate(linear_fit = map(data, ~ lm(formula = conc_mg_per_ml ~ Abs, data = .x))) %>%
        mutate(residuals = map(loess_fit, augment)) %>%
        mutate(std_err__loess = summary(loess_fit[[1]])$s) %>%
        mutate(Rsquared__linear = summary(linear_fit[[1]])$r.squared)

      fits_goodness <- fits %>% distinct(`Plate Number`, std_err__loess, Rsquared__linear)

      # get residuals
      residuals_tidy <- fits %>%
        select(`Plate Number`, residuals) %>%
        unnest(cols = c(residuals)) %>%
        ungroup()


      # add z-score to residuals
      z_score_threshold <- 3

      residuals_tidy <- residuals_tidy %>%
        mutate(z_score = (.resid - mean(.resid)) / sd(.resid)) %>%
        mutate(z_score_outlier = abs(z_score) > z_score_threshold)


      # add hard codes standard curve -------------------------------------------
      standard_hard_coded <- tibble(
        conc_mg_per_ml = c(0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.12, 0.16),
        meanAbs = c(0.275, 0.533, 0.764, 1.199, 1.586, 1.932, 2.647, 3.304)
      )


      incProgress(amount = 7 / 10, message = "generate output plots")

      # generate plots
      standard_plot <- ggplot(std_tidy_summary, aes(meanAbs, conc_mg_per_ml)) +
        stat_smooth(
          data = standard_hard_coded, mapping = aes(y = conc_mg_per_ml, x = meanAbs),
          method = "loess",
          se = F,
          size = 5,
          color = "#2E7D32",
          inherit.aes = F, formula = "y ~ x"
        ) +
        stat_smooth(method = "lm", color = "#4285F4", formula = "y ~ x", size = 2, se = F, fullrange = T) +
        stat_smooth(method = "loess", color = "black", formula = "y ~ x", size = 3, se = T, level = 0.95, fill = "grey") +
        theme_minimal(base_size = 18) +
        geom_point(size = 5, alpha = 0.8, color = "darkgrey") +
        geom_errorbar(mapping = aes(xmin = meanAbs - SD_Abs, xmax = meanAbs + SD_Abs), width = 0.005, color = "black") +
        scale_y_continuous(breaks = seq(0, 0.16, length.out = 9)) +
        scale_x_continuous(breaks = seq(0, round(max(standard_hard_coded$meanAbs), digits = 2), length.out = 11)) +
        labs(
          title = "standard curve (loess / linear fit)",
          y = "µg/µl",
          caption = "loess fit & 95% CI in black / linear fit in blue / hard coded std. curve in green"
        ) +
        coord_flip() +
        facet_wrap(~`Plate Number`, nrow = 1) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.background = element_rect(fill = "white", color = "white")
        )

      #* -1 coord flip
      standard_res_plot <- ggplot(residuals_tidy, aes(conc_mg_per_ml, .resid * -1)) +
        geom_hline(yintercept = 0) +
        geom_point(size = 5, alpha = 0.8, color = "darkgrey") +
        scale_x_continuous(breaks = seq(0, 0.16, length.out = 9)) +
        theme_minimal(base_size = 18) +
        labs(title = "loess fit residual plot", subtitle = "closer to 0 is better") +
        facet_wrap(~`Plate Number`) +
        theme(plot.background = element_rect(fill = "white", color = "white"))



      standard_zscore_plot <- ggplot(residuals_tidy, aes(conc_mg_per_ml, z_score * -1, color = z_score_outlier)) +
        geom_hline(yintercept = 0) +
        geom_point(size = 5, alpha = 0.8, color = "darkgrey") +
        geom_hline(yintercept = z_score_threshold, linetype = "dashed") +
        geom_hline(yintercept = -1 * z_score_threshold, linetype = "dashed") +
        scale_color_manual(values = c("TRUE" = "orangered", "FALSE" = "black")) +
        scale_x_continuous(breaks = seq(0, 0.16, length.out = 9)) +
        theme_minimal(base_size = 18) +
        labs(
          title = "z-score plot",
          caption = "z = residuals - mean(residuals)) / sd(residuals)",
          subtitle = "z-score threshold > 3 (= outlier)"
        ) +
        facet_wrap(~`Plate Number`) +
        theme(plot.background = element_rect(fill = "white", color = "white"))








      # calclulate protein conc of samples --------------------------------------
      incProgress(amount = 8 / 10, message = "calculate concentrations")

      # add loess model
      data_samples_summary <- left_join(data_samples_summary, fits %>%
        select(`Plate Number`, loess_fit), by = "Plate Number")
      # add concentrations
      data_samples_summary$conc_µg_per_µl_wo_dil_factor <- NA
      for (i in 1:nrow(data_samples_summary)) {
        data_samples_summary$conc_µg_per_µl_wo_dil_factor[i] <- predict(data_samples_summary$loess_fit[[i]], newdata = data_samples_summary$meanAbs[i])
      }
      # multiply with dilution factor
      data_samples_summary$conc_µg_per_µl <- as.numeric(unlist(data_samples_summary$conc_µg_per_µl_wo_dil_factor)) * as.numeric(unlist(data_samples_summary$dilution_factor))

      # remove fit object
      data_samples_summary <- data_samples_summary %>% select(-loess_fit)


      # add comments ------------------------------------------------------------
      incProgress(amount = 9 / 10, message = "adding comments")
      # <1std. red;<40 yellow; 40-120 green, 120-160 yellow, >160 red
      std_tidy_cutoffs_comment <- std_tidy_summary %>%
        group_by(`Plate Number`) %>%
        summarise(
          std__min = meanAbs[conc_mg_per_ml == 0.01],
          std__max = meanAbs[conc_mg_per_ml == 0.16],
          std__20 = meanAbs[conc_mg_per_ml == 0.02],
          std__120 = meanAbs[conc_mg_per_ml == 0.12]
        )

      # iterate
      data_samples_summary$outside_standard_curve <- FALSE
      data_samples_summary$outside_standard_curve_percentage <- 0
      data_samples_summary$comment <- NA
      for (i in 1:nrow(data_samples_summary)) {
        tmp_meanAbs <- data_samples_summary$meanAbs[i]
        tmp_std_tidy_cutoffs_comment <- std_tidy_cutoffs_comment %>%
          filter(`Plate Number` == data_samples_summary$`Plate Number`[i])

        # Error: Abs below std. curve
        if (tmp_meanAbs < tmp_std_tidy_cutoffs_comment$std__min) {
          data_samples_summary$conc_µg_per_µl[i] <- NA
          data_samples_summary$comment[i] <- "Error: Abs below std. curve"
          data_samples_summary$outside_standard_curve[i] <- TRUE
          data_samples_summary$outside_standard_curve_percentage[i] <- NA
        }
        # Error: Abs. above std. curve
        if (tmp_meanAbs > tmp_std_tidy_cutoffs_comment$std__max) {
          data_samples_summary$conc_µg_per_µl[i] <- NA
          data_samples_summary$comment[i] <- "Error: Abs. above std. curve"
          data_samples_summary$outside_standard_curve[i] <- TRUE
          data_samples_summary$outside_standard_curve_percentage[i] <- (tmp_meanAbs / tmp_std_tidy_cutoffs_comment$std__max * 100) - 100
        }

        # Warning: Abs. in lower range of std. curve
        if (tmp_meanAbs >= tmp_std_tidy_cutoffs_comment$std__min & tmp_meanAbs <= tmp_std_tidy_cutoffs_comment$std__20) {
          data_samples_summary$comment[i] <- "Warning: Abs. in lower range of std. curve"
        }
        # Warning: Abs. in upper range of std. curve
        if (tmp_meanAbs >= tmp_std_tidy_cutoffs_comment$std__120 & tmp_meanAbs <= tmp_std_tidy_cutoffs_comment$std__max) {
          data_samples_summary$comment[i] <- "Warning: Abs. in upper range of std. curve"
        }
        # measurement OK
        if (tmp_meanAbs > tmp_std_tidy_cutoffs_comment$std__20 & tmp_meanAbs < tmp_std_tidy_cutoffs_comment$std__120) {
          data_samples_summary$comment[i] <- "OK"
        }
      } # end of comment iterate


      # data_samples_summary detailed = add linear fit -------

      # add linear model
      data_samples_summary_detailed <- left_join(data_samples_summary, fits %>%
        select(`Plate Number`, linear_fit), by = "Plate Number")
      # add concentrations
      data_samples_summary_detailed$conc_µg_per_µl_wo_dil_factor_LINEAR_fit <- NA
      for (i in 1:nrow(data_samples_summary_detailed)) {
        data_samples_summary_detailed$conc_µg_per_µl_wo_dil_factor_LINEAR_fit[i] <- predict(data_samples_summary_detailed$linear_fit[[i]],
          newdata = new <- data.frame(Abs = unlist(data_samples_summary_detailed$meanAbs[i]))
        )
      }
      # multiply with dilution factor
      data_samples_summary_detailed$conc_µg_per_µl_LINEAR_fit <- as.numeric(unlist(data_samples_summary_detailed$conc_µg_per_µl_wo_dil_factor_LINEAR_fit)) * as.numeric(unlist(data_samples_summary_detailed$dilution_factor))

      # remove fit object
      data_samples_summary_detailed <- data_samples_summary_detailed %>%
        select(-linear_fit)

      # linear fit below 0 = NA
      data_samples_summary_detailed <- data_samples_summary_detailed %>%
        mutate(conc_µg_per_µl_LINEAR_fit = ifelse(conc_µg_per_µl_LINEAR_fit < 0, NA, conc_µg_per_µl_LINEAR_fit))


      # generate labeller names -------------------------------------------------
      dilution_factors <- unique(data_samples_summary$dilution_factor)
      dilution_factors_names <- paste("Dilution: ", dilution_factors, "x", sep = "")
      names(dilution_factors_names) <- dilution_factors
      plates_names <- unique(data_samples_summary$`Plate Number`)
      names(plates_names) <- plates_names
      # measurements plot -------------------------------------------------------

      measurements_plot <- ggplot(data_samples_summary) +
        geom_vline(mapping = aes(xintercept = meanAbs, color = comment), alpha = 0.5) +
        scale_color_manual(values = comments_colors) +
        geom_point(data = std_tidy_summary, mapping = aes(meanAbs, conc_mg_per_ml), size = 5, alpha = 0.8, color = "darkgrey") +
        stat_smooth(
          data = standard_hard_coded, mapping = aes(y = conc_mg_per_ml, x = meanAbs),
          method = "loess",
          se = F,
          size = 5,
          color = "#2E7D32",
          inherit.aes = F, formula = "y ~ x"
        ) +
        stat_smooth(
          data = std_tidy_summary,
          mapping = aes(meanAbs, conc_mg_per_ml),
          method = "loess",
          color = "black", fill = "grey", formula = "y ~ x", size = 2, se = T, level = 0.95
        ) +
        theme_minimal(base_size = 18) +
        labs(
          title = "standard curve (loess / linear fit)", y = "µg/µl",
          caption = "loess fit & 95% CI in black / linear fit in blue"
        ) +
        geom_hline(yintercept = min(std_tidy_summary$conc_mg_per_ml)) +
        geom_hline(yintercept = max(std_tidy_summary$conc_mg_per_ml)) +
        coord_flip() +
        facet_grid(`Plate Number` ~ dilution_factor, labeller = as_labeller(c(dilution_factors_names, plates_names))) +
        theme(
          strip.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        guides(color = "none")


      # density plot ------------------------------------------------------------

      measurements_plot_hist_dens <- ggplot(data_samples_summary) +
        geom_vline(mapping = aes(xintercept = meanAbs, color = comment)) +
        scale_color_manual(values = comments_colors) +
        geom_density(mapping = aes(x = meanAbs)) +
        coord_flip() +
        facet_grid(`Plate Number` ~ dilution_factor,
          scales = "free_x",
          labeller = as_labeller(c(dilution_factors_names, plates_names))
        ) +
        theme_minimal(base_size = 18) +
        theme(
          strip.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        guides(color = "none") +
        geom_vline(
          data = std_tidy_summary %>%
            group_by(`Plate Number`) %>%
            summarise(minAbs = min(meanAbs, na.rm = T)),
          mapping = aes(xintercept = minAbs),
          color = "red",
          linetype = "dashed"
        ) +
        geom_vline(
          data = std_tidy_summary %>%
            group_by(`Plate Number`) %>%
            summarise(maxAbs = max(meanAbs, na.rm = T)),
          mapping = aes(xintercept = maxAbs),
          color = "red",
          linetype = "dashed"
        ) +
        labs(
          title = "Histogram/Density of meanAbs of samples",
          subtitle = "dashed lines = min/max. of standard curve",
          caption = "solid lines: error = red; warning = yellow; OK = green"
        )




      # scatter plot  -----------------------------------------------------------

      max_scatter <- max(c(data_samples_summary_detailed$conc_µg_per_µl, data_samples_summary_detailed$conc_µg_per_µl_LINEAR_fit))

      scatter_conc <- ggplot(data_samples_summary_detailed) +
        stat_smooth(aes(
          x = conc_µg_per_µl,
          y = conc_µg_per_µl_LINEAR_fit
        ), method = "lm", color = "grey", se = F) +
        stat_smooth(aes(
          x = conc_µg_per_µl,
          y = conc_µg_per_µl_LINEAR_fit
        ), method = "loess", color = "black", se = F) +
        coord_cartesian(xlim = c(0, max(max_scatter)), ylim = c(0, max(max_scatter))) +
        theme_minimal(base_size = 18) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        labs(
          color = "% outside standard curve",
          x = "µg/µl (LOESS)", y = "µg/µl (LINEAR)",
          title = "scatter linear and loess calc. conc.",
          caption = "loess fit = black line / linear fit = blue grey"
        ) +
        facet_grid(`Plate Number` ~ dilution_factor, labeller = as_labeller(c(dilution_factors_names, plates_names))) +
        theme(
          strip.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = "white")
        )

      # add color gradient if there are out of std curve measurements
      if (sum(data_samples_summary_detailed$outside_standard_curve) != 0) {
        scatter_conc <- scatter_conc +
          geom_point(
            data = data_samples_summary_detailed %>% filter(outside_standard_curve == F),
            mapping = aes(
              x = conc_µg_per_µl_wo_dil_factor * dilution_factor,
              y = conc_µg_per_µl_wo_dil_factor_LINEAR_fit * dilution_factor,
              shape = outside_standard_curve,
              color = comment
            ),
            size = 5, alpha = 0.5
          ) +
          geom_point(
            data = data_samples_summary_detailed %>% filter(outside_standard_curve == T),
            mapping = aes(
              x = conc_µg_per_µl_wo_dil_factor * dilution_factor,
              y = conc_µg_per_µl_wo_dil_factor_LINEAR_fit * dilution_factor,
              shape = outside_standard_curve,
              color = comment
            ),
            size = 5, alpha = 0.5
          ) +
          scale_color_manual(values = comments_colors, labels = function(x) str_wrap(x, width = 20)) +
          geom_text_repel(
            data = data_samples_summary_detailed %>%
              filter(outside_standard_curve == T) %>%
              top_n(outside_standard_curve_percentage, n = 10),
            mapping = aes(
              x = conc_µg_per_µl_wo_dil_factor * dilution_factor,
              y = conc_µg_per_µl_wo_dil_factor_LINEAR_fit * dilution_factor,
              label = Sample
            ), min.segment.length = 0.001
          )
      } else {
        scatter_conc <- scatter_conc +
          geom_point(
            aes(
              x = conc_µg_per_µl_wo_dil_factor * dilution_factor,
              y = conc_µg_per_µl_wo_dil_factor_LINEAR_fit * dilution_factor,
              shape = outside_standard_curve,
              color = comment
            ),
            size = 5,
            alpha = 0.5
          ) +
          scale_color_manual(values = comments_colors, labels = function(x) str_wrap(x, width = 20))
      }


      # count barchart per diltuion and plate -----------------------------------
      comment_count <- data_samples_summary %>%
        mutate(comment = factor(comment, levels = names(comments_colors))) %>%
        group_by(`Plate Number`, Dilution, comment) %>%
        count(comment, .drop = FALSE)

      comment_count_barchart <- ggplot(
        data = comment_count,
        mapping = aes(
          x = comment,
          y = n,
          fill = comment
        )
      ) +
        geom_bar(stat = "identity") +
        facet_grid(`Plate Number` ~ Dilution,
          labeller = as_labeller(c(dilution_factors_names, plates_names))
        ) +
        scale_fill_manual(values = comments_colors) +
        theme_minimal(base_size = 18) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        labs(
          title = "comment counts",
          y = "count",
          x = ""
        ) +
        guides(fill = "none")


      incProgress(amount = 10 / 10, message = "output")

      # output of calculations
      list(
        standard_plot = standard_plot / std_cv_plot / standard_res_plot / standard_zscore_plot,
        residuals_tidy = residuals_tidy,
        fits_goodness = fits_goodness,
        std_tidy = std_tidy,
        std_tidy_summary = std_tidy_summary,
        data_cv_plot = data_cv_plot,
        data_samples = data_samples,
        dilutions = length(unique(data_samples_summary_detailed$dilution_factor)),
        plates = length(unique(data_samples_summary_detailed$`Plate Number`)),
        data_samples_summary = data_samples_summary,
        data_samples_summary_detailed = data_samples_summary_detailed,
        measurements_plot = measurements_plot / measurements_plot_hist_dens / scatter_conc,
        comment_count_barchart = comment_count_barchart
      )
    }
  )
}) # reactive calculations



# renderPlots and Tables output --------------------------------------------
# input data should be not NULL (NULL == ERROR)
# render CV plot
output$data_cv_plot_out <- renderPlot(calculations()$data_cv_plot)
# render standard plot
output$standard_plot_out <- renderPlot(calculations()$standard_plot)
# render table
output$data_samples_summary_out <- DT::renderDataTable({
  DT::datatable(
    calculations()$data_samples_summary_detailed %>%
      select(Sample, Dilution, meanAbs, SD_Abs, CV_Abs, conc_µg_per_µl, comment),
    filter = "top",
    options = list(
      pageLength = 20,
      autoWidth = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE
    )
  ) %>%
    DT::formatStyle("comment",
      backgroundColor = styleEqual(
        c(names(comments_colors)),
        c(comments_colors)
      )
    ) %>%
    DT::formatRound(c("SD_Abs", "CV_Abs", "conc_µg_per_µl"), 3)
})
# render measurement plot
output$measurements_plot_out <- renderPlot(calculations()$measurements_plot)

# render count barchart
output$comment_count_barchart_out <- renderPlot(calculations()$comment_count_barchart)

# render UI output plot render --------------------------------------------
output$data_cv_plot_out_ui <- renderUI({
  withSpinner(
    ui_element = plotOutput("data_cv_plot_out", height = if_else(calculations()$dilutions == 1, 500, 200 * calculations()$dilutions)),
    type = getOption("spinner.type", default = 8),
    proxy.height = "200px",
    color = getOption("spinner.color", default = "#813D9C")
  )
})

output$standard_plot_out_ui <- renderUI({
  withSpinner(
    ui_element = plotOutput("standard_plot_out", height = 400 * 4, width = if_else(calculations()$plates == 1, 700, 600 * calculations()$plates)),
    type = getOption("spinner.type", default = 8),
    proxy.height = "200px",
    color = getOption("spinner.color", default = "#813D9C")
  )
})
output$measurements_plot_out_ui <- renderUI({
  withSpinner(
    ui_element = plotOutput("measurements_plot_out", width = if_else(calculations()$dilutions == 1, 700, 600 * calculations()$dilutions), height = ifelse(calculations()$plates == 1, 1200, 200 * calculations()$plates * 3)),
    type = getOption("spinner.type", default = 8),
    proxy.height = "200px",
    color = getOption("spinner.color", default = "#813D9C")
  )
})


output$comment_count_barchart_out_ui <- renderUI({
  withSpinner(
    ui_element = plotOutput("comment_count_barchart_out", height = ifelse(calculations()$plates == 1, 600, 100 * calculations()$plates * 3)),
    type = getOption("spinner.type", default = 8),
    proxy.height = "200px",
    color = getOption("spinner.color", default = "#813D9C")
  )
})




# download folder with all tables and plots -------------------------------

output$BCA_download_analysis <- renderUI({
  if (!is.null(calculations()$data_samples_summary_detailed)) {
    tagList(
      p("download processed data:", style = "color:#84B135;margin-left: 5px"),
      downloadButton(
        outputId = "BCA_downloadProcessedData",
        label = "download processed BCA assay data",
        style = "color:#FFFFFF; background-color: #060606; border-color: #84B135; margin-left: 5px;width:100%"
      ),
      helpText("zipped folder is ready for download", style = "margin-left: 10px"),
    )
  }
})



output$BCA_downloadProcessedData <- downloadHandler(
  filename = paste(format(Sys.Date(), "%Y_%m_%d_"), "__", file_name(), "__shinyBCA_output.zip", sep = ""),
  content = function(fname) {
    # tmpdir <- gsub("//", "/", tempdir(), fixed = TRUE)
    # tmpdir <- gsub("\\", "\\\\", tempdir(), fixed = TRUE)
    # setwd(tmpdir)
    # print(tmpdir)

    # shinyalert
    shinyalert("generating download", "This might take while", type = "info", timer = 2000)

    withProgress(
      message = "generate download...",
      style = "notification",
      value = 0.1,
      {
        # generate plots downdload -----

        incProgress(amount = 1 / 5, message = "saving plots")
        # save plots
        ggsave(filename = paste(file_name(), "__data_CV_plot.png", sep = ""), plot = calculations()$data_cv_plot, device = "png", width = 20, height = 5 * calculations()$dilutions, dpi = 150, limitsize = FALSE)
        ggsave(filename = paste(file_name(), "__standard_plot.png", sep = ""), plot = calculations()$standard_plot, device = "png", width = 16, height = 20, dpi = 150, limitsize = FALSE)
        ggsave(filename = paste(file_name(), "__measurements_plot.png", sep = ""), plot = calculations()$measurements_plot, device = "png", width = ifelse(calculations()$dilutions == 1, 10, 5 * calculations()$dilutions), height = (4 * calculations()$plates) * 3, dpi = 150, limitsize = FALSE)



        incProgress(amount = 2 / 5, message = "generate excel output")

        # generate Excel file -----
        # create workbook
        excel_output <- createWorkbook()
        addWorksheet(wb = excel_output, sheetName = "data_samples_summary")
        addWorksheet(wb = excel_output, sheetName = "standard_tidy_summary")
        addWorksheet(wb = excel_output, sheetName = "standard_tidy")
        addWorksheet(wb = excel_output, sheetName = "fit_quality")
        addWorksheet(wb = excel_output, sheetName = "residuals_tidy")
        addWorksheet(wb = excel_output, sheetName = "data_samples")
        addWorksheet(wb = excel_output, sheetName = "data_samples_summary_detailed")

        writeDataTable(
          wb = excel_output,
          sheet = "data_samples_summary",
          x = calculations()$data_samples_summary_detailed %>%
            select(Sample, Dilution, meanAbs, SD_Abs, CV_Abs, conc_µg_per_µl, comment),
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )

        insertImage(wb = excel_output, "data_samples_summary", paste(file_name(), "__measurements_plot.png", sep = ""), startRow = 1, startCol = 9, width = ifelse(calculations()$dilutions == 1, 10, 5 * calculations()$dilutions), height = (4 * calculations()$plates) * 3)



        writeDataTable(
          wb = excel_output,
          sheet = "standard_tidy",
          x = calculations()$std_tidy %>%
            arrange(conc_mg_per_ml) %>%
            select(-Sample, -Dilution, -Replicate),
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )

        writeDataTable(
          wb = excel_output,
          sheet = "standard_tidy_summary",
          x = calculations()$std_tidy_summary,
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )

        insertImage(wb = excel_output, "standard_tidy_summary", paste(file_name(), "__standard_plot.png", sep = ""), startRow = 1, startCol = 6, width = 7 * calculations()$plates, height = 10 * calculations()$dilution)

        writeDataTable(
          wb = excel_output,
          sheet = "residuals_tidy",
          x = calculations()$residuals_tidy,
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )

        writeDataTable(
          wb = excel_output,
          sheet = "fit_quality",
          x = calculations()$fits_goodness,
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )

        writeDataTable(
          wb = excel_output,
          sheet = "data_samples",
          x = calculations()$data_samples,
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )
        writeDataTable(
          wb = excel_output,
          sheet = "data_samples_summary_detailed",
          x = calculations()$data_samples_summary_detailed,
          startRow = 1,
          startCol = 1,
          tableStyle = "TableStyleMedium1"
        )
        incProgress(amount = 3 / 5, message = "save excel output")
        # write data into workbook
        saveWorkbook(
          wb = excel_output,
          overwrite = T,
          file = paste(file_name(), "__shinyBCA_output.xlsx", sep = "")
        )




        incProgress(amount = 4 / 5, message = "generate file list")
        fs <- c(
          paste(file_name(), "__shinyBCA_output.xlsx", sep = ""),
          paste(file_name(), "__data_CV_plot.png", sep = ""),
          paste(file_name(), "__standard_plot.png", sep = ""),
          paste(file_name(), "__measurements_plot.png", sep = "")
        )
        # print
        print(fs)

        incProgress(amount = 5 / 5, message = "zip files")
        # zip files
        zip(zipfile = fname, files = fs)
        # remove temp files
        file.remove(fs)
      }
    )
  }
)
