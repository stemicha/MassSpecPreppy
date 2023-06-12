# ____________________________________________________________________#
## Script: shiny app: Mass Spec Preppy
## Author: Stephan Michalik
## part: ui.R
## Notes:
# ____________________________________________________________________#

library(tidyverse)
library(patchwork)
library(ggrepel)
library(readxl)
library(openxlsx)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(broom)
library(DT)
library(shinycssloaders)
library(knitr)
library(htmltools)
library(markdown)
library(magrittr)
library(shinymanager)
library(ggimage)
library(bslib)
library(thematic)
library(quarto)
source("www/helper_functions_BCA.R")
source("www/helper_functions_sample_digest.R")


# shiny options -----------------------------------------------------------

shinyOptions(plot.autocolors = TRUE)


# theming -----------------------------------------------------------------
shinyOptions(bslib = TRUE)
bs_global_theme(bg = "#002B36", fg = "#FFFFFF", primary = "#509DD1")
thematic_shiny()


# custom css for login page -----------------------------------------------
css <- HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #649CCC;
                  border-color: #649CCC;
              }
              .panel-primary {
                  border-color: #649CCC;
              }")

tags$head(tags$link(
  rel = "shortcut icon",
  href = "www/Mass_Spec_Preppy_hexbin_favicon.png"
))

# add app login -----------------------------------------------------------
secure_app(
  choose_language = FALSE,
  theme = shinythemes::shinytheme("cyborg"),
  tags_top = tags$div(
    tags$head(tags$style(css)),
    tags$img(
      src = "Mass_Spec_Preppy_hexbin_small.png", width = 200, height = 229, alt = "Logo not found", deleteFile = FALSE
    )
  ),
  # define page layout of app
  fluidPage(
    theme = shinytheme("cyborg"),
    # Application title
    titlePanel(title = img(src = "Mass_Spec_Preppy_logo.png", height = 100, align = "center"),windowTitle = "MassSpecPreppy"),
    # Sidebar with a slider input for number of bins
    sidebarLayout( #
      sidebarPanel(
        width = 3,
        # BCA or digest
        radioGroupButtons(
          inputId = "BCA_sample_digest_selection",
          label = p("BCA assay or Sample digest?"),
          choices = c("BCA assay", "Sample digest"),
          selected = "BCA assay",
          checkIcon = list(yes = icon("check")),
          width = "100%",
          justified = T
        ),
        # colorize radioGroupButtons
        tags$script("$(\"input:radio[name='BCA_sample_digest_selection'][value='BCA assay']\").parent().css('background-color', '#521253');"),
        tags$script("$(\"input:radio[name='BCA_sample_digest_selection'][value='Sample digest']\").parent().css('background-color', '#2A5D90');"),
        hr(),

        # sample digest UI --------------------------------------------------------

        conditionalPanel(
          condition = "input.BCA_sample_digest_selection == 'Sample digest'",
          # file upload
          br(),
          fileInput(inputId = "input_sample_file", label = "input sample file", accept = ".xlsx"),
          tags$h5("sample conc. must be between 0.5 - 2µg/µl", style = "color:#C95C54; font-weight: bold; align:center; margin-left: 15px"),
          downloadButton(
            outputId = "input_template_OT2",
            label = "OT-2 template download",
            style = "color:#fff; background-color: #333333; border-color: #2A5D90; margin-left: 5px;width:100%"
          ),
          bsPopover(
            id = "input_template_OT2",
            title = NULL,
            content = "download sample digest OT-2 template file",
            trigger = "hover",
            options = NULL
          ),
          br(),
          tags$p("( max. 96 samples - 4 slots - 24 samples per slot)", style = "color:grey; align:center; margin-left: 15px"),
          hr(),
          sliderInput(inputId = "input_sample_amount", label = "sample amount to digest (µg)", value = 4, min = 1, max = 10, step = 1),
          # decide enzyme mix or sequential
          hr(),
          awesomeRadio(
            inputId = "enzyme_or_mix", status = "danger",
            label = tags$p("trypsin/LysC sequential or Mix ?", style = "color:white; align:center; margin-left: 5px"),
            choices = c("Trypsin/LysC Mix", "Trypsin/LysC sequential"),
            selected = "Trypsin/LysC Mix"
          ),
          hr(),
          # Trypsin/LysC mix digest
          conditionalPanel(
            condition = "input.enzyme_or_mix == 'Trypsin/LysC Mix'",
            awesomeRadio(
              inputId = "Trypsin_LysC_Mix_ratio",
              label = "trypsin/LysC Mix ratio (e.g. 1:50 = 50)",
              choices = c(25, 50, 100), selected = 25
            ),
            uiOutput("Trypsin_LysC_Mix_stock_slider_ui"),
            uiOutput("Trypsin_LysC_Mix_stock_suggestion_ui")
          ),
          # sequential enzyme digest
          conditionalPanel(
            condition = "input.enzyme_or_mix == 'Trypsin/LysC sequential'",
            awesomeRadio(
              inputId = "trypsin_ratio",
              label = "trypsin ratio (e.g. 1:50 = 50)",
              choices = c(25, 50, 100),
              selected = 50
            ),
            uiOutput("trypsin_stock_slider_ui"),
            uiOutput("trypsin_stock_suggestion_ui"),
            awesomeRadio(
              inputId = "LysC_ratio",
              label = "LysC ratio (e.g. 1:100 = 100)",
              choices = c(25, 50, 100),
              selected = 100
            ),
            uiOutput("LysC_stock_slider_ui"),
            uiOutput("LysC_stock_suggestion_ui")
          ),
          # decide reduction alkylation
          awesomeRadio(
            inputId = "logical_red_alk",
            label = "reduction & alkylation",
            choices = c("yes", "no"),
            selected = "yes"
          ),
          # decide EvoTips or vials
          awesomeRadio(
            inputId = "EvoTips_vials",
            label = "peptides in Evotips or Vials",
            choices = c("EvoTips", "Vial"),
            selected = "Vial"
          ),
          uiOutput("evosep_amount_ui"),
          # processing button
          actionButton(
            inputId = "inputButton_generate_OT2_template",
            label = "processing...generate template",
            width = "100%",
            icon = icon("robot"),
            style = "color:#fff; background-color: #4B9449; border-color: #53A551; margin-left: 5px;width:100%"
          ),
          uiOutput("download_OT2_template")
        ),

        # BCA assay ui ------------------------------------------------------------
        conditionalPanel(
          condition = "input.BCA_sample_digest_selection == 'BCA assay'",
          
          wellPanel(
            h4("BCA assay on OT-2:"),
            style = "background-color: #0D0D0D;",
          # BCA OT-2 selection
          fileInput(
            inputId = "BCA_OT2_template_upload_file",
            label = p(
              span("upload OT-2 sample file"),
              span(icon("fa-light fa-circle-info"), id = "OT2_template_selection_icon", style = "color: white")
            ),
            accept = c("Excel", ".xlsx"),
            multiple = FALSE
          ),
          bsPopover(
            id = "OT2_template_selection_icon",
            title = NULL,
            content = "please provide the filled out OT-2 template file (max. 40 samples!!!)",
            trigger = "hover",
            options = NULL
          ),
          tags$h5("the smallest possible dilution is 5x", style = "color:#C95C54; font-weight: bold; align:center; margin-left: 15px"),
          downloadButton(
            outputId = "input_BCA_template_OT2",
            label = "BCA OT-2 template download",
            style = "color:#fff; background-color: #333333; border-color: #212121; margin-left: 5px;width:100%"
          ),
          br(),
          br(),
          actionButton(
            inputId = "inputButton_generate_BCA_OT2_template",
            label = "generate OT-2 protocol",
            width = "100%",
            icon = icon("table"),
            style = "color:#fff; background-color: #4B9449; border-color: #53A551; margin-left: 5px;width:100%"
          ),
          bsPopover(
            id = "inputButton_generate_BCA_OT2_template",
            title = NULL,
            content = "be sure you uploaded the modified OT-2 template before processing ...",
            trigger = "hover",
            options = NULL
          ),
          uiOutput("download_BCA_OT2_template"),
          hr()
          ),


          # BCA analysis -----------------------------------------------------------
          
          wellPanel(
            h4("analysis of measured data:"),
            style = "background-color: #0D0D0D;",
            # measured values file upload -------------------------------------------
            fileInput("BCA_xlsx_raw_96well",
                      accept = c("Excel", ".xlsx"),
                      label = p("upload measured BCA data (96well)"),
                      multiple = FALSE
            ),
            # meta file upload --------------------------------------------------------
            fileInput(
              inputId = "BCA_xlsx_meta",
              label = p(
                span("upload BCA assay meta data file"),
                span(icon("fa-light fa-circle-info"), id = "xlsx_meta_icon", style = "color: white")
              ),
              accept = c("Excel", ".xlsx"),
              multiple = FALSE
            ),
            bsPopover(
              id = "xlsx_meta_icon",
              title = NULL,
              content = "be sure you have used the meta file generated by the app before",
              placement = "bottom",
              trigger = "hover",
              options = NULL
            ),
            
            # action button -----------------------------------------------------------
            actionButton(
              inputId = "BCA_inputButton_data_processing",
              label = "RUN data processing...",
              width = "100%",
              icon = icon("youtube"),
              style = "color:#fff; background-color: #521253; border-color: #591C5D; margin-left: 5px;width:100%"
            ),
            bsPopover(
              id = "BCA_inputButton_data_processing",
              title = NULL,
              content = "be sure you have uploaded mesurement and meta file !!!",
              placement = "bottom",
              trigger = "hover",
              options = NULL
            ),
            br(),
            # BCA analysis download
            uiOutput("BCA_download_analysis")
          )

          
        ),


        
        # version  ----------------------------------------------------------------
        hr(),
        fluidRow(
          column(6,      
                 tags$a(icon("book"), href="manual/index.html",target="_blank",style = "font-size: 40px;")
                 ),
          column(6,
                 p("MassSpecPreppy version 1.0.0",style = "text-align: right")
                 )
          )
        
      ), # end sidebar panel
      # main panel --------------------------------------------------------------

      mainPanel(

        # sample digest output ----------------------------------------------------
        conditionalPanel(
          "input.BCA_sample_digest_selection == 'Sample digest'",
          tabsetPanel(
            type = "tabs",
            selected = "Pipetted sample volume with settings",
            tabPanel(
              "Pipetted sample volume with settings",
              withSpinner(uiOutput("volume_needed_plot"), color = "#649CCC", type = 5)
            ),
            tabPanel(
              "Samples data table",
              # format data table control color and hover highlight
              tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #0885C8 !important;
                                  }
                                  "))),
              tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                        color: #FFFFFF !important;
                                  }")),
              verticalLayout(
                fluidRow(column(align = "center", width = 11, DT::DTOutput("volume_needed_table")))
              )
            ),
            tabPanel(
              "Step 1 deck layout",
              withSpinner(plotOutput("step1_plot", height = 1000, width = 950), color = "#649CCC", type = 5)
            ),
            tabPanel(
              "Step 2 deck layout",
              withSpinner(plotOutput("step2_plot", height = 1000, width = 950), color = "#649CCC", type = 5)
            ),
            tabPanel(
              "Step 3 deck layout",
              withSpinner(plotOutput("step3_plot", height = 1000, width = 950), color = "#649CCC", type = 5)
            ),
            tabPanel(
              "how to ...",
              fluidRow(withMathJax(includeMarkdown("www/how_to_Mass_Spec_Preppy.md")))
              
            ),
            tabPanel(
              "disclaimer",
              fluidRow(withMathJax(includeMarkdown("www/disclaimer.md")))
              
            )
          )
        ),

        # BCA assay output --------------------------------------------------------
        conditionalPanel(
          "input.BCA_sample_digest_selection == 'BCA assay'",
          tabsetPanel(
            id = "BCA_assay_tabset",
            type = "tabs",
            selected = "CV plot over samples",
            tabPanel(
              "CV plot over samples",
              uiOutput("data_cv_plot_out_ui")
            ),
            tabPanel(
              "Standard plot",
              uiOutput("standard_plot_out_ui")
            ),
            tabPanel(
              "Samples data table",
              # format data table control color and hover highlight
              tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #0885C8 !important;
                                  }
                                  "))),
              tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                        color: #FFFFFF !important;
                                  }")),
              verticalLayout(
                fluidRow(column(align = "center", width = 9, DT::DTOutput("data_samples_summary_out")))
              )
            ),
            tabPanel(
              "Samples measurements",
              uiOutput("measurements_plot_out_ui")
            ),
            tabPanel(
              "comment counts",
              uiOutput("comment_count_barchart_out_ui")
            ),
            tabPanel(
              "BCA assay OT-2 layout",
              withSpinner(plotOutput("BCA_layout_plot", height = 1000, width = 950), color = "#649CCC", type = 5)
            ),
            tabPanel(
              "how to ...",
              fluidRow(withMathJax(includeMarkdown("www/BCA_description.md")))
            ),
            tabPanel(
              "disclaimer",
              fluidRow(withMathJax(includeMarkdown("www/disclaimer.md")))
              
            )
          )
        )
      ) # end main panel
    )
  )
) # end secure app / shinymanager
