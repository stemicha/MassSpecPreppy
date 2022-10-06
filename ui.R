#____________________________________________________________________#
## Script: shiny app: Mass Spec Preppy
## Author: Stephan Michalik
## part: ui.R
## Notes:
#____________________________________________________________________#

library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyWidgets)
library(bslib)
library(thematic)
library(patchwork)
library(readxl)
library(shinymanager)
library(tidyverse)
library(ggimage)
library(shinycssloaders)
source("www/helper_functions.R")

#shiny options
shinyOptions(plot.autocolors = TRUE)


#theming
shinyOptions(bslib = TRUE)
bs_global_theme(bg = "#002B36", fg = "#FFFFFF",primary = "#509DD1")
thematic_shiny()


#custom css for login page
css <- HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #649CCC;
                  border-color: #649CCC;
              }
              .panel-primary {
                  border-color: #649CCC;
              }")

#add app login
secure_app(choose_language = FALSE,
           theme = shinythemes::shinytheme("cyborg"),
           tags_top = tags$div(
             tags$head(tags$style(css)),
             tags$img(
               src = "Mass_Spec_Preppy_hexbin_small.png", width = 200, height = 229, alt="Logo not found", deleteFile=FALSE
             )),
# define page layout of app
fluidPage(
  theme = shinytheme("cyborg"),
      # Application title
    titlePanel(img(src="Mass_Spec_Preppy_logo.png", height = 200, align = "center")),
    # Sidebar with a slider input for number of bins
    sidebarLayout(#
        sidebarPanel(width = 2,
            #file upload
            fileInput(inputId = "input_sample_file",label = "input sample file",accept = ".xlsx"),
            tags$p("sample conc. should be approx. 0.5-2µg/µl", style = "color:#84B135; align:center; margin-left: 15px"),
            downloadButton(outputId = "input_template_OT2",
                           label = "OT-2 template download",
                           style="color:#fff; background-color: #333333; border-color: #212121; margin-left: 5px;width:100%"),
            br(),
            tags$p("(96 samples - 4 slots - 24 samples per slot)", style = "color:grey; align:center; margin-left: 15px"),
            hr(),
            sliderInput(inputId = "input_sample_amount",label = "sample amount to digest (µg)",value = 4,min = 1,max = 10,step = 1),
            #decide enzyme mix or sequential
            hr(),
            awesomeRadio(inputId = "enzyme_or_mix",status = "danger",
                         label = tags$p("trypsin/LysC sequential or Mix ?", style = "color:white; align:center; margin-left: 5px"),
                         choices = c("Trypsin/LysC Mix", "Trypsin/LysC sequential"),
                         selected = "Trypsin/LysC Mix"),
            hr(),
              #Trypsin/LysC mix digest
              conditionalPanel(condition = "input.enzyme_or_mix == 'Trypsin/LysC Mix'",
                               awesomeRadio(inputId = "Trypsin_LysC_Mix_ratio",
                                            label = "trypsin/LysC Mix ratio (e.g. 1:50 = 50)",
                                            choices = c(25,50,100),selected = 25),
                               uiOutput("Trypsin_LysC_Mix_stock_slider_ui"),
                               uiOutput("Trypsin_LysC_Mix_stock_suggestion_ui")
              ),
              #sequential enzyme digest
              conditionalPanel(condition = "input.enzyme_or_mix == 'Trypsin/LysC sequential'",
                               awesomeRadio(inputId = "trypsin_ratio",
                                            label = "trypsin ratio (e.g. 1:50 = 50)",
                                            choices = c(25,50,100),
                                            selected = 50),
                               uiOutput("trypsin_stock_slider_ui"),
                               uiOutput("trypsin_stock_suggestion_ui"),
                               awesomeRadio(inputId = "LysC_ratio",
                                            label = "LysC ratio (e.g. 1:100 = 100)",
                                            choices = c(25,50,100),
                                            selected = 100),
                               uiOutput("LysC_stock_slider_ui"),
                               uiOutput("LysC_stock_suggestion_ui")
              ),
            # decide reduction alkylation
            awesomeRadio(inputId = "logical_red_alk",
                         label = "reduction & alkylation",
                         choices = c(TRUE,FALSE),
                         selected = TRUE),
            # decide EvoTips or vials
            awesomeRadio(inputId = "EvoTips_vials",
                         label = "peptides in Evotips or Vials",
                         choices = c("EvoTips","Vial"),
                         selected = "Vial"),
            uiOutput("evosep_amount_ui"),
            # processing button
            actionButton(inputId = "inputButton_generate_OT2_template",
                         label = "processing...generate template",
                         width = "100%",
                         icon=icon("robot"),
                         style="color:#fff; background-color: #4B9449; border-color: #53A551; margin-left: 5px;width:100%"),
            uiOutput("download_OT2_template")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Step 1 deck layout",
                               withSpinner(plotOutput("step1_plot",height = 1000,width = 950),color = "#649CCC",type = 5)),
                      tabPanel("Step 2 deck layout",
                               withSpinner(plotOutput("step2_plot",height = 1000,width = 950),color = "#649CCC",type = 5)),
                      tabPanel("Step 3 deck layout",
                               withSpinner(plotOutput("step3_plot",height = 1000,width = 950),color = "#649CCC",type = 5)),
                      tabPanel("how to ...",
                               includeHTML("how_to.html"))
          )

          )
        )
    )

)#end secure app / shinymanager