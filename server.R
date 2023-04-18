# ____________________________________________________________________#
## Script: shiny app: Mass Spec Preppy
## Author: Stephan Michalik
## part: server.R
## Notes:
# ____________________________________________________________________#

# define some credentials
credentials <- data.frame(
  user = c("admin","reviewer"), # mandatory
  password = c("Protein!112", "Msp!34cvhvdo385461"), # mandatory
  expire = c(NA,NA),
  admin = c(TRUE,FALSE),
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

  # generate OT2_template download -------------------------------------------
  source("server/Server__sample_digest_OT2_generate_protocol_download.R", local = TRUE)$value

  # static template download ------------------------------------------------
  source("server/Server__static_template_downloads.R", local = TRUE)$value

  # BCA assay server --------------------------------------------------------
  source("server/Server__BCA_assay.R", local = TRUE)$value
})
