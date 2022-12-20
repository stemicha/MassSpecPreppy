# render sample digest static excel input template --------------------------------------

output$input_template_OT2 <- downloadHandler(
  filename="Mass_Spec_Preppy_sample_meta_template.xlsx",  # desired file name on client 
  content=function(con) {
    file.copy("www/Mass_Spec_Preppy_sample_meta_template.xlsx", con)
  }
)

# render BCA static excel input template --------------------------------------

output$input_BCA_template_OT2 <- downloadHandler(
  filename="BCA_assay_OT2_96well_half_area_template.xlsx",  # desired file name on client 
  content=function(con) {
    file.copy("www/BCA_assay_OT2_96well_half_area_template.xlsx", con)
  }
) 