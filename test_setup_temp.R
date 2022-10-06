input <- list(enzyme_or_mix="Trypsin/LysC Mix",
              input_sample_amount = 4,
              trypsin_ratio=50,
              trypsin_stock_conc=80,
              LysC_ratio=100,
              LysC_stock_conc=20,
              EvoTips_amount=400,
              Trypsin_LysC_Mix_stock_conc=80,
              Trypsin_LysC_Mix_ratio=25)


OT2_template <- read_excel(
  "www/DEMO_MSpreppy_working.xlsx",
  skip = 0,sheet = 1
) 
#OT2_template <- read_excel("www/DEMO_MSpreppy.xlsx")


#remove rows without sample name
OT2_template <- OT2_template %>% filter(!is.na(sample))

input_amount = input$input_sample_amount

OT2_template_tmp <- OT2_template %>% rowwise() %>%  mutate(volume_needed = input_amount/`protein concentration (µg/µl)`)



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

#add sample amount
input_JSON <- paste(input_JSON,"\"",",","\"sample_amount\"",":","\"",input$input_sample_amount,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"trypsin_ratio\"",":","\"",input$trypsin_ratio,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"trypsin_stock_concentration\"",":","\"",input$trypsin_stock_conc,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"LysC_ratio\"",":","\"",input$LysC_ratio,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"LysC_stock_concentration\"",":","\"",input$LysC_stock_conc,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"EvoTips_amount_ng\"",":","\"",input$EvoTips_amount,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"LysC_Trypsin_Mix_stock_concentration\"",":","\"",input$Trypsin_LysC_Mix_stock_conc,sep="")
input_JSON <- paste(input_JSON,"\"",",","\"LysC_Trypsin_Mix_ratio\"",":","\"",input$Trypsin_LysC_Mix_ratio,"\"",sep="")
input_JSON <- paste(input_JSON,"}\"\"\")",sep="")



## generate protocol output  
# part 1
OT2_protocol_part1_out <- OT2_protocol_part1
OT2_protocol_part1_out[16] <- input_JSON

# part 1 w/o alk red
OT2_protocol_part1_wo_alk_red_out <- OT2_protocol_part1_wo_alk_red
OT2_protocol_part1_wo_alk_red_out[16] <- input_JSON

# part 2 (enzyme sequential)
OT2_protocol_part2_sequential_out <- OT2_protocol_part2_sequential
OT2_protocol_part2_sequential_out[16] <- input_JSON

# part 2 (enzyme Mix)
OT2_protocol_part2_mix_out <- OT2_protocol_part2_mix
OT2_protocol_part2_mix_out[16] <- input_JSON

# part 3 MSvial
OT2_protocol_part3_vial_out <- OT2_protocol_part3_vial
OT2_protocol_part3_vial_out[16] <- input_JSON

# part 3 EvoTip
OT2_protocol_part3_EvoTip_out <- OT2_protocol_part3_EvoTip
OT2_protocol_part3_EvoTip_out[16] <- input_JSON

# part 4 EvoTip
OT2_protocol_part4_post_EvoTip_out <- OT2_protocol_part4_post_EvoTip
OT2_protocol_part4_post_EvoTip_out[16] <- input_JSON

#generate file name
inFile <- input$input_sample_file
file_out_tmp <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")




writeLines(text = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                         yes = OT2_protocol_part2_mix_out,
                         no = OT2_protocol_part2_sequential_out),
           con = ifelse(test = input$enzyme_or_mix=="Trypsin/LysC Mix",
                        yes = "tmp1.py",
                        no = "tmp2.py"))


OT2_template_generation <- function(){
  list(number_of_samples = dim(OT2_template)[1],
       input_sample_list = OT2_template,
       OT2_protocol_part1_out = OT2_protocol_part1_out,
       OT2_protocol_part2_sequential_out = OT2_protocol_part2_sequential_out,
       OT2_protocol_part2_mix_out = OT2_protocol_part2_mix_out,
       OT2_protocol_part3_vial_out = OT2_protocol_part3_vial_out,
       OT2_protocol_part3_EvoTip_out = OT2_protocol_part3_EvoTip_out,
       OT2_protocol_part1_wo_alk_red_out = OT2_protocol_part1_wo_alk_red_out,
       OT2_protocol_part4_post_EvoTip_out = OT2_protocol_part4_post_EvoTip_out,
       file_name = paste(str_replace_all(file_out_tmp,".xlsx",""),sep=""),
       file_output_part1 = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep.py",sep=""),
       file_output_part1_wo_alk_red = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_1_sample_prep__without_red_alk.py",sep=""),
       file_output_part2_sequential = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_sequential.py",sep=""),
       file_output_part2_mix = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_2_SP3_enzymes_mix.py",sep=""),
       file_output_part3_vial_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_3_MSvial_elution.py",sep=""),
       file_output_part3_EvoTip_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_3_EvoTip_loading.py",sep=""),
       file_output_part4_post_EvoTip_out = paste(str_replace_all(file_out_tmp,".xlsx",""),"__OT2_Mass_Spec_Preppy_protocol_part_4_post_EvoTip_loading.py",sep="")
  )
  
}

writeLines(text = OT2_template_generation()$OT2_protocol_part2_mix_out,con = OT2_template_generation()$file_output_part2_mix)

