# global function and dependicies

# load Synergy H1 measurement file function to tidy data ------------------

synergy_tidy <- function(df_in = df){
  na_lines<- which(is.na(df_in$`Plate Number`) & is.na(df_in$Well)& is.na(df_in$`562`))
  if(length(na_lines)!=0){#if no NA line is present e.g. one plate measurement
    rm_lines <- c()
    for(k in 1:length(na_lines)){
      rm_lines <- c(rm_lines,seq(na_lines[k], na_lines[k]+2))
    }
    df_in_tidy <- df_in[-rm_lines,] 
    
  }else{
    df_in_tidy <- df_in
  }
return(df_in_tidy)
}


# meta file check ---------------------------------------------------------

wells96_per_plate_check <- function(meta_input = meta_in){
  meta_tmp<- meta_input %>% 
    group_by(`Plate Number`) %>% 
    summarise(count = n()) %>% 
    mutate(logical_count96_error = ifelse(count<=96,F,T))
  if(sum(meta_tmp$logical_count96_error)!=0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
