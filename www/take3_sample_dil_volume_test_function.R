take3_sample_dil_volume_fct<- function(dilution = 30){
  #function is written for lowest sample volume to use for the assay / 1µl
  
  # minimal sample dilution possible
  min_vol = 25
  # minimal sample volume
  min_sample_vol = 1
  
  if(min_sample_vol*dilution<min_vol){
    buffer_volume <-  min_vol-(min_vol/dilution)
    sample_volume <- min_vol/dilution
    #paste0(sample_volume,"µl + ",buffer_volume,"µl")
    
  }else{
    buffer_volume <- (min_sample_vol*dilution)-1
    sample_volume <- min_sample_vol
    #paste0(sample_volume,"µl + ",buffer_volume,"µl")
  }
  
  return(list(buffer_volume = buffer_volume,sample_volume=sample_volume))
  
}

#test function
take3_sample_dil_volume_fct(3)
