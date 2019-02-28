
#The all_input object is reactive so refer to it using all_input()

#Create a tibble from the elements of each subscale list

#At some point, need to find a way to automatically include certain variables in this tibble
#based on information gained at clinician login: e.g. patient name, patient id, clinician name,
#clinician id

scale_data<- all_input() %>% {
  
  tibble(
    date = map_chr(., "date"),
    score = map_dbl(., "score"),
    mean = map_dbl(., "mean_value"),       
    mean_reference = map_chr(., "mean_reference"),
    sd = map_dbl(., "sd_value"),
    sd_reference = map_chr(., "sd_reference"),
    reliability = map_dbl(., "reliability"),
    reliability_reference = map_chr(., "reliability_reference"),
    cutoff_labels = map(., "cutoff_labels"), #Cutoff columns will be list columns, since we used 
                                            #map(). Needed b/c require multiple vals per cell
    cutoff_values = map(., "cutoff_value"),
    cutoff_references = map(., "cutoff_values_reference)")
    
  )
  
}

#After this, we can use mutate() to make columns for pts, se, ci, ci_upper, ci_lower,
#then rearrange the order of columns as desired using select()