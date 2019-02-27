
#The all_input object is reactive so refer to it using all_input()

#Create a tibble from the list of lists

all_input() %>% {
  tibble(
    date = map_chr(., "date"),
    score = map_dbl(., "score"),
    mean = map_dbl(., "mean_value"),       
    mean_reference = map_chr(., "mean_reference"),
    sd = map_dbl(., "sd_value"),
    sd_reference = map_chr(., "sd_reference"),
    reliability = map_dbl(., "reliability"),
    reliability_reference = map_chr(., "reliability_reference"),
    cutoff_labels = map(., "cutoff_labels") #Cutoff columns will be list columns, since we used 
                                            #map(). Needed b/c require multiple vals per cell
    cutoff_values = map(., "cutoff_value"),
    cutoff_references = map(., "cutoff_values_reference)")
  )
}

