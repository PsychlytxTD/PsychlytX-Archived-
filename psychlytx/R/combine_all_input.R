#' Combine All Input
#'
#' Combine all lists of widget input values
#'
#' @param id A string to create the namespace

combine_all_input_UI<- function(id) {

  ns<- NS(id)

  return(NULL)

}




#' Combine All Input
#'
#' Combine all lists of widget input values
#'
#' @param input_list A list of input lists (one for each subscale)
#'
#' @export


combine_all_input<- function(input, output, session, input_list) {

  reactive({


    all_input<- input_list() %>% purrr::map( ~ purrr::flatten(.x) %>% purrr::set_names(c("date", "score", "mean_value",
          "mean_reference","sd_value", "sd_reference", "reliability_value", "reliability_reference", "cutoff_label",
           "cutoff_value", "cutoff_reference", "confidence", "method")) )


    scale_data<- all_input %>% {

      tibble::tibble(
        date = format(lubridate::as_date(purrr::map_dbl(., "date")), "%d/%m/%Y"), #Convert to date class
        score = purrr::map_dbl(., "score"),
        mean = purrr::map_dbl(., "mean_value"),
        mean_reference = purrr::map_chr(., "mean_reference"),
        sd = purrr::map_dbl(., "sd_value"),
        sd_reference = purrr::map_chr(., "sd_reference"),
        reliability = purrr::map_dbl(., "reliability_value"),
        reliability_reference = purrr::map_chr(., "reliability_reference"),
        cutoff_labels = purrr::map(., "cutoff_label"), #Cutoff columns will be list columns, since we used
        #map(). Needed b/c require multiple vals per cell
        cutoff_values = purrr::map(., "cutoff_value"),
        cutoff_references = purrr::map(., "cutoff_reference"),
        confidence = purrr::map_dbl(., "confidence"),
        method = purrr::map_chr(., "method")

      )    #After this, we can use mutate() to make columns for pts, se, ci, ci_upper, ci_lower,
      #then rearrange the order of columns as desired using select()

    } %>% dplyr::mutate(

      pts = switch(method[1], # Check the first row of the method variable (should be the same for all rows (i.e. all subscales))
                   # Based on reliable change method used, pts and se are calculated differently.

                   "Nunnally & Bernstein (1994)" = (reliability * score) + (mean * (1 - reliability)),
                   "Jacobson & Truax (1991)" = score

      ),

      se = switch(method[1],

                  "Nunnally & Bernstein (1994)" = sd * sqrt(1 - reliability ^ 2),
                  "Jacobson & Truax (1991)" = sqrt((2 * (sd ^ 2)) * (1 - reliability))

      ),

      ci = confidence * se, #Calculate confidence intervals

      ci_upper = pts + ci,

      ci_lower = pts - ci


    ) %>% dplyr::mutate_if(is.numeric, round, 2) %>% dplyr::select(date, score, pts, se, ci, ci_upper, ci_lower, everything())







      #Create a separate dataframe with cutoff score data - which is in a longer format.


      # Select the cutoff score data from scale_data, unnest it and arrange it in ascending order by value to facilitate easy graphing later.

      cutoff_data<- scale_data %>% select(cutoff_values, cutoff_labels, cutoff_references) %>% tidyr::unnest() %>% dplyr::arrange(cutoff_values)


      #Add column names to allow spreading (we will spread() into these column names)


       cutoff_data$cutoff_label_colnames<- c("cutoff_label_1", "cutoff_label_2", "cutoff_label_3",
                                   "cutoff_label_4", "cutoff_label_5")

       cutoff_data$cutoff_value_colnames<- c("cutoff_value_1", "cutoff_value_2", "cutoff_value_3",
                                   "cutoff_value_4", "cutoff_value_5")

       cutoff_data$cutoff_reference_colnames<- c("cutoff_reference_1", "cutoff_reference_2",
                                       "cutoff_reference_3", "cutoff_reference_4",
                                       "cutoff_reference_5")

       #Spread seperately to add columns for cutoff labels, cutoff values and cutoff references

       df_cutoff_label<- cutoff_data %>% dplyr::select(cutoff_label_colnames, cutoff_labels) %>% tidyr::spread(cutoff_label_colnames, cutoff_labels)

       df_cutoff_value<- cutoff_data %>% dplyr::select(cutoff_value_colnames, cutoff_values) %>% tidyr::spread(cutoff_value_colnames, cutoff_values)

       df_cutoff_reference<- cutoff_data %>% dplyr::select(cutoff_reference_colnames, cutoff_references) %>% tidyr::spread(cutoff_reference_colnames, cutoff_references)

       #Combine the separate wide dataframes for cutoff labels, cutoff values and cutoff references to form the final cutoff data df that will be sent to the db.

       cutoff_data<- dplyr::bind_cols(df_cutoff_label, df_cutoff_value, df_cutoff_reference)


       return(list(scale_data, cutoff_data))


    })


}
