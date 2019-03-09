#' Data Submission & Report Generation Widgets
#'
#' Creates a submit action button and a report download button to submit data to a database and download a pdf report.
#'
#' @param id String to create a unique namespace.
#'
#' @export

download_UI<- function(id) {

  ns<- NS(id)

  tagList(

    br(),

    titlePanel(span(tagList(icon("file-pdf-o", lib = "font-awesome")), h4(tags$b("Download Clinical Report")))),
    downloadButton(ns('download_button'), 'Download Report')

  )


}

#' Data Submission & Report Generation Widgets
#'
#' Creates a submit action button and a report download button to submit data to a database and download a pdf report.
#'
#' @param all_input A list of sublists containing widget input (one sublist per subscale)
#'
#' @export


download<- function(input, output, session, all_input) {

  #The all_input object is reactive so refer to it using all_input(). It contains list of input lists (one per subscale)

  #Create a tibble from the elements of each subscale list

  #At some point, need to find a way to automatically include certain variables in this tibble
  #based on information gained at clinician login: e.g. patient name, patient id, clinician name,
  #clinician id

 observe({

   print(

    all_input() %>% {

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


   )

  })


}








