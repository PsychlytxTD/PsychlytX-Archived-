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

    actionButton(ns('submit_button'), 'Submit Results'),
    br(),
    br(),
    titlePanel(span(tagList(icon("file-pdf-o", lib = "font-awesome")), h4(tags$b("Download a Pdf Report of Results for this Patient")))),
    downloadButton(ns('download_button'), 'Download Report')

  )


}

#' Data Submission & Report Generation Widgets
#'
#' Creates a submit action button and a report download button to submit data to a database and download a pdf report.
#'
#' @param current_data Item scores and date
#'
#' @param mean_output_list_1 Mean widgets output
#'
#' @export

#The 'current_data' list (containing item scores and date) is accessed by being specified as an argument

download<- function(input, output, session, all_input) {

  #The all_input object is reactive so refer to it using all_input()

  #Create a tibble from the elements of each subscale list

  #At some point, need to find a way to automatically include certain variables in this tibble
  #based on information gained at clinician login: e.g. patient name, patient id, clinician name,
  #clinician id

 reactive({

    all_input() %>% {

    tibble::tibble(
      date = purrr::map_chr(., "date"),
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
      cutoff_references = purrr::map(., "cutoff_reference)"),
      confidence = purrr::map_dbl(., "confidence"),
      method = purrr::map_chr(., "method")

    )    #After this, we can use mutate() to make columns for pts, se, ci, ci_upper, ci_lower,
      #then rearrange the order of columns as desired using select()

    } %>% dplyr::mutate(

      pts = psychlytx::make_pts(score, mean, reliability, method),

      se = psychlytx::make_se(sd, reliability, method),

      ci = psychlytx::make_ci(confidence, se),

      ci_upper = pts + ci,

      ci_lower = pts - ci

    ) %>% dplyr::select(date, score, pts, se, ci, ci_upper, ci_lower, everything())


  })


}








