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

download<- function(input, output, session, current_data, mean_input_1) {


  #Store widget values (i.e. numeric value(s) and reference(s))

  params_output_1<- reactive({

    #Make sure date has date class

    date_1<- format(as.Date(current_data()$date), "%d/%m/%Y")

    #Collect item scores and store in vector

    item_scores_1<- as.numeric(unlist(strsplit(current_data()$item_scores, ",")))

    #Calculate the aggregate scale score

    score_1<- sum(item_scores_1, na.rm = TRUE)
                                                                     #We want the value (not the reference), so select first element only
    pts_1<- psychlytx::make_pts(score = score_1, mean = mean_input_1()[[1]], reliability = reliability_input_1()[[1]],
                                reliable_change_method = method_output_1())

    se_1<- psychlytx::make_se(sd = sd_input_1()[[1]], reliability = reliability_input_1()[[1]],
                              reliable_change_method = method_input_1())

    ci_1<- psychlytx::make_ci(pts_1, confidence_input_1())

    ci_upper_1<- psychlytx::make_ci_upper(pts = pts_1, ci = ci_1)

    ci_lower_1<- psychlytx::make_ci_lower(pts = pts_1, ci = ci_1)

    #Each widget input object is a list, so flatten the list after making it

    list(date, score, pts_1, se_1, ci_1, ci_upper_1, ci_lower_1, mean_input_1(), sd_input_1(), reliability_input_1(), cutoff_input_1(), confidence_input_1(),
         method_input_1()) %>% purrr::flatten() %>% purrr::set_names(c("date", "score", "pts", "se", "ci", "ci_upper", "ci_lower",
                                                                       "mean", "mean_reference", "sd", "sd_reference",
                                                                       "reliability", "reliability_reference",
                                                                       "cutoff_labels", "cutoff_values",
                                                                       "cutoff_values_references", "confidence", "method"))

    })






}

