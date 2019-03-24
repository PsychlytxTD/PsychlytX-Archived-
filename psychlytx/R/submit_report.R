#' Submit db_database db_data to Report
#'
#' Wrangle db_data pulled from db_database and pass it to an R Markdown report
#'
#' @param id String to create a unique namespace.
#'
#' @export

download_report_UI<- function(id) {

  ns<- NS(id)

  tagList(

    fluidRow(

      column(width = 2, offset = 8,

         br(),

          downloadButton(ns("report"), "Download Clinical Report", class = "reportbutton", lib = "font-awesome"),

          tags$head(tags$style(".reportbutton{background-color:#283747;} .reportbutton{color:#d35400;}"))
  )))


}




#' Submit db_database db_data to Report
#'
#' Wrangle db_data pulled from db_database and pass it to an R Markdown report
#'
#' @param db_data An unnested dataframe pulled from the database containing all timepoint scores for an individual on a specific measure.
#'
#' @export


download_report<- function(input, output, session, db_data) {

  #Nest the db_dataframe: create a list column of db_dataframes - one per each subscale

  nested_subscale_df<- db_data() %>% dplyr::group_by(scale) %>% tidyr::nest() %>% dplyr::mutate(subscale_info = list(psychlytx::gad7_params))



  #Add a 'change' variable to each db_dataframe showing statistically reliable change in scores

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate( db_data = purrr::map(db_data, ~ psychlytx::make_change_variable(.x)))



  #Change the value of the "confidence" variable so that percentages (not decimals) appear in the report

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate( db_data = purrr::map(db_data, ~ dplyr::mutate(.,
                          confidence = dplyr::case_when(confidence == 1.645 ~ "99%",
                                                        confidence == 1.96 ~ "95%",
                                                        confidence == 2.575 ~ "90%"))))


  #Create a seperate plot for each subscale (using the db_dataframe corresponding to that subscale) and store the plots in a list column

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate(plot = purrr::map2(db_data, subscale_info, ~ psychlytx::plot_subscale(.x, .y)))


  #Make a seperate table of scores ("scores_table") for each subscale and store the tables in a list column

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate(scores_table = purrr::map(db_data, ~ dplyr::select(., Date = date, Score = score, Change = change,
              `CI Upper Limit` = ci_upper, `CI Lower Limit` = ci_lower, `Predicted True Score` = pts, `Standard Error` = se) %>% dplyr::mutate(Change = #Sig change in red
               kableExtra::cell_spec(Change, "html", color = if_else(is.na(Change), "gray", if_else(grepl("\\*", Change), "red", "gray")))) %>% kableExtra::kable(.,
               format = "html", booktabs = T, escape = F) %>% kableExtra::kable_styling(full_width = F) %>% add_header_above(c("Outcome db_data"
               = 7)) %>% add_header_above(c("Table 1" = 7))))

  #Make two statistical info tables for each subscale and store the tables in list columns

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate(statistics_table_1 = purrr::map(db_data, ~dplyr::select(., Date = date, `Reference Population` = population,
                Mean = mean, Sd = sd, `Mean Reference` = mean_reference, `Sd Reference` = sd_reference) %>% kableExtra::kable(., format = "html",
               booktabs = T) %>% kableExtra::kable_styling(full_width = F) %>% add_header_above(c("db_data & References Used In Reliable Change Calculations"
               = 6)) %>% add_header_above(c("Table 2" = 6))))

  nested_subscale_df<- nested_subscale_df %>% dplyr::mutate(statistics_table_2 = purrr::map(db_data, ~dplyr::select(., Date = date, `Retest Reliability` = reliability,
                `Reliability Reference` = reliability_reference, Confidence = confidence, `Reliable Change Method` = method) %>% kableExtra::kable(.,
                format = "html", booktabs = T) %>% kableExtra::kable_styling(full_width = F) %>% add_header_above(c("db_data & References Used In Reliable
               Change Calculations (Contd.)" = 5)) %>% add_header_above(c("Table 3" = 5))))



  output$report <- downloadHandler(

    filename = paste0("Clinical Report", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Pass data objects to Rmd document
      params <- list(

        nested_subscale_df #We are passing the nested dataframe to the R markdown report.

      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv(),

                        withProgress(message = 'Your report is generating.',
                        detail = 'Please wait a moment...', value = 0, {
                        for (i in 1:25) {
                        incProgress(1/25)
                        Sys.sleep(0.25)
                                        }
       })))})


}








