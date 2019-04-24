#' Submit Nested Data to Report
#'
#' Wrangle selected_client_data pulled from database and pass it to an R Markdown report
#'
#' @param id String to create a unique namespace.
#'
#' @export

download_report_UI<- function(id) {

  ns<- NS(id)

  tagList(

    fluidPage(

      sidebarLayout( position = "right",

                     sidebarPanel(

                       downloadButton(ns("report"), "Report Download",
                                      class = "submit_data", lib = "font-awesome") %>%  helper(type = "inline",
                                                                                               title = "Problems with report generation",
                                                                                               colour = "#d35400",
                                                                                               content = c("<b>Your report may not generate for two reasons:</b>",
                                                                                                           "<b>1.</b> You have selected a client who has no outcomes recorded with a Psychlytx web application.",
                                                                                                           "<b>1.</b> When selecting your client, you forgot to click <code style='color:#d35400;'>Retrieve Outcomes</code>."),
                                                                                               size = "m")

                     ),


                     mainPanel(tags$b("This report will display outcomes across all measures completed by the client using
                                      PsychlytX web applications."),

                               br(),

                               "*Report generation may take a few moments.")

                     )))

}




#' Submit Nested Data to Report
#'
#' Wrangle selected_client_data pulled from database and pass it to an R Markdown report
#'
#' @param pool The pooled db connection.
#'
#' @param selected_client A string indicating the unique id of the selected client.
#'
#' @param measure The name of the psychological measure.
#'
#' @export


download_report<- function(input, output, session, pool, selected_client, measure, most_recent_client_data) {


   report_data <- reactive({

    #Nest the dataframe: create a list column of dataframes - one per each subscale.
    #We want to group the scores by subscale. So GAD7 should have its own df, PHQ9 should have its own df etc.

    subscale_df <- most_recent_client_data() %>%
      dplyr::group_by(subscale) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        subscale_info = list(psychlytx::gad7_info)
      ) #Each subscale needs its own info list (containing params that will be used for plotting)
    #So we make another list column containing subscale info lists


    #Add a 'change' variable for each subscale's dataframe showing statistically reliable change in scores. This is a custom function - see above.

    subscale_df <- subscale_df %>%
      dplyr::mutate(data = purrr::map(data, ~ make_change_variable(.x)))


    #Remove the underscore between words comprising the 'population' variable and replace with white space for report display

    subscale_df<- subscale_df %>% dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., population = gsub("_", " ", population))))

    #Change the value of the confidence variable so that percentages (not decimals) appear in the report

    subscale_df <- subscale_df %>%
      dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(
        .,
        confidence = dplyr::case_when(
          confidence == 1.645 ~ "99%",
          confidence == 1.96 ~ "95%",
          confidence == 2.575 ~ "90%"))))

    #Create a seperate plot for each subscale and store the plots in a list column
    #This is a custom plotting function - see above

    subscale_df <- subscale_df %>%
      dplyr::mutate(plot = purrr::map2(data, subscale_info, ~ plot_subscale(.x, .y)))


    #Make a seperate scores table for each subscale and store the tables in a list column

    subscale_df <- subscale_df %>%
      dplyr::mutate(
        scores_table = purrr::map(
          data, ~ dplyr::select(
            .,
            Date = date,
            Score = score,
            Change = change,
            `CI Upper Limit` = ci_upper,
            `CI Lower Limit` = ci_lower,
            `Predicted True Score` = pts,
            `Standard Error` = se
          ) %>%
            dplyr::mutate(
              Change = kableExtra::cell_spec(Change, "latex", color = if_else(
                is.na(Change), "gray", if_else(
                  grepl("\\*", Change), "red", "gray")))
            ) %>%
            kableExtra::kable(
              format = "latex",
              booktabs = T,
              escape = F
            ) %>%
            kableExtra::kable_styling(full_width = F) %>%
            kableExtra::add_header_above(c("Outcome Data" = 7)) %>%
            kableExtra::add_header_above(c("Table 1" = 7))
        )
      )

    #Make a statistical info table for each subscale and store the tables in a list column

    subscale_df <- subscale_df %>%
      dplyr::mutate(
        statistics_table_1 = purrr::map(
          data, ~ dplyr::select(
            .,
            Date = date,
            `Reference Population` = population,
            Mean = mean,
            Sd = sd,
            `Mean Reference` = mean_reference,
            `Sd Reference` = sd_reference
          ) %>%
            dplyr::mutate(
              date2 = as.Date(Date, "%m/%d/%Y")
            ) %>%
            dplyr::arrange(desc(date2)) %>%
            dplyr::slice(1) %>%
            dplyr::select(-date2) %>%
            kableExtra::kable(format = "latex", booktabs = T) %>%
            kableExtra::kable_styling(full_width = F) %>%
            kableExtra::column_spec(c(2, 5, 6), width = "3cm") %>%
            kableExtra::add_header_above(c("Data & References" = 6)) %>%
            kableExtra::add_header_above(c("Table 2" = 6))
        )
      )


    #Make another statistical info table for each subscale and store the tables in a list column

    subscale_df <- subscale_df %>%
      dplyr::mutate(
        statistics_table_2 = purrr::map(
          data, ~ dplyr::select(
            .,
            Date = date,
            `Retest Reliability` = reliability,
            `Reliability Reference` = reliability_reference,
            Confidence = confidence,
            `Reliable Change Method` = method
          ) %>%
            dplyr::mutate(
              date2 = as.Date(Date, "%m/%d/%Y")
            ) %>%
            dplyr::arrange(desc(date2)) %>%
            dplyr::slice(1) %>%
            dplyr::select(-date2) %>%
            kableExtra::kable(format = "latex", booktabs = T) %>%
            kableExtra::kable_styling(full_width = F) %>%
            kableExtra::column_spec(c(3, 5), width = "3cm") %>%
            kableExtra::add_header_above(c("Data & References" = 5)) %>%
            kableExtra::add_header_above(c("Table 2" = 5))
        )
      )
  })


  client_name<- reactive({

    client_name_sql<- "SELECT first_name, last_name, birth_date
    FROM client
    WHERE client_id = ?client_id;"

    client_name_query<- sqlInterpolate(pool, client_name_sql, client_id = selected_client() )

    client_name<- dbGetQuery( pool, client_name_query )


  })

  output$report <- downloadHandler(

    filename = paste0("Clinical Report", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempD <- tempdir()
      tempReport <- file.path(tempD, "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("logo.png", file.path(tempD, "logo.png"), overwrite = TRUE)

      # Pass data objects to Rmd document
      params <- list(

        report_data = report_data(), #We are passing the nested dataframe to the R markdown report.
        client_name = client_name()

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
                                                         # Sys.sleep(0.25)
                                                       }
                                                     })
                        ))})

}
