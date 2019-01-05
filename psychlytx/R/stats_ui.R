#' Tabpanel widget content for mean, sd, test-retest reliability and cutoff values
#'
#' Module generates the widgets (and associated references) required for inputting mean,
#' sd, reliability and cutoff values.
#'
#' @param id String to create a unique namespace.
#'
#' @export

stats_widgets_UI<- function(id) {

  ns<- NS(id)

  fluidRow(
    uiOutput(ns("stats_widgets_out"))
  )

}


#' Tabpanel widget content for mean, sd, test-retest reliability and cutoff values
#'
#' Module generates the widgets (and associated references) required for inputting mean,
#' sd, reliability and cutoff values.
#'
#' @param subscale_names A list of strings representing the names the subscales
#'
#' @param stats_df A dataframe. Column names indicate possible populations (e.g. veteran). Integers within
#' each column represent subscale values. Subscale values in columns must match the order in which subscales
#' are listed in the subscale_names argument.
#'
#' @param refs_df A dataframe. Column names indicate possible populations (e.g. veteran). Strings within each
#' column represent references corresponding to each subscale value. Subscale references in columns must match the order in which subscales
#' are listed in the subscale_names argument.
#'
#' @param pop A reactive value (string) derived from a widget.
#'
#' @export

stats_widgets<- function(input, output, session, subscale_names, stats_df, refs_df, pop) {



  stats_widgets_reac<- reactive({

    #Necessary when rendering UI object with modules.

    ns <- session$ns

    #Store the widget value in an object to avoid throwing an error.

    population<- gsub('([[:punct:]])|\\s+','_', pop())

    selected_pop_vals<- stats_df[ , population]

    selected_ref_vals<- refs_df[ , population]

    #For each of the subscales, generate a widget for inputing a stat (e.g. mean, sd, reliability or cutoff value) and a
    #reference to sit beneath the widget. Store the output in a reactive expression.

    widget_list <- lapply(seq_along(subscale_names), function(i) {

      default_value<- selected_pop_vals[i]
      id_label  <- subscale_names[i]
      reference<- selected_ref_vals[i]

      div(
        column(width = 2,
               numericInput(ns(id_label), id_label, value = default_value),
               h6(paste("Reference:", reference))
        )
      )

    }
    )

    do.call(tagList, widget_list)



  })


  #Render the widgets.

  output$stats_widgets_out<- renderUI({

    stats_widgets_reac()

  })



}
