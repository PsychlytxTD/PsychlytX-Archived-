#' Simplified Application Statistics Read-In
#'
#' Read in required statistics from holding table in DB, to be used when client completes simplified application measure.
#'
#' @param id A string to creat the namespace.
#'
#' @export
#'

read_holding_stats_UI<- function(id) {

  ns <- NS(id)

  tagList(

  textInput(ns("client_id"), "Please sign in with your ID"),
  br(),
  actionButton(ns("submit_id"), "Submit")

  )

}


#' Select Client
#'
#' Select client and display his/her data in abbreviated form.
#'
#' @param pool A pooled db object.
#'
#' @param measure A string indiating name of the measure.
#'
#' @export
#'

read_holding_stats<- function(input, output, session, pool, measure) {


  holding_statistics<- eventReactive(input$submit_id, { #Pull in the selected client's data (for this measure only) from the db.

    holding_stats_client_sql<- "SELECT *
    FROM holding
    WHERE client_id = ?client_id AND measure = ?measure;"

    holding_stats_client_query<- sqlInterpolate(pool, holding_stats_client_sql, client_id = input$client_id, measure = measure)

    dbGetQuery(pool, holding_stats_client_query)

  })


  reactive({ holding_statistics() })

}





