#' Multiply do.call()
#'
#' Multiply the do.call() function to produce widgets without repitition
#'
#' @param id String to create a unique namespace.
#'
#' @export


multiply_do_call_UI<- function(id, module_id) {

  ns<- NS(id)

  purrr::map(.x = module_id,

             ~ fluidRow(uiOutput(paste(.x))))

}


#' Multiply do.call()
#'
#' Multiply the do.call() function to produce widgets without repitition
#'
#' @param id String to create a unique namespace.
#'
#' @param module_id List of strings representing ids for each widget
#'
#' @param params_script List of original params scripts by subscale
#'
#' @export


multiply_do_call<- function(module_id, params_script, input_population) {

  input_population<- reactive({ input_population() })


  purrr::map2(.x = module_id, .y = params_script,

              ~ do.call(callModule, c(psychlytx::generate_mean_widget, paste(.x), input_population, paste(.y)))

  )


}
