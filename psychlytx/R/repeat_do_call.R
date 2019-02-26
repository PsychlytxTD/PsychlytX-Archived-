

repeat_do_call_UI<- function(unique_id) {

  purrr::map(unique_id,

         ~ psychlytx::generate_mean_widget_UI(.x)

  )



}




repeat_do_call<- function(unique_id, params_list, input_population) {

  purrr::map(unique_id,

             ~ do.call(callModule, c(psychlytx::generate_mean_widget, .x, input_population, params_list))
  )


}
