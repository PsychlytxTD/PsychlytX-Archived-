#' Params list generator
#'
#' Make a list of population-specific params lists for each type of widget (mean, sd, etc.)
#'
#' @param list_original_subscale_params list of original subscale-specific parameters scripts.
#'
#' @export


#Pass in a list of the original scripts of parmaters (e.g. list(anxiety_subscale_params, depression_subscale_params))
#add_do_calls_for_poplists() takes each script and plucks out the correct, population-specific list for each one
#

make_population_list<- function(list_original_subscale_params, input_population) {


  list_original_subscale_params %>% purrr::at_depth(3,

             ~ do.call(psychlytx::params_list_maker,

                       list(
                       subscale_name = subscale_name,
                       population_quantity = population_quantity,
                       populations = populations,
                       input_population = reactive({ input_population()}) , #input_population() is population (reactive object) selected from the selectInput widget in the parent app
                       means = means,
                       sds = sds,
                       mean_sd_references = mean_sd_references,
                       reliabilities = reliabilities,
                       reliability_references = reliability_references,
                       cutoffs = cutoffs,
                       cutoff_names = cutoff_names,
                       cutoff_references = cutoff_references,
                       cutoff_quantity = cutoff_quantity
                       )

                       )

  )





}

