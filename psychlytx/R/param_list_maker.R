#' List generator for population paramaters for a subscale
#'
#' Outputs the correct list of parameters for the population selected by the user (parameters are then available for default values.
#' of widgets).
#'
#' @param subscale_name A string (underscores should replace white space) indicating the name of the subscale for which the function is being used (e.g. "Anxiety").
#'
#' @param population_quantity A numeric value of possible populations from which the user can select.
#'
#' @param populations A list of strings (underscores should replace white space) indicating the possible range of populations.
#'
#' @param sds A list of numeric values representing the standard deviations for all populations on that subscale.
#'
#' @param means A list of numeric values representing the means for all populations on that subscale.
#'
#' @param mean_sd_references A list of strings indicating the references for each mean/standard deviation by population.
#'
#' @param reliabilities A list of numeric values representing the test-retest reliabilities for all populations on that subscale.
#'
#' @param reliability_references A list of strings indicating the references for each reliability value by population.
#'
#' @param cutoffs A list of concatenated numeric values representing the cutoff values on this subscale for each population.
#'
#' @param cutoff_names A list of concatenated strings indicating the cutoff value descriptors. Use rep() function to multiple by populations.
#'
#' @param cutoff_references A list of strings indicating the references for each reliability value by population.
#'
#' @param cutoff_quantity A numeric value indicating the number of cutoff scores for the subscale.
#'
#' @export


params_list_maker<- function(subscale_name, population_quantity, populations, input_population, sds, means, mean_sd_references, reliabilities, reliability_references,
                             cutoffs, cutoff_names, cutoff_references, cutoff_quantity) {

  params_list<- purrr::pmap(list(

    populations = populations,
    mean_sd_rel_ids = list(subscale_name),
    means = means,
    sds = sds,
    mean_sd_references = mean_sd_references,
    reliabilities = reliabilities,
    reliability_references = reliability_references,
    cutoff_ids = list(paste(subscale_name, "_", 1:cutoff_quantity, sep = "")),
    cutoff_name_ids = list(paste("cutoff", "_", 1:cutoff_quantity, sep = "")),
    cutoffs = cutoffs,
    cutoff_names = cutoff_names,
    cutoff_references = cutoff_references

  ),

  function(mean_sd_rel_ids, populations, means, sds, mean_sd_references, reliabilities, reliability_references, cutoffs, cutoff_names, cutoff_references, cutoff_ids, cutoff_name_ids) {

    list(mean_sd_rel_ids = mean_sd_rel_ids, populations = populations, means = means, sds = sds, mean_sd_references = mean_sd_references,
         reliabilities = reliabilities, reliability_references = reliability_references, cutoff_ids = cutoff_ids,
         cutoff_name_ids = cutoff_name_ids, cutoffs = cutoffs, cutoff_names = cutoff_names, cutoff_references = cutoff_references)

  }

  )

  names(params_list)<- populations

  population_selected<- gsub('([[:punct:]])|\\s+','_', input_population)

  params_list[[paste(population_selected)]]

}
