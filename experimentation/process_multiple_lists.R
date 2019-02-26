args<- list(subscale_name = "GAD-7",
population_quantity = 11,
populations = list("male_general_population", "female_general_population", "older_adult", "primary_care", "psychiatric", "Generalized_Anxiety_Disorder",
                   "chronic_musculoskeletal_pain", "coronary_heart_disease", "type_1_diabetes", "type_2_diabetes", "stroke"),
means = list(3.01, 4.07, 2, 5.75, 10.86, 12.59, 2.6, 11.9, 4.7, 4.5, 3.87),
sds = list(3.12, 3.53, 2.88, 4.76, 5.62, 3.96, 2.3, 5.3, 4.6, 4.9, 5.2),
mean_sd_references = list("Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Wild, Eckl, Herzog, Niehoff et al (2012)",
                          "Jordan, Shedden-Mora & Löwe (2017)", "Beard & Björgvinsson (2014)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)",
                          "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Conventry, Lovell, Dickens, Bower et al (2015)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)",
                          "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"),
reliabilities = list(.83, .83, .83, .83, .83, .83, .83, .83, .83, .83, .83),
reliability_references = list("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                              "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                              "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                              "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)"),
cutoffs = list(c(5, 10, 15, 3.01, 3.01 + 3.12), c(5, 10, 15, 4.07, 4.07 + 3.53), c(5, 10, 15, 2, 2 + 2.88), c(5, 10, 15, 4.75, 4.75 + 4.76),
               c(5, 10, 15, 10.86, 10.86 + 5.62), c(5, 10, 15, 12.59, 12.59 + 3.96), c(5, 10, 15, 2.6, 2.6 + 2.3),
               c(5, 10, 15, 11.9, 11.9 + 5.3), c(5, 10, 15, 4.7, 4.7 + 4.6), c(5, 10, 15, 4.5, 4.5 + 4.9),
               c(5, 10, 15, 3.87, 3.87 + 4.52)),
cutoff_names = list(rep(c("Mild", "Moderate: for further evaluation", "Severe", "Mean", "Mean + 1 Sd"), 11, length.out = 5)),
cutoff_references = list(c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Wild, Eckl, Herzog, Niehoff et al (2012)", "Wild, Eckl, Herzog, Niehoff et al (2012)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Jordan, Shedden-Mora & Löwe (2017)", "Jordan, Shedden-Mora & Löwe (2017)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Beard & Björgvinsson (2014)", "Beard & Björgvinsson (2014)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Bair, Wu, Damush, Sutherland & Kroenke (2008)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Conventry, Lovell, Dickens, Bower et al (2015)", "Conventry, Lovell, Dickens, Bower et al (2015)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" , "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                         c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                           "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)" , "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)")),
cutoff_quantity = 5)



args_2<- list(subscale_name = "PhQ",
            population_quantity = 11,
            populations = list("male_general_population", "female_general_population", "older_adult", "primary_care", "psychiatric", "Generalized_Anxiety_Disorder",
                               "chronic_musculoskeletal_pain", "coronary_heart_disease", "type_1_diabetes", "type_2_diabetes", "stroke"),
            means = list(3.01, 4.07, 2, 5.75, 10.86, 12.59, 2.6, 11.9, 4.7, 4.5, 3.87),
            sds = list(3.12, 3.53, 2.88, 4.76, 5.62, 3.96, 2.3, 5.3, 4.6, 4.9, 5.2),
            mean_sd_references = list("Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Wild, Eckl, Herzog, Niehoff et al (2012)",
                                      "Jordan, Shedden-Mora & Löwe (2017)", "Beard & Björgvinsson (2014)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)",
                                      "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Conventry, Lovell, Dickens, Bower et al (2015)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)",
                                      "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"),
            reliabilities = list(.83, .83, .83, .83, .83, .83, .83, .83, .83, .83, .83),
            reliability_references = list("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                          "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                          "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                          "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)"),
            cutoffs = list(c(5, 10, 15, 3.01, 3.01 + 3.12), c(5, 10, 15, 4.07, 4.07 + 3.53), c(5, 10, 15, 2, 2 + 2.88), c(5, 10, 15, 4.75, 4.75 + 4.76),
                           c(5, 10, 15, 10.86, 10.86 + 5.62), c(5, 10, 15, 12.59, 12.59 + 3.96), c(5, 10, 15, 2.6, 2.6 + 2.3),
                           c(5, 10, 15, 11.9, 11.9 + 5.3), c(5, 10, 15, 4.7, 4.7 + 4.6), c(5, 10, 15, 4.5, 4.5 + 4.9),
                           c(5, 10, 15, 3.87, 3.87 + 4.52)),
            cutoff_names = list(rep(c("Mild", "Moderate: for further evaluation", "Severe", "Mean", "Mean + 1 Sd"), 11, length.out = 5)),
            cutoff_references = list(c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Wild, Eckl, Herzog, Niehoff et al (2012)", "Wild, Eckl, Herzog, Niehoff et al (2012)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Jordan, Shedden-Mora & Löwe (2017)", "Jordan, Shedden-Mora & Löwe (2017)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Beard & Björgvinsson (2014)", "Beard & Björgvinsson (2014)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Bair, Wu, Damush, Sutherland & Kroenke (2008)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Conventry, Lovell, Dickens, Bower et al (2015)", "Conventry, Lovell, Dickens, Bower et al (2015)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" , "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                                     c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                       "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)" , "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)")),
            cutoff_quantity = 5)







my_func<- function(subscale_name, population_quantity, populations, input_population, sds, means, mean_sd_references, reliabilities, reliability_references,
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
  

  
  #Store the population selected by the user into the object population_selected
  
  population_selected<- "older_adult"
  
  #Use the population_selected object to return the correct list (i.e. the one containing the values of the population selected by the user)
  
  params_list[[paste(population_selected)]]
  
}


















do.call(my_func, args)


#Add do calls to param_list_maker() to create lists

add_do_calls<- function(list_of_params_lists) {
  
  purrr::map(.x = list_of_params_lists,
    
    ~ do.call(my_func, .x)
    
  )
  
  
}


add_do_calls(list_of_params_lists = list(args, args_2))


#Add do calls to mean_module

add_do_calls_mean<- function(list_of_final_lists) {
  
  purrr::map(.x = list_of_final_lists,
             
             .f = function(mean_sd_rel_ids, means, mean_sd_references, mean_sd_rel_reference_ids) {
               
               #Create a div containing the dynamically generated widgets
               
               div(column(width = 2,
                          
                          numericInput(inputId = ns(mean_sd_rel_ids), label = h4(tags$strong(panel_name)), value = means),
                          
                          textInput(inputId = ns(mean_sd_rel_reference_ids), label = "Reference", value = mean_sd_references)
                          
               ))
               
             } 
             
  )
  
}


