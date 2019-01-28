
  
 a<- purrr::pmap(list(
    id = list(rep("Anxiety", 2)),
    population = list("men", "women"),
    ms = list(5, 6),
    sds = list(2, 3),
    stats_refs = list("John et al., 1998", "Tony et al., 2000"),
    rels = list(.6, .8),
    rels_refs = list("Jane et al., 1998", "Wilson et al., 2000"),
    cutoff_vals = list(c(2,3,4,3,2),c(3,4,3,2,3)),
    cutoff_names = list(rep(c("low", "moderate", "high", "mean + 1 sd", "mean + 2sd"), 2)),
    cutoff_refs = list(c("Kim et al., 1998", "Jill et al., 2000", "Sono et al., 1998", "Takle et al., 2000", "Ream et al., 2000"),
                       c("Ralk et al., 1998", "Simaaj et al., 2000", "Teeno et al., 1998", "Sope et al., 2000", "Zlkj et al., 2000"))
  ), 
  
  function(id, population, ms, sds, stats_refs, rels, rels_refs, cutoff_vals, cutoff_names, cutoff_refs) {
     
   list(id = id[seq(from = 1, to = length(id), length.out = 1)], population = population, ms = ms, sds = sds, stats_refs = stats_refs, 
        rels = rels, rels_refs = rels_refs, cutoff_vals = cutoff_vals,
        cutoff_names = cutoff_names[seq(from = 1, to = length(cutoff_names), length.out = 5)], cutoff_refs = cutoff_refs)
  
  }
  
  
  )
 
names_vec<- c("girl", "boy")
 
names(a)<- names_vec
 
list2env(a, globalenv())
 
 
 
params_list_maker<- function(subscale_name, population_quantity, populations, input_population, sds, means, stat_references, reliabilities, reliability_references,
                        cutoffs, cutoff_names, cutoff_references) {
  
  params_list<- purrr::pmap(list(
  
  ids = list(rep(subscale_name, population_quantity)),
  populations = populations,
  means = means,
  sds = sds,
  stat_references = stat_references,
  reliabilities = reliabilities,
  reliability_references = reliability_references,
  cutoffs = cutoffs,
  cutoff_names = cutoff_names,
  cutoff_references = cutoff_references
  
  ), 
  
  function(ids, populations, means, sds, stat_references, reliabilities, reliability_references, cutoffs, cutoff_names, cutoff_references) {
    
    list(ids = ids[seq(from = 1, to = length(ids), length.out = 1)], populations = populations, means = means, sds = sds, stat_references = stat_references, 
         reliabilities = reliabilities, reliability_references = reliability_references, cutoffs = cutoffs,
         cutoff_names = cutoff_names[seq(from = 1, to = length(cutoff_names), length.out = 5)], cutoff_references = cutoff_references)
    
  }
  
  )
  
  names(params_list)<- populations
  
  population_selected<- gsub('([[:punct:]])|\\s+','_', input_population)
  
  params_list[[paste(population_selected)]]
  
}


params_list_maker("Depression", 2, list("happy_soldier", "funny_donkey"), "happy soldier", list(8, 9), list(.3, .6), list("Tim", "ake"), list(.8, .9), list("Tony", "Joa"),
                  list(c(2,3,4,3,2),c(3,4,3,2,3)), list(rep(c("low", "moderate", "high", "mean + 1 sd", "mean + 2sd"), 2)), 
                  list(c("Kim et al., 1998", "Jill et al., 2000", "Sono et al., 1998", "Takle et al., 2000", "Ream et al., 2000"),
                       c("Ralk et al., 1998", "Simaaj et al., 2000", "Teeno et al., 1998", "Sope et al., 2000", "Zlkj et al., 2000")))






list2env(params_list, globalenv())











 labs<- c("dog", "cat")
 
 assign(labs[1], a[[1]])
 assign(labs[2], a[[2]])

 
 "hello" = a[[1]]

"goodbye" = a[[2]]


  set_population_params<- function(subscale_name, population_names, mean_vals, sd_vals, reliability_vals) {}
  
  
  set_population_params(list("Anxiety", "Anxiety"), list("men", "women"), list(3.4, 5.6), list(0.3, 0.6), list(.83, .95))

male_general_popopulation <-    
  list(
    mean_id = list("anxiety_1"),
    mean_lab = list("Anxiety"),
    mean_val = list(3.4),
    mean_ref = list("halj"),
    sd_id = list("anxiety_1"),
    sd_lab = list("Anxiety"),
    sd_val = list(2.4),
    sd_ref = list("halj"),
    reliability_id = list("anxiety_1"),
    reliability_lab = list("Anxiety"),
    reliability_val = list(3.4),
    reliability_ref = list("halj"),
    cutoff_id = list("anx_cutoff_1", "anx_cutoff_2", "anx_cutoff_3"),
    cutoff_val = list(3, 5, 8),
    cutoff_lab = list("normal", "mean", "high"),
    cutoff_ref = list("john", "taylor", "harry")
  )

female_general_population <- list(
  mean_id = list("anxiety_1"),
  mean_lab = list("Anxiety"),
  mean_val = list(3.4),
  mean_ref = list("halj"),
  sd_id = list("anxiety_1"),
  sd_lab = list("Anxiety"),
  sd_val = list(2.4),
  sd_ref = list("halj"),
  reliability_id = list("anxiety_1"),
  reliability_lab = list("Anxiety"),
  reliability_val = list(3.4),
  reliability_ref = list("halj"),
  cutoff_id = list("anx_cutoff_1", "anx_cutoff_2", "anx_cutoff_3"),
  cutoff_val = list(3, 5, 8),
  cutoff_lab = list("normal", "mean", "high"),
  cutoff_ref = list("john", "taylor", "harry")
)


