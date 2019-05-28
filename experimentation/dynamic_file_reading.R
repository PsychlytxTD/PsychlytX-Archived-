scale<- c("gad_7", "gad_7", "gad_7", "cpaq_20", "cpaq_20_activity_engagement", "cpaq_20_pain_willingness")
date<- c("01/01/2004", "01/01/2004", "04/02/2005", "04/02/2005", "04/02/2005", "04/02/2005")
score<- c( 8, 8, 3, 5, 3, 2)

subscale_dataframe<- data.frame(scale, date, score)

gad_7<- list(
  
  populations = list("dog", "cat"),
  mean = c(5, 6)
  
)

phq_9<- list(
  
  populations = list("lion", "tiger"),
  mean = c(1, 2)
  
)

cpaq_20<- list(
  
  populations = list("shark", "whale"),
  mean = c(7, 8)
  
)

cpaq_20_activity_engagement<- list(
  
  populations = list("tree", "bush"),
  mean = c(6, 9)
  
)

cpaq_20_pain_willingness<- list(
  
  populations = list("ocean", "forrest"),
  mean = c(32, 33)
  
)


params_all_lists<- list(cpaq_20 = cpaq_20, 
                        cpaq_20_activity_engagement = cpaq_20_activity_engagement, 
                        cpaq_20_pain_willingness = cpaq_20_pain_willingness, 
                        gad_7 = gad_7,
                        phq_9 = phq_9)


filtered_list<- names(params_all_lists) %in% nested_dataframe$scale

kept<-purrr::keep(params_all_lists, filtered_list)


nested_dataframe<- subscale_dataframe %>%
  dplyr::group_by(scale) %>%
  tidyr::nest() %>% dplyr::arrange(scale) %>% dplyr::mutate(subscale_info = kept)


