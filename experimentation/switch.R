
pops<- c("funny soldiers", "angry people")

safe_pops<- list(funny_soldiers = list(1,2), angry_people = list(4,5))



pop_func<- function(input_val, population_names) {
  
  population_selected<- gsub('([[:punct:]])|\\s+','_', input_val)
  
  population_names$funny_soldiers
  
  
}

pop_func("funny soldiers", safe_pops)

