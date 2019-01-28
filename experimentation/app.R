params_list_maker<- function(subscale_name, population_quantity, populations, input_population, sds, means, mean_sd_references, reliabilities, reliability_references,
                             cutoffs, cutoff_names, cutoff_references, cutoff_quantity) {
  
  params_list<- purrr::pmap(list(
    
    ids = list(subscale_name),
    populations = populations,
    means = means,
    sds = sds,
    mean_sd_references = mean_sd_references,
    reliabilities = reliabilities,
    reliability_references = reliability_references,
    cutoffs = cutoffs,
    cutoff_names = cutoff_names,
    cutoff_references = cutoff_references,
    cutoff_ids = list(paste(subscale_name, "_", 1:cutoff_quantity, sep = "")),
    cutoff_name_ids = list(paste("cutoff", "_", 1:cutoff_quantity, sep = ""))
    
    
  ), 
  
  function(ids, populations, means, sds, mean_sd_references, reliabilities, reliability_references, cutoffs, cutoff_names, cutoff_references, cutoff_ids, cutoff_name_ids) {
    
    list(ids = ids, populations = populations, means = means, sds = sds, mean_sd_references = mean_sd_references, 
         reliabilities = reliabilities, reliability_references = reliability_references, cutoffs = cutoffs,
         cutoff_names = cutoff_names, cutoff_references = cutoff_references,
         cutoff_ids = cutoff_ids, cutoff_name_ids = cutoff_name_ids)
    
  }
  
  )
  
  names(params_list)<- populations
  
  population_selected<- gsub('([[:punct:]])|\\s+','_', input_population)
  
  params_list[[paste(population_selected)]]
  
}



ui <- fluidPage(
  
  selectInput("popu", "select", c("male", "female")),
  
  fluidRow(
  uiOutput("cutoff_widgets_out")
  ),
  
  fluidRow(
  
  uiOutput("mean_widgets_out")
  
  ),
  
  textOutput("out1")
  
)

server <- function(input, output, session) {
  

  mean_widgets_reac<- reactive({
    
    panel_title<- h3(tags$strong("Anxiety"))
    
    mean_widget_list<- 
      
      purrr::pmap(params_list_maker("Anxiety", 2, list("male", "female"), input$popu, list(8, 9), list(.3, .6), list("Tim", "ake"), list(.8, .9), list("Tony", "Joa"),
                                    list(c(2,3,4,3,2),c(3,4,3,2,3)), list(rep(c("low", "moderate", "high", "mean + 1 sd", "mean + 2sd"), 2)), 
                                    list(c("Kim et al., 1998", "Jill et al., 2000", "Sono et al., 1998", "Takle et al., 2000", "Ream et al., 2000"),
                                         c("Ralk et al., 1998", "Simaaj et al., 2000", "Teeno et al., 1998", "Sope et al., 2000", "Zlkj et al., 2000")), cutoff_quantity = 5)[c(1,3,5)],
                  
                  function(ids, means, mean_sd_references) {
                    
                    div(
                      column(width = 2,
                             numericInput(inputId = ids, label = "mean", value = means),
                             h6(paste("Reference:", mean_sd_references))
                      )
                    )
                    
                  }
                  
      )
    
    do.call(tagList, list(panel_title, mean_widget_list))
    
  })
  
  
  output$mean_widgets_out<- renderUI({
    
    
    mean_widgets_reac()
    
  })   
 
  
 
  
  
  
  cutoff_widgets_reac<- reactive({
    
    panel_title<- h3(tags$strong("Anxiety"))
    
    widget_list<- 
      
      purrr::pmap(params_list_maker(subscale_name = "Anxiety", population_quantity =  2, populations =  list("male", "female"), input_population = input$popu, means = list(8, 9), sds =  list(.3, .6), mean_sd_references =  list("Tim", "ake"), 
                                    reliabilities = list(.8, .9), reliability_references = list("Tony", "Joa"),
                                    cutoffs = list(c(2,3,4,3,2),c(3,4,3,2,3)), cutoff_names = list(rep(c("low", "moderate", "high", "mean + 1 sd", "mean + 2sd"), 2, length.out = 5)), 
                                   cutoff_references = list(c("Kim et al., 1998", "Jill et al., 2000", "Sono et al., 1998", "Takle et al., 2000", "Ream et al., 2000"),
                                         c("Ralk et al., 1998", "Simaaj et al., 2000", "Teeno et al., 1998", "Sope et al., 2000", "Zlkj et al., 2000")), cutoff_quantity = 5)[c(8,9,10,11,12)],
                  
                  function(cutoff_ids, cutoff_name_ids, cutoffs, cutoff_names, cutoff_references) {
                    
                    div(
                      column(width = 2,
                             textInput(inputId = cutoff_name_ids, label = "cutoff", value = cutoff_names),
                             numericInput(inputId = cutoff_ids, label = "", value = cutoffs),
                             h6(paste("Reference:", cutoff_references))
                      )
                    )
                    
                  }
                  
      )
    
    do.call(tagList, list(panel_title, widget_list))
    
  })
  
  
  output$cutoff_widgets_out<- renderUI({
    
    
    cutoff_widgets_reac()
    
  }) 
  
  
  output$out1<- renderPrint({
  
    input$Anxiety_5
    
  })
   

  
}

shinyApp(ui, server)