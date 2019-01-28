
mean_widgets_reac<- reactive({
  
  panel_title<- h3(tags$strong("Anxiety"))
  
  mean_widget_list<- 
    
    purrr::pmap(pop_func(input$popu)[1:4],
                
                function(mean_id, mean_lab, mean_val, mean_ref) {
                  
                  div(
                    column(width = 2,
                           numericInput(inputId = mean_id, label = mean_lab, value = mean_val),
                           h6(paste("Reference:", mean_ref))
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
    
    purrr::pmap(params_list_maker("Anxiety", 2, list("male", "female"), input$popu, list(8, 9), list(.3, .6), list("Tim", "ake"), list(.8, .9), list("Tony", "Joa"),
                                  list(c(2,3,4,3,2),c(3,4,3,2,3)), list(rep(c("low", "moderate", "high", "mean + 1 sd", "mean + 2sd"), 2)), 
                                  list(c("Kim et al., 1998", "Jill et al., 2000", "Sono et al., 1998", "Takle et al., 2000", "Ream et al., 2000"),
                                       c("Ralk et al., 1998", "Simaaj et al., 2000", "Teeno et al., 1998", "Sope et al., 2000", "Zlkj et al., 2000"))),
                
                function(cutoff_id, cutoff_val, cutoff_lab, cutoff_ref) {
                  
                  div(
                    column(width = 2,
                           numericInput(inputId = cutoff_id, label = cutoff_lab, value = cutoff_val),
                           h6(paste("Reference:", cutoff_ref))
                    )
                  )
                  
                }
                
    )
  
  do.call(tagList, list(panel_title, widget_list))
  
})


output$cutoff_widgets_out<- renderUI({
  
  
  cutoff_widgets_reac()
  
})