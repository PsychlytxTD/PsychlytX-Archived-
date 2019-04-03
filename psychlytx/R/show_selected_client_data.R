
show_selected_client_scores<- function(selected_client_data) {


  DT::renderDataTable({


    DT::datatable(

      selected_client_data(),
      extensions = 'Scroller', rownames = FALSE,
      options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
        "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" )

    )


  })




}
