
show_selected_client_scores<- function(selected_client_data) {


  DT::renderDataTable({

    snapshot_selected_client_data<- if(length( selected_client_data() )  < 1) {
      return(NULL) } else {selected_client_data() %>% dplyr::select(date, measure, subscale, score) %>% dplyr::rename_all(toupper) }


    DT::datatable(

      snapshot_selected_client_data,
      extensions = 'Scroller', rownames = FALSE,
      options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
        "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" )

    )


  })




}
