


pull_clients_for_dropdown<- function(pool, clinician_id) {

  reactive({

  # Querying code to be called when clinician clicks 'existing client' tab further down. To generate dropdown of clients.

  client_list_sql<- "SELECT clinician_id, client_id, first_name, last_name, birth_date
  FROM client
  WHERE clinician_id = ?clinician_id;"

  client_list_query<- sqlInterpolate(pool, client_list_sql, clinician_id = clinician_id)

  client_list<- dbGetQuery( pool, client_list_query )

  while(length(client_list) < 1) {

    ""

  }

    client_list <- client_list %>%
      tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)

    client_list<- client_list %>%
      collect  %>%
      split( .$dropdown_client ) %>%    # Field that will be used for the labels
      purrr::map(~.$client_id)          #Field that will be returned when the clinician actually chooses the client


})

}
