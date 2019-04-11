#' Pre-Therapy Analytics Widgets
#'
#' Module generates pre-therapy analytics widgets
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_pretherapy_UI<- function(id) {

  ns<- NS(id)

  tagList(

                   tagList(
                     sidebarLayout(
                      sidebarPanel(width = 9,
                     h3("Please Provide Some Demographic Information"),
                     br(),
                     textInput(ns("first_name"), "First Name", width = '50%'),
                     textInput(ns("last_name"), "Last Name", width = '50%'),
                     selectInput(ns("sex"), "Sex", c("", "Male", "Female", "Other"), width = '20%'),
                     dateInput(ns("birth_date"), "Date of Birth", value = "", width = '20%'),
                     textInput(ns("postcode"), "Postcode", value = "", width = '20%'),
                     selectInput(ns("marital_status"), "Marital Status", c("", "Never Married", "Currently Married", "Separated", "Divorced", "Widowed", "Cohabiting"), width = '30%'),
                     selectInput(ns("sexuality"), "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer", "Other"), width = '30%'),
                     selectInput(ns("ethnicity"), "Ethnicity", c("", "Caucasian", "Latino/Hispanic", "Middle Eastern", "African", "Caribbean", "South Asian",
                                                                 "East Asian", "Mixed", "Other"), width = '30%'),
                     radioButtons(ns("indigenous"), "Identifies as Being of Aboriginal or Torres Strait Islander Descent", choices = c("No", "Yes"), selected = character(0), width = '20%'),
                     numericInput(ns("children"), "Number of Dependent Children", value = "", width = '20%'),
                     selectInput(ns("workforce_status"), "Primary Workforce Status", c("", "Working Full-Time", "Working Part-Time", "Working Casual Hours", "Studying", "Unemployed", "Retired"), width = '30%'),
                     selectInput(ns("education"), "Highest Education Level", c("", "No education", "Primary Education", "Secondary Education", "Post-Secondary/Tertiary Education",
                                                                               "Bachelor or Equivalent", "Master or Equivalent", "Doctoral or Equivalent"), width = '30%'),

                     br(),

                     selectInput(ns("population"), "Which population below most closely matches the
                                 characteristics of your client?", width = '70%',
                                 choices = c("male general population", "female general population", "older adult",
                                             "primary care", "psychiatric", "Generalized Anxiety Disorder",
                                             "chronic musculoskeletal pain", "coronary heart disease",
                                             "type 1 diabetes", "type 2 diabetes", "stroke")),

                     actionButton(ns("submit_analytics_pretherapy"), "Register Client")

                      ),

                     mainPanel()

                   )))


}



#' New or existing client status widget and headings
#'
#' Module generates client status widget and headings.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_pretherapy<- function(input, output, session) {

  #Need to return input to make input parameters available

  eventReactive(input$submit_analytics_pretherapy, {

   client_id<- uuid::UUIDgenerate() #Generate unique client ID


    #Use req() to avoid error messages if the values are NULL

    pretherapy_analytics_items<- list( req(clinician_id), req(client_id), req(input$first_name), req(input$last_name), req(input$sex),
                                       req(input$birth_date), req(input$postcode), req(input$marital_status),
                                       req(input$sexuality), req(input$ethnicity), req(input$indigenous), req(input$children),
                                       req(input$workforce_status), req(input$education), req(input$population) ) %>% purrr::set_names(c("clinician_id", "client_id", "first_name", "last_name", "sex", "birth_date",
                                       "postcode", "marital_status", "sexuality", "ethnicity", "indigenous", "children", "workforce_status", "education", "population"))




    pretherapy_analytics_df<- list( pretherapy_analytics_items ) %>% {

      tibble::tibble(  #Create a dataframe with all the pre-therapy analytics widget values as columns
                       #Specify the variable type explicitly

      clinician_id = purrr::map_chr(., "clinician_id"),
      client_id = purrr::map_chr(., "client_id"),
      first_name = purrr::map_chr(., "first_name"),
      last_name = purrr::map_chr(., "last_name"),
      sex = purrr::map_chr(., "sex"),
      birth_date = format(lubridate::as_date(purrr::map_dbl(., "birth_date")), "%d/%m/%Y"), #Convert to date class
      postcode = purrr::map_chr(., "postcode"),
      marital_status = purrr::map_chr(., "marital_status"),
      sexuality = purrr::map_chr(., "sexuality"),
      ethnicity = purrr::map_chr(., "ethnicity"),
      indigenous = purrr::map_chr(., "indigenous"),
      children = purrr::map_dbl(., "children"),
      workforce_status = purrr::map_chr(., "workforce_status"),
      education = purrr::map_chr(., "education"),
      population = purrr::map_chr(., "population")

    )


    }

  })

}




#' Post-Therapy Analytics Widgets
#'
#' Module generates post-therapy analytics widgets
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_posttherapy_UI<- function(id) {

  ns<- NS(id)


    conditionalPanel(condition = "input.last_assessment == 'last'", ns = ns,

                     tagList(
                       sidebarLayout(
                         sidebarPanel(width = 9,
                                      tagList(
                                        h3("Before Proceeding, Please Provide Important Information About Clinical Outcomes"),
                                        br(),
                                        selectInput(ns("principal_diagnosis"), "Presenting Principal Diagnosis", psychlytx::diagnosis_list, width = '60%'),
                                        selectizeInput(ns("secondary_diagnosis"), "Additional Presenting Diagnosis/Diagnoses", psychlytx::diagnosis_list, multiple = TRUE, width = '60%'),
                                        textInput(ns("referrer"), "Referrer", value = "", width = '50%'),
                                        selectInput(ns("attendance_schedule"), "Schedule of Attendance", c("", "Varied", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "Greater Than 1 Month Apart"), width = '40%'),
                                        numericInput(ns("non_attendances"), "Number of Non-Attendances (DNAs)", value = "", width = '20%'),
                                        numericInput(ns("attendances"), "Number of Sessions Attended", value = "", width = '20%'),
                                        radioButtons(ns("premature_dropout"), "Premature Dropout", choices = c("Yes", "No"), selected = character(0), width = '20%'),
                                        selectInput(ns("therapy"), "Therapeutic Approach Used", psychlytx::therapies_list, width = '50%'),
                                        selectInput(ns("funding"), "Funding Source", choices = c("", "Entirely Self-Funded", "Partly Medicare Funded", "Entirely Medicare Funded (Bulk Billing)",
                                                                                                "Private Health Fund",
                                                                                                "WorkCover", "Transport Accident Commission (TAC)",
                                                                                                "Department of Veterans Affairs (DVA)",
                                                                                                "Victims of Crime Assistance Tribunal (VOCAT)",
                                                                                                "Other"), width = '40%'),
                                        numericInput(ns("out_of_pocket"), "Out-Of-Pocket Expense", value = "", width = '20%'),
                                        actionButton(ns("submit_analytics_posttherapy"), "Submit Data")
                                           )),

                         mainPanel()

                       )))


}



#' New or existing client status widget and headings
#'
#' Module generates client status widget and headings.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_posttherapy<- function(input, output, session, selected_client) {

  #Need to return input to make input parameters available


  eventReactive(input$submit_analytics_posttherapy, {

    #For now use temp vars to check that writing works: in reality, at clinician argument to function
    # and bring it in from Shiny Proxy login details

    clinician_id<- 12345

    client_id<- selected_client

    posttherapy_analytics_items<- list( req(clinician_id), req(client_id), req(input$principal_diagnosis), req(input$secondary_diagnosis), req(input$referrer), req(input$attendance_schedule), req(input$non_attendances),
          req(input$attendances), req(input$premature_dropout), req(input$therapy), req(input$funding), req(input$out_of_pocket) ) %>% purrr::set_names(c("clinician_id", "client_id", "principal_diagnosis",
          "secondary_diagnosis", "referrer", "attendance_schedule", "non_attendances", "attendances", "premature_dropout", "therapy", "funding", "out_of_pocket"))


    list( posttherapy_analytics_items ) %>% {

      tibble::tibble(

        clinician_id = purrr::map_chr(., "clinician_id"),
        client_id = purrr::map_chr(., "client_id"),
        principal_diagnosis = purrr::map_chr(., "principal_diagnosis"),
        secondary_diagnosis = purrr::map_chr(., "secondary_diagnosis"),
        referrer = purrr::map_chr(., "referrer"),
        attendance_schedule = purrr::map_chr(., "attendance_schedule"),
        non_attendances = purrr::map_dbl(., "non_attendances"),
        attendances = purrr::map_dbl(., "attendances"),
        premature_dropout = purrr::map_chr(., "premature_dropout"),
        therapy = purrr::map_chr(., "therapy"),
        funding = purrr::map_chr(., "funding"),
        out_of_pocket = purrr::map_dbl(., "out_of_pocket")

      )

    }


    })

}

