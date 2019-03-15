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

  checkboxGroupInput(ns("new_client"), "", choices = c("Register A New Client"), selected = character(0)),

  conditionalPanel(condition = "input.new_client == 'Register A New Client'", ns = ns,

                   tagList(
                     sidebarLayout(
                      sidebarPanel(width = 9,
                     h3("Registration"),
                     br(),
                     textInput(ns("first_name"), "First Name", width = '50%'),
                     textInput(ns("last_name"), "Last Name", width = '50%'),
                     selectInput(ns("sex"), "Sex", c("", "Male", "Female", "Other"), width = '20%'),
                     dateInput(ns("dob"), "Date of Birth", value = "", width = '20%'),
                     textInput(ns("postcode"), "Postcode", value = "", width = '20%'),
                     selectInput(ns("marital_status"), "Marital Status", c("", "Never Married", "Currently Married", "Separated", "Divorced", "Widowed", "Cohabiting"), width = '30%'),
                     selectInput(ns("sexuality"), "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer", "Other"), width = '30%'),
                     selectInput(ns("ethnicity"), "Ethnicity", c("", "Caucasian", "Latino/Hispanic", "Middle Eastern", "African", "Caribbean", "South Asian",
                                                                 "East Asian", "Mixed", "Other"), width = '30%'),
                     radioButtons(ns("indigenous"), "Identifies as Being of Aboriginal or Torres Strait Islander Descent", choices = c("No", "Yes"), selected = character(0), width = '20%'),
                     numericInput(ns("children"), "Number of Dependent Children", value = "", width = '20%'),
                     selectInput(ns("workforce"), "Primary Workforce Status", c("", "Working Full-Time", "Working Part-Time", "Working Casual Hours", "Studying", "Unemployed", "Retired"), width = '30%'),
                     selectInput(ns("education"), "Highest Education Level", c("", "No education", "Primary Education", "Secondary Education", "Post-Secondary/Tertiary Education",
                                                                               "Bachelor or Equivalent", "Master or Equivalent", "Doctoral or Equivalent"), width = '30%'),
                     actionButton(ns("submit_analytics_pretherapy"), "Register Client")
                      ),

                     mainPanel()

                   ))))


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

    pretherapy_analytics_items<- list( req(client_id), req(input$first_name), req(input$last_name), req(input$sex),
                                       req(input$dob), req(input$postcode), req(input$marital_status),
                                       req(input$sexuality), req(input$ethnicity), req(input$indigenous), req(input$children),
                                       req(input$workforce), req(input$education) ) %>% purrr::set_names(c("client_id", "first_name", "last_name", "sex", "dob",
                                       "postcode", "marital_status", "sexuality", "ethnicity", "indigenous", "children", "workforce", "education"))




    pretherapy_analytics_df<- list( pretherapy_analytics_items ) %>% {

      tibble::tibble(  #Create a dataframe with all the pre-therapy analytics widget values as columns
                       #Specify the variable type explicitly

      client_id = purrr::map_chr(., "client_id"),
      first_name = purrr::map_chr(., "first_name"),
      last_name = purrr::map_chr(., "last_name"),
      sex = purrr::map_chr(., "sex"),
      birth_date = format(lubridate::as_date(purrr::map_dbl(., "dob")), "%d/%m/%Y"), #Convert to date class
      postcode = purrr::map_chr(., "postcode"),
      marital_status = purrr::map_chr(., "marital_status"),
      sexuality = purrr::map_chr(., "sexuality"),
      ethnicity = purrr::map_chr(., "ethnicity"),
      indigenous_status = purrr::map_chr(., "indigenous"),
      dependent_children = purrr::map_dbl(., "children"),
      workforce_status = purrr::map_chr(., "workforce"),
      highest_education = purrr::map_chr(., "education")

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

  tagList(

    checkboxGroupInput(ns("existing_client"), "", choices = c("Select An Existing Client"), selected = character(0)),

    conditionalPanel(condition = "input.existing_client == 'Select An Existing Client'", ns = ns,

                     selectInput(ns("select_client"), "Select Your Client", choices = c("")), #If existing client, check whether it is the last assessment
                                                                                              #and if so, show post-treatment analytics panel with items

                     checkboxGroupInput(ns("last_assessment"), "", choices = c("I am administering a scale to this client
                                                                               for the last time" = "last"))),

    conditionalPanel(condition = "input.last_assessment == 'last'", ns = ns,

                     tagList(
                       sidebarLayout(
                         sidebarPanel(width = 9,
                                      tagList(
                                        h3("End-Of-Treatment Data Collection"),
                                        br(),
                                        selectInput(ns("principal_diagnosis"), "Presenting Principal Diagnosis", psychlytx::diagnosis_list, width = '60%'),
                                        selectizeInput(ns("secondary_diagnosis"), "Additional Presenting Diagnosis/Diagnoses", psychlytx::diagnosis_list, multiple = TRUE, width = '60%'),
                                        textInput(ns("referrer"), "Referrer", value = "", width = '50%'),
                                        selectInput(ns("attendance_schedule"), "Schedule of Attendance", c("", "Varied", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "Greater Than 1 Month Apart"), width = '40%'),
                                        numericInput(ns("dna"), "Number of Non-Attendances (DNAs)", value = "", width = '20%'),
                                        numericInput(ns("duration"), "Number of Sessions Attended", value = "", width = '20%'),
                                        radioButtons(ns("dropout"), "Premature Dropout", choices = c("Yes", "No"), selected = character(0), width = '20%'),
                                        selectInput(ns("therapy"), "Therapeutic Approach Used", psychlytx::therapies_list, width = '50%'),
                                        selectInput(ns("funder"), "Funding Source", choices = c("", "Entirely Self-Funded", "Partly Medicare Funded", "Entirely Medicare Funded (Bulk Billing)",
                                                                                                "Private Health Fund",
                                                                                                "WorkCover", "Transport Accident Commission (TAC)",
                                                                                                "Department of Veterans Affairs (DVA)",
                                                                                                "Victims of Crime Assistance Tribunal (VOCAT)",
                                                                                                "Other"), width = '40%'),
                                        numericInput(ns("out_of_pocket"), "Out-Of-Pocket Expense", value = "", width = '20%'),
                                        actionButton(ns("submit_analytics_posttherapy"), "Submit Data")
                                           )),

                         mainPanel()

                       ))))


}



#' New or existing client status widget and headings
#'
#' Module generates client status widget and headings.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_posttherapy<- function(input, output, session) {

  #Need to return input to make input parameters available

  eventReactive(input$submit_analytics_posttherapy, {

    posttherapy_analytics_items<- list( req(input$principal_diagnosis), req(input$secondary_diagnosis), req(input$referrer), req(input$attendance_schedule), req(input$dna),
          req(input$duration), req(input$dropout), req(input$therapy), req(input$funder), req(input$out_of_pocket) ) %>% purrr::set_names(c("principal_diagnosis",
          "secondary_diagnosis", "referrer", "attendance_schedule", "non_attendances", "duration", "dropout", "therapy", "funder", "out_of_pocket"))


    list( posttherapy_analytics_items ) %>% {

      tibble::tibble(

        principal_diagnosis = purrr::map_chr(., "principal_diagnosis"),
        secondary_diagnosis = purrr::map_chr(., "secondary_diagnosis"),
        referrer = purrr::map_chr(., "referrer"),
        attendance_schedule = purrr::map_chr(., "attendance_schedule"),
        non_attendances = purrr::map_dbl(., "non_attendances"),
        sessions_attended = purrr::map_dbl(., "duration"),
        premature_dropout = purrr::map_chr(., "dropout"),
        therapy = purrr::map_chr(., "therapy"),
        funding = purrr::map_chr(., "funder"),
        out_of_pocket_cost = purrr::map_dbl(., "out_of_pocket")

      )

    }


    })

}

