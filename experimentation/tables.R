library(dplyr)
library(tibble)
library(chron)
library(ggrepel)
library(ggplot2)
library(magrittr)
library(purrr)
library(tidyr)
library(purrrlyr)
library(kableExtra)
library(stringr)


scale<- c("PHQ9", "GAD7", "PHQ9", "GAD7", "GAD7")
date<- c("02/02/2003", "02/02/2003", "01/01/2004", "01/01/2004", "04/02/2005")
date<- as.character(date)
score<- c(10, 12, 8, 8, 8)
pts<- c(11, 13, 9, 5, 4)
se<- c(1.3, 1.8, 1.3, 1.5, 1.5)
ci<- c(3.41, 2.8, 3.2, 3.6, 3.6)
ci_upper<- c(14, 13, 15, 9, 4)
ci_lower<- c(10, 11, 9, 7, 1)
mean<-c(9, 9, 9, 9, 9)
mean_reference<- c("yes", "yes", "yes", "yes", "yes")
sd<- c(3, 3, 3, 3, 3)
sd_reference<- c("yes", "yes", "yes", "yes", "yes")
reliability<- c(.9, .9, .9, .9, .9)
reliability_reference<- c("yes", "yes", "yes", "yes", "yes")
confidence<- c(1.96, 1.645, 1.96, 2.575, 1.645)
method<- c("nun", "nun", "nun", "nun", "nun")
population<- c("veteran", "veteran", "veteran", "veteran", "veteran")
cutoff_label_1<- c("mod", "mod", "mod", "mod", "mod")
cutoff_label_2<- c("med", "med", "med", "med", "med")
cutoff_label_3<- c("ced", "ced", "ced", "ced", "ced")
cutoff_label_4<- c("red", "red", "red", "red", "red")
cutoff_label_5<- c("bed", "bed", "bed", "bed", "bed")
cutoff_label_6<- c("fed", "fed", "fed", "fed", "fed")
cutoff_value_1<- c(2, 2, 2, 2, 2)
cutoff_value_2<- c(4, 4, 4, 4, 4)
cutoff_value_3<- c(9, 9, 9 , 9, 9)
cutoff_value_4<- c(12, 12, 12, 12, 12)
cutoff_value_5<- c(14, 14, 14, 14, 14)
cutoff_value_6<- c(16, 16, 16, 16, 16)
cutoff_reference_1<- c("jod", "jod", "jod", "jod", "jod")
cutoff_reference_2<- c("jed", "jed", "jed", "jed", "jed")
cutoff_reference_3<- c("ked", "ked", "ked", "ked", "ked")
cutoff_reference_4<- c("led", "led", "led", "led", "led")
cutoff_reference_5<- c("aed", "aed", "aed", "aed", "aed")
cutoff_reference_5<- c("oed", "oed", "oed", "oed", "oed")
cutoff_reference_6<- c("wed", "wed", "wed", "wed", "wed")


#create sample df - same as what we'll get from db

data<- data.frame(scale, date, score, pts, se, ci, ci_upper, ci_lower, mean, mean_reference, sd, sd_reference, reliability, reliability_reference,
                         confidence, method, population, cutoff_label_1, cutoff_label_2, cutoff_label_3, cutoff_label_4, cutoff_label_5, cutoff_label_6,
                         cutoff_value_1, cutoff_value_2, cutoff_value_3, cutoff_value_4, cutoff_value_5, cutoff_value_6,
                         cutoff_reference_1, cutoff_reference_2, cutoff_reference_3, cutoff_reference_4, cutoff_reference_5, cutoff_reference_6,
                         stringsAsFactors = F) 

#Nest the dataframe: create a list column of dataframes - one per each subscale.

subscale_df<- data %>% dplyr::group_by(scale) %>% tidyr::nest() %>% dplyr::mutate(subscale_info = list(psychlytx::gad7_info))


#Add a 'change' variable to each dataframe showing statistically reliable change in scores 

subscale_df<- subscale_df %>% dplyr::mutate( data = purrr::map(data, ~ psychlytx::make_change_variable(.x))) 

#Change the value of the confidence variable so that percentages (not decimals) appear in the report 

subscale_df<- subscale_df %>% dplyr::mutate( data = purrr::map(data, ~ dplyr::mutate(.,
  confidence = dplyr::case_when(
  confidence == 1.645 ~ "99%", 
  confidence == 1.96 ~ "95%",
  confidence == 2.575 ~ "90%"))))

#Create a seperate plot for each subscale and store the plots in a list column

subscale_df<- subscale_df %>% dplyr::mutate(plot = purrr::map2(data, subscale_info, ~ psychlytx::plot_subscale(.x, .y)))


#Make a seperate scores table for each subscale and store the tables in a list column

subscale_df<- subscale_df %>% dplyr::mutate(scores_table = purrr::map(data, ~ dplyr::select(., Date = date, Score = score, Change = change, 
              `CI Upper Limit` = ci_upper, `CI Lower Limit` = ci_lower, `Predicted True Score` = pts, 
              `Standard Error` = se) %>% dplyr::mutate(Change = kableExtra::cell_spec(Change, "html", color = 
               if_else(is.na(Change), "gray", if_else(grepl("\\*", Change), "red", "gray")))) %>% kableExtra::kable(., format = "html", 
              booktabs = T, escape = F, caption "hello") %>% kableExtra::kable_styling(full_width = F)))

#Make a statistical info table for each subscale and store the tables in a list column

subscale_df<- subscale_df %>% dplyr::mutate(statistics_table_1 = purrr::map(data, ~dplyr::select(., Date = date, `Reference Population` = population, 
                                            Mean = mean, Sd = sd, `Mean Reference` = mean_reference, `Sd Reference` = sd_reference) %>% kableExtra::kable(., format = "html", 
              booktabs = T) %>% kableExtra::kable_styling(full_width = F) %>% add_header_above(c("Data & References Used In Reliable 
              Change Calculations" = 6)) %>% add_header_above(c("Table 2" = 6))))

subscale_df<- subscale_df %>% dplyr::mutate(statistics_table_2 = purrr::map(data, ~dplyr::select(., Date = date,
             `Retest Reliability` = reliability, `Reliability Reference` = reliability_reference, Confidence = confidence, 
             `Reliable Change Method` = method) %>% kableExtra::kable(., format = "html", 
              booktabs = T) %>% kableExtra::kable_styling(full_width = F) %>% add_header_above(c("Data & References Used In Reliable 
              Change Calculations (Contd.)" = 5)) %>% add_header_above(c("Table 2" = 5))))

subscale_df$statistics_table_2[1]

subscale_df$scores_table[1]


                                                                