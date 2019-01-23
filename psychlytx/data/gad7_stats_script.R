#Define default mean values that vary by population

male_general_population<- 3.01
female_general_population<- 4.07
older_adult<- 2
primary_care<- 5.75
psychiatric<- 10.86
Generalized_Anxiety_Disorder<- 12.59
chronic_musculoskeletal_pain<- 2.6
coronary_heart_disease<- 11.9
type_1_diabetes<- 4.7
type_2_diabetes<- 4.5
stroke<- 3.87

gad7_means_df<- data.frame(male_general_population, female_general_population, older_adult, primary_care, psychiatric, Generalized_Anxiety_Disorder,
             chronic_musculoskeletal_pain, coronary_heart_disease, type_1_diabetes, type_2_diabetes, stroke)


#Define default sd values that vary by population


male_general_population<- 3.12
female_general_population<- 3.53
older_adult<- 2.88
primary_care<- 4.76
psychiatric<- 5.62
Generalized_Anxiety_Disorder<- 3.96
chronic_musculoskeletal_pain<- 2.3
coronary_heart_disease<- 5.3
type_1_diabetes<- 4.6
type_2_diabetes<- 4.9
stroke<- 4.52


gad7_sds_df<- data.frame(male_general_population, female_general_population, older_adult, primary_care, psychiatric, Generalized_Anxiety_Disorder,
                         chronic_musculoskeletal_pain, coronary_heart_disease, type_1_diabetes, type_2_diabetes, stroke)



#Define references for the means and sds that vary by population

male_general_population<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
female_general_population<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
older_adult<- "Wild, Eckl, Herzog, Niehoff et al (2012)"
primary_care<- "Jordan, Shedden-Mora & Löwe (2017)"
psychiatric<- "Beard & Björgvinsson (2014)"
Generalized_Anxiety_Disorder<- "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"
chronic_musculoskeletal_pain<- "Bair, Wu, Damush, Sutherland & Kroenke (2008)"
coronary_heart_disease<- "Conventry, Lovell, Dickens, Bower et al (2015)"
type_1_diabetes<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
type_2_diabetes<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
stroke<- "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"

gad7_refs_df<- data.frame(male_general_population, female_general_population, older_adult, primary_care, psychiatric, Generalized_Anxiety_Disorder,
                 chronic_musculoskeletal_pain, coronary_heart_disease, type_1_diabetes, type_2_diabetes, stroke)


#Define default reliability valies that vary by population


male_general_population<- .83
female_general_population<- .83
older_adult<- .83
primary_care<- .83
psychiatric<- .83
Generalized_Anxiety_Disorder<- .83
chronic_musculoskeletal_pain<- .83
coronary_heart_disease<- .83
type_1_diabetes<- .83
type_2_diabetes<- .83
stroke<- .83

gad7_rels_df<- data.frame(male_general_population, female_general_population, older_adult, primary_care, psychiatric, Generalized_Anxiety_Disorder,
                          chronic_musculoskeletal_pain, coronary_heart_disease, type_1_diabetes, type_2_diabetes, stroke)


#Define references for the reliability values that vary by population (there will typically be repetition here because reliabilities
#are rarer than means and sds in research papers)


male_general_population<- "Spitzer, Kroenke, Williams & Löwe (2006)"
female_general_population<- "Spitzer, Kroenke, Williams & Löwe (2006)"
older_adult<- "Spitzer, Kroenke, Williams & Löwe (2006)"
primary_care<- "Spitzer, Kroenke, Williams & Löwe (2006)"
psychiatric<- "Spitzer, Kroenke, Williams & Löwe (2006)"
Generalized_Anxiety_Disorder<- "Spitzer, Kroenke, Williams & Löwe (2006)"
chronic_musculoskeletal_pain<- "Spitzer, Kroenke, Williams & Löwe (2006)"
coronary_heart_disease<- "Spitzer, Kroenke, Williams & Löwe (2006)"
type_1_diabetes<- "Spitzer, Kroenke, Williams & Löwe (2006)"
type_2_diabetes<- "Spitzer, Kroenke, Williams & Löwe (2006)"
stroke<- "Spitzer, Kroenke, Williams & Löwe (2006)"

gad7_refs_rels_df<- data.frame(male_general_population, female_general_population, older_adult, primary_care, psychiatric, Generalized_Anxiety_Disorder,
                               chronic_musculoskeletal_pain, coronary_heart_disease, type_1_diabetes, type_2_diabetes, stroke)
