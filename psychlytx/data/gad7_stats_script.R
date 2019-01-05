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
