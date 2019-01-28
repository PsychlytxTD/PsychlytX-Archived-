cutoff_ids<- rep("Anxiety", 5)


stringr::str_replace(cutoff_ids, "Anxiety", c("Anxiety_1", "Anxiety_2", "Anxiety_3", "Anxiety_4",
                                              "Anxiety_5"))

labs<- 1:5
subscale_name<- "Anxiety"

a<- paste("Anxiety", "_", labs, sep = "")


rep(paste(subscale_name, "_", 1:5, sep = ""), 2)
