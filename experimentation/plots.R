library(dplyr)
library(tibble)
library(chron)
library(ggrepel)
library(ggplot2)


scale<- c("PHQ", "GAD7", "PHQ9", "GAD7", "GAD7")
date<- c("02/02/2003", "02/02/2003", "01/01/2004", "01/01/2004", "04/02/2005")
date<- as.character(date)
score<- c(10, 12, 8, 8, 3)
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
confidence<- c(1.96, 1.96, 1.96, 1.96, 1.96)
method<- c("nun", "nun", "nun", "nun", "nun")
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

subscale_df<- data.frame(scale, date, score, pts, se, ci, ci_upper, ci_lower, mean, mean_reference, sd, sd_reference, reliability, reliability_reference,
                  confidence, method, cutoff_label_1, cutoff_label_2, cutoff_label_3, cutoff_label_4, cutoff_label_5, cutoff_label_6,
                  cutoff_value_1, cutoff_value_2, cutoff_value_3, cutoff_value_4, cutoff_value_5, cutoff_value_6,
                  cutoff_reference_1, cutoff_reference_2, cutoff_reference_3, cutoff_reference_4, cutoff_reference_5, cutoff_reference_6,
                  stringsAsFactors = F) 

subscale_df<- filter(subscale_df, scale == "GAD7")


plot_subscale<- function(subscale_df, params_list) {


#Convert date variable to date class

subscale_df <- transform(subscale_df, date = chron(date, format = "d/m/Y"))

#calculate change variable

subscale_df<- subscale_df %>% dplyr::mutate(change = score - lag(score)) %>% dplyr::select(scale, 
date, score, change, everything())

#Calculate the ymin and ymax values to use with cutoff shading

ymin_cutoff_1<- subscale_df$cutoff_value_1[length(subscale_df$cutoff_value_1)]
ymax_cutoff_1<- subscale_df$cutoff_value_2[length(subscale_df$cutoff_value_1)] - 0.1
ymin_cutoff_2<- subscale_df$cutoff_value_2[length(subscale_df$cutoff_value_2)]
ymax_cutoff_2<- subscale_df$cutoff_value_3[length(subscale_df$cutoff_value_3)] - 0.1
ymin_cutoff_3<- subscale_df$cutoff_value_3[length(subscale_df$cutoff_value_3)] 
ymax_cutoff_3<- subscale_df$cutoff_value_4[length(subscale_df$cutoff_value_4)] - 0.1
ymin_cutoff_4<- subscale_df$cutoff_value_4[length(subscale_df$cutoff_value_4)]
ymax_cutoff_4<- subscale_df$cutoff_value_5[length(subscale_df$cutoff_value_5)] - 0.1
ymin_cutoff_5<- subscale_df$cutoff_value_5[length(subscale_df$cutoff_value_5)]
ymax_cutoff_5<- subscale_df$cutoff_value_6[length(subscale_df$cutoff_value_6)] - 0.1
ymin_cutoff_6<- subscale_df$cutoff_value_6[length(subscale_df$cutoff_value_6)]
ymax_cutoff_6<- Inf


#Need to make sure confidence intervals don't disappear from plot - so manually increase y-axis limits
#to the max/min score plus/minus 10% (this should be enough)

y_axis_upper_expansion<- params_list$max_score + (params_list$max_score * 0.1)
y_axis_lower_expansion<- params_list$min_score - (params_list$min_score * 0.1)


#Need to increase space between 0 and first tick on x-axis to make space for cutoff labels

x_axis_lower_expansion<- subscale_df$date[1] - 186


#Plot breaks & break labs - need to replace this with the minimum and maximum possible scores on the scale

plotting_increment<- params_list$max_score * 0.2
plotting_breaks<- seq(from = params_list$min_score, to = params_list$max_score, by = round(plotting_increment, 0))
plotting_break_labels<- paste(plotting_breaks)



plot<- ggplot(subscale_df, aes(x = date, y = score, group = 1)) + theme_bw() + annotate("rect", xmin = -Inf, xmax = Inf, #Make the cutoff shading rectangles
     ymin = ymin_cutoff_1, ymax = ymax_cutoff_1, alpha = 0.8, fill= "#ffffd4") + annotate("rect", xmin = -Inf, xmax = Inf, 
     ymin = ymin_cutoff_2, ymax = ymax_cutoff_2, alpha = 0.8, fill= "#fee391") + annotate("rect", xmin = -Inf, xmax = Inf, 
     ymin = ymin_cutoff_3, ymax = ymax_cutoff_3, alpha = 0.8, fill= "#fec44f") + annotate("rect", xmin = -Inf, xmax = Inf, 
     ymin = ymin_cutoff_4, ymax = ymax_cutoff_4, alpha = 0.8, fill= "#fe9929") + annotate("rect", xmin = -Inf, xmax = Inf, 
     ymin = ymin_cutoff_5, ymax = ymax_cutoff_5, alpha = 0.8, fill= "#d95f0e") + annotate("rect", xmin = -Inf, xmax = Inf, 
     ymin = ymin_cutoff_6, ymax = ymax_cutoff_6, alpha = 0.8, fill= "#993404") + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), #Make lines and error bars
     width = 10, size = 1, color = "#00004c") + geom_line(color = "#00004c", size = 1.8) + geom_point(shape=21, fill="#00004c", color="#00004c",  #Make points
     size=4) + expand_limits(y=c(y_axis_lower_expansion, y_axis_upper_expansion)) + theme(panel.border = element_rect(colour = "#00004c", fill=NA, #Expand y limits
     size=3), axis.text.x = element_text(angle = 70, hjust = 1, size = 13, family = "Palatino", colour = "black"), axis.text.y = element_text(size = 13, #Style axes, labels and border
     family = "Palatino", colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 15, 
     family = "Palatino")) + ggrepel::geom_label_repel(aes(label = paste0(subscale_df$score)), #Make labels for scores 
     size = 4, family = "Palatino") + scale_y_continuous(breaks = plotting_breaks, #Customise y-axis breaks and labels 
     labels = plotting_break_labels) + scale_x_chron(breaks = subscale_df$date, #Customise x-axis date breaks and labels to be the same as the data
     format = "%d/%m/%Y") + xlab("Date") + ylab(paste(params_list$subscale_name, "Score")) #Make x and y plot labels sentence case


#Position cutoff labels

plot<- plot + geom_text(aes(label = cutoff_label_1, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_1 ), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_2, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_2), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_3, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_3), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_4, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_4), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_5, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_5), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_6, 
                      x = as.Date(x_axis_lower_expansion), y = cutoff_value_6), nudge_y = 0.7, family = "Palatino", size = 5)

plot

}


plot_subscale(subscale_df = subscale_df, params_list = psychlytx::gad7_params)

















#Convert date variable to date class

data <- transform(data, date = chron(date, format = "d/m/Y"))

#Only use data from the selected scale

subscale_df<- dplyr::filter(data, scale == "GAD7")


#calculate change variable

subscale_df<- subscale_df %>% mutate(change = score - lag(score)) %>% dplyr::select(scale, 
                                                                                    date, score, change, everything())

ymin_cutoff_1<- subscale_df$cutoff_value_1[length(subscale_df$cutoff_value_1)]
ymax_cutoff_1<- subscale_df$cutoff_value_2[length(subscale_df$cutoff_value_1)] - 0.1
ymin_cutoff_2<- subscale_df$cutoff_value_2[length(subscale_df$cutoff_value_2)]
ymax_cutoff_2<- subscale_df$cutoff_value_3[length(subscale_df$cutoff_value_3)] - 0.1
ymin_cutoff_3<- subscale_df$cutoff_value_3[length(subscale_df$cutoff_value_3)] 
ymax_cutoff_3<- subscale_df$cutoff_value_4[length(subscale_df$cutoff_value_4)] - 0.1
ymin_cutoff_4<- subscale_df$cutoff_value_4[length(subscale_df$cutoff_value_4)]
ymax_cutoff_4<- subscale_df$cutoff_value_5[length(subscale_df$cutoff_value_5)] - 0.1
ymin_cutoff_5<- subscale_df$cutoff_value_5[length(subscale_df$cutoff_value_5)]
ymax_cutoff_5<- subscale_df$cutoff_value_6[length(subscale_df$cutoff_value_6)] - 0.1
ymin_cutoff_6<- subscale_df$cutoff_value_6[length(subscale_df$cutoff_value_6)]
ymax_cutoff_6<- Inf


#Need to make sure confidence intervals don't disappear from plot - so manually increase y-axis limits
#to the max/min score plus/minus 10% (this should be enough)

y_axis_upper_expansion<- max(subscale_df$score) + max(subscale_df$score * 0.1)
y_axis_lower_expansion<- min(subscale_df$score) - min(subscale_df$score * 0.1)

#Need to increase space between 0 and first tick on x-axis to make space for cutoff labels

x_axis_lower_expansion<- subscale_df$date[1] - 186

x_axis_upper_expansion<- subscale_df$date[1] + 186

#Plot breaks & break labs - need to replace this with the minimum and maximum possible scores on the scale

plotting_increment<- max(subscale_df$score * 0.2)
plotting_breaks<- seq(from = min(subscale_df$score), to = max(subscale_df$score), by = round(plotting_increment, 0))
plotting_break_labels<- paste(plotting_breaks)



plot<- ggplot(subscale_df, aes(x = date, y = score, group = 1)) + theme_bw() + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                        ymin = ymin_cutoff_1, ymax = ymax_cutoff_1, alpha = 0.8, fill= "#ffffd4") + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                                                                                                             ymin = ymin_cutoff_2, ymax = ymax_cutoff_2, alpha = 0.8, fill= "#fee391") + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                                                                                                                                                                                                  ymin = ymin_cutoff_3, ymax = ymax_cutoff_3, alpha = 0.8, fill= "#fec44f") + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                                                                                                                                                                                                                                                                                       ymin = ymin_cutoff_4, ymax = ymax_cutoff_4, alpha = 0.8, fill= "#fe9929") + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                                                                                                                                                                                                                                                                                                                                                                            ymin = ymin_cutoff_5, ymax = ymax_cutoff_5, alpha = 0.8, fill= "#d95f0e") + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ymin = ymin_cutoff_6, ymax = ymax_cutoff_6, alpha = 0.8, fill= "#993404") + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           width = 10, size = 1, color = "#00004c") + geom_line(color = "#00004c", size = 1.8) + geom_point(shape=21, fill="#00004c", color="#00004c", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            size=4) + expand_limits(y=c(y_axis_lower_expansion, y_axis_upper_expansion)) + theme(panel.border = element_rect(colour = "#00004c", fill=NA, size=3), axis.text.x = 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   element_text(angle = 70, hjust = 1, size = 13, family = "Palatino", colour = "black"), axis.text.y = element_text(size = 13, family = "Palatino", colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 15, family = "Palatino")) + ggrepel::geom_label_repel(aes(label = paste0(subscale_df$score)), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       size = 4, family = "Palatino") + scale_y_continuous(breaks = plotting_breaks, labels = plotting_break_labels) + scale_x_chron(breaks = subscale_df$date, format = "%d/%m/%Y") + xlab("Date") + ylab("Score")


#Position cutoff labels

plot<- plot + geom_text(aes(label = cutoff_label_1, 
                            x = as.Date(x_axis_lower_expansion), y = cutoff_value_1 ), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_2, 
                                                                                                                                                     x = as.Date(x_axis_lower_expansion), y = cutoff_value_2), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_3, 
                                                                                                                                                                                                                                                                             x = as.Date(x_axis_lower_expansion), y = cutoff_value_3), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_4, 
                                                                                                                                                                                                                                                                                                                                                                                                     x = as.Date(x_axis_lower_expansion), y = cutoff_value_4), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_5, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x = as.Date(x_axis_lower_expansion), y = cutoff_value_5), nudge_y = 0.7, family = "Palatino", size = 5) + geom_text(aes(label = cutoff_label_6, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x = as.Date(x_axis_lower_expansion), y = cutoff_value_6), nudge_y = 0.7, family = "Palatino", size = 5)

plot
