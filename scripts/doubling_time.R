# To find the doubling time of bacteria or fungi use the labelled values in abscissa 
slope <- coef(lm(log10(mean_density) ~ sample, data = group_summary_num[group_summary_num$sample %in% c(0, 4), ]))[2]
log10(2) / slope