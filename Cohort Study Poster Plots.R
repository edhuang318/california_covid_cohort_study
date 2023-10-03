library(tidyverse)
#install.packages("ggsignif")
library(ggsignif)

#don't forget to add error bars
#make IRR bar graph
irr_upper_ci_values <- c(0.1978, 0.1006, 0.1075, 0.4881, 0.2263, 0.7200, 0.3166, 0.1205, 0.1658)
irr_lower_ci_values <- c(0.1957, 0.0879, 0.1023, 0.4557, 0.1013, 0.5710, 0.3151, 0.1110, 0.1603)
irr_values <- data.frame(x = c(0.1967, 0.0941, 0.1049, 0.4716, 0.1514, 0.6412, 0.3158, 0.1156, 0.1630),
                         grouping = rep(c("Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 3),
                         subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths", "Vaccinations vs. Hospitalizations"))
ggplot(irr_values, aes(x = grouping, y = x, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes( x= grouping, ymin = irr_lower_ci_values, ymax = irr_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
  ggtitle("Incidence Risk Ratios During the California COVID-19 Epidemic", ) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  xlab("Time Period") +
  ylab("Incidence Risk Ratio") +
  labs(fill = "Variables Compared") +
  theme(legend.position = c(0.25, 0.6), 
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray',)) +
  geom_signif(comparisons = list(c("Delta Wave", "Pre-Delta Period", "Omicron Wave")), 
              map_signif_level=TRUE,
              annotations = "***")





#Sample Code I found on the internet
  # sp + geom_hline(yintercept=20, linetype="dashed", 
  #                 color = "red", size=2)
  # my_sum <- data %>%
  #   group_by(Species) %>%
  #   summarise( 
  #     n=n(),
  #     mean=mean(Sepal.Length),
  #     sd=sd(Sepal.Length)
  #   ) %>%
  #   mutate( se=sd/sqrt(n))  %>%
  #   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  # 
# # Standard deviation
# ggplot(my_sum) +
#   geom_bar( aes(x=Species, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
#   geom_errorbar( aes(x=Species, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
#   ggtitle("using standard deviation")



# Create data
# gfg <- data.frame(x = c(7,9,8,1,4,6,5,9,6,4,8,5), 
#                   grp = rep(c("group 1", "group 2",
#                               "group 3","group 4"),
#                             each = 3),
#                   subgroup = LETTERS[1:3])
# 
# # Create grouped barplot using ggplot2
# ggplot(gfg,aes(x = grp, y =x, fill = subgroup)) +
#   geom_bar(stat = "identity", position = "dodge")

# 
# 
# # Standard Error
# ggplot(my_sum) +
#   geom_bar( aes(x=Species, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
#   geom_errorbar( aes(x=Species, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
#   ggtitle("using standard error")
# 
# # Confidence Interval
# ggplot(my_sum) +
#   geom_bar( aes(x=Species, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
#   geom_errorbar( aes(x=Species, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
#   ggtitle("using confidence interval")




#make OR bar graph
or_upper_ci_values <- c(0.1907, 0.1006, 0.1072, 0.4871, 0.2263, 0.7199, 0.2718, 0.1203, 0.1649)
or_lower_ci_values <- c(0.1886, 0.0879, 0.1020, 0.4547, 0.1013, 0.5710, 0.2704, 0.1108, 0.1595)
or_values <- data.frame(x = c(0.1896, 0.0940, 0.1046, 0.4706, 0.1514, 0.6411, 0.2711, 0.1155, 0.1622),
                         grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 3),
                         subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths", "Vaccinations vs. Hospitalizations"))
ggplot(or_values, aes(x = grouping, y = x, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes( x= grouping, ymin = or_lower_ci_values, ymax = or_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
  ggtitle("Odds Ratios During the California COVID-19 Epidemic", ) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
  xlab("Time Period") +
  ylab("Odds Ratio") +
  labs(fill = "Variables Compared") +
  theme(legend.position = c(0.25, 0.6), 
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray',)) +
  geom_signif(comparisons = list(c("Delta Wave", "Pre-Delta Period", "Omicron Wave")), 
              map_signif_level=TRUE,
              annotations = "***")

# #make attributable risk in the exposed bar graph
# are_upper_ci_values <- c(-3.5538, -0.0456, -3.5977,-0.3130, -0.2059, -0.0070, 3.3327, -0.0074, -13.2636, -0.1130, -1.5218, -0.5183)
# are_lower_ci_values <- c(-3.5791, -0.0483, -3.8377, -0.3201, -0.2195, -0.0083, 1.3787, -0.0115, -13.3286, -0.1187, -1.5985, -0.5308)
# are_values <- data.frame(x = c(-3.5665, -0.0470, -3.7177, -0.3166, -0.2127, -0.0077, 2.3557, -0.0095, -13.2961, -0.1158, -1.5601,-0.5246),
#                         grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 4),
#                         subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths","Vaccinations vs. Hospitalizations in Positive Cases", "Vaccinations vs. Hospitalizations in General Population"))
# ggplot(are_values, aes(x = grouping, y = x, fill = subgroup)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes( x= grouping, ymin = are_lower_ci_values, ymax = are_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
#   ggtitle("Attributable Risk in the Exposed During the California COVID-19 Epidemic", ) +
# #  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
#   xlab("Time Period") +
#   ylab("Attributable Risk") +
#   labs(fill = "Variables Compared") +
#   theme_bw()
# 
# #make attributable fraction of exposed bar graph
# afe_upper_ci_values <- c(-405.6554, -893.8703, -83.0698, -830.1302, -104.8791, -341.8631, 34.1579, -38.8891, -215.8905, -730.0374, -90.5348, -503.1798)
# afe_lower_ci_values <- c(-411.1091, -1037.1371, -92.0758, -877.0432, -119.4645, -887.1255, 17.8437, -75.1209, -217.3573, -801.2995, -96.9404, -523.6594)
# afe_values <- data.frame(x = c(-408.3749, -963.0930, -87.5187, -853.2981, -112.0464, -560.4350, 26.4518, -55.9564, -216.6231, -764.9348, -93.7111, -513.3341),
#                          grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 4),
#                          subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths","Vaccinations vs. Hospitalizations in Positive Cases", "Vaccinations vs. Hospitalizations in General Population"))
# ggplot(afe_values, aes(x = grouping, y = x, fill = subgroup)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes( x= grouping, ymin = afe_lower_ci_values, ymax = afe_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
#   ggtitle("Attributable Fraction in the Exposed During the California COVID-19 Epidemic", ) +
# #  geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 1) +
#   xlab("Time Period") +
#   ylab("Attributable Fraction(%)") +
#   labs(fill = "Variables Compared") +
#   theme_bw()
# 
# #make attributable risk in population bar graph
# arp_upper_ci_values <- c(-2.2757, -0.0287, -0.8723, -0.1994, -0.0089, 0.0000, 0.2647, 0.0003, -10.1194, -0.0855, -0.7512, -0.3941)
# arp_lower_ci_values <- c(-2.3017, -0.0316, -1.0649, -0.2069, -0.0153, -0.0009, -0.1342, -0.0014, -10.1847, -0.0914, -0.8243, -0.4069)
# arp_values <- data.frame(x = c(-2.2887, -0.0301, -0.9686, -0.2031, -0.0121, -0.0004, 0.0652, -0.0005, -10.1520, -0.0884, -0.7877, -0.4005),
#                          grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 4),
#                          subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths","Vaccinations vs. Hospitalizations in Positive Cases", "Vaccinations vs. Hospitalizations in General Population"))
# ggplot(arp_values, aes(x = grouping, y = x, fill = subgroup)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes( x= grouping, ymin = arp_lower_ci_values, ymax = arp_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
#   ggtitle("Attributable Risk in the Population During the California COVID-19 Epidemic", ) +
# #  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
#   xlab("Time Period") +
#   ylab("Attributable Risk") +
#   labs(fill = "Variables Compared") +
#   theme_bw()
# #make attributable fraction in population bar graph
# afp_upper_ci_values <- c(-106.1007, -136.5578, -13.4066, -134.0489, -3.0052, -4.6973, 1.3954, -1.6463, -109.1246, -204.5068, -31.5659, -175.4499)
# afp_lower_ci_values <- c(-106.6888, -141.1991, -14.2805, -135.8850, -3.2018, -5.4630, 0.5755, -2.5287, -109.6238, -211.4070, -33.0785, -178.6469)
# afp_values <- data.frame(x = c(-106.3945, -138.8672, -13.8427, -134.9652, -3.1035, -5.0795, 0.9863, -2.0865, -109.3740, -207.9376, -32.3200, -177.0438),
#                          grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 4),
#                          subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths","Vaccinations vs. Hospitalizations in Positive Cases", "Vaccinations vs. Hospitalizations in General Population"))
# ggplot(afp_values, aes(x = grouping, y = x, fill = subgroup)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes( x= grouping, ymin = afp_lower_ci_values, ymax = afp_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
#   ggtitle("Attributable Fraction in the Population During the California COVID-19 Epidemic", ) +
# #  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
#   xlab("Time Period") +
#   ylab("Attributable Fraction(%)") +
#   labs(fill = "Variables Compared") +
#   theme_bw()

#make preventive fraction bar graph
pf_upper_ci_values <- c(80.4347, 91.2060, 89.7650, 54.4346, 89.8696, 42.8966, 68.4898, 88.9049, 83.9656)
pf_lower_ci_values <- c(80.2237, 89.9383, 89.2488, 51.1907, 77.3686, 28.0001, 68.3435, 87.9524, 83.4212)
pf_values <- data.frame(x = c(80.3295, 90.5935, 89.5101, 52.8405, 84.8585, 35.8795, 68.4167, 88.4384, 83.6957),
                         grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 3),
                         subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths", "Vaccinations vs. Hospitalizations"))
ggplot(pf_values, aes(x = grouping, y = x, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes( x= grouping, ymin = pf_lower_ci_values, ymax = pf_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
  ggtitle("Preventive Fraction Among Unexposed During the California COVID-19 Epidemic", ) +
  #  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  xlab("Time Period") +
  ylab("Preventive Fraction(%)") +
  labs(fill = "Variables Compared") +
#  theme(legend.position = c(0.4, 0.9)) +
  geom_signif(comparisons = list(c("Delta Wave", "Pre-Delta Period", "Omicron Wave")), 
              map_signif_level=TRUE,
              annotations = "***") +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray',))

#make population prevented fraction plot
ppf_df <- c(59.4011,77.5309,75.3524,51.3769,84.0894,34.5416,33.8728,64.3976,54.8299)
ppf_lower_ci_values <- c(59.2394,76.1981,74.8365,49.7238,76.3055,26.8148,33.7954,63.3137,54.3329)
ppf_upper_ci_values <- c(59.5622,78.7891,75.8577,52.9757,89.3162,41.4526,33.9501,65.4495,55.3215)
ppf_values <- data.frame(x = ppf_df,
                        grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 3),
                        subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths", "Vaccinations vs. Hospitalizations"))
ggplot(ppf_values, aes(x = grouping, y = x, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes( x= grouping, ymin = ppf_lower_ci_values, ymax = ppf_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
  ggtitle("Population Prevented Fraction During the California COVID-19 Epidemic", ) +
  #  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  xlab("Time Period") +
  ylab("Prevented Fraction(%)") +
  labs(fill = "Variables Compared") +
#  theme(legend.position = c(0.4, 0.75)) +
  geom_signif(comparisons = list(c("Delta Wave", "Pre-Delta Period", "Omicron Wave")), 
              map_signif_level=TRUE,
              annotations = "***") +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray',))

#make absolute risk reduction plot
arr_df <- c(1.2778,0.0168,0.1134,0.2006,0.0072,0.0089,3.1441,0.0274,0.1240)
arr_lower_ci_values <- c(1.2713,0.0162,0.1118,0.1938,0.0066,0.0069,3.1290,0.0264,0.1218)
arr_upper_ci_values <- c(1.2843,0.0174,0.1150,0.2074,0.0079,0.0109,3.1592,0.0283,0.1263)
arr_values <- data.frame(x = arr_df,
                         grouping = rep(c( "Delta Wave", "Pre-Delta Period", "Omicron Wave"), each = 3),
                         subgroup = c("Vaccinations vs. Cases", "Vaccinations vs. Deaths", "Vaccinations vs. Hospitalizations"))
ggplot(arr_values, aes(x = grouping, y = x, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes( x= grouping, ymin = arr_lower_ci_values, ymax = arr_upper_ci_values), width = 0.5, color = "black", alpha = 0.5, size = 0.5, position = position_dodge(0.9)) +
  ggtitle("Absolute Risk Reduction During the California COVID-19 Epidemic", ) +
  #  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  xlab("Time Period") +
  ylab("Absolute Risk Reduction (per 100 individuals)") +
  labs(fill = "Variables Compared") +
  #  theme(legend.position = c(0.4, 0.75)) +
  geom_signif(comparisons = list(c("Delta Wave", "Pre-Delta Period", "Omicron Wave")), 
              map_signif_level=TRUE,
              annotations = "***") +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray',))