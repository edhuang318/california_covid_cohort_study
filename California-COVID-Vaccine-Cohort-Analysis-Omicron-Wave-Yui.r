#There's boosted vaccination data in this dataset. Analyze that for the paper later.

library(tidyverse)
#install.packages("epiR")
library(epiR)

#set working directory
setwd("~/Documents")
#open dataset
covidvax <- read.csv("covid19postvaxstatewidestatsnew.csv")
#add relevant columns
covidvax <- covidvax %>%
  mutate(total_cases = unvaccinated_cases + vaccinated_cases)
covidvax <- covidvax %>%
  mutate(Submission_Date = as.Date(date, "%m/%d/%Y"))
covidvax <- covidvax %>%
  mutate("cum_cases" = cumsum(total_cases))
covidvax <- covidvax %>%
  mutate(total_deaths = unvaccinated_deaths + vaccinated_deaths)
covidvax <- covidvax %>%
  mutate(population = population_unvaccinated + population_vaccinated)
covidvax <- covidvax %>%
  mutate(cum_unvax_deaths = cumsum(unvaccinated_deaths))
covidvax <- covidvax %>%
  mutate(cum_vax_deaths = cumsum(vaccinated_deaths))
covidvax <- covidvax %>%
  mutate(cum_unvax_deaths = cumsum(unvaccinated_deaths))
covidvax <- covidvax %>%
  mutate(cum_vax = cumsum(vaccinated_cases))
covidvax <- covidvax %>%
  mutate(cum_unvax = cumsum(unvaccinated_cases))

#Identify time period of omicron wave

covidvax_graph <- ggplot(covidvax, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
  geom_point(data = covidvax, aes(y = population_vaccinated, color = "Susceptible-vaccinated")) +
  geom_point(data = covidvax, aes(y = population_unvaccinated, color = "Susceptible-unvaccinated")) +
  geom_point(data = covidvax, aes(y = cum_vax, color = "Infectious-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax, color = "Infectious-unvaccinated")) +
  geom_point(data = covidvax, aes(y = cum_vax_deaths, color = "Dead-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax_deaths, color = "Dead-unvaccinated")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Susceptible-vaccinated", "Susceptible-unvaccinated", "Infectious-vaccinated", "Infectious-unvaccinated", "Recovered-vaccinated", "Recovered-unvaccinated", "Dead-vaccinated", "Dead-vaccinated"),
                     values = c("red", "orange", "black", "brown", "green", "yellow", "blue", "pink")) + theme_bw()
covidvax_graph
covidvax_graph_no_S <- ggplot(covidvax, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
  geom_point(data = covidvax, aes(y = cum_vax, color = "Infectious-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax, color = "Infectious-unvaccinated")) +
  geom_point(data = covidvax, aes(y = cum_vax_deaths, color = "Dead-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax_deaths, color = "Dead-unvaccinated")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Susceptible-vaccinated", "Susceptible-unvaccinated", "Infectious-vaccinated", "Infectious-unvaccinated", "Recovered-vaccinated", "Recovered-unvaccinated", "Dead-vaccinated", "Dead-vaccinated"),
                     values = c("red", "orange", "black", "brown", "green", "yellow", "blue", "pink")) + theme_bw()
covidvax_graph_no_S
covidvax_graph_no_S <- ggplot(covidvax, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
  geom_point(data = covidvax, aes(y = cum_vax, color = "Infectious-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax, color = "Infectious-unvaccinated")) +
  geom_point(data = covidvax, aes(y = cum_vax_deaths, color = "Dead-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax_deaths, color = "Dead-unvaccinated")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Susceptible-vaccinated", "Susceptible-unvaccinated", "Infectious-vaccinated", "Infectious-unvaccinated", "Recovered-vaccinated", "Recovered-unvaccinated", "Dead-vaccinated", "Dead-vaccinated"),
                     values = c("red", "orange", "black", "brown", "green", "yellow", "blue", "pink")) + theme_bw()
covidvax_graph_no_S
covidvax_graph_no_S <- ggplot(covidvax, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
  geom_point(data = covidvax, aes(y = cum_vax, color = "Infectious-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax, color = "Infectious-unvaccinated")) +
  geom_point(data = covidvax, aes(y = cum_vax_deaths, color = "Dead-vaccinated")) +
  geom_point(data = covidvax, aes(y = cum_unvax_deaths, color = "Dead-unvaccinated")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Susceptible-vaccinated", "Susceptible-unvaccinated", "Infectious-vaccinated", "Infectious-unvaccinated", "Recovered-vaccinated", "Recovered-unvaccinated", "Dead-vaccinated", "Dead-vaccinated"),
                     values = c("red", "orange", "black", "brown", "green", "yellow", "blue", "pink")) + theme_bw()
covidvax_graph_no_S
covidvax_graph_daily <- ggplot(covidvax, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Daily Totals, Data Only") +
  geom_point(data = covidvax, aes(y = vaccinated_cases, color = "Vaccinated Cases")) +
  geom_point(data = covidvax, aes(y = unvaccinated_cases, color = "Unvaccinated Cases")) +
  geom_point(data = covidvax, aes(y = vaccinated_deaths, color = "Vaccinated Deaths")) +
  geom_point(data = covidvax, aes(y = unvaccinated_deaths, color = "Unvaccinated Deaths")) +
  geom_point(data = covidvax, aes(y = vaccinated_hosp, color = "Vaccinated Hospitalizations")) +
  geom_point(data = covidvax, aes(y = unvaccinated_hosp, color = "Unvaccinated Hospitalizations")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Vaccinated Cases", "Unvaccinated Cases", "Vaccinated Deaths", "Unvaccinated Deaths", "Vaccinated Hospitalizations", "Unvaccinated Hospitalizations"),
                     values = c("red", "orange", "black", "brown", "green", "yellow")) + theme_bw()
covidvax_graph_daily
dec_to_feb <- covidvax %>%
  filter(Submission_Date >= "2021-12-01" & Submission_Date <= "2022-02-28")
dec_to_feb
covidvax_graph_dec_to_feb <- ggplot(dec_to_feb, aes(x=Submission_Date)) +
  labs(y="Individuals",
       x="Time",
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
  geom_point(aes(y = vaccinated_cases, color = "Vaccinated Cases")) +
  geom_point(aes(y = unvaccinated_cases, color = "Unvaccinated Cases")) +
  geom_point(aes(y = vaccinated_deaths, color = "Vaccinated Deaths")) +
  geom_point(aes(y = unvaccinated_deaths, color = "Unvaccinated Deaths")) +
  geom_point(aes(y = vaccinated_hosp, color = "Vaccinated Hospitalizations")) +
  geom_point(aes(y = unvaccinated_hosp, color = "Unvaccinated Hospitalizations")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "",
                     breaks = c("Vaccinated Cases", "Unvaccinated Cases", "Vaccinated Deaths", "Unvaccinated Deaths", "Vaccinated Hospitalizations", "Unvaccinated Hospitalizations"),
                     values = c("red", "orange", "black", "brown", "green", "yellow")) + theme_bw()
covidvax_graph_dec_to_feb
#The omicron period appears to be roughly from December through February
omicron <- covidvax %>%
  filter(Submission_Date >="2021-12-01" & Submission_Date <= "2022-02-28")
#Numbers for 2x2 Table
unvax_case <- sum(omicron$unvaccinated_cases) #1235711
unvax_case
vax_case <- sum(omicron$vaccinated_cases) #1260195
vax_case
unvax_hosp <- sum(omicron$unvaccinated_hosp) #30900
unvax_hosp
vax_hosp <- sum(omicron$vaccinated_hosp) #39851
vax_hosp
unvax_death <- sum(omicron$unvaccinated_deaths) #8328
unvax_death
vax_death <- sum(omicron$vaccinated_deaths) #3109
vax_death
pop_unvax <- omicron$population_unvaccinated[1] #6358506
pop_unvax
pop_vax <- omicron$population_vaccinated[1] #20531396
pop_vax

#Make a function for Population Prevented Fraction
ppf_ci <- function(e_pos_o_pos,e_pos_o_neg,e_neg_o_pos,e_neg_o_neg){
  ppf <- ((e_pos_o_pos + e_pos_o_neg)/(e_pos_o_pos + e_pos_o_neg + e_neg_o_pos + e_neg_o_neg)) * ((1 - ((e_pos_o_pos/(e_pos_o_pos + e_pos_o_neg)))/((e_neg_o_pos)/(e_neg_o_pos+e_neg_o_neg))))
  n <- e_pos_o_pos + e_pos_o_neg + e_neg_o_pos + e_neg_o_neg
  margin <- qnorm(0.975)*sqrt(ppf*(1-ppf)/n)
  lower_interval <- ppf + margin
  upper_interval <- ppf - margin
  print(c("Population Prevented Fraction:", ppf))
  print(c("Lower Interval:", lower_interval))
  print(c("Upper Interval:", upper_interval))
}

#Making a 2x2 table for Vaccinations vs. Cases
#Vaccinated and No Case
vax_nocase <- pop_vax - vax_case
vax_nocase #19271201
#Unvaccinated and No Case
unvax_nocase <- pop_unvax - unvax_case
unvax_nocase #5122795
# cases_table <- data.frame(x1 = c(177312,23593965),
#                           x2 = c(503266,7262508))
# cases_table
#This counts as a retrospective cohort study. BE CAREFUL WHICH VALUE GOES IN WHICH SQUARE.
#Side note: The command to comment out multiple lines is Command/Control + Shift + C.
epi.2by2(c(vax_case,vax_nocase,unvax_case,unvax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_case,vax_nocase,unvax_case,unvax_nocase)
# [1] "Population Prevented Fraction:" "0.522385857185222"             
# [1] "Lower Interval:"   "0.522574651006011"
# [1] "Upper Interval:"   "0.522197063364433"
#             Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +      1260195     19271201   20531396              6.14      0.0654
# Exposed -      1235711      5122795    6358506             19.43      0.2412
# Total          2495906     24393996   26889902              9.28      0.1023
# Population Prevented Fraction
(20531396/26889902)*(1-0.3158) #0.522411
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.3158 (0.3151, 0.3166)
# Odds ratio                                     0.2711 (0.2704, 0.2718)
# Attrib risk in the exposed *                   -13.2961 (-13.3286, -13.2636)
# Attrib fraction in the exposed (%)            -216.6231 (-217.3573, -215.8905)
# Attrib risk in the population *                -10.1520 (-10.1847, -10.1194)
# Attrib fraction in the population (%)         -109.3740 (-109.6238, -109.1246)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 1019294.232 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.32 (95% CI 0.32 to 0.32) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.27 (95% CI 0.27 to 0.27) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -13.3 (95% CI -13.33 to -13.26) per 100 population units. -216.6% of outcomes in the exposed were attributable to exposure (95% CI -217.4% to -215.9%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 8 (95% CI 8 to 8). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -10.15 (95% CI -10.18 to -10.12) per 100 population units. -109.4% of outcomes in the population were attributable to exposure (95% CI -109.6% to -109.1%). 


#2x2 Table for Vaccinations vs. Deaths
#Vaccinated and Survived
vax_surv <- pop_vax-vax_death
vax_surv #20528287
#Unvaccinated and Survived
unvax_surv <- pop_unvax-unvax_death
unvax_surv #6350178
epi.2by2(c(vax_death,vax_surv,unvax_death,unvax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_death,vax_surv,unvax_death,unvax_surv)
# [1] "Population Prevented Fraction:" "0.67525889016345"              
# [1] "Lower Interval:"  "0.67543588364491"
# [1] "Upper Interval:"   "0.675081896681989"
#               Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         3109     20528287   20531396            0.0151    0.000151
# Exposed -         8328      6350178    6358506            0.1310    0.001311
# Total            11437     26878465   26889902            0.0425    0.000426
# Population Prevented Fraction
(20531396/26889902)*(1-0.1156) #0.6752708
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.1156 (0.1110, 0.1205)
# Odds ratio                                     0.1155 (0.1108, 0.1203)
# Attrib risk in the exposed *                   -0.1158 (-0.1187, -0.1130)
# Attrib fraction in the exposed (%)            -764.9348 (-801.2995, -730.0374)
# Attrib risk in the population *                -0.0884 (-0.0914, -0.0855)
# Attrib fraction in the population (%)         -207.9376 (-211.4070, -204.5068)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 15321.439 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.12 (95% CI 0.11 to 0.12) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.12 (95% CI 0.11 to 0.12) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.12 (95% CI -0.12 to -0.11) per 100 population units. -764.9% of outcomes in the exposed were attributable to exposure (95% CI -801.3% to -730%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 863 (95% CI 885 to 843). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.09 (95% CI -0.09 to -0.09) per 100 population units. -207.9% of outcomes in the population were attributable to exposure (95% CI -211.4% to -204.5%). 


# #2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases
# #Vaccinated Cases and Unhospitalized
# vax_unhosp_pos <- vax_case - vax_hosp
# vax_unhosp_pos #1239215
# #Unvaccinated Cases and Unhospitalized
# unvax_unhosp_pos <- unvax_case - unvax_hosp
# unvax_unhosp_pos #1195860
# epi.2by2(c(vax_hosp,vax_unhosp_pos,unvax_hosp,unvax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# 
# # Outcome +    Outcome -      Total        Inc risk *        Odds
# # Exposed +        20980      1239215    1260195              1.66      0.0169
# # Exposed -        39851      1195860    1235711              3.22      0.0333
# # Total            60831      2435075    2495906              2.44      0.0250
# # Population Prevented Fraction
# (1260195/2495906)*(1-0.5162) #0.244273
# # Point estimates and 95% CIs:
# #   -------------------------------------------------------------------
# #   Inc risk ratio                                 0.5162 (0.5078, 0.5248)
# # Odds ratio                                     0.5080 (0.4995, 0.5167)
# # Attrib risk in the exposed *                   -1.5601 (-1.5985, -1.5218)
# # Attrib fraction in the exposed (%)            -93.7111 (-96.9404, -90.5348)
# # Attrib risk in the population *                -0.7877 (-0.8243, -0.7512)
# # Attrib fraction in the population (%)         -32.3200 (-33.0785, -31.5659)
# # -------------------------------------------------------------------
# #   Uncorrected chi2 test that OR = 1: chi2(1) = 6386.507 Pr>chi2 = <0.001
# # Fisher exact test that OR = 1: Pr>chi2 = <0.001
# # Wald confidence limits
# # CI: confidence interval
# # * Outcomes per 100 population units 
# # 
# # Measures of association strength:
# #   The outcome risk among the exposed was 0.52 (95% CI 0.51 to 0.52) times less than the outcome risk among the unexposed. 
# # 
# # The outcome odds among the exposed was 0.51 (95% CI 0.5 to 0.52) times less than the outcome odds among the unexposed. 
# # 
# # Measures of effect in the exposed:
# #   Exposure changed outcome risk in the exposed by -1.56 (95% CI -1.6 to -1.52) per 100 population units. -93.7% of outcomes in the exposed were attributable to exposure (95% CI -96.9% to -90.5%). 
# #                                                          
# #                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
# #                                                            The number needed to treat for one subject to be harmed (NNTH) is 64 (95% CI 66 to 63). 
# #                                                          
# #                                                          Measures of effect in the population:
# #                                                            Exposure changed outcome risk in the population by -0.79 (95% CI -0.82 to -0.75) per 100 population units. -32.3% of outcomes in the population were attributable to exposure (95% CI -33.1% to -31.6%). 


#2x2 Table for Vaccinations vs. Hospitalizations in General Population
#Vaccinated and Unhospitalized
vax_unhosp <- pop_vax - vax_hosp
#Unvaccinated and Unhospitalized
unvax_unhosp <- pop_unvax - unvax_hosp
epi.2by2(c(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp)
# [1] "Population Prevented Fraction:" "0.639046216817199"             
# [1] "Lower Interval:"   "0.639227745566056"
# [1] "Upper Interval:"   "0.638864688068341"
 #                Outcome +    Outcome -      Total        Inc risk *        Odds
 # Exposed +        20980     20510416   20531396             0.102     0.00102
 # Exposed -        39851      6318655    6358506             0.627     0.00631
 # Total            60831     26829071   26889902             0.226     0.00227
 # Population Prevented Fraction
(20531396/26889902)*(1-0.1630) #0.6390793
 # Point estimates and 95% CIs:
 #   -------------------------------------------------------------------
 #   Inc risk ratio                                 0.1630 (0.1603, 0.1658)
 # Odds ratio                                     0.1622 (0.1595, 0.1649)
 # Attrib risk in the exposed *                   -0.5246 (-0.5308, -0.5183)
 # Attrib fraction in the exposed (%)            -513.3341 (-523.6594, -503.1798)
 # Attrib risk in the population *                -0.4005 (-0.4069, -0.3941)
 # Attrib fraction in the population (%)         -177.0438 (-178.6469, -175.4499)
 # -------------------------------------------------------------------
 #   Uncorrected chi2 test that OR = 1: chi2(1) = 59184.296 Pr>chi2 = <0.001
 # Fisher exact test that OR = 1: Pr>chi2 = <0.001
 # Wald confidence limits
 # CI: confidence interval
 # * Outcomes per 100 population units 
 # 
 # Measures of association strength:
 #   The outcome risk among the exposed was 0.16 (95% CI 0.16 to 0.17) times less than the outcome risk among the unexposed. 
 # 
 # The outcome odds among the exposed was 0.16 (95% CI 0.16 to 0.16) times less than the outcome odds among the unexposed. 
 # 
 # Measures of effect in the exposed:
 #   Exposure changed outcome risk in the exposed by -0.52 (95% CI -0.53 to -0.52) per 100 population units. -513.3% of outcomes in the exposed were attributable to exposure (95% CI -523.7% to -503.2%). 
 #                                                          
 #                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
 #                                                            The number needed to treat for one subject to be harmed (NNTH) is 191 (95% CI 193 to 188). 
 #                                                          
 #                                                          Measures of effect in the population:
 #                                                            Exposure changed outcome risk in the population by -0.4 (95% CI -0.41 to -0.39) per 100 population units. -177% of outcomes in the population were attributable to exposure (95% CI -178.6% to -175.4%). 




#2x2 Table for Vaccinations vs. Deaths in Positive Cases
#Vaccinated and Survived
vax_surv_pos <- vax_case - vax_death
vax_surv_pos #176322
#Unvaccinated and Survived
unvax_surv_pos <- unvax_case - unvax_death
unvax_surv_pos #497390
epi.2by2(c(vax_death,vax_surv_pos,unvax_death,unvax_surv_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
#                 Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         3109      1257086    1260195             0.247     0.00247
# Exposed -         8328      1227383    1235711             0.674     0.00679
# Total            11437      2484469    2495906             0.458     0.00460
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.3661 (0.3513, 0.3814)
# Odds ratio                                     0.3645 (0.3498, 0.3799)
# Attrib risk in the exposed *                   -0.4272 (-0.4441, -0.4104)
# Attrib fraction in the exposed (%)            -173.1749 (-184.6415, -162.1703)
# Attrib risk in the population *                -0.2157 (-0.2324, -0.1990)
# Attrib fraction in the population (%)         -47.0754 (-48.7307, -45.4384)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 2496.739 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.37 (95% CI 0.35 to 0.38) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.36 (95% CI 0.35 to 0.38) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.43 (95% CI -0.44 to -0.41) per 100 population units. -173.2% of outcomes in the exposed were attributable to exposure (95% CI -184.6% to -162.2%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 234 (95% CI 244 to 225). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.22 (95% CI -0.23 to -0.2) per 100 population units. -47.1% of outcomes in the population were attributable to exposure (95% CI -48.7% to -45.4%). 
 
#2x2 Table for Vaccinations vs. Cases REVERSED
epi.2by2(c(unvax_case,unvax_nocase,vax_case,vax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +      1235711      5122795    6358506             19.43      0.2412
# Exposed -      1260195     19271201   20531396              6.14      0.0654
# Total          2495906     24393996   26889902              9.28      0.1023
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 3.1662 (3.1589, 3.1736)
# Odds ratio                                     3.6888 (3.6789, 3.6986)
# Attrib risk in the exposed *                   13.2961 (13.2636, 13.3286)
# Attrib fraction in the exposed (%)            68.4167 (68.3435, 68.4898)
# Attrib risk in the population *                3.1441 (3.1290, 3.1592)
# Attrib fraction in the population (%)         33.8728 (33.7954, 33.9501)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 1019294.232 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 3.17 (95% CI 3.16 to 3.17) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 3.69 (95% CI 3.68 to 3.7) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 13.3 (95% CI 13.26 to 13.33) per 100 population units. 68.4% of outcomes in the exposed were attributable to exposure (95% CI 68.3% to 68.5%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 8 (95% CI 8 to 8). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 3.14 (95% CI 3.13 to 3.16) per 100 population units. 33.9% of outcomes in the population were attributable to exposure (95% CI 33.8% to 34%). 
#2x2 Table for Vaccinations vs. Deaths REVERSED
epi.2by2(c(unvax_death,unvax_surv,vax_death,vax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         8328      6350178    6358506            0.1310    0.001311
# Exposed -         3109     20528287   20531396            0.0151    0.000151
# Total            11437     26878465   26889902            0.0425    0.000426
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 8.6493 (8.3004, 9.0130)
# Odds ratio                                     8.6594 (8.3098, 9.0236)
# Attrib risk in the exposed *                   0.1158 (0.1130, 0.1187)
# Attrib fraction in the exposed (%)            88.4384 (87.9524, 88.9049)
# Attrib risk in the population *                0.0274 (0.0264, 0.0283)
# Attrib fraction in the population (%)         64.3976 (63.3137, 65.4495)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 15321.439 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 8.65 (95% CI 8.3 to 9.01) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 8.66 (95% CI 8.31 to 9.02) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.12 (95% CI 0.11 to 0.12) per 100 population units. 88.4% of outcomes in the exposed were attributable to exposure (95% CI 88% to 88.9%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 863 (95% CI 843 to 885). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.03 (95% CI 0.03 to 0.03) per 100 population units. 64.4% of outcomes in the population were attributable to exposure (95% CI 63.3% to 65.4%). 

#2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases REVERSED
epi.2by2(c(unvax_hosp,unvax_unhosp_pos,vax_hosp,vax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +        39851      1195860    1235711              3.22      0.0333
# Exposed -        20980      1239215    1260195              1.66      0.0169
# Total            60831      2435075    2495906              2.44      0.0250
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 1.9371 (1.9053, 1.9694)
# Odds ratio                                     1.9683 (1.9353, 2.0019)
# Attrib risk in the exposed *                   1.5601 (1.5218, 1.5985)
# Attrib fraction in the exposed (%)            48.3767 (47.5162, 49.2232)
# Attrib risk in the population *                0.7724 (0.7430, 0.8018)
# Attrib fraction in the population (%)         31.6921 (30.9477, 32.4284)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 6386.507 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 1.94 (95% CI 1.91 to 1.97) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 1.97 (95% CI 1.94 to 2) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 1.56 (95% CI 1.52 to 1.6) per 100 population units. 48.4% of outcomes in the exposed were attributable to exposure (95% CI 47.5% to 49.2%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 64 (95% CI 63 to 66). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.77 (95% CI 0.74 to 0.8) per 100 population units. 31.7% of outcomes in the population were attributable to exposure (95% CI 30.9% to 32.4%). 
#2x2 Table for Vaccinations vs. Hospitalizations in General Population
epi.2by2(c(unvax_hosp,unvax_unhosp,vax_hosp,vax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +        39851      6318655    6358506             0.627     0.00631
# Exposed -        20980     20510416   20531396             0.102     0.00102
# Total            60831     26829071   26889902             0.226     0.00227
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 6.1333 (6.0318, 6.2366)
# Odds ratio                                     6.1657 (6.0634, 6.2698)
# Attrib risk in the exposed *                   0.5246 (0.5183, 0.5308)
# Attrib fraction in the exposed (%)            83.6957 (83.4212, 83.9656)
# Attrib risk in the population *                0.1240 (0.1218, 0.1263)
# Attrib fraction in the population (%)         54.8299 (54.3329, 55.3215)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 59184.296 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 6.13 (95% CI 6.03 to 6.24) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 6.17 (95% CI 6.06 to 6.27) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.52 (95% CI 0.52 to 0.53) per 100 population units. 83.7% of outcomes in the exposed were attributable to exposure (95% CI 83.4% to 84%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 191 (95% CI 188 to 193). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.12 (95% CI 0.12 to 0.13) per 100 population units. 54.8% of outcomes in the population were attributable to exposure (95% CI 54.3% to 55.3%). 

#2x2 Table for Vaccinations vs. Deaths in Positive Cases REVERSED
epi.2by2(c(unvax_death,unvax_surv_pos,vax_death,vax_surv_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         8328      1227383    1235711             0.674     0.00679
# Exposed -         3109      1257086    1260195             0.247     0.00247
# Total            11437      2484469    2495906             0.458     0.00460
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 2.7317 (2.6217, 2.8464)
# Odds ratio                                     2.7435 (2.6326, 2.8591)
# Attrib risk in the exposed *                   0.4272 (0.4104, 0.4441)
# Attrib fraction in the exposed (%)            63.3934 (61.8568, 64.8681)
# Attrib risk in the population *                0.2115 (0.1995, 0.2236)
# Attrib fraction in the population (%)         46.1607 (44.5241, 47.7491)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 2496.739 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 2.73 (95% CI 2.62 to 2.85) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 2.74 (95% CI 2.63 to 2.86) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.43 (95% CI 0.41 to 0.44) per 100 population units. 63.4% of outcomes in the exposed were attributable to exposure (95% CI 61.9% to 64.9%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 234 (95% CI 225 to 244). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.21 (95% CI 0.2 to 0.22) per 100 population units. 46.2% of outcomes in the population were attributable to exposure (95% CI 44.5% to 47.7%). 
