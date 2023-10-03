library(tidyverse)
#install.packages("epiR")
library(epiR)

#set working directory
setwd("~/Documents")
#open dataset
covidvax <- read.csv("covid19postvaxstatewidestats.csv")
#add relevant columns
covidvax <- covidvax %>%
  mutate(total_cases = unvaccinated_cases + vaccinated_cases)
covidvax <- covidvax %>%
  mutate(Submission_Date = as.Date(date))
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

#Identify time period of Delta wave

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
#The Delta wave appears to be roughly from July to September
delta <- covidvax %>%
  filter(Submission_Date >="2021-07-01" & Submission_Date <= "2021-9-30")
#Numbers for 2x2 Table
unvax_case <- sum(delta$unvaccinated_cases) #503266
vax_case <- sum(delta$vaccinated_cases) #177312
unvax_hosp <- sum(delta$unvaccinated_hosp) #40088
vax_hosp <- sum(delta$vaccinated_hosp) #7532
unvax_death <- sum(delta$unvaccinated_deaths) #5876
vax_death <- sum(delta$vaccinated_deaths) #990
pop_unvax <- delta$population_unvaccinated[1] #11335256
pop_vax <- delta$population_vaccinated[1] #20302803

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
vax_nocase
20302803-177312 #20125491
#Unvaccinated and No Case
unvax_nocase <- pop_unvax - unvax_case
unvax_nocase
11335256-503266 #10831990
# cases_table <- data.frame(x1 = c(177312,23593965),
#                           x2 = c(503266,7262508))
# cases_table
#This counts as a retrospective cohort study. BE CAREFUL WHICH VALUE GOES IN WHICH SQUARE.
#Side note: The command to comment out multiple lines is Command/Control + Shift + C.
epi.2by2(c(vax_case,vax_nocase,unvax_case,unvax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_case,vax_nocase,unvax_case,unvax_nocase)

# [1] "Population Prevented Fraction:" "0.515491035981027"             
# [1] "Lower Interval:"   "0.515665178427827"
# [1] "Upper Interval:"   "0.515316893534227"
#                 Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +       177312     20125491   20302803             0.873     0.00881
# Exposed -       503266     10831990   11335256             4.440     0.04646
# Total           680578     30957481   31638059             2.151     0.02198
#Population Prevented Fraction
(20302803/31638059)*(1-0.1967) #0.5154944
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.1967 (0.1957, 0.1978)
# Odds ratio                                     0.1896 (0.1886, 0.1907)
# Attrib risk in the exposed *                   -3.5665 (-3.5791, -3.5538)
# Attrib fraction in the exposed (%)            -408.3749 (-411.1091, -405.6554)
# Attrib risk in the population *                -2.2887 (-2.3017, -2.2757)
# Attrib fraction in the population (%)         -106.3945 (-106.6888, -106.1007)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 439578.110 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.2 (95% CI 0.2 to 0.2) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.19 (95% CI 0.19 to 0.19) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -3.57 (95% CI -3.58 to -3.55) per 100 population units. -408.4% of outcomes in the exposed were attributable to exposure (95% CI -411.1% to -405.7%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 28 (95% CI 28 to 28). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -2.29 (95% CI -2.3 to -2.28) per 100 population units. -106.4% of outcomes in the population were attributable to exposure (95% CI -106.7% to -106.1%). 
#2x2 Table for Vaccinations vs. Deaths
#Vaccinated and Survived
vax_surv <- pop_vax - vax_death
vax_surv
20302803-990 #20301813
#Unvaccinated and Survived
unvax_surv <- pop_unvax - unvax_death
unvax_surv
11335256-5876 #11329380
epi.2by2(c(vax_death,vax_surv,unvax_death,unvax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_death,vax_surv,unvax_death,unvax_surv)
# [1] "Population Prevented Fraction:" "0.581357309124237"             
# [1] "Lower Interval:"  "0.58152921332928"
# [1] "Upper Interval:"   "0.581185404919194"
#                 Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +          990     20301813   20302803           0.00488    4.88e-05
# Exposed -         5876     11329380   11335256           0.05184    5.19e-04
# Total             6866     31631193   31638059           0.02170    2.17e-04
# Population Prevented Fraction
(20302803/31638059)*(1-0.0941) #0.5813349
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.0941 (0.0879, 0.1006)
# Odds ratio                                     0.0940 (0.0879, 0.1006)
# Attrib risk in the exposed *                   -0.0470 (-0.0483, -0.0456)
# Attrib fraction in the exposed (%)            -963.0930 (-1037.1371, -893.8703)
# Attrib risk in the population *                -0.0301 (-0.0316, -0.0287)
# Attrib fraction in the population (%)         -138.8672 (-141.1991, -136.5578)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 7393.884 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.09 (95% CI 0.09 to 0.1) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.09 (95% CI 0.09 to 0.1) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.05 (95% CI -0.05 to -0.05) per 100 population units. -963.1% of outcomes in the exposed were attributable to exposure (95% CI -1037.1% to -893.9%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 2129 (95% CI 2193 to 2069). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.03 (95% CI -0.03 to -0.03) per 100 population units. -138.9% of outcomes in the population were attributable to exposure (95% CI -141.2% to -136.6%). 
                                                                                                                     
# #2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases
# #Vaccinated Cases and Unhospitalized
# vax_unhosp_pos <- vax_case - vax_hosp
# vax_unhosp_pos
# 177312-7532 #169780
# #Unvaccinated Cases and Unhospitalized
# 503266-40088 #463178
# unvax_unhosp_pos <- unvax_case - unvax_hosp
# unvax_unhosp_pos
# epi.2by2(c(vax_hosp,vax_unhosp_pos,unvax_hosp,unvax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# 
# # Outcome +    Outcome -      Total        Inc risk *        Odds
# # Exposed +         7532       169780     177312              4.25      0.0444
# # Exposed -        40088       463178     503266              7.97      0.0865
# # Total            47620       632958     680578              7.00      0.0752
# # Population Prevented Fraction
# (177312/680578)*(1-0.5333) #0.12159
# # Point estimates and 95% CIs:
# #   -------------------------------------------------------------------
# #   Inc risk ratio                                 0.5333 (0.5206, 0.5462)
# # Odds ratio                                     0.5126 (0.4998, 0.5257)
# # Attrib risk in the exposed *                   -3.7177 (-3.8377, -3.5977)
# # Attrib fraction in the exposed (%)            -87.5187 (-92.0758, -83.0698)
# # Attrib risk in the population *                -0.9686 (-1.0649, -0.8723)
# # Attrib fraction in the population (%)         -13.8427 (-14.2805, -13.4066)
# # -------------------------------------------------------------------
# #   Uncorrected chi2 test that OR = 1: chi2(1) = 2784.810 Pr>chi2 = <0.001
# # Fisher exact test that OR = 1: Pr>chi2 = <0.001
# # Wald confidence limits
# # CI: confidence interval
# # * Outcomes per 100 population units 
# # 
# # Measures of association strength:
# #   The outcome risk among the exposed was 0.53 (95% CI 0.52 to 0.55) times less than the outcome risk among the unexposed. 
# # 
# # The outcome odds among the exposed was 0.51 (95% CI 0.5 to 0.53) times less than the outcome odds among the unexposed. 
# # 
# # Measures of effect in the exposed:
# #   Exposure changed outcome risk in the exposed by -3.72 (95% CI -3.84 to -3.6) per 100 population units. -87.5% of outcomes in the exposed were attributable to exposure (95% CI -92.1% to -83.1%). 
# #                                                          
# #                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
# #                                                            The number needed to treat for one subject to be harmed (NNTH) is 27 (95% CI 28 to 26). 
# #                                                          
# #                                                          Measures of effect in the population:
# #                                                            Exposure changed outcome risk in the population by -0.97 (95% CI -1.06 to -0.87) per 100 population units. -13.8% of outcomes in the population were attributable to exposure (95% CI -14.3% to -13.4%). 


#2x2 Table for Vaccinations vs. Hospitalizations in General Population
#Vaccinated Cases and Unospitalized
vax_unhosp <- pop_vax - vax_hosp
vax_unhosp
20302803-7532 #20295271
#Unvaccinated and Unhospitalized
unvax_unhosp <- pop_unvax - unvax_hosp
unvax_unhosp
11335256-40088 #11295168
epi.2by2(c(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp)
# [1] "Population Prevented Fraction:" "0.57440501423912"              
# [1] "Lower Interval:"   "0.574577300457187"
# [1] "Upper Interval:"   "0.574232728021054"
#                 Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         7532     20295271   20302803            0.0371    0.000371
# Exposed -        40088     11295168   11335256            0.3537    0.003549
# Total            47620     31590439   31638059            0.1505    0.001507
# Population Prevented Fraction
(20302803/31638059)*(1-0.1049) #0.5744044
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.1049 (0.1023, 0.1075)
# Odds ratio                                     0.1046 (0.1020, 0.1072)
# Attrib risk in the exposed *                   -0.3166 (-0.3201, -0.3130)
# Attrib fraction in the exposed (%)            -853.2981 (-877.0432, -830.1302)
# Attrib risk in the population *                -0.2031 (-0.2069, -0.1994)
# Attrib fraction in the population (%)         -134.9652 (-135.8850, -134.0489)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 48502.300 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.1 (95% CI 0.1 to 0.11) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.1 (95% CI 0.1 to 0.11) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.32 (95% CI -0.32 to -0.31) per 100 population units. -853.3% of outcomes in the exposed were attributable to exposure (95% CI -877% to -830.1%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 316 (95% CI 319 to 312). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.2 (95% CI -0.21 to -0.2) per 100 population units. -135% of outcomes in the population were attributable to exposure (95% CI -135.9% to -134%).

#2x2 Table for Vaccinations vs. Deaths in Positive Cases
#Vaccinated and Survived
vax_surv_pos <- vax_case - vax_death
vax_surv_pos #176322
#Unvaccinated and Survived
unvax_surv_pos <- unvax_case - unvax_death
unvax_surv_pos #497390
epi.2by2(c(vax_death,vax_surv_pos,unvax_death,unvax_surv_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +          990       176322     177312             0.558     0.00561
# Exposed -         5876       497390     503266             1.168     0.01181
# Total             6866       673712     680578             1.009     0.01019
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.4782 (0.4472, 0.5114)
# Odds ratio                                     0.4753 (0.4442, 0.5085)
# Attrib risk in the exposed *                   -0.6092 (-0.6549, -0.5636)
# Attrib fraction in the exposed (%)            -109.1159 (-123.6329, -95.5413)
# Attrib risk in the population *                -0.1587 (-0.1967, -0.1207)
# Attrib fraction in the population (%)         -15.7333 (-16.8611, -14.6163)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 487.311 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.48 (95% CI 0.45 to 0.51) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.48 (95% CI 0.44 to 0.51) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.61 (95% CI -0.65 to -0.56) per 100 population units. -109.1% of outcomes in the exposed were attributable to exposure (95% CI -123.6% to -95.5%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 164 (95% CI 177 to 153). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.16 (95% CI -0.2 to -0.12) per 100 population units. -15.7% of outcomes in the population were attributable to exposure (95% CI -16.9% to -14.6%). 


#2x2 Vaccinations vs. Cases REVERSED
epi.2by2(c(unvax_case, unvax_nocase, vax_case, vax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +       503266     10831990   11335256             4.440     0.04646
# Exposed -       177312     20125491   20302803             0.873     0.00881
# Total           680578     30957481   31638059             2.151     0.02198
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 5.0837 (5.0566, 5.1111)
# Odds ratio                                     5.2735 (5.2448, 5.3024)
# Attrib risk in the exposed *                   3.5665 (3.5538, 3.5791)
# Attrib fraction in the exposed (%)            80.3295 (80.2237, 80.4347)
# Attrib risk in the population *                1.2778 (1.2713, 1.2843)
# Attrib fraction in the population (%)         59.4011 (59.2394, 59.5622)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 439578.110 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 5.08 (95% CI 5.06 to 5.11) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 5.27 (95% CI 5.24 to 5.3) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 3.57 (95% CI 3.55 to 3.58) per 100 population units. 80.3% of outcomes in the exposed were attributable to exposure (95% CI 80.2% to 80.4%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 28 (95% CI 28 to 28). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 1.28 (95% CI 1.27 to 1.28) per 100 population units. 59.4% of outcomes in the population were attributable to exposure (95% CI 59.2% to 59.6%). 

#2x2 Table for Vaccinations vs. Deaths REVERSED
epi.2by2(c(unvax_death,unvax_surv, vax_death,vax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         5876     11329380   11335256           0.05184    5.19e-04
# Exposed -          990     20301813   20302803           0.00488    4.88e-05
# Total             6866     31631193   31638059           0.02170    2.17e-04
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 10.6309 (9.9387, 11.3714)
# Odds ratio                                     10.6359 (9.9433, 11.3768)
# Attrib risk in the exposed *                   0.0470 (0.0456, 0.0483)
# Attrib fraction in the exposed (%)            90.5935 (89.9383, 91.2060)
# Attrib risk in the population *                0.0168 (0.0162, 0.0174)
# Attrib fraction in the population (%)         77.5309 (76.1981, 78.7891)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 7393.884 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 10.63 (95% CI 9.94 to 11.37) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 10.64 (95% CI 9.94 to 11.38) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.05 (95% CI 0.05 to 0.05) per 100 population units. 90.6% of outcomes in the exposed were attributable to exposure (95% CI 89.9% to 91.2%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 2129 (95% CI 2069 to 2193). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.02 (95% CI 0.02 to 0.02) per 100 population units. 77.5% of outcomes in the population were attributable to exposure (95% CI 76.2% to 78.8%). 

#2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases REVERSED
epi.2by2(c(unvax_hosp,unvax_unhosp_pos,vax_hosp,vax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +        40088       463178     503266              7.97      0.0865
# Exposed -         7532       169780     177312              4.25      0.0444
# Total            47620       632958     680578              7.00      0.0752
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 1.8752 (1.8307, 1.9208)
# Odds ratio                                     1.9509 (1.9023, 2.0008)
# Attrib risk in the exposed *                   3.7177 (3.5977, 3.8377)
# Attrib fraction in the exposed (%)            46.6720 (45.3760, 47.9372)
# Attrib risk in the population *                2.7491 (2.6374, 2.8609)
# Attrib fraction in the population (%)         39.2899 (38.0498, 40.5052)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 2784.810 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 1.88 (95% CI 1.83 to 1.92) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 1.95 (95% CI 1.9 to 2) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 3.72 (95% CI 3.6 to 3.84) per 100 population units. 46.7% of outcomes in the exposed were attributable to exposure (95% CI 45.4% to 47.9%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 27 (95% CI 26 to 28). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 2.75 (95% CI 2.64 to 2.86) per 100 population units. 39.3% of outcomes in the population were attributable to exposure (95% CI 38% to 40.5%). 

# 2x2 Table for Vaccinations vs. Hospitalizations in General Population REVERSED
epi.2by2(c(unvax_hosp,unvax_unhosp,vax_hosp,vax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +        40088     11295168   11335256            0.3537    0.003549
# Exposed -         7532     20295271   20302803            0.0371    0.000371
# Total            47620     31590439   31638059            0.1505    0.001507
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 9.5330 (9.3013, 9.7704)
# Odds ratio                                     9.5633 (9.3306, 9.8017)
# Attrib risk in the exposed *                   0.3166 (0.3130, 0.3201)
# Attrib fraction in the exposed (%)            89.5101 (89.2488, 89.7650)
# Attrib risk in the population *                0.1134 (0.1118, 0.1150)
# Attrib fraction in the population (%)         75.3524 (74.8365, 75.8577)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 48502.300 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 9.53 (95% CI 9.3 to 9.77) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 9.56 (95% CI 9.33 to 9.8) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.32 (95% CI 0.31 to 0.32) per 100 population units. 89.5% of outcomes in the exposed were attributable to exposure (95% CI 89.2% to 89.8%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 316 (95% CI 312 to 319). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.11 (95% CI 0.11 to 0.12) per 100 population units. 75.4% of outcomes in the population were attributable to exposure (95% CI 74.8% to 75.9%). 

#Preventive Fraction = PF = (CIe-CIu)/CIu
#Attributable Fraction in Exposed = RD/CIe = (CIe-CIu)/CIe
#Population Prevented Fraction = Pe*(1-RR) = (exposed/total population)(1-RR)=((Ie/Tote)/(Iu/Totu))*(1-RR)



