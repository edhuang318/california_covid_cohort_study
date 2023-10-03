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

#Identify time period of pre_delta wave

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
       title="SIRD Vaccination Model, Cumulative Totals, Data Only") +
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
#The flat period appears to be roughly from March through April
pre_delta <- covidvax %>%
  filter(Submission_Date >="2021-03-01" & Submission_Date <= "2021-04-30")
#Numbers for 2x2 Table
unvax_case <- sum(pre_delta$unvaccinated_cases) #503266
vax_case <- sum(pre_delta$vaccinated_cases) #177312
unvax_hosp <- sum(pre_delta$unvaccinated_hosp) #40088
vax_hosp <- sum(pre_delta$vaccinated_hosp) #7532
unvax_death <- sum(pre_delta$unvaccinated_deaths) #5876
vax_death <- sum(pre_delta$vaccinated_deaths) #990
pop_unvax<- pre_delta$population_unvaccinated[1] #11335256
pop_vax <- pre_delta$population_vaccinated[1] #20302803

ppf_ci <- function(e_pos_o_pos,e_pos_o_neg,e_neg_o_pos,e_neg_o_neg){
  Ip <- (e_pos_o_pos + e_pos_o_neg)/(e_pos_o_pos + e_pos_o_neg + e_neg_o_pos + e_neg_o_neg)
  Ie <- e_pos_o_pos/(e_pos_o_pos + e_pos_o_neg)
  ppf <- (Ip-Ie)/Ip
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
20302803-177312 #20125491
#Unvaccinated and No Case
unvax_nocase <- pop_unvax - unvax_case
11335256-503266 #10831990
# cases_table <- data.frame(x1 = c(177312,23593965),
#                           x2 = c(503266,7262508))
# cases_table
#This counts as a retrospective cohort study. BE CAREFUL WHICH VALUE GOES IN WHICH SQUARE.
#Side note: The command to comment out multiple lines is Command/Control + Shift + C.
epi.2by2(c(vax_case,vax_nocase,unvax_case,unvax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_case,vax_nocase,unvax_case,unvax_nocase)
# [1] "Population Prevented Fraction:" "0.0301004169557887"            
# [1] "Lower Interval:"    "0.0301607204030936"
# [1] "Upper Interval:"    "0.0300401135084838"
#               Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         3335      1753442    1756777             0.190     0.00190
# Exposed -       117071     28965926   29082997             0.403     0.00404
# Total           120406     30719368   30839774             0.390     0.00392
# Population Prevented Fraction
(1756777/30839774)*(1-0.4716) #0.03010012
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.4716 (0.4557, 0.4881)
# Odds ratio                                     0.4706 (0.4547, 0.4871)
# Attrib risk in the exposed *                   -0.2127 (-0.2195, -0.2059)
# Attrib fraction in the exposed (%)            -112.0464 (-119.4645, -104.8791)
# Attrib risk in the population *                -0.0121 (-0.0153, -0.0089)
# Attrib fraction in the population (%)         -3.1035 (-3.2018, -3.0052)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 1927.352 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.47 (95% CI 0.46 to 0.49) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.47 (95% CI 0.45 to 0.49) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.21 (95% CI -0.22 to -0.21) per 100 population units. -112% of outcomes in the exposed were attributable to exposure (95% CI -119.5% to -104.9%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 470 (95% CI 486 to 455). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.01 (95% CI -0.02 to -0.01) per 100 population units. -3.1% of outcomes in the population were attributable to exposure (95% CI -3.2% to -3%). 


#2x2 Table for Vaccinations vs. Deaths
#Vaccinated and Survived
20302803-990 #20301813
vax_surv <- pop_vax-vax_death
#Unvaccinated and Survived
unvax_surv <- pop_unvax-unvax_death
11335256-5876 #11329380
epi.2by2(c(vax_death,vax_surv,unvax_death,unvax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_death,vax_surv,unvax_death,unvax_surv)
# [1] "Population Prevented Fraction:" "0.0483393289022979"            
# [1] "Lower Interval:"    "0.0484150268084616"
# [1] "Upper Interval:"    "0.0482636309961343"
#               Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +           24      1756753    1756777           0.00137    1.37e-05
# Exposed -         2624     29080373   29082997           0.00902    9.02e-05
# Total             2648     30837126   30839774           0.00859    8.59e-05
# Population Prevented Fraction
(1756777/30839774)*(1-0.1514) #0.0483402
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.1514 (0.1013, 0.2263)
# Odds ratio                                     0.1514 (0.1013, 0.2263)
# Attrib risk in the exposed *                   -0.0077 (-0.0083, -0.0070)
# Attrib fraction in the exposed (%)            -560.4350 (-887.1255, -341.8631)
# Attrib risk in the population *                -0.0004 (-0.0009, 0.0000)
# Attrib fraction in the population (%)         -5.0795 (-5.4630, -4.6973)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 113.114 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.15 (95% CI 0.1 to 0.23) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.15 (95% CI 0.1 to 0.23) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.01 (95% CI -0.01 to -0.01) per 100 population units. -560.4% of outcomes in the exposed were attributable to exposure (95% CI -887.1% to -341.9%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 13061 (95% CI 14266 to 12044). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by 0 (95% CI 0 to 0) per 100 population units. -5.1% of outcomes in the population were attributable to exposure (95% CI -5.5% to -4.7%). 



#2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases
#Vaccinated Cases and Unhospitalized
177312-7532 #169780
vax_unhosp_pos <- vax_case - vax_hosp
#Unvaccinated Cases and Unhospitalized
503266-40088 #463178
unvax_unhosp_pos <- unvax_case - unvax_hosp
epi.2by2(c(vax_hosp,vax_unhosp_pos,unvax_hosp,unvax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")

# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +          297         3038       3335              8.91      0.0978
# Exposed -         7668       109403     117071              6.55      0.0701
# Total             7965       112441     120406              6.62      0.0708
# Population Prevented Fraction
(3335/120406)*(1-1.3597) #-0.009962955
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 1.3597 (1.2172, 1.5188)
# Odds ratio                                     1.3948 (1.2354, 1.5748)
# Attrib risk in the exposed *                   2.3557 (1.3787, 3.3327)
# Attrib fraction in the exposed (%)            26.4518 (17.8437, 34.1579)
# Attrib risk in the population *                0.0652 (-0.1342, 0.2647)
# Attrib fraction in the population (%)         0.9863 (0.5755, 1.3954)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 29.128 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 1.36 (95% CI 1.22 to 1.52) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 1.39 (95% CI 1.24 to 1.57) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 2.36 (95% CI 1.38 to 3.33) per 100 population units. 26.5% of outcomes in the exposed were attributable to exposure (95% CI 17.8% to 34.2%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 42 (95% CI 30 to 73). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.07 (95% CI -0.13 to 0.26) per 100 population units. 1% of outcomes in the population were attributable to exposure (95% CI 0.6% to 1.4%). 

#2x2 Table for Vaccinations vs. Deaths in Positive Cases
#Vaccinated and Survived
vax_surv_pos <- vax_case - vax_death
vax_surv_pos #176322
#Unvaccinated and Survived
unvax_surv_pos <- unvax_case - unvax_death
unvax_surv_pos #497390
epi.2by2(c(vax_death,vax_surv_pos,unvax_death,unvax_surv_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
#               Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +           24         3311       3335              0.72     0.00725
# Exposed -         2624       114447     117071              2.24     0.02293
# Total             2648       117758     120406              2.20     0.02249
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.3211 (0.2151, 0.4792)
# Odds ratio                                     0.3161 (0.2112, 0.4732)
# Attrib risk in the exposed *                   -1.5217 (-1.8209, -1.2226)
# Attrib fraction in the exposed (%)            -211.4577 (-364.8378, -108.6877)
# Attrib risk in the population *                -0.0421 (-0.1607, 0.0764)
# Attrib fraction in the population (%)         -1.9165 (-2.2928, -1.5416)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 34.911 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.32 (95% CI 0.22 to 0.48) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.32 (95% CI 0.21 to 0.47) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -1.52 (95% CI -1.82 to -1.22) per 100 population units. -211.5% of outcomes in the exposed were attributable to exposure (95% CI -364.8% to -108.7%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 66 (95% CI 82 to 55). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -0.04 (95% CI -0.16 to 0.08) per 100 population units. -1.9% of outcomes in the population were attributable to exposure (95% CI -2.3% to -1.5%). 


#2x2 Table for Vaccinations vs. Hospitalizations in General Population
#Vaccinated and Unhospitalized
vax_unhosp <- pop_vax - vax_hosp
#Unvaccinated and Unhospitalized
unvax_unhosp <- pop_unvax - unvax_hosp
epi.2by2(c(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
ppf_ci(vax_hosp,vax_unhosp,unvax_hosp,unvax_unhosp)
# [1] "Population Prevented Fraction:" "0.02043863521325"              
# [1] "Lower Interval:"    "0.0204885735723155"
# [1] "Upper Interval:"    "0.0203886968541844"
#               Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +          297      1756480    1756777            0.0169    0.000169
# Exposed -         7668     29075329   29082997            0.0264    0.000264
# Total             7965     30831809   30839774            0.0258    0.000258
# Population Prevented Fraction
(1756777/30839774)*(1-0.6412) #0.02043892
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.6412 (0.5710, 0.7200)
# Odds ratio                                     0.6411 (0.5710, 0.7199)
# Attrib risk in the exposed *                   -0.0095 (-0.0115, -0.0074)
# Attrib fraction in the exposed (%)            -55.9564 (-75.1209, -38.8891)
# Attrib risk in the population *                -0.0005 (-0.0014, 0.0003)
# Attrib fraction in the population (%)         -2.0865 (-2.5287, -1.6463)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 57.420 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.64 (95% CI 0.57 to 0.72) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.64 (95% CI 0.57 to 0.72) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -0.01 (95% CI -0.01 to -0.01) per 100 population units. -56% of outcomes in the exposed were attributable to exposure (95% CI -75.1% to -38.9%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 10571 (95% CI 13425 to 8718). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by 0 (95% CI 0 to 0) per 100 population units. -2.1% of outcomes in the population were attributable to exposure (95% CI -2.5% to -1.6%). 

#2x2 Table for Vaccinations vs. Cases REVERSED
epi.2by2(c(unvax_case,unvax_nocase,vax_case,vax_nocase), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +       117071     28965926   29082997             0.403     0.00404
# Exposed -         3335      1753442    1756777             0.190     0.00190
# Total           120406     30719368   30839774             0.390     0.00392
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 2.1205 (2.0488, 2.1946)
# Odds ratio                                     2.1250 (2.0530, 2.1995)
# Attrib risk in the exposed *                   0.2127 (0.2059, 0.2195)
# Attrib fraction in the exposed (%)            52.8405 (51.1907, 54.4346)
# Attrib risk in the population *                0.2006 (0.1938, 0.2074)
# Attrib fraction in the population (%)         51.3769 (49.7238, 52.9757)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 1927.352 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 2.12 (95% CI 2.05 to 2.19) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 2.12 (95% CI 2.05 to 2.2) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.21 (95% CI 0.21 to 0.22) per 100 population units. 52.8% of outcomes in the exposed were attributable to exposure (95% CI 51.2% to 54.4%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 470 (95% CI 455 to 486). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.2 (95% CI 0.19 to 0.21) per 100 population units. 51.4% of outcomes in the population were attributable to exposure (95% CI 49.7% to 53%). 

#2x2 Table for Vaccinations vs. Deaths REVERSED
epi.2by2(c(unvax_death,unvax_surv,vax_death,vax_surv), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         2624     29080373   29082997           0.00902    9.02e-05
# Exposed -           24      1756753    1756777           0.00137    1.37e-05
# Total             2648     30837126   30839774           0.00859    8.59e-05
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 6.6043 (4.4186, 9.8713)
# Odds ratio                                     6.6049 (4.4189, 9.8721)
# Attrib risk in the exposed *                   0.0077 (0.0070, 0.0083)
# Attrib fraction in the exposed (%)            84.8585 (77.3686, 89.8696)
# Attrib risk in the population *                0.0072 (0.0066, 0.0079)
# Attrib fraction in the population (%)         84.0894 (76.3055, 89.3162)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 113.114 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 6.6 (95% CI 4.42 to 9.87) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 6.6 (95% CI 4.42 to 9.87) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.01 (95% CI 0.01 to 0.01) per 100 population units. 84.9% of outcomes in the exposed were attributable to exposure (95% CI 77.4% to 89.9%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 13061 (95% CI 12044 to 14266). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.01 (95% CI 0.01 to 0.01) per 100 population units. 84.1% of outcomes in the population were attributable to exposure (95% CI 76.3% to 89.3%). 

#2x2 Table for Vaccinations vs. Hospitalizations in Positive Cases REVERSED
epi.2by2(c(unvax_hosp,unvax_unhosp_pos,vax_hosp,vax_unhosp_pos), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         7668       109403     117071              6.55      0.0701
# Exposed -          297         3038       3335              8.91      0.0978
# Total             7965       112441     120406              6.62      0.0708
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 0.7355 (0.6584, 0.8216)
# Odds ratio                                     0.7169 (0.6350, 0.8095)
# Attrib risk in the exposed *                   -2.3557 (-3.3327, -1.3787)
# Attrib fraction in the exposed (%)            -35.9652 (-51.8785, -21.7192)
# Attrib risk in the population *                -2.2904 (-3.2672, -1.3136)
# Attrib fraction in the population (%)         -34.6241 (-49.7612, -21.0170)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 29.128 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 0.74 (95% CI 0.66 to 0.82) times less than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 0.72 (95% CI 0.63 to 0.81) times less than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by -2.36 (95% CI -3.33 to -1.38) per 100 population units. -36% of outcomes in the exposed were attributable to exposure (95% CI -51.9% to -21.7%). 
#                                                          
#                                                          Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                            The number needed to treat for one subject to be harmed (NNTH) is 42 (95% CI 73 to 30). 
#                                                          
#                                                          Measures of effect in the population:
#                                                            Exposure changed outcome risk in the population by -2.29 (95% CI -3.27 to -1.31) per 100 population units. -34.6% of outcomes in the population were attributable to exposure (95% CI -49.8% to -21%). 

#2x2 Table for Vaccinations vs. Hospitalizations in General Population REVERSED
epi.2by2(c(unvax_hosp,unvax_unhosp,vax_hosp,vax_unhosp), method = "cohort.count", digits = 4, conf.level = 0.95, units = 100, interpret = TRUE, outcome = "as.columns")
# Outcome +    Outcome -      Total        Inc risk *        Odds
# Exposed +         7668     29075329   29082997            0.0264    0.000264
# Exposed -          297      1756480    1756777            0.0169    0.000169
# Total             7965     30831809   30839774            0.0258    0.000258
# 
# Point estimates and 95% CIs:
#   -------------------------------------------------------------------
#   Inc risk ratio                                 1.5596 (1.3889, 1.7512)
# Odds ratio                                     1.5597 (1.3890, 1.7514)
# Attrib risk in the exposed *                   0.0095 (0.0074, 0.0115)
# Attrib fraction in the exposed (%)            35.8795 (28.0001, 42.8966)
# Attrib risk in the population *                0.0089 (0.0069, 0.0109)
# Attrib fraction in the population (%)         34.5416 (26.8148, 41.4526)
# -------------------------------------------------------------------
#   Uncorrected chi2 test that OR = 1: chi2(1) = 57.420 Pr>chi2 = <0.001
# Fisher exact test that OR = 1: Pr>chi2 = <0.001
# Wald confidence limits
# CI: confidence interval
# * Outcomes per 100 population units 
# 
# Measures of association strength:
#   The outcome risk among the exposed was 1.56 (95% CI 1.39 to 1.75) times greater than the outcome risk among the unexposed. 
# 
# The outcome odds among the exposed was 1.56 (95% CI 1.39 to 1.75) times greater than the outcome odds among the unexposed. 
# 
# Measures of effect in the exposed:
#   Exposure changed outcome risk in the exposed by 0.01 (95% CI 0.01 to 0.01) per 100 population units. 35.9% of outcomes in the exposed were attributable to exposure (95% CI 28% to 42.9%). 
#                                                         
#                                                         Number needed to treat for benefit (NNTB) and harm (NNTH):
#                                                           The number needed to treat for one subject to benefit (NNTB) is 10571 (95% CI 8718 to 13425). 
#                                                         
#                                                         Measures of effect in the population:
#                                                           Exposure changed outcome risk in the population by 0.01 (95% CI 0.01 to 0.01) per 100 population units. 34.5% of outcomes in the population were attributable to exposure (95% CI 26.8% to 41.5%). 

