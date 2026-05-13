library(readr)
library(dplyr)
library(cjoint)
library(survival)
library(sandwich)
library(lmtest)
library(modelsummary)
library(kableExtra)
library(broom)
library(ggplot2)
install.packages('FindIt')
library(FindIt)
library(cregg)
library(car)
install.packages('stargazer')
library(stargazer)
library(xtable)
install.packages('texreg')
library(texreg)
install.packages("doParallel")
install.packages("foreach")   
install.packages('interflex')
library(interflex)



unique_levels <- function(x) {
  if (is.factor(x)) {
    levels(x)
  } else {
    sort(unique(x))
  }
}

df <- read.csv(

  "https://raw.githubusercontent.com/mitskm/Bachelor-Thesis/refs/heads/main/conjoint_df.csv"
)
df_unattent <- read.csv('conjoint_df_with_unattent.csv')

### Main Analysis

df_for_amce <- df |>
  mutate(
    Collaboration     = factor(collaboration_illiberal,     levels = c(0, 1), labels = c("Liberal", "Illiberal")),
    Economic          = factor(economy_illiberal,          levels = c(0, 1), labels = c("Liberal", "Illiberal")),
    Freedom_of_Speech = factor(FreedomSpeech_illiberal, levels = c(0, 1), labels = c("Liberal", "Illiberal")),
    Surveillance      = factor(Surveillance_illiberal,      levels = c(0, 1), labels = c("Liberal", "Illiberal"))
  )

df_for_amce <- df_for_amce %>%
  mutate(RespTask = paste0(respondent_id, "_", task_num))


df_cregg <- df_for_amce %>%
  
  mutate(
    
    Collaboration_mm = factor(
      
      Collaboration,
      
      levels = c("Liberal", "Illiberal"),
      
      labels = c("Collaboration: Liberal", "Collaboration: Illiberal")
      
    ),
    
    Economic_mm = factor(
      
      Economic,
      
      levels = c("Liberal", "Illiberal"),
      
      labels = c("Economic: Liberal", "Economic: Illiberal")
      
    ),
    
    Freedom_of_Speech_mm = factor(
      
      Freedom_of_Speech,
      
      levels = c("Liberal", "Illiberal"),
      
      labels = c("Freedom of Speech: Liberal", "Freedom of Speech: Illiberal")
      
    ),
    
    Surveillance_mm = factor(
      
      Surveillance,
      
      levels = c("Liberal", "Illiberal"),
      
      labels = c("Surveillance: Liberal", "Surveillance: Illiberal")
      
    )
    
  )

df_findit <- df_for_amce %>%
  
  mutate(
    
    Collaboration = relevel(Collaboration, ref = "Liberal"),
    
    Economic = relevel(Economic, ref = "Liberal"),
    
    Freedom_of_Speech = relevel(Freedom_of_Speech, ref = "Liberal"),
    
    Surveillance = relevel(Surveillance, ref = "Liberal")
    
  )

#AMCE Estimation


restricted_model <- estimatr::lm_robust(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance ,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)
res_amce <- cjoint::amce(
  data = df_for_amce,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  respondent.id = "respondent_id"
)
summary(res_amce)

summary(restricted_model)

p <- plot(
  
  res_amce,
  
  main = "Effect of Attributes on Candidate Choice",
  
  xlab = "Change in Probability of Candidate Choice",
  
  attribute.names = c(
    
    "International Collaboration",
    
    "Free Trade",
    
    "Freedom of Speech",
    
    "State Surveillance"
    
  ),
  
  label.baseline = TRUE,
  
  text.size = 13,
  
  point.size = 1.6,
  
  dodge.size = 0.6,
  
  plot.theme = ggplot2::theme_minimal()
  
)
unique(p$data$Level)

unique(p$data$Attribute)
class(p)

names(p)

str(p$data)

names(p$data)
p <- plot(
  
  res_amce,
  
  main = "Effect of Attributes on Candidate Choice",
  
  xlab = "Change in Probability of Candidate Choice",
  
  attribute.names = c(
    
    "International Collaboration",
    
    "Free Trade",
    
    "Freedom of Speech",
    
    "State Surveillance"
    
  ),
  
  label.baseline = TRUE,
  
  text.size = 13,
  
  point.size = 1.6,
  
  dodge.size = 0.6,
  
  plot.theme = ggplot2::theme_minimal() +
    
    ggplot2::theme(legend.position = "none")
  
)

# Add hollow baseline dots at zero

p_final <- p +
  
  geom_point(
    
    data = data.frame(
      
      x = 0,
      
      y = c(10, 7, 4, 1)  # baseline rows: Liberal for each attribute
      
    ),
    
    aes(x = x, y = y),
    
    inherit.aes = FALSE,
    
    color = "grey35",
    
    size = 2.8,
    
    shape = 1,
    
    stroke = 1.1
    
  ) +
  
  geom_vline(
    
    xintercept = 0,
    
    linetype = "dashed",
    
    color = "black"
    
  ) +
  
  scale_x_continuous(
    
    limits = c(-0.25, 0.12),
    
    breaks = seq(-0.25, 0.10, by = 0.05)
    
  ) +
  
  theme(
    
    legend.position = "none",
    
    plot.title = element_text(hjust = 0.5, size = 16),
    
    axis.title.y = element_blank(),
    
    axis.text.y = element_text(size = 12),
    
    axis.title.x = element_text(size = 13),
    
    panel.grid.minor = element_blank()
    
  )
p_final
#Marginal Means Estimation
mm_results <- cregg::mm(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration_mm + Economic_mm + Freedom_of_Speech_mm + Surveillance_mm,
  
  id = ~ respondent_id,
  
  h0 = 0.5,
  
  feature_order = c(
    
    "Collaboration_mm",
    
    "Economic_mm",
    
    "Freedom_of_Speech_mm",
    
    "Surveillance_mm"
    
  ),
  
  feature_labels = list(
    
    Collaboration_mm = "Collaboration",
    
    Economic_mm = "Economic",
    
    Freedom_of_Speech_mm = "Freedom of Speech",
    
    Surveillance_mm = "Surveillance"
    
  ),
  
  level_order = "ascending"
  
)

mm_table <- as.data.frame(mm_results)

plot_data <- mm_table %>%
  
  mutate(
    
    level_clean = case_when(
      
      grepl("Illiberal", level) ~ "Illiberal",
      
      grepl("Liberal", level) ~ "Liberal",
      
      TRUE ~ as.character(level)
      
    ),
    
    feature = factor(
      
      feature,
      
      levels = c(
        
        "Collaboration",
        
        "Free Trade",
        
        "Freedom of Speech",
        
        "Surveillance"
        
      )
      
    )
    
  )

ggplot(plot_data, aes(x = estimate, y = feature, color = level_clean)) +
  
  geom_point(
    
    position = position_dodge(width = 0.4),
    
    size = 3
    
  ) +
  
  geom_errorbarh(
    
    aes(xmin = lower, xmax = upper),
    
    position = position_dodge(width = 0.4),
    
    height = 0.2
    
  ) +
  
  geom_vline(
    
    xintercept = 0.5,
    
    linetype = "dashed"
    
  ) +
  
  labs(
    
    x = "Predicted Probability of Choice",
    
    y = "Attribute",
    
    color = "Level"
    
  ) +
  
  theme_minimal(base_size = 14)

#Robustness check with conditional logit

clogit_model <- clogit(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    strata(RespTask),
  data = df_for_amce
)
summary(clogit_model)

modelsummary(
  
  clogit_model,
  
  output = "latex",
  
  stars = TRUE,
  
  coef_map = c(
    
    "CollaborationIlliberal" = "International Collaboration (Illiberal)",
    
    "EconomicIlliberal" = "Free Trade (Illiberal)",
    
    "Freedom_of_SpeechIlliberal" = "Freedom of Speech (Illiberal)",
    
    "SurveillanceIlliberal" = "State Surveillance (Illiberal)"
    
  ),
  
  gof_omit = "IC|Log|Adj|AIC|BIC"
  
)

modelsummary(
  
  clogit_model,
  
  output = "clogit_model.tex",
  
  stars = TRUE,
  
  coef_map = c(
    
    "CollaborationIlliberal" = "International Collaboration (Illiberal)",
    
    "EconomicIlliberal" = "Free Trade (Illiberal)",
    
    "Freedom_of_SpeechIlliberal" = "Freedom of Speech (Illiberal)",
    
    "SurveillanceIlliberal" = "State Surveillance (Illiberal)"
    
  ),
  
  gof_omit = "IC|Log|Adj|AIC|BIC"
  
)

#Diagnostic with profile-order and task number


amce_control_for_task_order_candidate <- estimatr::lm_robust(outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num),
                                         data = df_for_amce, 
                                         clusters = respondent_id,
                                         se_type = "stata")
summary(amce_control_for_task_order_candidate)


test_order_task <- car::linearHypothesis(
  
  amce_control_for_task_order_candidate,
  
  c(
    
    "factor(candidate)2 = 0",
    
    "factor(task_num)2 = 0",
    
    "factor(task_num)3 = 0",
    
    "factor(task_num)4 = 0"
    
  ),
  
  vcov. = vcov(amce_control_for_task_order_candidate),
  
  test = "F"
  
)
amce_restricted <- estimatr::lm_robust(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  data = df_for_amce, 
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)
summary(amce_restricted)

modelsummary(
  
  amce_restricted,
  
  stars = TRUE,
  
  output = "base_model.tex"
  
)





amce_int_profile_order <-  estimatr::lm_robust(outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)* factor(candidate),
                                     data = df_for_amce, 
                                     clusters = respondent_id,
                                     se_type = "stata")

summary(amce_int_profile_order)

car::linearHypothesis(
  
  amce_int_profile_order,
  
  c(
    
    "CollaborationIlliberal:factor(candidate)2 = 0",
    
    "EconomicIlliberal:factor(candidate)2 = 0",
    
    "Freedom_of_SpeechIlliberal:factor(candidate)2 = 0",
    
    "SurveillanceIlliberal:factor(candidate)2 = 0"
    
  ),
  
  test = "F"
  
)

test_profile_order_int <- car::linearHypothesis(
  
  amce_int_profile_order,
  
  c(
    
    "CollaborationIlliberal:factor(candidate)2 = 0",
    
    "EconomicIlliberal:factor(candidate)2 = 0",
    
    "Freedom_of_SpeechIlliberal:factor(candidate)2 = 0",
    
    "SurveillanceIlliberal:factor(candidate)2 = 0"
    
  ),
  
  vcov. = vcov(amce_int_profile_order),
  
  test = "F"
  
)

amce_int_task_num <-  estimatr::lm_robust(outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)* factor(task_num),
                                               data = df_for_amce, 
                                               clusters = respondent_id,
                                               se_type = "stata")

summary(amce_int_task_num)

coef_names <- names(coef(amce_int_task_num))

int_terms <- coef_names[
  
  grepl(":", coef_names) &
    
    grepl("factor\\(task_num\\)", coef_names)
  
]

R <- matrix(0, nrow = length(int_terms), ncol = length(coef_names))

colnames(R) <- coef_names

rownames(R) <- int_terms

for (i in seq_along(int_terms)) {
  
  R[i, int_terms[i]] <- 1
  
}

test_task_num_int <- car::linearHypothesis(
  
  amce_int_task_num,
  
  hypothesis.matrix = R,
  
  rhs = rep(0, length(int_terms)),
  
  vcov. = vcov(amce_int_task_num),
  
  test = "F"
  
)
############################################################
## Extract F-test statistics and p-values
############################################################

get_f <- function(test_object) {
  test_object[2, "F"]
}

get_p <- function(test_object) {
  test_object[2, "Pr(>F)"]
}

extra_rows <- data.frame(
  term = c(
    "Candidate/task order F-test",
    "Candidate/task order p-value",
    "Candidate-order interactions F-test",
    "Candidate-order interactions p-value",
    "Task-number interactions F-test",
    "Task-number interactions p-value"
  ),
  `(1)` = c("", "", "", "", "", ""),
  `(2)` = c(
    round(get_f(test_order_task), 3),
    round(get_p(test_order_task), 3),
    "",
    "",
    "",
    ""
  ),
  `(3)` = c(
    "",
    "",
    round(get_f(test_profile_order_int), 3),
    round(get_p(test_profile_order_int), 3),
    "",
    ""
  ),
  `(4)` = c(
    "",
    "",
    "",
    "",
    round(get_f(test_task_num_int), 3),
    round(get_p(test_task_num_int), 3)
  ),
  check.names = FALSE
)

############################################################
## Modelsummary table
############################################################

modelsummary(
  list(
    "(1) Baseline AMCE" = restricted_model,
    "(2) Order controls" = amce_control_for_task_order_candidate,
    "(3) Candidate-order interactions" = amce_int_profile_order,
    "(4) Task-number interactions" = amce_int_task_num
  ),
  output = "balance_checks_models.tex",
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "CollaborationIlliberal" = "International Collaboration (Illiberal)",
    "EconomicIlliberal" = "Free Trade (Illiberal)",
    "Freedom_of_SpeechIlliberal" = "Freedom of Speech (Illiberal)",
    "SurveillanceIlliberal" = "State Surveillance (Illiberal)"
  ),
  add_rows = extra_rows,
  gof_map = data.frame(
    raw = c("nobs", "r.squared"),
    clean = c("N", "R-squared"),
    fmt = c(0, 3)
  ),
  notes = "Clustered standard errors by respondent in parentheses. F-tests use the cluster-robust variance-covariance matrix from lm_robust."
)




attributes <- c(
  
  "Collaboration",
  
  "Economic",
  
  "Freedom_of_Speech",
  
  "Surveillance"
  
)

balance_tables <- lapply(attributes, function(a) {
  
  df_for_amce_unattent |>
    
    dplyr::count(candidate, level = .data[[a]], name = "n") |>
    
    dplyr::group_by(candidate) |>
    
    dplyr::mutate(prop = n / sum(n)) |>
    
    dplyr::ungroup() |>
    
    dplyr::mutate(attribute = a) |>
    
    dplyr::select(attribute, candidate, attribute_level = level, n, prop)
  
}) |>
  
  dplyr::bind_rows()

balance_tables
balance_chisq <- lapply(attributes, function(a) {
  
  tab <- table(df_for_amce[[a]], df_for_amce$candidate)
  
  test <- chisq.test(tab)
  
  
  
  data.frame(
    
    attribute = a,
    
    statistic = unname(test$statistic),
    
    df = unname(test$parameter),
    
    p_value = unname(test$p.value)
    
  )
  
}) |>
  
  bind_rows()


balance_tables
balance_chisq


#AMCEs with soc-dem covariates

amce_covariates_soc_dem <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num)+
    Age + Female + factor(Income_Level) + factor(Education_Level),
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)
summary(amce_covariates_soc_dem)

amce_covariates_soc_dem_with_no_cat <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num)+
    Age + Female + Income_Level + Education_Level,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)
summary(amce_covariates_soc_dem_with_no_cat)

amce_covariates_soc_dem_logit <- glm(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    factor(candidate) + factor(task_num) +
    Age + Female + Income_Level + Education_Level,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_soc_dem_logit <- sandwich::vcovCL(
  amce_covariates_soc_dem_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  amce_covariates_soc_dem_logit,
  vcov = cluster_se_soc_dem_logit
)

logit_amce_covariates_soc_dem <- glm(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    
    Age + Female + factor(Income_Level) + factor(Education_Level),
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

cluster_se <- vcovCL(logit_amce_covariates_soc_dem, cluster = ~ respondent_id)

coeftest(logit_amce_covariates_soc_dem, vcov = cluster_se)

amce_covariates_with_pol <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num)
    + Authoritarianism_c + President_Trust_c + Nat_Interests_v_Ind_Rights_c + Strong_Leader_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)
summary(amce_covariates_with_pol)

logit_amce_covariates_with_pol <- glm(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num) +
    
    Authoritarianism_c + President_Trust_c + Nat_Interests_v_Ind_Rights_c +
    
    Strong_Leader_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

cluster_se <- sandwich::vcovCL(
  
  logit_amce_covariates_with_pol,
  
  cluster = ~ respondent_id,
  
  type = "HC3"
  
)

coeftest(
  
  logit_amce_covariates_with_pol,
  
  vcov = cluster_se
  
)

amce_covariates_with_pol_soc <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num)
    + Age + Female + factor(Income_Level) + factor(Education_Level) +Authoritarianism_c + President_Trust_c + Nat_Interests_v_Ind_Rights_c + Strong_Leader_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)
summary(amce_covariates_with_pol_soc)

amce_covariates_with_pol_soc_no_cat <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance + factor(candidate) + factor(task_num)
  + Age + Female + Income_Level + Education_Level +Authoritarianism_c + President_Trust_c + Nat_Interests_v_Ind_Rights_c + Strong_Leader_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)
summary(amce_covariates_with_pol_soc_no_cat)

amce_covariates_with_pol_soc_media <- estimatr::lm_robust(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    factor(candidate) + factor(task_num) +
    Age + Female +
    factor(Income_Level) + factor(Education_Level) +
    Authoritarianism_c + President_Trust_c +
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c +
    Television_Frequency_c,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
)

summary(amce_covariates_with_pol_soc_media)


amce_covariates_with_pol_soc_media_logit <- glm(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    factor(candidate) + factor(task_num) +
    Age + Female +
    factor(Income_Level) + factor(Education_Level) +
    Authoritarianism_c + President_Trust_c +
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c +
    Television_Frequency_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
)

cluster_se_pol_soc_media_logit <- sandwich::vcovCL(
  amce_covariates_with_pol_soc_media_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  amce_covariates_with_pol_soc_media_logit,
  vcov = cluster_se_pol_soc_media_logit
)

amce_covariates_with_pol_soc_media_no_cat <- estimatr::lm_robust(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    
    factor(candidate) + factor(task_num) +
    
    Age + Female +
    
    Income_Level + Education_Level +
    
    Authoritarianism_c + President_Trust_c +
    
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c +
    
    Television_Frequency_c,
  
  
  
  data = df_for_amce,
  
  
  
  clusters = respondent_id,
  
  
  
  se_type = "stata"
  
)

summary(amce_covariates_with_pol_soc_media_no_cat)

# -----------------------------

# Logit: numeric income/education

# -----------------------------

amce_covariates_with_pol_soc_media_no_cat_logit <- glm(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    
    factor(candidate) + factor(task_num) +
    
    Age + Female +
    
    Income_Level + Education_Level +
    
    Authoritarianism_c + President_Trust_c +
    
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c +
    
    Television_Frequency_c,
  
  
  
  data = df_for_amce,
  
  
  
  family = binomial(link = "logit")
  
)

cluster_se_pol_soc_media_no_cat_logit <- sandwich::vcovCL(
  
  amce_covariates_with_pol_soc_media_no_cat_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

lmtest::coeftest(
  
  amce_covariates_with_pol_soc_media_no_cat_logit,
  
  vcov = cluster_se_pol_soc_media_no_cat_logit
  
)

amce_covariates_with_pol_soc_logit <- glm(
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    factor(candidate) + factor(task_num) +
    Age + Female + Income_Level + Education_Level +
    Authoritarianism_c + President_Trust_c +
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_pol_soc_logit <- sandwich::vcovCL(
  amce_covariates_with_pol_soc_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  amce_covariates_with_pol_soc_logit,
  vcov = cluster_se_pol_soc_logit
)

logit_amce_covariates_with_pol_soc <- glm(
  
  outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +
    
    factor(candidate) + factor(task_num) +
    
    Age + Female + factor(Income_Level) + factor(Education_Level) +
    
    Authoritarianism_c + President_Trust_c +
    
    Nat_Interests_v_Ind_Rights_c + Strong_Leader_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

coeftest(
  
  logit_amce_covariates_with_pol_soc,
  
  vcov. = vcovCL(
    
    logit_amce_covariates_with_pol_soc,
    
    cluster = ~ respondent_id,
    
    data = df_for_amce,
    
    type = "HC1"
    
  )
  
)

combined_models <- list(
  "(1) LPM: Baseline" = amce_restricted,
  "(2) LPM: Soc-dem" = amce_covariates_soc_dem,
  "(3) LPM: Political" = amce_covariates_with_pol,
  "(4) LPM: Full" = amce_covariates_with_pol_soc,
  "(5) Logit: Soc-dem" = logit_amce_covariates_soc_dem,
  "(6) Logit: Political" = logit_amce_covariates_with_pol,
  "(7) Logit: Full" = logit_amce_covariates_with_pol_soc
)

combined_vcov <- list(
  vcov(amce_restricted),
  vcov(amce_covariates_soc_dem),
  vcov(amce_covariates_with_pol),
  vcov(amce_covariates_with_pol_soc),
  sandwich::vcovCL(logit_amce_covariates_soc_dem,
                   cluster = df_for_amce$respondent_id,
                   type = "HC1"),
  sandwich::vcovCL(logit_amce_covariates_with_pol,
                   cluster = df_for_amce$respondent_id,
                   type = "HC1"),
  sandwich::vcovCL(logit_amce_covariates_with_pol_soc,
                   cluster = df_for_amce$respondent_id,
                   type = "HC1")
)

extra_rows_combined <- data.frame(
  term = c(
    "Estimator",
    "Candidate/task order controls",
    "Socio-demographic covariates",
    "Political covariates"
  ),
  `(1)` = c("LPM",   "No",  "No",  "No"),
  `(2)` = c("LPM",   "Yes", "Yes", "No"),
  `(3)` = c("LPM",   "Yes", "No",  "Yes"),
  `(4)` = c("LPM",   "Yes", "Yes", "Yes"),
  `(5)` = c("Logit", "Yes", "Yes", "No"),
  `(6)` = c("Logit", "Yes", "No",  "Yes"),
  `(7)` = c("Logit", "Yes", "Yes", "Yes"),
  check.names = FALSE
)

modelsummary(
  combined_models,
  vcov = combined_vcov,
  output = "amce_lpm_logit_covariate_robustness.tex",
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "CollaborationIlliberal" = "International Collaboration (Illiberal)",
    "EconomicIlliberal" = "Free Trade (Illiberal)",
    "Freedom_of_SpeechIlliberal" = "Freedom of Speech (Illiberal)",
    "SurveillanceIlliberal" = "State Surveillance (Illiberal)"
  ),
  coef_omit = paste(
    "factor\\(candidate\\)",
    "factor\\(task_num\\)",
    "Age", "Female",
    "Income_Level", "Education_Level",
    "Authoritarianism_c", "President_Trust_c",
    "Nat_Interests_v_Ind_Rights_c", "Strong_Leader_c",
    sep = "|"
  ),
  add_rows = extra_rows_combined,
  gof_map = data.frame(
    raw = c("nobs", "r.squared"),
    clean = c("N", "R-squared"),
    fmt = c(0, 3)
  ),
  notes = "Cluster-robust standard errors by respondent in parentheses. LPM coefficients are probability effects; logit coefficients are log-odds. Only main attribute coefficients are reported."
)

Female_amces <- estimatr::lm_robust(Female ~ Collaboration + Economic + Freedom_of_Speech + Surveillance, 
                                              data = df_for_amce,
                                              clusters = respondent_id,
                                              se_type = "stata")
summary(Female_amces)

Television_use_amces <- estimatr::lm_robust(Television_Use ~ Collaboration + Economic + Freedom_of_Speech + Surveillance, 
                                    data = df_for_amce,
                                    clusters = respondent_id,
                                    se_type = "stata")
summary(Television_use_amces)

Authoritarianism_amces <- estimatr::lm_robust(Authoritarianism ~ Collaboration + Economic + Freedom_of_Speech + Surveillance, 
                                            data = df_for_amce,
                                            clusters = respondent_id,
                                            se_type = "stata")
summary(Authoritarianism_amces)

president_trust_amces <- estimatr::lm_robust(President_Trust ~ Collaboration + Economic + Freedom_of_Speech + Surveillance, 
                                              data = df_for_amce,
                                              clusters = respondent_id,
                                              se_type = "stata")
summary(president_trust_amces)



age_amces <- estimatr::lm_robust(Age ~ Collaboration + Economic + Freedom_of_Speech + Surveillance, 
                                 data = df_for_amce,
                                 clusters = respondent_id,
                                 se_type = "stata" )
summary(age_amces)


### Interaction Effects Between Attributes

#Baseline Model

res_amce_interactions <- cjoint::amce(
  
  data = df_for_amce,
  
  formula = outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)^2,
  
  respondent.id = "respondent_id"
)

summary(res_amce_interactions)
amce_interactions_lm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)^2,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(amce_interactions_lm)

amce_int_sum <- summary(res_amce_interactions)

amce_int_df_raw <- as.data.frame(amce_int_sum$acie)

amce_int_df <- amce_int_df_raw %>%
  
  dplyr::filter(grepl(":", Level)) %>%
  
  dplyr::mutate(
    
    
    
    # Build interaction label using attribute names
    
    Interaction = paste0(Attribute, ": ", Level),
    
    
    
    # Clean attribute names
    
    Interaction = gsub("Collaboration", "International Collaboration", Interaction),
    
    Interaction = gsub("Economic", "Free Trade", Interaction),
    
    Interaction = gsub("Freedom_of_Speech", "Freedom of Speech", Interaction),
    
    Interaction = gsub("Surveillance", "State Surveillance", Interaction),
    
    
    
    # Add stars
    
    stars = dplyr::case_when(
      
      `Pr(>|z|)` < 0.001 ~ "***",
      
      `Pr(>|z|)` < 0.01  ~ "**",
      
      `Pr(>|z|)` < 0.05  ~ "*",
      
      `Pr(>|z|)` < 0.1   ~ "+",
      
      TRUE ~ ""
      
    ),
    
    
    
    Estimate = paste0(sprintf("%.3f", Estimate), stars),
    
    `Std. Error` = paste0("(", sprintf("%.3f", `Std. Err`), ")")
    
    
    
  ) %>%
  
  dplyr::select(
    
    Interaction,
    
    Estimate,
    
    `Std. Error`
    
  )

kableExtra::kbl(
  
  amce_int_df,
  
  format = "latex",
  
  booktabs = TRUE,
  
  escape = FALSE,
  
  caption = "Pairwise Interaction Effects (ACIE) in the Conjoint Experiment",
  
  col.names = c("Interaction", "Estimate", "Std. Error")
  
) %>%
  
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  
  kableExtra::save_kable("acie_interaction_effects.tex")



modelsummary(
  
  amce_interactions_lm,
  
  output = "amce_interactions_lm.tex",
  
  stars = TRUE,
  
  estimate = "{estimate}{stars}",
  
  statistic = "({std.error})",
  
  
  
  coef_map = c(
    
    # Main effects
    
    "CollaborationIlliberal" = "International Collaboration (Illiberal)",
    
    "EconomicIlliberal" = "Free Trade (Illiberal)",
    
    "Freedom_of_SpeechIlliberal" = "Freedom of Speech (Illiberal)",
    
    "SurveillanceIlliberal" = "State Surveillance (Illiberal)",
    
    
    
    # Interactions
    
    "CollaborationIlliberal:EconomicIlliberal" =
      
      "International Collaboration × Free Trade",
    
    "CollaborationIlliberal:Freedom_of_SpeechIlliberal" =
      
      "International Collaboration × Freedom of Speech",
    
    "CollaborationIlliberal:SurveillanceIlliberal" =
      
      "International Collaboration × Surveillance",
    
    "EconomicIlliberal:Freedom_of_SpeechIlliberal" =
      
      "Free Trade × Freedom of Speech",
    
    "EconomicIlliberal:SurveillanceIlliberal" =
      
      "Free Trade × Surveillance",
    
    "Freedom_of_SpeechIlliberal:SurveillanceIlliberal" =
      
      "Freedom of Speech × Surveillance"
    
  ),
  
  
  
  gof_map = data.frame(
    
    raw = c("nobs", "r.squared"),
    
    clean = c("N", "R-squared"),
    
    fmt = c(0, 3)
    
  ),
  
  
  
  notes = "Cluster-robust standard errors by respondent in parentheses."
  
)



res_amce_interactions$formula



res_amie_for_inter <- cjoint::amce(
  
  data = df_for_amce,
  
  formula = outcome ~ Collaboration:Economic + Freedom_of_Speech:Surveillance,
  
  respondent.id = "respondent_id"
  
)
summary(res_amie_for_inter)

############################################################

## 1. Selected interaction model

############################################################

amie_for_inter_lm <- estimatr::lm_robust(
  
  outcome ~ Collaboration*Economic + Freedom_of_Speech*Surveillance,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(amie_for_inter_lm)

############################################################

## 2. Full pairwise interaction model

############################################################



############################################################

## F-tests for interaction terms

############################################################

# Selected model: all interaction terms

coef_names_selected <- names(coef(amie_for_inter_lm))

selected_int_terms <- coef_names_selected[
  
  grepl(":", coef_names_selected)
  
]

R_selected <- matrix(0, nrow = length(selected_int_terms), ncol = length(coef_names_selected))

colnames(R_selected) <- coef_names_selected

rownames(R_selected) <- selected_int_terms

for (i in seq_along(selected_int_terms)) {
  
  R_selected[i, selected_int_terms[i]] <- 1
  
}

f_test_selected_interactions <- car::linearHypothesis(
  
  amie_for_inter_lm,
  
  hypothesis.matrix = R_selected,
  
  rhs = rep(0, length(selected_int_terms)),
  
  vcov. = vcov(amie_for_inter_lm),
  
  test = "F"
  
)

# Full model: all pairwise interaction terms

coef_names_full <- names(coef(amce_interactions_lm))

full_int_terms <- coef_names_full[
  
  grepl(":", coef_names_full)
  
]

R_full <- matrix(0, nrow = length(full_int_terms), ncol = length(coef_names_full))

colnames(R_full) <- coef_names_full

rownames(R_full) <- full_int_terms

for (i in seq_along(full_int_terms)) {
  
  R_full[i, full_int_terms[i]] <- 1
  
}

f_test_full_interactions <- car::linearHypothesis(
  
  amce_interactions_lm,
  
  hypothesis.matrix = R_full,
  
  rhs = rep(0, length(full_int_terms)),
  
  vcov. = vcov(amce_interactions_lm),
  
  test = "F"
  
)

############################################################

## Extract F-test values

############################################################

get_f <- function(x) round(x[2, "F"], 3)

get_p <- function(x) round(x[2, "Pr(>F)"], 3)

extra_rows_interactions <- data.frame(
  
  term = c(
    
    "Interaction terms F-test",
    
    "Interaction terms p-value"
    
  ),
  
  `(1)` = c(
    
    get_f(f_test_selected_interactions),
    
    get_p(f_test_selected_interactions)
    
  ),
  
  `(2)` = c(
    
    get_f(f_test_full_interactions),
    
    get_p(f_test_full_interactions)
    
  ),
  
  check.names = FALSE
  
)

############################################################

## Modelsummary table

############################################################

modelsummary(
  
  list(
    
    "(1) Selected interactions" = amie_for_inter_lm,
    
    "(2) Full pairwise interactions" = amce_interactions_lm
    
  ),
  
  output = "amce_interaction_models.tex",
  
  stars = TRUE,
  
  estimate = "{estimate}{stars}",
  
  statistic = "({std.error})",
  
  coef_map = c(
    
    # Main effects, mostly for full model
    
    "CollaborationIlliberal" =
      
      "International Collaboration (Illiberal)",
    
    "EconomicIlliberal" =
      
      "Free Trade (Illiberal)",
    
    "Freedom_of_SpeechIlliberal" =
      
      "Freedom of Speech (Illiberal)",
    
    "SurveillanceIlliberal" =
      
      "State Surveillance (Illiberal)",
    
    # Selected/full interaction terms
    
    "CollaborationIlliberal:EconomicIlliberal" =
      
      "International Collaboration × Free Trade",
    
    "Freedom_of_SpeechIlliberal:SurveillanceIlliberal" =
      
      "Freedom of Speech × State Surveillance",
    
    "CollaborationIlliberal:Freedom_of_SpeechIlliberal" =
      
      "International Collaboration × Freedom of Speech",
    
    "CollaborationIlliberal:SurveillanceIlliberal" =
      
      "International Collaboration × State Surveillance",
    
    "EconomicIlliberal:Freedom_of_SpeechIlliberal" =
      
      "Free Trade × Freedom of Speech",
    
    "EconomicIlliberal:SurveillanceIlliberal" =
      
      "Free Trade × State Surveillance"
    
  ),
  
  add_rows = extra_rows_interactions,
  
  gof_map = data.frame(
    
    raw = c("nobs", "r.squared"),
    
    clean = c("N", "R-squared"),
    
    fmt = c(0, 3)
    
  ),
  
  notes = "Cluster-robust standard errors by respondent in parentheses. F-tests use the cluster-robust variance-covariance matrix."
  
)



fit_ame <- FindIt::CausalANOVA(
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  data = df_findit,
  
  pair.id = df_findit$RespTask,
  
  diff = TRUE,
  
  nway = 2,
  
  family = "binomial",
  
  cluster = df_findit$respondent_id,
  
  screen = FALSE,
  
  collapse = FALSE,
  
  verbose = TRUE
  
)

summary(fit_ame)


### Interaction Effects with Respondent Characteristics


## Authoritarianism





Authoritarianism_test <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Authoritarianism_c ,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Authoritarianism_c"
)

summary(Authoritarianism_test)


unrestricted_model_auth <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Authoritarianism_c,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(unrestricted_model_auth)


car::linearHypothesis(
  
  unrestricted_model_auth,
  
  c("Authoritarianism_c = 0",
    
    "CollaborationIlliberal:Authoritarianism_c = 0",
    
    "EconomicIlliberal:Authoritarianism_c = 0",
    
    "Freedom_of_SpeechIlliberal:Authoritarianism_c = 0",
    
    "SurveillanceIlliberal:Authoritarianism_c = 0"
    
  ),
  test = 'F')




Authoritarianism_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Authoritarianism_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

cluster_se <- sandwich::vcovCL(
  
  Authoritarianism_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

lmtest::coeftest(
  
  Authoritarianism_logit,
  
  vcov = cluster_se
  
)
cluster_se <- sandwich::vcovCL(
  Authoritarianism_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

car::linearHypothesis(
  Authoritarianism_logit,
  c(
    "Authoritarianism_c = 0",
    "CollaborationIlliberal:Authoritarianism_c = 0",
    "EconomicIlliberal:Authoritarianism_c = 0",
    "Freedom_of_SpeechIlliberal:Authoritarianism_c = 0",
    "SurveillanceIlliberal:Authoritarianism_c = 0"
  ),
  vcov. = cluster_se,
  test = "Chisq"
)

cluster_se_auth_logit <- sandwich::vcovCL(
  
  Authoritarianism_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

Authoritarianism_probit <- glm(
  
  
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Authoritarianism_c,
  
  
  
  data = df_for_amce,
  
  
  
  family = binomial(link = "probit")
  
  
  
)

cluster_se_probit <- sandwich::vcovCL(
  
  
  
  Authoritarianism_probit,
  
  
  
  cluster = ~ respondent_id,
  
  
  
  type = "HC1"
  
  
  
)

lmtest::coeftest(
  
  
  
  Authoritarianism_probit,
  
  
  
  vcov = cluster_se_probit
  
  
  
)

car::linearHypothesis(
  
  
  
  Authoritarianism_probit,
  
  
  
  c(
    
    "Authoritarianism_c = 0",
    
    "CollaborationIlliberal:Authoritarianism_c = 0",
    
    "EconomicIlliberal:Authoritarianism_c = 0",
    
    "Freedom_of_SpeechIlliberal:Authoritarianism_c = 0",
    
    "SurveillanceIlliberal:Authoritarianism_c = 0"
    
  ),
  
  
  
  vcov. = cluster_se_probit,
  
  
  
  test = "Chisq"
  
  
  
)

cluster_se_auth_probit <- sandwich::vcovCL(
  
  
  
  Authoritarianism_probit,
  
  
  
  cluster = ~ respondent_id,
  
  
  
  type = "HC1"
  
  

# LPM joint F-test

# LPM F-test

auth_lpm_test <- car::linearHypothesis(
  
  unrestricted_model_auth,
  
  c(
    
    "Authoritarianism_c = 0",
    
    "CollaborationIlliberal:Authoritarianism_c = 0",
    
    "EconomicIlliberal:Authoritarianism_c = 0",
    
    "Freedom_of_SpeechIlliberal:Authoritarianism_c = 0",
    
    "SurveillanceIlliberal:Authoritarianism_c = 0"
    
  ),
  
  test = "F"
  
)

F_stat  <- auth_lpm_test$F[2]

F_pval  <- auth_lpm_test$`Pr(>F)`[2]

# Logit chi-square test

auth_logit_test <- car::linearHypothesis(
  
  Authoritarianism_logit,
  
  c(
    
    "Authoritarianism_c = 0",
    
    "CollaborationIlliberal:Authoritarianism_c = 0",
    
    "EconomicIlliberal:Authoritarianism_c = 0",
    
    "Freedom_of_SpeechIlliberal:Authoritarianism_c = 0",
    
    "SurveillanceIlliberal:Authoritarianism_c = 0"
    
  ),
  
  vcov. = cluster_se_auth_logit,
  
  test = "Chisq"
  
)

Chi_stat <- auth_logit_test$Chisq[2]

Chi_pval <- auth_logit_test$`Pr(>Chisq)`[2]
# Logit joint Wald chi-square test
extra_rows <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    sprintf("%.2f", F_stat),
    
    sprintf("%.3f", F_pval)
    
  ),
  
  Logit = c(
    
    sprintf("%.2f", Chi_stat),
    
    sprintf("%.3f", Chi_pval)
    
  )
  
)
modelsummary(
  
  list(
    
    "LPM" = unrestricted_model_auth,
    
    "Logit" = Authoritarianism_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    cluster_se_auth_logit
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows,
  
  title = "Interaction Between Conjoint Attributes and Authoritarianism",
  
  output = "auth_interaction_models.tex"
  
)
auth_sum <- summary(Authoritarianism_test)

auth_25 <- as.data.frame(auth_sum$Authoritarianismc1amce)

auth_50 <- as.data.frame(auth_sum$Authoritarianismc2amce)

auth_75 <- as.data.frame(auth_sum$Authoritarianismc3amce)

auth_25$quantile <- "25%"

auth_50$quantile <- "50%"

auth_75$quantile <- "75%"

conditional_amces_auth <- rbind(auth_25, auth_50, auth_75)

conditional_amces_auth
ggplot(
  
  conditional_amces_auth,
  
  aes(x = Estimate, y = Level, color = quantile)
  
) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "Authoritarianism",
    
    title = "Conditional AMCEs by Authoritarianism"
    
  ) +
  
  theme_minimal()





out_collab_auth <- interflex(
  
  Y = "outcome",
  
  D = "Collaboration",
  
  X = "Authoritarianism_c",
  
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"
        
        ),
  
  data = df_for_amce,
  
  estimator = "binning",
  
  vcov.type = "cluster",
  
  nbins = 3,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Xlabel = "Authoritarianism",
  Dlabel = "Illiberal Collaboration",
  Ylabel = "Candidate Choice",
  full.moderate = TRUE
  
)

plot(out_collab_auth)

out_econ_auth <- interflex(
  
  Y = "outcome",
  
  D = "Economic",
  
  X = "Authoritarianism_c",
  
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  
  data = df_for_amce,
  
  estimator = "binning",
  
  vcov.type = "cluster",
  
  nbins = 3,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Xlabel = "Authoritarianism",
  Dlabel = "Illiberal Foreign Economic Policy",
  Ylabel = "Candidate Choice",
  full.moderate = TRUE
  
)

plot(out_econ_auth)

out_speech_auth <- interflex(
  
  Y = "outcome",
  
  D = "Freedom_of_Speech",
  
  X = "Authoritarianism_c",
  
  Z = c("Collaboration", "Economic", "Surveillance"),
  
  data = df_for_amce,
  
  estimator = "binning",
  
  vcov.type = "cluster",
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Xlabel = "Authoritarianism",
  Dlabel = "Illiberal Freedom of Speech",
  Ylabel = "Candidate Choice",
  full.moderate = TRUE
  
)

plot(out_speech_auth)

out_surv_auth <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Authoritarianism_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal", 
  full.moderate = TRUE,
  main = "",
  Xlabel = "Authoritarianism",
  Dlabel = "Illiberal Surveillance",
  Ylabel = "Candidate Choice"
)

plot(out_surv_auth)
out_surv_auth$model.binning


out_surv_auth$est.bin
q_auth <- quantile(
  
  df_for_amce$Authoritarianism_c,
  
  probs = c(0.25, 0.50, 0.75),
  
  na.rm = TRUE
  
)
q_auth
plots_auth <- list(
  
  collab = out_collab_auth,
  
  econ   = out_econ_auth,
  
  speech = out_speech_auth,
  
  surv   = out_surv_auth
  
)
  
  
  



## President Trust
restricted_model <- estimatr::lm_robust(
outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
data = df_for_amce,
clusters = respondent_id,
se_type = "stata"
)


unrestricted_model_president_trust <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * President_Trust_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(unrestricted_model_president_trust)

pres_lpm_test <- car::linearHypothesis(
  unrestricted_model_president_trust,
  c(
    "President_Trust_c = 0",
    "CollaborationIlliberal:President_Trust_c = 0",
    "EconomicIlliberal:President_Trust_c = 0",
    "Freedom_of_SpeechIlliberal:President_Trust_c = 0",
    "SurveillanceIlliberal:President_Trust_c = 0"
  ),
  test = "F"
)

F_stat_pres <- pres_lpm_test$F[2]
F_pval_pres <- pres_lpm_test$`Pr(>F)`[2]


President_Trust_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * President_Trust_c,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_pres_logit <- sandwich::vcovCL(
  President_Trust_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  President_Trust_logit,
  vcov = cluster_se_pres_logit
)

pres_logit_test <- car::linearHypothesis(
  President_Trust_logit,
  c(
    "President_Trust_c = 0",
    "CollaborationIlliberal:President_Trust_c = 0",
    "EconomicIlliberal:President_Trust_c = 0",
    "Freedom_of_SpeechIlliberal:President_Trust_c = 0",
    "SurveillanceIlliberal:President_Trust_c = 0"
  ),
  vcov. = cluster_se_pres_logit,
  test = "Chisq"
)

Chi_stat_pres <- pres_logit_test$Chisq[2]
Chi_pval_pres <- pres_logit_test$`Pr(>Chisq)`[2]


extra_rows_pres <- data.frame(
  term = c("Joint test (F / Chi²)", "p-value"),
  LPM = c(
    sprintf("%.2f", F_stat_pres),
    sprintf("%.3f", F_pval_pres)
  ),
  Logit = c(
    sprintf("%.2f", Chi_stat_pres),
    sprintf("%.3f", Chi_pval_pres)
  )
)

modelsummary::modelsummary(
  list(
    "LPM" = unrestricted_model_president_trust,
    "Logit" = President_Trust_logit
  ),
  vcov = list(
    NULL,
    cluster_se_pres_logit
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  add_rows = extra_rows_pres,
  title = "Interaction Between Conjoint Attributes and President Trust",
  output = "president_trust_interaction_models.tex"
)

President_Trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * President_Trust_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "President_Trust_c"
)

summary(President_Trust_int)
pres_sum <- summary(President_Trust_int)
pres_sum$PresidentTrustc1amce
pres_25 <- as.data.frame(pres_sum$PresidentTrustc1amce)

pres_50 <- as.data.frame(pres_sum$PresidentTrustc2amce)

pres_75 <- as.data.frame(pres_sum$PresidentTrustc3amce)

pres_25$quantile <- "25%"

pres_50$quantile <- "50%"

pres_75$quantile <- "75%"

conditional_amces_pres <- rbind(pres_25, pres_50, pres_75)
ggplot(
  
  conditional_amces_pres,
  
  aes(x = Estimate, y = Level, color = quantile)
  
) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "President trust",
    
    title = "Conditional AMCEs by President Trust"
    
  ) +
  
  theme_minimal()

out_collab_trust <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "President_Trust_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "President Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Collaboration"
)

plot(out_collab_trust)

out_econ_trust <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "President_Trust_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "President Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Free Trade"
)

plot(out_econ_trust)

out_speech_trust <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "President_Trust_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "President Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Freedom of Speech"
)

plot(out_speech_trust)

out_surv_trust <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "President_Trust_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "President Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Surveillance"
)

plot(out_surv_trust)


## National Interests vs Individual Rights



# LPM

NatInt_Rights_int_lpm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Nat_Interests_v_Ind_Rights_c,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(NatInt_Rights_int_lpm)

# Logit

NatInt_Rights_int_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Nat_Interests_v_Ind_Rights_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

cluster_se_natint_logit <- sandwich::vcovCL(
  
  NatInt_Rights_int_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

lmtest::coeftest(
  
  NatInt_Rights_int_logit,
  
  vcov = cluster_se_natint_logit
  
)

# LPM joint F-test

natint_lpm_test <- car::linearHypothesis(
  
  NatInt_Rights_int_lpm,
  
  c(
    
    "Nat_Interests_v_Ind_Rights_c = 0",
    
    "CollaborationIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "EconomicIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "Freedom_of_SpeechIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "SurveillanceIlliberal:Nat_Interests_v_Ind_Rights_c = 0"
    
  ),
  
  test = "F"
  
)

natint_lpm_test

# Logit joint Wald chi-square test

natint_logit_test <- car::linearHypothesis(
  
  NatInt_Rights_int_logit,
  
  c(
    
    "Nat_Interests_v_Ind_Rights_c = 0",
    
    "CollaborationIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "EconomicIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "Freedom_of_SpeechIlliberal:Nat_Interests_v_Ind_Rights_c = 0",
    
    "SurveillanceIlliberal:Nat_Interests_v_Ind_Rights_c = 0"
    
  ),
  
  vcov. = cluster_se_natint_logit,
  
  test = "Chisq"
  
)

# Extract test statistics

F_stat_natint <- natint_lpm_test$F[2]

F_pval_natint <- natint_lpm_test$`Pr(>F)`[2]

Chi_stat_natint <- natint_logit_test$Chisq[2]

Chi_pval_natint <- natint_logit_test$`Pr(>Chisq)`[2]

extra_rows_natint <- data.frame(
  
  term = c("Joint test: Moderator + Interactions", "p-value"),
  
  LPM = c(
    
    sprintf("%.2f", F_stat_natint),
    
    sprintf("%.3f", F_pval_natint)
    
  ),
  
  Logit = c(
    
    sprintf("%.2f", Chi_stat_natint),
    
    sprintf("%.3f", Chi_pval_natint)
    
  )
  
)

modelsummary(
  
  list(
    
    "LPM" = NatInt_Rights_int_lpm,
    
    "Logit" = NatInt_Rights_int_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    cluster_se_natint_logit
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows_natint,
  
  title = "Interaction Between Conjoint Attributes and National Interests vs Individual Rights",
  
  output = "natint_rights_interaction_models.tex"
  
)

NatInt_Rights_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Nat_Interests_v_Ind_Rights_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Nat_Interests_v_Ind_Rights_c"
)

summary(NatInt_Rights_int)

natint_sum <- summary(NatInt_Rights_int)

natint_25 <- as.data.frame(natint_sum$NatInterestsvIndRightsc1amce)
natint_50 <- as.data.frame(natint_sum$NatInterestsvIndRightsc2amce)
natint_75 <- as.data.frame(natint_sum$NatInterestsvIndRightsc3amce)

natint_25$quantile <- "25%"
natint_50$quantile <- "50%"
natint_75$quantile <- "75%"

conditional_amces_natint <- rbind(
  natint_25,
  natint_50,
  natint_75
)

ggplot(
  
  conditional_amces_natint,
  
  aes(x = Estimate, y = Level, color = quantile)
  
) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "Individual Rights vs National Interests",
    
    title = "Conditional AMCEs by Preference for Individual Interests"
    
  ) +
  
  theme_minimal()
out_collab_natint <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Nat_Interests_v_Ind_Rights_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Preference for National Interests",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Collaboration"
)

plot(out_collab_natint)


out_econ_natint <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Nat_Interests_v_Ind_Rights_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Preference for National Interests",
  Ylabel = "Candidate Choice", 
  Dlabel = "Illiberal Free Trade"
)

plot(out_econ_natint)


out_speech_natint <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Nat_Interests_v_Ind_Rights_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Preference for National Interests",
  Ylabel = "Candidate Choice",
  Dlabel = "Freedom of Speech"
  
)

plot(out_speech_natint)


out_surv_natint <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Nat_Interests_v_Ind_Rights_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Preference for National Interests",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Surveillance"
)

plot(out_surv_natint)

## Strong Leader
strong_leader_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Strong_Leader_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Strong_Leader_c"
)

summary(strong_leader_int)

strong_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Strong_Leader_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(strong_model)

strong_lpm_test <- car::linearHypothesis(
  strong_model,
  c(
    "Strong_Leader_c = 0",
    "CollaborationIlliberal:Strong_Leader_c = 0",
    "EconomicIlliberal:Strong_Leader_c = 0",
    "Freedom_of_SpeechIlliberal:Strong_Leader_c = 0",
    "SurveillanceIlliberal:Strong_Leader_c = 0"
  ),
  test = "F"
)

F_stat_strong <- strong_lpm_test$F[2]
F_pval_strong <- strong_lpm_test$`Pr(>F)`[2]


Strong_Leader_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Strong_Leader_c,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_strong_logit <- sandwich::vcovCL(
  Strong_Leader_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  Strong_Leader_logit,
  vcov = cluster_se_strong_logit
)

strong_logit_test <- car::linearHypothesis(
  Strong_Leader_logit,
  c(
    "Strong_Leader_c = 0",
    "CollaborationIlliberal:Strong_Leader_c = 0",
    "EconomicIlliberal:Strong_Leader_c = 0",
    "Freedom_of_SpeechIlliberal:Strong_Leader_c = 0",
    "SurveillanceIlliberal:Strong_Leader_c = 0"
  ),
  vcov. = cluster_se_strong_logit,
  test = "Chisq"
)

Chi_stat_strong <- strong_logit_test$Chisq[2]
Chi_pval_strong <- strong_logit_test$`Pr(>Chisq)`[2]


extra_rows_strong <- data.frame(
  term = c("Joint test (F / Chi²)", "p-value"),
  LPM = c(
    sprintf("%.2f", F_stat_strong),
    sprintf("%.3f", F_pval_strong)
  ),
  Logit = c(
    sprintf("%.2f", Chi_stat_strong),
    sprintf("%.3f", Chi_pval_strong)
  )
)

modelsummary::modelsummary(
  list(
    "LPM" = strong_model,
    "Logit" = Strong_Leader_logit
  ),
  vcov = list(
    NULL,
    cluster_se_strong_logit
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  add_rows = extra_rows_strong,
  title = "Interaction Between Conjoint Attributes and Strong Leader Support",
  output = "strong_leader_interaction_models.tex"
)

strong_sum <- summary(strong_leader_int)


strong_25 <- as.data.frame(strong_sum$StrongLeaderc1amce)
strong_50 <- as.data.frame(strong_sum$StrongLeaderc2amce)
strong_75 <- as.data.frame(strong_sum$StrongLeaderc3amce)

strong_25$quantile <- "25%"
strong_50$quantile <- "50%"
strong_75$quantile <- "75%"

conditional_amces_strong <- rbind(strong_25, strong_50, strong_75)

ggplot(
  conditional_amces_strong,
  aes(x = Estimate, y = Level, color = quantile)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Strong leader",
    title = "Conditional AMCEs by Strong Leader"
  ) +
  theme_minimal()
out_collab_strong <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Strong_Leader_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Support for Strong Leader",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Collaboration"
)

plot(out_collab_strong)


out_econ_strong <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Strong_Leader_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Support for Strong Leader",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Free Trade"
)

plot(out_econ_strong)


out_speech_strong <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Strong_Leader_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Support for Strong Leader",
  Ylabel = "Candidate Choice",
  Dlabel = "Freedom of Speech"
)

plot(out_speech_strong)


out_surv_strong <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Strong_Leader_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Support for Strong Leader",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Surveillance"
)

plot(out_surv_strong)

## Television Exposure
df_for_amce$Televisionnever <- factor(
  
  df_for_amce$Television_never,
  
  levels = c(0, 1),
  
  labels = c("Exposed to TV Information", "Never Exposed")
  
)

df_cregg <- df_for_amce

df_cregg$Collaboration <- factor(
  
  df_cregg$Collaboration,
  
  levels = c("Liberal", "Illiberal"),
  
  labels = c("Collaboration: Liberal", "Collaboration: Illiberal")
  
)

df_cregg$Economic <- factor(
  
  df_cregg$Economic,
  
  levels = c("Liberal", "Illiberal"),
  
  labels = c("Economic: Liberal", "Economic: Illiberal")
  
)

df_cregg$Freedom_of_Speech <- factor(
  
  df_cregg$Freedom_of_Speech,
  
  levels = c("Liberal", "Illiberal"),
  
  labels = c("Speech: Liberal", "Speech: Illiberal")
  
)

df_cregg$Surveillance <- factor(
  
  df_cregg$Surveillance,
  
  levels = c("Liberal", "Illiberal"),
  
  labels = c("Surveillance: Liberal", "Surveillance: Illiberal")
  
)

tv_cj_anova <- cregg::cj_anova(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  by = ~ Televisionnever
  
)

tv_cj_anova


tv_amce_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "amce",
  
  by = ~ Televisionnever
  
)

tv_amce_cregg

plot(tv_amce_cregg, group = "Televisionnever")

tv_mm_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "mm",
  
  by = ~ Televisionnever
  
)

plot(tv_mm_cregg, group = "Televisionnever", vline = 0.5)


df_cregg$TelevisionUse <- factor(
  
  df_cregg$Television_Use,
  
  levels = c(0, 1),
  
  labels = c("Does not use TV", "Uses TV")
  
)
tv_use_cj_anova <- cregg::cj_anova(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  by = ~ TelevisionUse
  
)

tv_use_cj_anova

tv_use_model <- estimatr::lm_robust(outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)*TelevisionUse, 
                                    data = df_cregg,
                                    clusters = respondent_id,
                                    
                                    se_type = "stata")
summary(tv_use_model)

tv_use_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "amce",
  
  by = ~ TelevisionUse
  
)

plot(tv_use_cregg, group = "TelevisionUse")

tv_use_model_short <- estimatr::lm_robust(outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance*TelevisionUse, 
                                    data = df_cregg,
                                    clusters = respondent_id,
                                    
                                    se_type = "stata")
summary(tv_use_model_short)


## Television Frequency 


tel_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Television_Frequency_c"
)
summary(tel_freq_int)


tel_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Frequency_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(tel_freq_lm)

tel_freq_lpm_test <- car::linearHypothesis(
  tel_freq_lm,
  c(
    "Television_Frequency_c = 0",
    "CollaborationIlliberal:Television_Frequency_c = 0",
    "EconomicIlliberal:Television_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Television_Frequency_c = 0",
    "SurveillanceIlliberal:Television_Frequency_c = 0"
  ),
  vcov. = vcov(tel_freq_lm),
  test = "F"
)

F_stat_tel_freq <- tel_freq_lpm_test$F[2]
F_pval_tel_freq <- tel_freq_lpm_test$`Pr(>F)`[2]


Television_Frequency_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Frequency_c,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_tel_freq_logit <- sandwich::vcovCL(
  Television_Frequency_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  Television_Frequency_logit,
  vcov = cluster_se_tel_freq_logit
)

tel_freq_logit_test <- car::linearHypothesis(
  Television_Frequency_logit,
  c(
    "Television_Frequency_c = 0",
    "CollaborationIlliberal:Television_Frequency_c = 0",
    "EconomicIlliberal:Television_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Television_Frequency_c = 0",
    "SurveillanceIlliberal:Television_Frequency_c = 0"
  ),
  vcov. = cluster_se_tel_freq_logit,
  test = "Chisq"
)

Chi_stat_tel_freq <- tel_freq_logit_test$Chisq[2]
Chi_pval_tel_freq <- tel_freq_logit_test$`Pr(>Chisq)`[2]


extra_rows_tel_freq <- data.frame(
  term = c("Joint test (F / Chi²)", "p-value"),
  LPM = c(
    sprintf("%.2f", F_stat_tel_freq),
    sprintf("%.3f", F_pval_tel_freq)
  ),
  Logit = c(
    sprintf("%.2f", Chi_stat_tel_freq),
    sprintf("%.3f", Chi_pval_tel_freq)
  )
)

modelsummary::modelsummary(
  list(
    "LPM" = tel_freq_lm,
    "Logit" = Television_Frequency_logit
  ),
  vcov = list(
    NULL,
    cluster_se_tel_freq_logit
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  add_rows = extra_rows_tel_freq,
  title = "Interaction Between Conjoint Attributes and Television Frequency",
  output = "television_frequency_interaction_models.tex"
)


tel_freq_sum <- summary(tel_freq_int)
tel_freq_sum$TelevisionFrequencyc1amce

tel_freq_25 <- as.data.frame(tel_freq_sum$TelevisionFrequencyc1amce)

tel_freq_50 <- as.data.frame(tel_freq_sum$TelevisionFrequencyc2amce)

tel_freq_75 <- as.data.frame(tel_freq_sum$TelevisionFrequencyc3amce)

tel_freq_25$quantile <- "25%"

tel_freq_50$quantile <- "50%"

tel_freq_75$quantile <- "75%"

conditional_amces_tel_freq <- rbind(tel_freq_25, tel_freq_50, tel_freq_75)

ggplot(
  
  conditional_amces_tel_freq,
  
  aes(x = Estimate, y = Level, color = quantile)
  
) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "Television frequency",
    
    title = "Conditional AMCEs by Television Frequency"
    
  ) +
  
  theme_minimal()

out_collab_tvfreq <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Television_Frequency_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Television Frequency",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Collaboration"
)

plot(out_collab_tvfreq)


out_econ_tvfreq <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Television_Frequency_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Television Frequency",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Foreign Economic Policy"
)

plot(out_econ_tvfreq)


out_speech_tvfreq <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Television_Frequency_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Television Frequency",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Freedom of Speech"
)

plot(out_speech_tvfreq)


out_surv_tvfreq <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Television_Frequency_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Television Frequency",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Surveillance"
)

plot(out_surv_tvfreq)

df_for_amce_trust_television <- df_for_amce |>
  
  filter(!is.na(Television_Trust_Clean_c))

tel_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Trust_Clean_c,
  data = df_for_amce_trust_television,
  respondent.id = "respondent_id",
  respondent.varying = "Television_Trust_Clean_c",
  na.ignore = TRUE
)
summary(tel_trust_int)

tel_trust_model <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Trust_Clean_c,
  
  data = df_for_amce_trust_television,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(tel_trust_model)

tel_trust_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Television_Trust_Clean_c,
  
  data = df_for_amce_trust_television,
  
  family = binomial(link = "logit")
  
)

# Cluster-robust coefficient table

tel_trust_logit_cr <- coeftest(
  
  tel_trust_logit,
  
  vcov. = vcovCL(
    
    tel_trust_logit,
    
    cluster = ~ respondent_id,
    
    data = df_for_amce_trust_television,
    
    type = "HC1"
    
  )
  
)

tel_trust_logit_cr
car::linearHypothesis(
  
  tel_trust_model,
  
  c("Television_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Television_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Television_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Television_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Television_Trust_Clean_c = 0"
    
  ),
  
  vcov. = vcov(tel_trust_model)
  
)

# Cluster-robust VCOV for logit

tel_trust_logit_vcov <- sandwich::vcovCL(
  
  tel_trust_logit,
  
  cluster = ~ respondent_id,
  
  data = df_for_amce_trust_television,
  
  type = "HC1"
  
)

# Joint F-test for LPM

tel_trust_lpm_joint <- car::linearHypothesis(
  
  tel_trust_model,
  
  c(
    
    "Television_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Television_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Television_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Television_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Television_Trust_Clean_c = 0"
    
  ),
  
  vcov. = vcov(tel_trust_model),
  
  test = "F"
  
)

# Joint Wald Chi-square test for Logit

tel_trust_logit_joint <- car::linearHypothesis(
  
  tel_trust_logit,
  
  c(
    
    "Television_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Television_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Television_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Television_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Television_Trust_Clean_c = 0"
    
  ),
  
  vcov. = tel_trust_logit_vcov,
  
  test = "Chisq"
  
)

# Extract joint test statistics

lpm_F <- tel_trust_lpm_joint$F[2]

lpm_p <- tel_trust_lpm_joint$`Pr(>F)`[2]

logit_chi <- tel_trust_logit_joint$Chisq[2]

logit_p <- tel_trust_logit_joint$`Pr(>Chisq)`[2]

# Extra rows for modelsummary

extra_rows <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    sprintf("%.2f", lpm_F),
    
    sprintf("%.3f", lpm_p)
    
  ),
  
  Logit = c(
    
    sprintf("%.2f", logit_chi),
    
    sprintf("%.3f", logit_p)
    
  )
  
)

# Export LaTeX table

modelsummary(
  
  list(
    
    "LPM" = tel_trust_model,
    
    "Logit" = tel_trust_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    tel_trust_logit_vcov
    
  ),
  
  statistic = "({std.error})",
  
  stars = c(
    
    "+" = 0.1,
    
    "*" = 0.05,
    
    "**" = 0.01,
    
    "***" = 0.001
    
  ),
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows,
  
  title = "Interaction Between Conjoint Attributes and Television Trust",
  
  output = "tel_trust_interaction_table.tex"
  
)

tel_trust_sum <- summary(tel_trust_int)

tel_trust_25 <- as.data.frame(tel_trust_sum$TelevisionTrustCleanc1amce)

tel_trust_50 <- as.data.frame(tel_trust_sum$TelevisionTrustCleanc2amce)

tel_trust_75 <- as.data.frame(tel_trust_sum$TelevisionTrustCleanc3amce)

tel_trust_25$quantile <- "25%"

tel_trust_50$quantile <- "50%"

tel_trust_75$quantile <- "75%"

conditional_amces_tel_trust <- rbind(
  
  tel_trust_25,
  
  tel_trust_50,
  
  tel_trust_75
  
)

ggplot(
  
  conditional_amces_tel_trust,
  
  aes(x = Estimate, y = Level, color = quantile)
  
) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "Television trust",
    
    title = "Conditional AMCEs by Television Trust"
    
  ) +
  
  theme_minimal()
df_for_amce_trust_television$Collaboration_illiberal_num <- 
  
  as.numeric(df_for_amce_trust_television$Collaboration == "Illiberal")

df_for_amce_trust_television$Economic_illiberal_num <- 
  
  as.numeric(df_for_amce_trust_television$Economic == "Illiberal")

df_for_amce_trust_television$Freedom_of_Speech_illiberal_num <- 
  
  as.numeric(df_for_amce_trust_television$Freedom_of_Speech == "Illiberal")

df_for_amce_trust_television$Surveillance_illiberal_num <- 
  
  as.numeric(df_for_amce_trust_television$Surveillance == "Illiberal")
# Collaboration × Television Trust

interflex_collab_tel_trust <- interflex(
  
  Y = "outcome",
  
  D = "Collaboration",
  
  X = "Television_Trust_Clean_c",
  
  data = df_for_amce_trust_television,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  base = 'Liberal',
  full.moderate = TRUE,
  Xlabel = "Television Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Illiberal Collaboration"
  
)
interflex_collab_tel_trust$figure

# Economic × Television Trust

interflex_econ_tel_trust <- interflex(
  
  Y = "outcome",
  
  D = "Economic",
  
  X = "Television_Trust_Clean_c",
  
  data = df_for_amce_trust_television,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  base = 'Liberal',
  full.moderate = TRUE,
  Xlabel = "Television Trust",
  Ylabel = "Candidate Choice",
  Dlabel = 
  
)
interflex_econ_tel_trust$figure

# Freedom of Speech × Television Trust

interflex_fspeech_tel_trust <- interflex(
  
  Y = "outcome",
  
  D = "Freedom_of_Speech",
  
  X = "Television_Trust_Clean_c",
  
  data = df_for_amce_trust_television,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  base = 'Liberal', 
  full.moderate = TRUE,
  Xlabel = "Television Trust",
  Ylabel = "Candidate Choice"
  
)
interflex_fspeech_tel_trust$figure

# Surveillance × Television Trust

interflex_surv_tel_trust <- interflex(
  
  Y = "outcome",
  
  D = "Surveillance",
  
  X = "Television_Trust_Clean_c",
  
  data = df_for_amce_trust_television,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  base = 'Liberal',
  full.moderate = TRUE,
  Xlabel = "Television Trust",
  Ylabel = "Candidate Choice"
  
)

interflex_surv_tel_trust$figure

############################################################
## Internet: Never exposed
############################################################

df_cregg$Internetnever <- factor(
  df_cregg$Internet_never,
  levels = c(0, 1),
  labels = c("Exposed to Internet Information", "Never Exposed")
)

internet_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Internetnever
)

internet_cj_anova

internet_exp_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Internetnever,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)
summary(internet_exp_lm)

internet_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Internetnever
)

internet_amce_cregg

plot(internet_amce_cregg, group = "Internetnever")

internet_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Internetnever
)

plot(internet_mm_cregg, group = "Internetnever", vline = 0.5)

############################################################
## Internet: Use
############################################################

df_cregg$InternetUse <- factor(
  df_cregg$Internet_Trust_Use,
  levels = c(0, 1),
  labels = c("Does not use Internet", "Uses Internet")
)

internet_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ InternetUse
)

internet_use_cj_anova

internet_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * InternetUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(internet_use_model)

internet_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ InternetUse
)

plot(internet_use_cregg, group = "InternetUse")

############################################################
## Internet Frequency
############################################################

internet_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Internet_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Internet_Frequency_c"
)

summary(internet_freq_int)



internet_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Internet_Frequency,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(internet_freq_lm)

internet_freq_lpm_test <- car::linearHypothesis(
  internet_freq_lm,
  c(
    "Internet_Frequency = 0",
    "CollaborationIlliberal:Internet_Frequency = 0",
    "EconomicIlliberal:Internet_Frequency = 0",
    "Freedom_of_SpeechIlliberal:Internet_Frequency = 0",
    "SurveillanceIlliberal:Internet_Frequency = 0"
  ),
  vcov. = vcov(internet_freq_lm),
  test = "F"
)

F_stat_internet_freq <- internet_freq_lpm_test$F[2]
F_pval_internet_freq <- internet_freq_lpm_test$`Pr(>F)`[2]


Internet_Frequency_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Internet_Frequency,
  data = df_for_amce,
  family = binomial(link = "logit")
)

cluster_se_internet_freq_logit <- sandwich::vcovCL(
  Internet_Frequency_logit,
  cluster = ~ respondent_id,
  type = "HC1"
)

lmtest::coeftest(
  Internet_Frequency_logit,
  vcov = cluster_se_internet_freq_logit
)

internet_freq_logit_test <- car::linearHypothesis(
  Internet_Frequency_logit,
  c(
    "Internet_Frequency = 0",
    "CollaborationIlliberal:Internet_Frequency = 0",
    "EconomicIlliberal:Internet_Frequency = 0",
    "Freedom_of_SpeechIlliberal:Internet_Frequency = 0",
    "SurveillanceIlliberal:Internet_Frequency = 0"
  ),
  vcov. = cluster_se_internet_freq_logit,
  test = "Chisq"
)

Chi_stat_internet_freq <- internet_freq_logit_test$Chisq[2]
Chi_pval_internet_freq <- internet_freq_logit_test$`Pr(>Chisq)`[2]


extra_rows_internet_freq <- data.frame(
  term = c("Joint test (F / Chi²)", "p-value"),
  LPM = c(
    sprintf("%.2f", F_stat_internet_freq),
    sprintf("%.3f", F_pval_internet_freq)
  ),
  Logit = c(
    sprintf("%.2f", Chi_stat_internet_freq),
    sprintf("%.3f", Chi_pval_internet_freq)
  )
)

modelsummary::modelsummary(
  list(
    "LPM" = internet_freq_lm,
    "Logit" = Internet_Frequency_logit
  ),
  vcov = list(
    NULL,
    cluster_se_internet_freq_logit
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  add_rows = extra_rows_internet_freq,
  title = "Interaction Between Conjoint Attributes and Internet Frequency",
  output = "internet_frequency_interaction_models.tex"
)


out_collab_netfreq <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Internet_Frequency_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_collab_netfreq)


out_econ_netfreq <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Internet_Frequency_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_econ_netfreq)


out_speech_netfreq <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Internet_Frequency_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_speech_netfreq)


out_surv_netfreq <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Internet_Frequency_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_surv_netfreq)
############################################################
## Internet Trust
############################################################

df_for_amce_trust_internet <- df_for_amce |>
  filter(!is.na(Internet_Trust_Trust_Clean))

internet_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Internet_Trust_Trust_Clean,
  data = df_for_amce_trust_internet,
  respondent.id = "respondent_id",
  respondent.varying = "Internet_Trust_Trust_Clean",
  na.ignore = TRUE
)

summary(internet_trust_int)

internet_trust_sum <- summary(internet_trust_int)

names(internet_trust_sum)


internet_trust_lpm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Internet_Trust_Trust_Clean,
  
  data = df_for_amce_trust_internet,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(internet_trust_lpm)

# -----------------------------

# 2. Joint F-test for LPM

# Moderator main effect + all moderator interactions

# -----------------------------

internet_trust_lpm_test <- car::linearHypothesis(
  
  internet_trust_lpm,
  
  c(
    
    "Internet_Trust_Trust_Clean = 0",
    
    "CollaborationIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "EconomicIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "Freedom_of_SpeechIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "SurveillanceIlliberal:Internet_Trust_Trust_Clean = 0"
    
  ),
  
  test = "F"
  
)

internet_trust_lpm_test

internet_trust_F_stat <- internet_trust_lpm_test$F[2]

internet_trust_F_pval <- internet_trust_lpm_test$`Pr(>F)`[2]

# -----------------------------

# 3. Logit model

# -----------------------------

internet_trust_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Internet_Trust_Trust_Clean,
  
  data = df_for_amce_trust_internet,
  
  family = binomial(link = "logit")
  
)

summary(internet_trust_logit)

# -----------------------------

# 4. Clustered SEs for logit

# -----------------------------

internet_trust_logit_vcov <- sandwich::vcovCL(
  
  internet_trust_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

lmtest::coeftest(
  
  internet_trust_logit,
  
  vcov = internet_trust_logit_vcov
  
)

# -----------------------------

# 5. Joint Wald chi-square test for logit

# Moderator main effect + all moderator interactions

# -----------------------------

internet_trust_logit_test <- car::linearHypothesis(
  
  internet_trust_logit,
  
  c(
    
    "Internet_Trust_Trust_Clean = 0",
    
    "CollaborationIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "EconomicIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "Freedom_of_SpeechIlliberal:Internet_Trust_Trust_Clean = 0",
    
    "SurveillanceIlliberal:Internet_Trust_Trust_Clean = 0"
    
  ),
  
  vcov. = internet_trust_logit_vcov,
  
  test = "Chisq"
  
)

internet_trust_logit_test

internet_trust_Chi_stat <- internet_trust_logit_test$Chisq[2]

internet_trust_Chi_pval <- internet_trust_logit_test$`Pr(>Chisq)`[2]

# -----------------------------

# 6. Extra rows for modelsummary

# -----------------------------

extra_rows_internet_trust <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    round(internet_trust_F_stat, 3),
    
    round(internet_trust_F_pval, 3)
    
  ),
  
  Logit = c(
    
    round(internet_trust_Chi_stat, 3),
    
    round(internet_trust_Chi_pval, 3)
    
  )
  
)

# -----------------------------

# 7. Modelsummary export

# -----------------------------

modelsummary(
  
  list(
    
    "LPM" = internet_trust_lpm,
    
    "Logit" = internet_trust_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    internet_trust_logit_vcov
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows_internet_trust,
  
  title = "Internet Trust as a Moderator of Candidate Choice",
  
  output = "internet_trust_moderator_models.tex"
  
)

internet_trust_25 <- as.data.frame(internet_trust_sum$InternetTrustTrustClean1amce)
internet_trust_50 <- as.data.frame(internet_trust_sum$InternetTrustTrustClean2amce)
internet_trust_75 <- as.data.frame(internet_trust_sum$InternetTrustTrustClean3amce)

internet_trust_25$quantile <- "25%"
internet_trust_50$quantile <- "50%"
internet_trust_75$quantile <- "75%"

conditional_amces_internet_trust <- rbind(
  internet_trust_25,
  internet_trust_50,
  internet_trust_75
)

ggplot(
  conditional_amces_internet_trust,
  aes(x = Estimate, y = Level, color = quantile)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Internet trust",
    title = "Conditional AMCEs by Internet Trust"
  ) +
  theme_minimal()

############################################################
## Interflex: Internet Trust
############################################################

interflex_collab_internet_trust <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Internet_Trust_Trust_Clean",
  data = df_for_amce_trust_internet,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Trust",
  Ylabel = "Candidate Choice"
)

interflex_collab_internet_trust$figure

interflex_econ_internet_trust <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Internet_Trust_Trust_Clean",
  data = df_for_amce_trust_internet,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Trust",
  Ylabel = "Candidate Choice",
  Dlabel = 'Foreign Economic Policy'
)

interflex_econ_internet_trust$figure

interflex_fspeech_internet_trust <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Internet_Trust_Trust_Clean",
  data = df_for_amce_trust_internet,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Freedom of Speech"
)

interflex_fspeech_internet_trust$figure

interflex_surv_internet_trust <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Internet_Trust_Trust_Clean",
  data = df_for_amce_trust_internet,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Internet Trust",
  Ylabel = "Candidate Choice"
)

interflex_surv_internet_trust$figure
############################################################
## Social Media: Never exposed
############################################################

df_cregg$SocialMedianever <- factor(
  df_cregg$Social_Media_never,
  levels = c(0, 1),
  labels = c("Exposed to Social Media Information", "Never Exposed")
)

social_media_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ SocialMedianever
)

social_media_cj_anova

social_media_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ SocialMedianever
)

social_media_amce_cregg

plot(social_media_amce_cregg, group = "SocialMedianever")

social_media_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ SocialMedianever
)

plot(social_media_mm_cregg, group = "SocialMedianever", vline = 0.5)
############################################################
## Social Media: Use
############################################################

df_cregg$SocialMediaUse <- factor(
  df_cregg$Social_Media_Use,
  levels = c(0, 1),
  labels = c("Does not use Social Media", "Uses Social Media")
)

social_media_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ SocialMediaUse
)

social_media_use_cj_anova

social_media_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * SocialMediaUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(social_media_use_model)

social_media_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ SocialMediaUse
)

plot(social_media_use_cregg, group = "SocialMediaUse")

############################################################
## Social Media Frequency
############################################################

social_media_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Social_Media_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Social_Media_Frequency_c"
)

summary(social_media_freq_int)

social_media_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Social_Media_Frequency_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(social_media_freq_lm)

car::linearHypothesis(
  social_media_freq_lm,
  c(
    "Social_Media_Frequency_c = 0",
    "CollaborationIlliberal:Social_Media_Frequency_c = 0",
    "EconomicIlliberal:Social_Media_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Social_Media_Frequency_c = 0",
    "SurveillanceIlliberal:Social_Media_Frequency_c = 0"
  ),
  vcov. = vcov(social_media_freq_lm)
)

out_collab_smfreq <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Social_Media_Frequency_c",
  Z = c("Economic", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Social Media Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_collab_smfreq)


out_econ_smfreq <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Social_Media_Frequency_c",
  Z = c("Collaboration", "Freedom_of_Speech", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Social Media Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_econ_smfreq)


out_speech_smfreq <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Social_Media_Frequency_c",
  Z = c("Collaboration", "Economic", "Surveillance"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Social Media Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_speech_smfreq)


out_surv_smfreq <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Social_Media_Frequency_c",
  Z = c("Collaboration", "Economic", "Freedom_of_Speech"),
  data = df_for_amce,
  estimator = "binning",
  vcov.type = "cluster",
  nbins = 3,
  cl = "respondent_id",
  base = "Liberal",
  full.moderate = TRUE,
  Xlabel = "Social Media Frequency (centered)",
  Ylabel = "Candidate Choice"
)

plot(out_surv_smfreq)

############################################################
## Social Media Trust
############################################################

df_for_amce_trust_social_media <- df_for_amce |>
  filter(!is.na(Social_Media_Trust_Clean_c))

social_media_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Social_Media_Trust_Clean_c,
  data = df_for_amce_trust_social_media,
  respondent.id = "respondent_id",
  respondent.varying = "Social_Media_Trust_Clean_c",
  na.ignore = TRUE
)

summary(social_media_trust_int)

# -----------------------------

# 1. LPM with clustered SEs

# -----------------------------

social_media_trust_lpm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Social_Media_Trust_Clean_c,
  
  data = df_for_amce_trust_social_media,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(social_media_trust_lpm)

# -----------------------------

# 2. Joint F-test for LPM

# Moderator main effect + all moderator interactions

# -----------------------------

social_media_trust_lpm_test <- car::linearHypothesis(
  
  social_media_trust_lpm,
  
  c(
    
    "Social_Media_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Social_Media_Trust_Clean_c = 0"
    
  ),
  
  test = "F"
  
)

social_media_trust_lpm_test

social_media_trust_F_stat <- social_media_trust_lpm_test$F[2]

social_media_trust_F_pval <- social_media_trust_lpm_test$`Pr(>F)`[2]

# -----------------------------

# 3. Logit model

# -----------------------------

social_media_trust_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Social_Media_Trust_Clean_c,
  
  data = df_for_amce_trust_social_media,
  
  family = binomial(link = "logit")
  
)

summary(social_media_trust_logit)

# -----------------------------

# 4. Clustered SEs for logit

# -----------------------------

social_media_trust_logit_vcov <- sandwich::vcovCL(
  
  social_media_trust_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

social_media_trust_logit_cr <- lmtest::coeftest(
  
  social_media_trust_logit,
  
  vcov. = social_media_trust_logit_vcov
  
)

social_media_trust_logit_cr

# -----------------------------

# 5. Joint Wald chi-square test for logit

# Moderator main effect + all moderator interactions

# -----------------------------

social_media_trust_logit_test <- car::linearHypothesis(
  
  social_media_trust_logit,
  
  c(
    
    "Social_Media_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Social_Media_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Social_Media_Trust_Clean_c = 0"
    
  ),
  
  vcov. = social_media_trust_logit_vcov,
  
  test = "Chisq"
  
)

social_media_trust_logit_test

social_media_trust_Chi_stat <- social_media_trust_logit_test$Chisq[2]

social_media_trust_Chi_pval <- social_media_trust_logit_test$`Pr(>Chisq)`[2]

# -----------------------------

# 6. Extra rows for modelsummary

# -----------------------------

extra_rows_social_media_trust <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    round(social_media_trust_F_stat, 3),
    
    round(social_media_trust_F_pval, 3)
    
  ),
  
  Logit = c(
    
    round(social_media_trust_Chi_stat, 3),
    
    round(social_media_trust_Chi_pval, 3)
    
  )
  
)

# -----------------------------

# 7. Modelsummary export

# -----------------------------

modelsummary(
  
  list(
    
    "LPM" = social_media_trust_lpm,
    
    "Logit" = social_media_trust_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    social_media_trust_logit_vcov
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows_social_media_trust,
  
  title = "Social Media Trust as a Moderator of Candidate Choice",
  
  output = "social_media_trust_moderator_models.tex"
  
)
social_media_trust_sum <- summary(social_media_trust_int)

names(social_media_trust_sum)

social_media_trust_25 <- as.data.frame(social_media_trust_sum$SocialMediaTrustCleanc1amce)
social_media_trust_50 <- as.data.frame(social_media_trust_sum$SocialMediaTrustCleanc2amce)
social_media_trust_75 <- as.data.frame(social_media_trust_sum$SocialMediaTrustCleanc3amce)

social_media_trust_25$quantile <- "25%"
social_media_trust_50$quantile <- "50%"
social_media_trust_75$quantile <- "75%"

conditional_amces_social_media_trust <- rbind(
  social_media_trust_25,
  social_media_trust_50,
  social_media_trust_75
)

ggplot(
  conditional_amces_social_media_trust,
  aes(x = Estimate, y = Level, color = quantile)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Social media trust",
    title = "Conditional AMCEs by Social Media Trust"
  ) +
  theme_minimal()

############################################################
## Interflex: Social Media Trust
############################################################

interflex_collab_social_media_trust <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Social_Media_Trust_Clean_c",
  data = df_for_amce_trust_social_media,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Xlabel = "Social Media Trust",
  Ylabel = "Candidate Choice"
)

interflex_collab_social_media_trust$figure

interflex_econ_social_media_trust <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Social_Media_Trust_Clean_c",
  data = df_for_amce_trust_social_media,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Xlabel = "Social Media Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Foreign Economic Policy"
)

interflex_econ_social_media_trust$figure

interflex_fspeech_social_media_trust <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Social_Media_Trust_Clean_c",
  data = df_for_amce_trust_social_media,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Xlabel = "Social Media Trust",
  Ylabel = "Candidate Choice",
  Dlabel = "Freedom of Speech"
)

interflex_fspeech_social_media_trust$figure

interflex_surv_social_media_trust <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Social_Media_Trust_Clean_c",
  data = df_for_amce_trust_social_media,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Xlabel = "Social Media Trust",
  Ylabel = "Candidate Choice"
)

interflex_surv_social_media_trust$figure


############################################################
## Messengers: Never exposed
############################################################

df_cregg$Messengersnever <- factor(
  df_cregg$Messengers_never,
  levels = c(0, 1),
  labels = c("Exposed to Messenger Information", "Never Exposed")
)

messengers_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Messengersnever
)

messengers_cj_anova

messengers_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Messengersnever
)

messengers_amce_cregg

plot(messengers_amce_cregg, group = "Messengersnever")

messengers_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Messengersnever
)

plot(messengers_mm_cregg, group = "Messengersnever", vline = 0.5)

############################################################
## Messengers: Use
############################################################

df_cregg$MessengersUse <- factor(
  df_cregg$Messengers_Use,
  levels = c(0, 1),
  labels = c("Does not use Messengers", "Uses Messengers")
)

messengers_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ MessengersUse
)

messengers_use_cj_anova

messengers_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * MessengersUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(messengers_use_model)

messengers_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ MessengersUse
)

plot(messengers_use_cregg, group = "MessengersUse")

############################################################
## Messengers Frequency
############################################################

messengers_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Messengers_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Messengers_Frequency_c"
)

summary(messengers_freq_int)

messengers_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Messengers_Frequency_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(messengers_freq_lm)

car::linearHypothesis(
  messengers_freq_lm,
  c(
    "Messengers_Frequency_c = 0",
    "CollaborationIlliberal:Messengers_Frequency_c = 0",
    "EconomicIlliberal:Messengers_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Messengers_Frequency_c = 0",
    "SurveillanceIlliberal:Messengers_Frequency_c = 0"
  ),
  vcov. = vcov(messengers_freq_lm)
)

############################################################
## Messengers Trust
############################################################

df_for_amce_trust_messengers <- df_for_amce |>
  filter(!is.na(Messengers_Trust_Clean_c))

messengers_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Messengers_Trust_Clean_c,
  data = df_for_amce_trust_messengers,
  respondent.id = "respondent_id",
  respondent.varying = "Messengers_Trust_Clean_c",
  na.ignore = TRUE
)

summary(messengers_trust_int)

messengers_trust_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Messengers_Trust_Clean_c,
  data = df_for_amce_trust_messengers,
  clusters = respondent_id,
  se_type = "stata"
)

summary(messengers_trust_model)

messengers_trust_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Messengers_Trust_Clean_c,
  data = df_for_amce_trust_messengers,
  family = binomial(link = "logit")
)

messengers_trust_logit_cr <- coeftest(
  messengers_trust_logit,
  vcov. = vcovCL(
    messengers_trust_logit,
    cluster = ~ respondent_id,
    data = df_for_amce_trust_messengers,
    type = "HC1"
  )
)

messengers_trust_logit_cr

car::linearHypothesis(
  messengers_trust_model,
  c(
    "Messengers_Trust_Clean_c = 0",
    "CollaborationIlliberal:Messengers_Trust_Clean_c = 0",
    "EconomicIlliberal:Messengers_Trust_Clean_c = 0",
    "Freedom_of_SpeechIlliberal:Messengers_Trust_Clean_c = 0",
    "SurveillanceIlliberal:Messengers_Trust_Clean_c = 0"
  ),
  vcov. = vcov(messengers_trust_model)
)
############################################################
## Radio: Never exposed
############################################################

df_cregg$Radionever <- factor(
  df_cregg$Radio_never,
  levels = c(0, 1),
  labels = c("Exposed to Radio Information", "Never Exposed")
)

radio_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Radionever
)

radio_cj_anova

radio_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Radionever
)

radio_amce_cregg

plot(radio_amce_cregg, group = "Radionever")

radio_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Radionever
)

plot(radio_mm_cregg, group = "Radionever", vline = 0.5)

############################################################
## Radio: Use
############################################################

df_cregg$RadioUse <- factor(
  df_cregg$Radio_Use,
  levels = c(0, 1),
  labels = c("Does not use Radio", "Uses Radio")
)

radio_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ RadioUse
)

radio_use_cj_anova

radio_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * RadioUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(radio_use_model)

radio_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ RadioUse
)

plot(radio_use_cregg, group = "RadioUse")

radio_use_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ RadioUse
)

plot(radio_use_mm_cregg, group = "RadioUse", vline = 0.5)

radio_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * RadioUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(radio_use_model)

############################################################
## Newspapers: Never exposed
############################################################

df_cregg$Newspapersnever <- factor(
  df_cregg$Newspapers_never,
  levels = c(0, 1),
  labels = c("Exposed to Newspapers Information", "Never Exposed")
)

newspapers_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Newspapersnever
)
newspapers_cj_anova

newspapers_lm <- estimatr::lm_robust(outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)*Newspapersnever,
                                     data = df_cregg, 
                                     clusters = respondent_id,
                                     se_type = 'stata')

summary(newspapers_lm)
newspapers_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Newspapersnever
)

newspapers_amce_cregg

plot(newspapers_amce_cregg, group = "Newspapersnever")

newspapers_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Newspapersnever
)

plot(newspapers_mm_cregg, group = "Newspapersnever", vline = 0.5)

############################################################
## Newspapers: Use
############################################################

df_cregg$NewspapersUse <- factor(
  df_cregg$Newspapers_Use,
  levels = c(0, 1),
  labels = c("Does not use Newspapers", "Uses Newspapers")
)

newspapers_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ NewspapersUse
)

newspapers_use_cj_anova

newspapers_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * NewspapersUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(newspapers_use_model)

newspapers_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ NewspapersUse
)

plot(newspapers_use_cregg, group = "NewspapersUse")

newspapers_use_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ NewspapersUse
)

plot(newspapers_use_mm_cregg, group = "NewspapersUse", vline = 0.5)

newspapers_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * NewspapersUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(newspapers_use_model)

############################################################
## Newspapers Frequency
############################################################

newspapers_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Newspapers_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Newspapers_Frequency_c"
)

summary(newspapers_freq_int)

newspapers_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Newspapers_Frequency_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(newspapers_freq_lm)

car::linearHypothesis(
  newspapers_freq_lm,
  c(
    "Newspapers_Frequency_c = 0",
    "CollaborationIlliberal:Newspapers_Frequency_c = 0",
    "EconomicIlliberal:Newspapers_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Newspapers_Frequency_c = 0",
    "SurveillanceIlliberal:Newspapers_Frequency_c = 0"
  ),
  vcov. = vcov(newspapers_freq_lm)
)

newspapers_freq_sum <- summary(newspapers_freq_int)

names(newspapers_freq_sum)

newspapers_freq_25 <- as.data.frame(newspapers_freq_sum$NewspapersFrequencyc1amce)
newspapers_freq_50 <- as.data.frame(newspapers_freq_sum$NewspapersFrequencyc2amce)
newspapers_freq_75 <- as.data.frame(newspapers_freq_sum$NewspapersFrequencyc3amce)

newspapers_freq_25$quantile <- "25%"
newspapers_freq_50$quantile <- "50%"
newspapers_freq_75$quantile <- "75%"

conditional_amces_newspapers_freq <- rbind(
  newspapers_freq_25,
  newspapers_freq_50,
  newspapers_freq_75
)

ggplot(
  conditional_amces_newspapers_freq,
  aes(x = Estimate, y = Level, color = quantile)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Newspapers frequency",
    title = "Conditional AMCEs by Newspapers Frequency"
  ) +
  theme_minimal()

############################################################
## Newspapers Trust
############################################################

df_for_amce_trust_newspapers <- df_for_amce |>
  filter(!is.na(Newspapers_Trust_Clean_c))

newspapers_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Newspapers_Trust_Clean_c,
  data = df_for_amce_trust_newspapers,
  respondent.id = "respondent_id",
  respondent.varying = "Newspapers_Trust_Clean_c",
  na.ignore = TRUE
)

summary(newspapers_trust_int)

newspapers_trust_lpm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Newspapers_Trust_Clean_c,
  
  data = df_for_amce_trust_newspapers,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(newspapers_trust_lpm)

# -----------------------------

# 2. Joint F-test for LPM

# Moderator main effect + all moderator interactions

# -----------------------------

newspapers_trust_lpm_test <- car::linearHypothesis(
  
  newspapers_trust_lpm,
  
  c(
    
    "Newspapers_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Newspapers_Trust_Clean_c = 0"
    
  ),
  
  test = "F"
  
)

newspapers_trust_lpm_test

newspapers_trust_F_stat <- newspapers_trust_lpm_test$F[2]

newspapers_trust_F_pval <- newspapers_trust_lpm_test$`Pr(>F)`[2]

# -----------------------------

# 3. Logit model

# -----------------------------

newspapers_trust_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Newspapers_Trust_Clean_c,
  
  data = df_for_amce_trust_newspapers,
  
  family = binomial(link = "logit")
  
)

summary(newspapers_trust_logit)

# -----------------------------

# 4. Clustered SEs for logit

# -----------------------------

newspapers_trust_logit_vcov <- sandwich::vcovCL(
  
  newspapers_trust_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

newspapers_trust_logit_cr <- lmtest::coeftest(
  
  newspapers_trust_logit,
  
  vcov. = newspapers_trust_logit_vcov
  
)

newspapers_trust_logit_cr

# -----------------------------

# 5. Joint Wald chi-square test for logit

# Moderator main effect + all moderator interactions

# -----------------------------

newspapers_trust_logit_test <- car::linearHypothesis(
  
  newspapers_trust_logit,
  
  c(
    
    "Newspapers_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Newspapers_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Newspapers_Trust_Clean_c = 0"
    
  ),
  
  vcov. = newspapers_trust_logit_vcov,
  
  test = "Chisq"
  
)

newspapers_trust_logit_test

newspapers_trust_Chi_stat <- newspapers_trust_logit_test$Chisq[2]

newspapers_trust_Chi_pval <- newspapers_trust_logit_test$`Pr(>Chisq)`[2]

# -----------------------------

# 6. Extra rows for modelsummary

# -----------------------------

extra_rows_newspapers_trust <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    round(newspapers_trust_F_stat, 3),
    
    round(newspapers_trust_F_pval, 3)
    
  ),
  
  Logit = c(
    
    round(newspapers_trust_Chi_stat, 3),
    
    round(newspapers_trust_Chi_pval, 3)
    
  )
  
)

# -----------------------------

# 7. Modelsummary export

# -----------------------------

modelsummary(
  
  list(
    
    "LPM" = newspapers_trust_lpm,
    
    "Logit" = newspapers_trust_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    newspapers_trust_logit_vcov
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows_newspapers_trust,
  
  title = "Newspapers Trust as a Moderator of Candidate Choice",
  
  output = "newspapers_trust_moderator_models.tex"
  
)
newspapers_trust_sum <- summary(newspapers_trust_int)

names(newspapers_trust_sum)

newspapers_trust_25 <- as.data.frame(newspapers_trust_sum$NewspapersTrustCleanc1amce)
newspapers_trust_50 <- as.data.frame(newspapers_trust_sum$NewspapersTrustCleanc2amce)
newspapers_trust_75 <- as.data.frame(newspapers_trust_sum$NewspapersTrustCleanc3amce)

newspapers_trust_25$quantile <- "25%"
newspapers_trust_50$quantile <- "50%"
newspapers_trust_75$quantile <- "75%"
newspapers_trust_25
conditional_amces_newspapers_trust <- rbind(
  newspapers_trust_25,
  newspapers_trust_50,
  newspapers_trust_75
)

ggplot(
  conditional_amces_newspapers_trust,
  aes(x = Estimate, y = Level, color = quantile)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Newspapers trust",
    title = "Conditional AMCEs by Newspapers Trust"
  ) +
  theme_minimal()

############################################################
## Interflex: Newspapers Trust
############################################################

interflex_collab_newspapers_trust <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Newspapers_Trust_Clean_c",
  data = df_for_amce_trust_newspapers,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Ylabel = "Candidate Choice",
  Xlabel = "Newspapers Trust"
)

interflex_collab_newspapers_trust$figure

interflex_econ_newspapers_trust <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Newspapers_Trust_Clean_c",
  data = df_for_amce_trust_newspapers,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Ylabel = "Candidate Choice",
  Xlabel = "Newspapers Trust",
  Dlabel = "Foreign Economic Policy"
)

interflex_econ_newspapers_trust$figure

interflex_fspeech_newspapers_trust <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Newspapers_Trust_Clean_c",
  data = df_for_amce_trust_newspapers,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Ylabel = "Candidate Choice",
  Xlabel = "Newspapers Trust",
  Dlabel = "Freedom of Speech"
)

interflex_fspeech_newspapers_trust$figure

interflex_surv_newspapers_trust <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Newspapers_Trust_Clean_c",
  data = df_for_amce_trust_newspapers,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal",
  Ylabel = "Candidate Choice",
  Xlabel = "Newspapers Trust",
)

interflex_surv_newspapers_trust$figure

#####################
#####################
#####################


df_cregg$Magazinesnever <- factor(
  df_cregg$Magazines_never,
  levels = c(0, 1),
  labels = c("Exposed to Magazines Information", "Never Exposed")
)

magazines_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Magazinesnever
)

magazines_cj_anova

magazines_lm_model <- estimatr::lm_robust(outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance)*Magazinesnever,
                                          data = df_cregg, 
                                          clusters = respondent_id,
                                          se_type = 'stata')
summary(magazines_lm_model)

magazines_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Magazinesnever
)

plot(magazines_amce_cregg, group = "Magazinesnever")

magazines_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Magazinesnever
)

plot(magazines_mm_cregg, group = "Magazinesnever", vline = 0.5)

## Magazines Use
df_cregg$MagazinesUse <- factor(
  df_cregg$Magazines_Use,
  levels = c(0, 1),
  labels = c("Does not use Magazines", "Uses Magazines")
)

magazines_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ MagazinesUse
)
magazines_use_cj_anova

magazines_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * MagazinesUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(magazines_use_model)

magazines_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ MagazinesUse
)

plot(magazines_use_cregg, group = "MagazinesUse")

magazines_use_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ MagazinesUse
)

plot(magazines_use_mm_cregg, group = "MagazinesUse", vline = 0.5)


## Magazines Frequency


magazines_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Magazines_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Magazines_Frequency_c"
)

summary(magazines_freq_int)

# -----------------------------

# 1. LPM with clustered SEs

# -----------------------------

magazines_freq_lpm <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Magazines_Frequency_c,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)

summary(magazines_freq_lpm)

# -----------------------------

# 2. Joint F-test for LPM

# Moderator main effect + all moderator interactions

# -----------------------------

magazines_freq_lpm_test <- car::linearHypothesis(
  
  magazines_freq_lpm,
  
  c(
    
    "Magazines_Frequency_c = 0",
    
    "CollaborationIlliberal:Magazines_Frequency_c = 0",
    
    "EconomicIlliberal:Magazines_Frequency_c = 0",
    
    "Freedom_of_SpeechIlliberal:Magazines_Frequency_c = 0",
    
    "SurveillanceIlliberal:Magazines_Frequency_c = 0"
    
  ),
  
  test = "F"
  
)

magazines_freq_lpm_test

magazines_freq_F_stat <- magazines_freq_lpm_test$F[2]

magazines_freq_F_pval <- magazines_freq_lpm_test$`Pr(>F)`[2]

# -----------------------------

# 3. Logit model

# -----------------------------

magazines_freq_logit <- glm(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) *
    
    Magazines_Frequency_c,
  
  data = df_for_amce,
  
  family = binomial(link = "logit")
  
)

summary(magazines_freq_logit)

# -----------------------------

# 4. Clustered SEs for logit

# -----------------------------

magazines_freq_logit_vcov <- sandwich::vcovCL(
  
  magazines_freq_logit,
  
  cluster = ~ respondent_id,
  
  type = "HC1"
  
)

magazines_freq_logit_cr <- lmtest::coeftest(
  
  magazines_freq_logit,
  
  vcov. = magazines_freq_logit_vcov
  
)

magazines_freq_logit_cr

# -----------------------------

# 5. Joint Wald chi-square test for logit

# Moderator main effect + all moderator interactions

# -----------------------------

magazines_freq_logit_test <- car::linearHypothesis(
  
  magazines_freq_logit,
  
  c(
    
    "Magazines_Frequency_c = 0",
    
    "CollaborationIlliberal:Magazines_Frequency_c = 0",
    
    "EconomicIlliberal:Magazines_Frequency_c = 0",
    
    "Freedom_of_SpeechIlliberal:Magazines_Frequency_c = 0",
    
    "SurveillanceIlliberal:Magazines_Frequency_c = 0"
    
  ),
  
  vcov. = magazines_freq_logit_vcov,
  
  test = "Chisq"
  
)

magazines_freq_logit_test

magazines_freq_Chi_stat <- magazines_freq_logit_test$Chisq[2]

magazines_freq_Chi_pval <- magazines_freq_logit_test$`Pr(>Chisq)`[2]

# -----------------------------

# 6. Extra rows for modelsummary

# -----------------------------

extra_rows_magazines_freq <- data.frame(
  
  term = c("Joint test (F / Chi²)", "p-value"),
  
  LPM = c(
    
    round(magazines_freq_F_stat, 3),
    
    round(magazines_freq_F_pval, 3)
    
  ),
  
  Logit = c(
    
    round(magazines_freq_Chi_stat, 3),
    
    round(magazines_freq_Chi_pval, 3)
    
  )
  
)

# -----------------------------

# 7. Modelsummary export

# -----------------------------

modelsummary(
  
  list(
    
    "LPM" = magazines_freq_lpm,
    
    "Logit" = magazines_freq_logit
    
  ),
  
  vcov = list(
    
    NULL,
    
    magazines_freq_logit_vcov
    
  ),
  
  statistic = "({std.error})",
  
  stars = TRUE,
  
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  
  add_rows = extra_rows_magazines_freq,
  
  title = "Magazines Frequency as a Moderator of Candidate Choice",
  
  output = "magazines_frequency_moderator_models.tex"
  
)

magazines_freq_sum <- summary(magazines_freq_int)
names(magazines_freq_sum)

magazines_freq_25 <- as.data.frame(magazines_freq_sum$MagazinesFrequencyc1amce)
magazines_freq_50 <- as.data.frame(magazines_freq_sum$MagazinesFrequencyc2amce)
magazines_freq_75 <- as.data.frame(magazines_freq_sum$MagazinesFrequencyc3amce)

magazines_freq_25$quantile <- "25%"
magazines_freq_50$quantile <- "50%"
magazines_freq_75$quantile <- "75%"

conditional_amces_magazines_freq <- rbind(
  magazines_freq_25,
  magazines_freq_50,
  magazines_freq_75
)

ggplot(conditional_amces_magazines_freq,
       aes(x = Estimate, y = Level, color = quantile)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Magazines frequency",
    title = "Conditional AMCEs by Magazines Frequency"
  ) +
  theme_minimal()

interflex_collab_magazines_freq <- interflex(
  
  Y = "outcome",
  
  D = "Collaboration",
  
  X = "Magazines_Frequency_c",
  
  data = df_for_amce,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Ylabel = "Candidate Choice",
  
  Xlabel = "Magazines Frequency"
  
)

interflex_collab_magazines_freq$figure

# -----------------------------

# 2. Economic Policy

# -----------------------------

interflex_econ_magazines_freq <- interflex(
  
  Y = "outcome",
  
  D = "Economic",
  
  X = "Magazines_Frequency_c",
  
  data = df_for_amce,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Ylabel = "Candidate Choice",
  
  Xlabel = "Magazines Frequency",
  
  Dlabel = "Foreign Economic Policy"
  
)

interflex_econ_magazines_freq$figure

# -----------------------------

# 3. Freedom of Speech

# -----------------------------

interflex_fspeech_magazines_freq <- interflex(
  
  Y = "outcome",
  
  D = "Freedom_of_Speech",
  
  X = "Magazines_Frequency_c",
  
  data = df_for_amce,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Ylabel = "Candidate Choice",
  
  Xlabel = "Magazines Frequency",
  
  Dlabel = "Freedom of Speech"
  
)

interflex_fspeech_magazines_freq$figure

# -----------------------------

# 4. Surveillance

# -----------------------------

interflex_surv_magazines_freq <- interflex(
  
  Y = "outcome",
  
  D = "Surveillance",
  
  X = "Magazines_Frequency_c",
  
  data = df_for_amce,
  
  estimator = "binning",
  
  FE = NULL,
  
  cl = "respondent_id",
  
  base = "Liberal",
  
  Ylabel = "Candidate Choice",
  
  Xlabel = "Magazines Frequency"
  
)

interflex_surv_magazines_freq$figure

df_for_amce_trust_magazines <- df_for_amce |>
  filter(!is.na(Magazines_Trust_Clean_c))

magazines_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Magazines_Trust_Clean_c,
  data = df_for_amce_trust_magazines,
  respondent.id = "respondent_id",
  respondent.varying = "Magazines_Trust_Clean_c",
  na.ignore = TRUE
)

summary(magazines_trust_int)

magazines_trust_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Magazines_Trust_Clean_c,
  data = df_for_amce_trust_magazines,
  clusters = respondent_id,
  se_type = "stata"
)

summary(magazines_trust_model)

car::linearHypothesis(
  magazines_trust_model,
  c(
    "Magazines_Trust_Clean_c = 0",
    "CollaborationIlliberal:Magazines_Trust_Clean_c = 0",
    "EconomicIlliberal:Magazines_Trust_Clean_c = 0",
    "Freedom_of_SpeechIlliberal:Magazines_Trust_Clean_c = 0",
    "SurveillanceIlliberal:Magazines_Trust_Clean_c = 0"
  ),
  vcov. = vcov(magazines_trust_model)
)

magazines_trust_logit <- glm(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Magazines_Trust_Clean_c,
  data = df_for_amce_trust_magazines,
  family = binomial(link = "logit")
)

magazines_trust_logit_cr <- coeftest(
  magazines_trust_logit,
  vcov. = vcovCL(
    magazines_trust_logit,
    cluster = ~ respondent_id,
    data = df_for_amce_trust_magazines,
    type = "HC1"
  )
)

magazines_trust_sum <- summary(magazines_trust_int)
names(magazines_trust_sum)

magazines_trust_25 <- as.data.frame(magazines_trust_sum$MagazinesTrustCleanc1amce)
magazines_trust_50 <- as.data.frame(magazines_trust_sum$MagazinesTrustCleanc2amce)
magazines_trust_75 <- as.data.frame(magazines_trust_sum$MagazinesTrustCleanc3amce)

magazines_trust_25$quantile <- "25%"
magazines_trust_50$quantile <- "50%"
magazines_trust_75$quantile <- "75%"

conditional_amces_magazines_trust <- rbind(
  magazines_trust_25,
  magazines_trust_50,
  magazines_trust_75
)

ggplot(conditional_amces_magazines_trust,
       aes(x = Estimate, y = Level, color = quantile)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Magazines trust",
    title = "Conditional AMCEs by Magazines Trust"
  ) +
  theme_minimal()
############################################################
## Interflex: Magazines Trust
############################################################

interflex_collab_magazines_trust <- interflex(
  Y = "outcome",
  D = "Collaboration",
  X = "Magazines_Trust_Clean_c",
  data = df_for_amce_trust_magazines,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal"
)

interflex_collab_magazines_trust$figure


interflex_econ_magazines_trust <- interflex(
  Y = "outcome",
  D = "Economic",
  X = "Magazines_Trust_Clean_c",
  data = df_for_amce_trust_magazines,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal"
)

interflex_econ_magazines_trust$figure


interflex_fspeech_magazines_trust <- interflex(
  Y = "outcome",
  D = "Freedom_of_Speech",
  X = "Magazines_Trust_Clean_c",
  data = df_for_amce_trust_magazines,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal"
)

interflex_fspeech_magazines_trust$figure


interflex_surv_magazines_trust <- interflex(
  Y = "outcome",
  D = "Surveillance",
  X = "Magazines_Trust_Clean_c",
  data = df_for_amce_trust_magazines,
  estimator = "binning",
  FE = NULL,
  cl = "respondent_id",
  base = "Liberal"
)

interflex_surv_magazines_trust$figure

###########
###########
###########

df_cregg$Interpersonalnever <- factor(
  df_cregg$Interpersonal_Communication_never,
  levels = c(0, 1),
  labels = c("Exposed to Interpersonal Communication", "Never Exposed")
)

interpersonal_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ Interpersonalnever
)

interpersonal_cj_anova

interpersonal_amce_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ Interpersonalnever
)

plot(interpersonal_amce_cregg, group = "Interpersonalnever")

interpersonal_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ Interpersonalnever
)

plot(interpersonal_mm_cregg, group = "Interpersonalnever", vline = 0.5)

df_cregg$InterpersonalUse <- factor(
  df_cregg$Interpersonal_Communication_Use,
  levels = c(0, 1),
  labels = c("Does not use Interpersonal Communication", "Uses Interpersonal Communication")
)

interpersonal_use_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ InterpersonalUse
)

interpersonal_use_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * InterpersonalUse,
  data = df_cregg,
  clusters = respondent_id,
  se_type = "stata"
)

summary(interpersonal_use_model)

interpersonal_use_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "amce",
  by = ~ InterpersonalUse
)

plot(interpersonal_use_cregg, group = "InterpersonalUse")

interpersonal_use_mm_cregg <- cregg::cj(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  estimate = "mm",
  by = ~ InterpersonalUse
)

plot(interpersonal_use_mm_cregg, group = "InterpersonalUse", vline = 0.5)


interpersonal_freq_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Interpersonal_Communication_Frequency_c,
  data = df_for_amce,
  respondent.id = "respondent_id",
  respondent.varying = "Interpersonal_Communication_Frequency_c"
)

summary(interpersonal_freq_int)

interpersonal_freq_lm <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Interpersonal_Communication_Frequency_c,
  data = df_for_amce,
  clusters = respondent_id,
  se_type = "stata"
)

summary(interpersonal_freq_lm)

car::linearHypothesis(
  interpersonal_freq_lm,
  c(
    "Interpersonal_Communication_Frequency_c = 0",
    "CollaborationIlliberal:Interpersonal_Communication_Frequency_c = 0",
    "EconomicIlliberal:Interpersonal_Communication_Frequency_c = 0",
    "Freedom_of_SpeechIlliberal:Interpersonal_Communication_Frequency_c = 0",
    "SurveillanceIlliberal:Interpersonal_Communication_Frequency_c = 0"
  ),
  vcov. = vcov(interpersonal_freq_lm)
)

interpersonal_freq_sum <- summary(interpersonal_freq_int)
names(interpersonal_freq_sum)

interpersonal_freq_25 <- as.data.frame(interpersonal_freq_sum$InterpersonalCommunicationFrequencyc1amce)
interpersonal_freq_50 <- as.data.frame(interpersonal_freq_sum$InterpersonalCommunicationFrequencyc2amce)
interpersonal_freq_75 <- as.data.frame(interpersonal_freq_sum$InterpersonalCommunicationFrequencyc3amce)

interpersonal_freq_25$quantile <- "25%"
interpersonal_freq_50$quantile <- "50%"
interpersonal_freq_75$quantile <- "75%"

conditional_amces_interpersonal_freq <- rbind(
  interpersonal_freq_25,
  interpersonal_freq_50,
  interpersonal_freq_75
)

ggplot(conditional_amces_interpersonal_freq,
       aes(x = Estimate, y = Level, color = quantile)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      xmin = Estimate - 1.96 * `Std. Err`,
      xmax = Estimate + 1.96 * `Std. Err`
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    x = "Conditional AMCE",
    y = NULL,
    color = "Interpersonal communication frequency",
    title = "Conditional AMCEs by Interpersonal Communication Frequency"
  ) +
  theme_minimal()


df_for_amce_trust_interpersonal <- df_for_amce |>
  filter(!is.na(Interpersonal_Communication_Trust_Clean_c))

interpersonal_trust_int <- cjoint::amce(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Interpersonal_Communication_Trust_Clean_c,
  data = df_for_amce_trust_interpersonal,
  respondent.id = "respondent_id",
  respondent.varying = "Interpersonal_Communication_Trust_Clean_c",
  na.ignore = TRUE
)

summary(interpersonal_trust_int)

interpersonal_trust_model <- estimatr::lm_robust(
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Interpersonal_Communication_Trust_Clean_c,
  data = df_for_amce_trust_interpersonal,
  clusters = respondent_id,
  se_type = "stata"
)

summary(interpersonal_trust_model)

car::linearHypothesis(
  
  interpersonal_trust_model,
  
  c(
    
    "Interpersonal_Communication_Trust_Clean_c = 0",
    
    "CollaborationIlliberal:Interpersonal_Communication_Trust_Clean_c = 0",
    
    "EconomicIlliberal:Interpersonal_Communication_Trust_Clean_c = 0",
    
    "Freedom_of_SpeechIlliberal:Interpersonal_Communication_Trust_Clean_c = 0",
    
    "SurveillanceIlliberal:Interpersonal_Communication_Trust_Clean_c = 0"
    
  ),
  
  vcov. = vcov(interpersonal_trust_model)
  
)

interpersonal_trust_sum <- summary(interpersonal_trust_int)

names(interpersonal_trust_sum)

interpersonal_trust_25 <- as.data.frame(interpersonal_trust_sum$InterpersonalCommunicationTrustCleanc1amce)

interpersonal_trust_50 <- as.data.frame(interpersonal_trust_sum$InterpersonalCommunicationTrustCleanc2amce)

interpersonal_trust_75 <- as.data.frame(interpersonal_trust_sum$InterpersonalCommunicationTrustCleanc3amce)

interpersonal_trust_25$quantile <- "25%"

interpersonal_trust_50$quantile <- "50%"

interpersonal_trust_75$quantile <- "75%"

conditional_amces_interpersonal_trust <- rbind(
  
  interpersonal_trust_25,
  
  interpersonal_trust_50,
  
  interpersonal_trust_75
  
)

ggplot(conditional_amces_interpersonal_trust,
       
       aes(x = Estimate, y = Level, color = quantile)) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(
    
    aes(
      
      xmin = Estimate - 1.96 * `Std. Err`,
      
      xmax = Estimate + 1.96 * `Std. Err`
      
    ),
    
    position = position_dodge(width = 0.5)
    
  ) +
  
  facet_wrap(~ Attribute, scales = "free_y") +
  
  labs(
    
    x = "Conditional AMCE",
    
    y = NULL,
    
    color = "Interpersonal communication trust",
    
    title = "Conditional AMCEs by Interpersonal Communication Trust"
    
  ) +
  
  theme_minimal()

## Demographic Interaction Effects
## 
df_cregg$IncomeLevel <- factor(

  df_cregg$Income_Level,

  levels = 1:5,

  labels = c(
  
    "Cannot afford food",
    
    "Food only",
    
    "Food + clothes",
    
    "Durables",
    
    "Affluent"
    
  ),
  
  ordered = TRUE
  
  )

income_int_model <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * IncomeLevel,
  
  data = df_cregg,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)
summary(income_int_model)

income_int_model_num <- estimatr::lm_robust(
  
  outcome ~ (Collaboration + Economic + Freedom_of_Speech + Surveillance) * Income_Level,
  
  data = df_for_amce,
  
  clusters = respondent_id,
  
  se_type = "stata"
  
)
summary(income_int_model_num)

income_cj_anova <- cregg::cj_anova(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  by = ~ IncomeLevel
  
)
income_cj_anova
income_amce_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "amce",
  
  by = ~ IncomeLevel
  
)

plot(income_amce_cregg, group = "IncomeLevel")

income_mm_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "mm",
  
  by = ~ IncomeLevel
  
)

plot(income_mm_cregg, group = "IncomeLevel")

df_cregg$EducationLevel <- factor(
  
  df_cregg$Education_Level,
  
  levels = 1:5,
  
  labels = c(
    
    "Primary or less",
    
    "Secondary (school)",
    
    "Vocational",
    
    "Incomplete higher",
    
    "Higher education"
    
  ),
  
  ordered = TRUE
  
)

sum(df_cregg$EducationLevel == "Primary or less", na.rm = TRUE)

prop.table(table(df_cregg$EducationLevel))

df_cregg$EducationLevel <- droplevels(df_cregg$EducationLevel)

education_cj_anova <- cregg::cj_anova(
  data = df_cregg,
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  id = ~ respondent_id,
  by = ~ EducationLevel
)

education_cj_anova

education_amce_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "amce",
  
  by = ~ EducationLevel
  
)

plot(education_amce_cregg, group = "EducationLevel")

education_mm_cregg <- cregg::cj(
  
  data = df_cregg,
  
  formula = outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance,
  
  id = ~ respondent_id,
  
  estimate = "mm",
  
  by = ~ EducationLevel
  
)

plot(education_mm_cregg, group = "EducationLevel", vline = 0.5)
df_for_amce$profile_heterogeneity


heterogeneity_check <- estimatr::lm_robust(outcome ~ factor(profile_heterogeneity),
                    data = df_for_amce, 
                    clusters = respondent_id,
                    se_type = "stata")
summary(heterogeneity_check)
heterogeneity_check_add <- estimatr::lm_robust(outcome ~ Collaboration + Economic + Freedom_of_Speech + Surveillance +factor(profile_heterogeneity),
                                               data = df_for_amce, 
                                               clusters = respondent_id,
                                               se_type = "stata")
summary(heterogeneity_check_add)
modelsummary(
  list(
    "(1)" = heterogeneity_check,
    "(2)" = heterogeneity_check_add
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  output = "heterogeneity_check.tex"
)

df_for_amce$profile_heterogeneity
table(df_for_amce$profile_heterogeneity)

ab_counts <- df_for_amce %>%
  
  count(profile_heterogeneity)

# proportions

tab_props <- df_for_amce %>%
  
  count(profile_heterogeneity) %>%
  
  mutate(prop = n / sum(n))

tab_combined <- df_for_amce %>%
  
  count(profile_heterogeneity) %>%
  
  mutate(
    
    prop = n / sum(n),
    
    percent = 100 * prop
    
  )

kable(
  
  tab_combined,
  
  format = "latex",
  
  booktabs = TRUE,
  
  digits = 3,
  
  col.names = c("Profile heterogeneity", "N", "Proportion", "Percent"),
  
  caption = "Distribution of Profile Heterogeneity"
  
)
latex_tab <- kable(
  tab_combined,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  col.names = c("Profile heterogeneity", "N", "Proportion", "Percent"),
  caption = "Distribution of Profile Heterogeneity"
)

writeLines(latex_tab, "heterogeneity_table.tex")
prop.table(table(df_for_amce$profile_heterogeneity))