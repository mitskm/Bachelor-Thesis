library(DeclareDesign)
library(tidyverse)
library(rdss)
library(cjoint)
library(readr)


N_subjects <- 500
N_tasks <- 4


levels_list <- list(
  collaboration    = c("Liberal", "Illiberal"),
  freetrading      = c("Liberal", "Illiberal"),
  freedomofspeech  = c("Liberal", "Illiberal"),
  surveillance     = c("Liberal", "Illiberal")
)


conjoint_utility <- function(data) {
  data |>
    mutate(
      U =
        0.05 * (collaboration == "Illiberal") +
        0.05 * (freetrading == "Illiberal") +
        0.05 * (freedomofspeech == "Illiberal") +
        0.05 * (surveillance == "Illiberal") +
        uij
    )
}

# -----------------------------
# Declare conjoint design
# -----------------------------

conjoint_design_equal_illiberal <-
  declare_model(
    subject = add_level(N = N_subjects),
    task = add_level(N = N_tasks, task = 1:N_tasks),
    profile = add_level(
      N = 2,
      profile = 1:2,
      uij = rnorm(N, sd = 0.2)
    )
  ) +
  declare_inquiry(
    handler = conjoint_inquiries,
    levels_list = levels_list,
    utility_fn = conjoint_utility
  ) +
  declare_assignment(
    handler = conjoint_assignment,
    levels_list = levels_list
  ) +
  declare_measurement(
    handler = conjoint_measurement,
    utility_fn = conjoint_utility
  ) +
  declare_estimator(
    choice ~ collaboration + freetrading + freedomofspeech + surveillance,
    respondent.id = "subject",
    .method = cjoint::amce
  )

# -----------------------------
# Diagnose baseline design
# -----------------------------

diagnosis_equal_illiberal <-
  diagnose_design(conjoint_design_equal_illiberal)

View(reshape_diagnosis(diagnosis_equal_illiberal))

# -----------------------------
# Diagnose redesigned versions
# -----------------------------

diagnosis_tasks_by_n <-
  conjoint_design_equal_illiberal |>
  redesign(
    N_subjects = c(429, 500),
    N_tasks = c(4, 5)
  ) |>
  diagnose_designs()

diag_df <- reshape_diagnosis(diagnosis_tasks_by_n)

View(diag_df)

# -----------------------------
# Clean diagnosis output
# -----------------------------

diag_clean <- diag_df |>
  rename_with(tolower) |>
  mutate(
    n_subjects = as.integer(n_subjects),
    n_tasks = as.integer(n_tasks),
    power = as.numeric(power)
  ) |>
  filter(!is.na(power))

# -----------------------------
# Summarise mean power
# -----------------------------

diag_summary <- diag_clean |>
  group_by(n_subjects, n_tasks) |>
  summarise(
    mean_power = mean(power, na.rm = TRUE),
    .groups = "drop"
  )

View(diag_summary)

# -----------------------------
# Plot power
# -----------------------------

ggplot(
  diag_summary,
  aes(
    x = n_subjects,
    y = mean_power,
    color = factor(n_tasks),
    group = n_tasks
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Number of respondents",
    y = "Mean statistical power",
    color = "Number of tasks",
    title = "Average Power by Sample Size and Number of Tasks"
  ) +
  theme_minimal(base_size = 13)

# -----------------------------
# Export full diagnosis
# -----------------------------

write_csv(
  diag_df,
  "all_simulated_models.csv"
)