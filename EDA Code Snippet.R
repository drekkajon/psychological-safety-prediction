library(readxl)
library(tidyverse)   # includes dplyr, tidyr, ggplot2
library(MASS)        # for polr()
library(car)         # for VIF
library(effects)     # for Effect()

gss <- read_excel("C:/Users/ajlan/Downloads/Capstone Data.xlsx")

gss_clean <- gss %>%
  mutate(across(
    where(is.character),
    ~ ifelse(grepl("^\\.[a-z]:", .x), NA, .x)
  )) %>%
  filter(wrkstat %in% c("Working full time", "Working part time"))

model_df <- gss_clean %>%
  dplyr::select(
    manvsemp,
    trustman,
    respect,
    spvtrfair,
    age,
    educ,
    sex,
    race,
    year
  ) %>%
  tidyr::drop_na()

model_df <- model_df %>%
  mutate(
    manvsemp = factor(manvsemp, ordered = TRUE),
    trustman = factor(trustman, ordered = TRUE),
    respect = factor(respect, ordered = TRUE),
    spvtrfair = factor(spvtrfair, ordered = TRUE),
    sex = factor(sex),
    race = factor(race),
    educ = as.numeric(educ),
    age = as.numeric(age)
  )

model_df <- model_df %>% dplyr::select(-educ)

vif(lm(as.numeric(manvsemp) ~ trustman + respect + spvtrfair + age + sex + race,
       data = model_df))

model <- polr(
  manvsemp ~ trustman + respect + spvtrfair + age + sex + race,
  data = model_df,
  Hess = TRUE
)

summary(model)

eff_trust <- Effect("trustman", model)
eff_df <- as.data.frame(eff_trust)

eff_fair <- Effect("spvtrfair", model)
eff_df2 <- as.data.frame(eff_fair)

ggplot(eff_df, aes(x = trustman, y = fit)) +
  geom_line(size = 1.2, color = "#2C3E50") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(
    title = "Predicted Probability of Positive Relationship Quality by Trust in Management",
    x = "Trust in Management",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14)

colnames(eff_df)

ggplot(eff_df, aes(x = trustman, y = `Very Good`)) +
  geom_line(linewidth = 1.2, color = "#2C3E50") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(
    title = "Predicted Probability of Reporting 'Very Good' Relationship Quality by Trust in Management",
    x = "Trust in Management",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14)

ggplot(eff_df, aes(x = trustman, y = prob.Very.good, group = 1)) +
  geom_line(linewidth = 1.2, color = "#2C3E50") +
  geom_ribbon(aes(ymin = L.prob.Very.good, ymax = U.prob.Very.good), alpha = 0.2) +
  labs(
    title = "Predicted Probability of Reporting 'Very Good' Relationship Quality by Trust in Management",
    x = "Trust in Management",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14)

ggplot(eff_df2, aes(x = spvtrfair, y = prob.Very.good, group = 1)) +
  geom_line(linewidth = 1.2, color = "#1F618D") +
  geom_ribbon(aes(ymin = L.prob.Very.good, ymax = U.prob.Very.good), alpha = 0.2) +
  labs(
    title = "Predicted Probability of Reporting 'Very Good' Relationship Quality by Supervisory Fairness",
    x = "Supervisory Fairness",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14)


# Initial dataset
n_initial <- nrow(gss)

# After employment filter
gss_emp <- gss %>%
  filter(wrkstat %in% c("Working full time", "Working part time"))

n_employed <- nrow(gss_emp)

# After reserve code cleaning
gss_clean <- gss %>%
  mutate(across(
    where(is.character),
    ~ ifelse(grepl("^\\.[a-z]:", .x), NA, .x)
  )) %>%
  filter(wrkstat %in% c("Working full time", "Working part time"))

n_clean <- nrow(gss_clean)

# Final modeling dataset (complete cases)
n_final <- nrow(model_df)

# Print results
n_initial
n_employed
n_clean
n_final

km <- kmeans(
  scale(
    model_df %>%
      dplyr::mutate(
        trustman = as.numeric(trustman),
        respect = as.numeric(respect),
        spvtrfair = as.numeric(spvtrfair)
      ) %>%
      dplyr::select(trustman, respect, spvtrfair)
  ),
  centers = 3
)

km