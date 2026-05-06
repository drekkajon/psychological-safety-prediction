library(readxl)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

gss <- read_excel("C:/Users/ajlan/Downloads/Capstone Data.xlsx")

gss_clean <- gss %>%
  mutate(across(
    where(is.character),
    ~ ifelse(grepl("^\\.[a-z]:", .x), NA, .x)
  )) %>%
  filter(wrkstat %in% c("Working full time", "Working part time"))

model_data <- gss_clean %>%
  select(manvsemp, trustman, respect, spvtrfair, age, sex, race) %>%
  drop_na()

model_data <- model_data %>%
  mutate(
    manvsemp = factor(manvsemp),
    trustman = factor(trustman),
    respect = factor(respect),
    spvtrfair = factor(spvtrfair),
    sex = factor(sex),
    race = factor(race),
    age = as.numeric(age)
  )

set.seed(123)

train_index <- createDataPartition(
  model_data$manvsemp,
  p = 0.7,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

ctrl <- trainControl(
  method = "cv",
  number = 5
)

tree_grid <- expand.grid(cp = c(0.001, 0.01, 0.05))

tree_model <- train(
  manvsemp ~ trustman + respect + spvtrfair + age + sex + race,
  data = train_data,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = tree_grid
)

print(tree_model)

tree_preds <- predict(tree_model, newdata = test_data)

confusionMatrix(tree_preds, test_data$manvsemp)

rpart.plot(tree_model$finalModel)

confusionMatrix(tree_preds, test_data$manvsemp)

