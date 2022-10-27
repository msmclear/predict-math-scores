# predict-math-scores
math score prediction using student performance data from Kaggle


---
title: "Math Score Prediction"
author: "Monique McLeary"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

library('car')
library('tidyverse')
library('ggrepel')
library('ggthemes')
library('scales')
library('mice')
library('randomForest')
library('data.table')
library('gridExtra')
library('corrplot')
library('GGally')
library('e1071')
library('caTools')
library('janitor')
library(readxl)
library(pastecs)
library(openxlsx)

clean_data <- read_xlsx("clean-data.xlsx")
```


## EDA and Visualizations

```{r numeric-descriptives}
#creating a numeric variable only dataset

numeric_df <- select_if(clean_data, is.numeric)

numeric_df %>%  stat.desc(norm = TRUE) %>%
  round(2)

```

## Model Building and Testing

### Train and Test Datasets

```{r sampling}
set.seed(12345)
split <- sample.split(clean_data$math_score, SplitRatio = 0.7)
train <- subset(clean_data, split==TRUE)
test <- subset(clean_data, split==FALSE)
```

### Compute the analysis of variance

```{r analysis-of-variance}
gender.aov <- aov(math_score ~ gender, data = train)

summary(gender.aov)
```

This shows that the gender differences in reading scores are significant

### Test normality of scores by gender

```{r}
plot(gender.aov, 2)
```

### Test for homogeneity of variance

This test tells us ...

```{r levenes-test, echo=FALSE}
leveneTest(reading_score ~ as.factor(gender), data = train)
```

#"From the output above we can see that the p-value is not less than the significance level of 0.05.
#This means that there is no evidence to suggest that the variance across groups is statistically significantly different.
#Therefore, we can assume the homogeneity of variances in the different treatment groups."


# Extract the residuals

```{r residuals, echo=FALSE}
aov_residuals <- residuals(object = gender.aov )

```


# Run Shapiro-Wilk test
```{r SW-test, echo=FALSE}
shapiro.test(x = aov_residuals )
```


## predict math scores
```{r prediction}
math_model = lm(math_score ~ gender + race_ethnicity + parental_level_of_education + lunch+  test_preparation_course + reading_score + writing_score ,
                data = clean_data)

summary(math_model)

Anova(math_model,
      type = "II")
```


### Predict math scores using the new model

```{r test-model}
predict <- predict(math_model, test)

write.xlsx(predict, "math-score-prediction.xlsx")

```


## Model Evaluation

### RMSE

#not bad

```{r model-eval}
sqrt(mean((test$math_score - predict)^2))
```


