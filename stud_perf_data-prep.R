#data including student performance on math, reading, and writing tests
#suitable for regression

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
library ('rgl')
library('RMSE')
library(janitor)

StudentsPerformance <- read_xlsx("StudentsPerformance.xlsx")

StudentsPerformance <- StudentsPerformance %>%
  clean_names() #make the variable names more usable


#clean the data for final model building
#
StudentsPerformance<- StudentsPerformance %>%
  mutate(parental_level_of_education = ordered(parental_level_of_education, labels = c(1:6), levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree", "master's degree"))) %>%
  mutate(college_or_not = as.factor(ifelse(parental_level_of_education>=3, "yes", "no"))) %>% #new variables that I may or may not use
  mutate(race_e = as.factor((ifelse(race_ethnicity=="group E" , "group E", "other")))) %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  mutate(avg_score = round((sum(math_score,reading_score,writing_score)/3),2)) %>%
  data.frame()

write.xlsx(StudentsPerformance, "clean-data.xlsx")

summary(StudentsPerformance)

StudentsPerformance %>%
group_by(gender) %>%
  summarise(
    count = n(),
    mean.math = mean(math_score, na.rm = TRUE),
    sd.math = sd(math_score, na.rm = TRUE),
    mean.reading = mean(reading_score, na.rm=TRUE),
    sd.reading = sd(reading_score, na.rm = TRUE),
    mean.writing = mean(writing_score, na.rm=TRUE),
    sd.writing = sd(writing_score, na.rm = TRUE)
  )

StudentsPerformance %>%
  group_by(test_preparation_course) %>%
  summarise(
    count = n(),
    mean.math = mean(math_score, na.rm = TRUE),
    sd.math = sd(math_score, na.rm = TRUE),
    mean.reading = mean(reading_score, na.rm=TRUE),
    sd.reading = sd(reading_score, na.rm = TRUE),
    mean.writing = mean(writing_score, na.rm=TRUE),
    sd.writing = sd(writing_score, na.rm = TRUE)
  )

StudentsPerformance %>%
  group_by(race_ethnicity) %>% #hmm..performance seems to increase as you go through the groups
  summarise(
    count = n(),
    mean.math = mean(math_score, na.rm = TRUE),
    sd.math = sd(math_score, na.rm = TRUE),
    mean.reading = mean(reading_score, na.rm=TRUE),
    sd.reading = sd(reading_score, na.rm = TRUE),
    mean.writing = mean(writing_score, na.rm=TRUE),
    sd.writing = sd(writing_score, na.rm = TRUE)
  )

StudentsPerformance %>%
  mutate(parental_level_of_education = ordered(parental_level_of_education, levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree", "master's degree"))) %>%
  group_by(parental_level_of_education) %>%
  summarise(
    count = n(),
    mean.math = mean(math_score, na.rm = TRUE),
    sd.math = sd(math_score, na.rm = TRUE),
    mean.reading = mean(reading_score, na.rm=TRUE),
    sd.reading = sd(reading_score, na.rm = TRUE),
    mean.writing = mean(writing_score, na.rm=TRUE),
    sd.writing = sd(writing_score, na.rm = TRUE)
  )

# basic histogram of target variables
#
hist(StudentsPerformance$math_score, col='green')
hist(StudentsPerformance$reading_score,col='gray', add=TRUE)
hist(StudentsPerformance$writing_score,col='red', add=TRUE)

#math score by parental degree level and gender
# fewer students have parents with master's degree, but for those who do, their score appear
#  slightly more negatively skewed
#  scores for males also looks more negatively skewed (higher mean than females)
#  check to see if these gender differences are meaningful
#
StudentsPerformance %>%
  mutate(parental_level_of_education = ordered(parental_level_of_education, levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree", "master's degree"))) %>%
  ggplot(aes(math_score))+
  geom_histogram(aes(color=gender, fill=gender), alpha=0.6, position='identity', bins = 40)+
  facet_wrap(~parental_level_of_education, nrow=2, ncol = 3)

#reading scores by parental degree level and gender
#it looks like females mean might be higher on reading

StudentsPerformance %>%
mutate(parental_level_of_education = ordered(parental_level_of_education, levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree", "master's degree"))) %>%
  ggplot(aes(reading_score))+
  geom_histogram(aes(color=gender, fill=gender), alpha=0.6, position='identity', bins = 40)+
  facet_wrap(~parental_level_of_education, nrow=2, ncol = 3)

#writing scores by parental degree level and gender
#also looks higher for females

StudentsPerformance %>%
  mutate(parental_level_of_education = ordered(parental_level_of_education, levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree", "master's degree"))) %>%
  ggplot(aes(writing_score))+
  geom_histogram(aes(color=gender, fill=gender), alpha=0.6, position='identity', bins = 40)+
  facet_wrap(~parental_level_of_education, nrow=2, ncol = 3)



