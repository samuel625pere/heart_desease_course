library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(broom)
library(purrr)

set.seed(1,sample.kind = "rounding") # for the sake of replicablity
heart_data <- read.csv("heart.csv")

# The age column contains the ages of individuals

summary(heart_data$ï..age)
head(heart_data$ï..age)

# The sex column contains the sex information regarding the
# sex of the individual 1 = male 0 female
# this column was changed from integer to factor to better fit the analysis

heart_data <- heart_data %>% mutate(sex = as.factor(sex))
levels(heart_data$sex) <- c("female","male")

# The cp column indicates the type of chest pain felt by the patients
# it was changed to a factor variable better accommodate the calculations

heart_data <- heart_data %>% mutate(cp = as.factor(cp))
levels(heart_data$cp) <- c("asymptomatic","atypical angina","non-anginal pain","typical angina")

summary(heart_data$cp)

# The trestbps contains resting blood pressure in
# mm Hg

summary(heart_data$trestbps)


# The chol column have the concentration of cholesterol
# in mg/dl

summary(heart_data$chol)

# The column fbs have if the blood sugar levels registered
# collected after fasting were equivalent to 120 mg/dl 0 = False 1= True
# this column was also changed to better fit the models

heart_data <- heart_data %>% mutate(fbs = as.factor(fbs))
levels(heart_data$fbs) <- c("False","True")


# The restecg column have the results of a resting
# electrocardiogram divided into: 0 = normal 1 = ST abnormality
# 2 = ventricular Hypertrophy

heart_data <- heart_data %>% mutate(restecg = as.factor(restecg))
levels(heart_data$restecg) <- c("normal","ST","Hyper")

# The thalach column contains the values of maximum heart rate registered
# of the patients

summary(heart_data$thalach)

# The column exang show which patients had exercise induced angima
# 0 = no 1 = yes

heart_data <- heart_data %>% mutate(exang = as.factor(exang))
levels(heart_data$exang) <- c("no","yes")

# The oldpeak column show the sistolic depression induced by exercise
# in after rest

summary(heart_data$oldpeak)

boxplot(heart_data$oldpeak)


# The slope column contains the peak slope of the peak
# exercise in the ST segment 0 = upsloping , 1 = flat 2 = downsloping

heart_data <- heart_data %>% mutate(slope = as.factor(slope))
levels(heart_data$slope) <- c("upsloping","flat","downsloping")

# The ca column shows the number of major blood vessels
# coloured by flourosopy

heart_data <- heart_data %>% filter(ca != 4)
# the 4 in the columns are NAs
summary(heart_data)



# The Thal column has the identification of
# a heart defect, if was fixed or if it's normal 1 = normal 2 = fixed defect 7 = reversible defect
heart_data <- heart_data %>% filter(thal !=0)
heart_data <- heart_data %>% mutate(thal = as.factor(thal))
levels(heart_data$thal) <- c("normal","fixed defect","reversible defect")

# The target column is the variable this project is attempting to predict
# it contains the identification if a patient suffers from a heart disease
# with 1 = yes and 0 = no

heart_data <- heart_data %>% mutate(target = as.factor(target))
levels(heart_data$target) <- c("yes","no")


#modeling

#creating validation set

val_index <- createDataPartition(y = heart_data$target, times = 1, p = 0.1, list = FALSE)
validation_set <- heart_data[val_index,]

rm(val_index)

#Creating training and test sets

test_index <- createDataPartition(y = heart_data$target, times = 1, p = 0.2, list = FALSE)

test_set <- heart_data[test_index,]
train_set <- heart_data[-test_index,]
rm(test_index)