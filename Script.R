library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(broom)
library(purrr)
library(gam)

set.seed(1, sample.kind = "Rounding")# for the sake of replicablity
heart_data <- read.csv("heart.csv")

# The age column contains the ages of individuals

summary(heart_data$ï..age)
head(heart_data$ï..age)
colnames(heart_data)[1] <- "age"

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
levels(heart_data$exang) <- c("not present","present")

# The oldpeak column show the sistolic depression induced by exercise
# in after rest

summary(heart_data$oldpeak)

# The slope column contains the peak slope of the peak
# exercise in the ST segment 0 = upsloping , 1 = flat 2 = downsloping

heart_data <- heart_data %>% mutate(slope = as.factor(slope))
levels(heart_data$slope) <- c("upsloping","flat","downsloping")

# The ca column shows the number of major blood vessels
# coloured by flourosopy

heart_data <- heart_data %>% filter(ca != 4)
# the 4 in the columns are NAs




# The Thal column has the identification of a heart defect via a thalium imaging
#test , if was fixed or if it's normal 1 = fixed defect 
#2 = normal 7 = reversible defect
heart_data <- heart_data %>% filter(thal !=0)
heart_data <- heart_data %>% mutate(thal = as.factor(thal))
levels(heart_data$thal) <- c("fixed defect","normal","reversible defect")

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

# first let's check the characteristics of the data we are using
# via exploratory analysis to see if there

exp_01 <- heart_data %>% ggplot(aes(x = target, y = age,  col = target))+
  geom_jitter()+
  labs(x = "target", y = "age of the patients")+
  theme_bw()

exp_02 <- heart_data %>% ggplot(aes(x = sex, fill = target))+
  geom_bar(stat = "count",position = "fill")+
  labs(x = "sex of patient")+
  theme_bw()

exp_03 <- heart_data %>% ggplot(aes(x = cp, fill = target))+
  geom_bar(stat = "count",position = "fill")+
  labs(x = "chest pain type")+
  theme_bw()

exp_04 <- heart_data %>% ggplot(aes(x = target, y = trestbps, fill = target))+
  geom_boxplot()+
  labs(x = "resting blood pressure mm Hg")+
  theme_bw()

exp_05 <- heart_data %>% ggplot(aes( x = chol, fill = target))+
  geom_density(alpha = 0.2)+
  labs(x = "cholesterol ml/dl")+
  theme_bw()

exp_06 <- heart_data %>% ggplot(aes(x = fbs, fill = target))+
  geom_bar(position = "fill")+
  labs(x = "fasting blood sugar > 120 mg/dl")+
  theme_bw()

exp_07 <- heart_data %>% ggplot(aes(x = restecg, fill = target))+
  geom_bar(position = "fill")+
  labs(x = "fasting blood sugar > 120 mg/dl")+
  theme_bw()

exp_08 <- heart_data %>% ggplot(aes(x = thalach, fill = target))+
  geom_density(alpha = 0.2)+
  labs(x ="maximum heart rate achieved")+
  theme_bw()

exp_09 <- heart_data %>% ggplot(aes(x = exang, fill = target))+
  geom_bar(position = "fill")+
  labs(x="exercise induced angina")+
  theme_bw()

exp_10 <- heart_data %>% ggplot(aes(x = target, y = oldpeak, fill = target))+
  geom_boxplot()+
  labs(x = "target", y = "ST depression induced by exercise relative to rest")+
  theme_bw()

exp_11 <- heart_data %>% ggplot(aes(x = slope, fill = target))+
  geom_bar(position = "fill")+
  labs(x = "Slope of the peak exercise ST segment")+
  theme_bw()

exp_12 <- heart_data %>% ggplot(aes(x = as.factor(ca), fill = target))+
  geom_bar()+
  labs(x = "n of major blood vessels shown in flouroscopy")+
  theme_bw()

 exp_13 <- heart_data %>% ggplot(aes(x = thal, fill = target))+
   geom_bar(position = "fill")+
   labs(x = "thallium test results")

# The measure to evaluate the effectiveness will be the accuracy of the
# estimates

#  logistic regression model

glm_fit <- train(x = train_set[,1:13],y = train_set$target,method = "glm")

glm_acc <- mean(test_set$target == predict(object = glm_fit, newdata = test_set[,1:13],
                                           test_set$target, type = "raw"))

# decision tree

rpart_fit <- train(x = train_set[,1:13],y = train_set$target, method = "rpart",
                  tuneGrid= data.frame(cp = seq(0,0.04,len = 20)))

rpart_acc <- mean(test_set$target == predict(object = rpart_fit, newdata = test_set[,1:13],
                                             test_set$target, type = "raw"))

# Random Forest

forest_fit <- train(x = train_set[,1:13],y = train_set$target, method = "rf")

forest_acc <- mean(test_set$target == predict(object = forest_fit, newdata = test_set[,1:13],
                                              test_set$target, type = "raw"))

# Ensemble

models<- c("glm","rpart","rf")

#training the models

fits <- lapply(models, function(model){ 
  train(x = train_set[,1:13],y = train_set$target, method = model)
})


#making predictions for each fitted model

names(fits) <- models

p_models <- sapply(fits, function(model){
  predict(object = model, newdata = test_set[,1:13],test_set$target,type="raw")
})



probs <- rowMeans(p_models == "yes") 
prediction <- ifelse(probs > 0.5,"yes","no")
ens_acc <- mean(prediction == test_set$target)


# validating test via accuracy

predict_heart <- function(data){

models<- c("glm","rpart","rf")



names(fits) <- models

f_models <- sapply(fits, function(model){
  predict(object = model, newdata = data[,1:13],data$target,type="raw")
})



probs <- rowMeans(f_models == "yes") 
f_prediction <- ifelse(probs > 0.5,"yes","no")
val_acc <- mean(f_prediction == data$target)
print(val_acc)
}

# calculating validation for all models

final_accs <- c(predict_heart(validation_set),
                mean(validation_set$target == predict(object = glm_fit, newdata = validation_set[,1:13],
                                                validation_set$target, type = "raw")),
                mean(validation_set$target == predict(object = rpart_fit, newdata = validation_set[,1:13],
                                                validation_set$target, type = "raw")),
                mean(validation_set$target == predict(object = forest_fit, newdata = validation_set[,1:13],
                                                validation_set$target, type = "raw"))
                )


