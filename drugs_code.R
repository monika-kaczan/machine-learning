# Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(tibble)
library(purrr)
library(corrplot)
library(DescTools)
library(glmnet)
library(AER)
library(lmtest)
library(nnet)
library(pROC)
library(ROSE)
library(janitor)
library(DMwR)
library(bestNormalize)
library(Information)
library(smbinning)
library(lattice)
library(ggplot2)
library(woeBinning)



#### Data preparation ####


# Firstly, we loaded the data, checked how they look like and whether there are any missing observations. We dropped the id column.



###### General ######


# No data missing
drugs <- read_csv("drugs_train.csv")
any(is.na(drugs))
glimpse(drugs)
drugs <- subset(drugs, select = -c(id) )


###### Ordinal and categorical variables ######


# As ordinal and categorical variables are stored as characters, we needed to convert it to factors.
drugs$gender = factor(drugs$gender, levels = c("male", "female"), ordered = TRUE)
drugs$consumption_cocaine_last_month = factor(drugs$consumption_cocaine_last_month, levels = c("No", "Yes"), ordered = TRUE)
glimpse(drugs)

# Most people are either from Australia or USA. Other countries (Canada, Ireland, UK and New Zeland) all have under 10% of representation. Therefore we decided to include them as "Other".  
table(drugs$country)
drugs$country[drugs$country %in% c("Canada","Ireland", "UK", "New Zealand")] <- "Other"
drugs$country <- as.factor(drugs$country)

# Over 90% of observations are Mixed-Black/Asian which suggest that our data
# is homogeneous. Therefore we decided to drop variable ethnicity from our data.
table(drugs$ethnicity)
drugs <- subset(drugs, select = -c(ethnicity) )


# Some categories describing age and consumption of different drugs have very few observations compared to others. This is why we decided to merge selected categories which we believed presented similar data. 

table(drugs$age)
drugs$age[drugs$age == "65+"] <- "55-64"
drugs$age[drugs$age == "55-64"] <- "55+"
drugs$age <- factor(drugs$age, 
                    levels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                    ordered = TRUE)
drugs$age <- droplevels(drugs$age)

table(drugs$education)
drugs$education[drugs$education == "Left school before 16 years"] <- "Left school at/before 18 years"
drugs$education[drugs$education == "Left school at 16 years"] <- "Left school at/before 18 years"
drugs$education[drugs$education == "Left school at 17 years"] <- "Left school at/before 18 years"
drugs$education[drugs$education == "Left school at 18 years"] <- "Left school at/before 18 years"
drugs$education[drugs$education == "Masters degree"] <- "Masters degree or higher"
drugs$education[drugs$education == "Doctorate degree"] <- "Masters degree or higher"
table(drugs$education)
drugs$education <- factor(drugs$education,
                          levels = c("Left school at/before 18 years",
                                     "Some college or university, no certificate or degree",
                                     "Professional certificate/ diploma",
                                     "University degree",
                                     "Masters degree or higher"),
                          ordered = TRUE)
drugs$education <- droplevels(drugs$education)

table(drugs$consumption_alcohol) # Here we changed variables names
drugs$consumption_alcohol <- factor(drugs$consumption_alcohol,
                                    levels = c("never used",
                                               "used over a decade ago",
                                               "used in last decade",
                                               "used in last year",
                                               "used in last month",
                                               "used in last week", 
                                               "used in last day"),
                                    ordered = TRUE)
drugs$consumption_amphetamines <- factor(drugs$consumption_amphetamines,
                                         levels = c("never used",
                                                    "used over a decade ago",
                                                    "used in last decade",
                                                    "used in last year",
                                                    "used in last month",
                                                    "used in last week", 
                                                    "used in last day"),
                                         ordered = TRUE)
drugs$consumption_caffeine <- factor(drugs$consumption_caffeine,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)
drugs$consumption_cannabis <- factor(drugs$consumption_cannabis,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)
drugs$consumption_chocolate <- factor(drugs$consumption_chocolate,
                                      levels = c("never used",
                                                 "used over a decade ago",
                                                 "used in last decade",
                                                 "used in last year",
                                                 "used in last month",
                                                 "used in last week", 
                                                 "used in last day"),
                                      ordered = TRUE)
drugs$consumption_mushrooms <- factor(drugs$consumption_mushrooms,
                                      levels = c("never used",
                                                 "used over a decade ago",
                                                 "used in last decade",
                                                 "used in last year",
                                                 "used in last month",
                                                 "used in last week", 
                                                 "used in last day"),
                                      ordered = TRUE)
drugs$consumption_nicotine <- factor(drugs$consumption_nicotine,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)

drugs$consumption_alcohol[drugs$consumption_alcohol == "never used"] <- "used over a decade ago"
drugs$consumption_alcohol[drugs$consumption_alcohol == "used in last day"] <- "used in last week"
drugs$consumption_alcohol <- droplevels(drugs$consumption_alcohol)

drugs$consumption_amphetamines[drugs$consumption_amphetamines == "never used"] <- "used over a decade ago"
drugs$consumption_amphetamines[drugs$consumption_amphetamines == "used in last day"] <- "used in last week"
drugs$consumption_amphetamines <- droplevels(drugs$consumption_amphetamines)

drugs$consumption_caffeine[drugs$consumption_caffeine == "never used"] <- "used over a decade ago"
drugs$consumption_caffeine[drugs$consumption_caffeine == "used in last day"] <- "used in last week"
drugs$consumption_caffeine <- droplevels(drugs$consumption_caffeine)

drugs$consumption_cannabis[drugs$consumption_cannabis == "never used"] <- "used over a decade ago"
drugs$consumption_cannabis[drugs$consumption_cannabis == "used in last day"] <- "used in last week"
drugs$consumption_cannabis <- droplevels(drugs$consumption_cannabis)

drugs$consumption_chocolate[drugs$consumption_chocolate == "never used"] <- "used over a decade ago"
drugs$consumption_chocolate[drugs$consumption_chocolate == "used in last day"] <- "used in last week"
drugs$consumption_chocolate <- droplevels(drugs$consumption_chocolate)

drugs$consumption_mushrooms[drugs$consumption_mushrooms == "never used"] <- "used over a decade ago"
drugs$consumption_mushrooms[drugs$consumption_mushrooms == "used in last day"] <- "used in last week"
drugs$consumption_mushrooms <- droplevels(drugs$consumption_mushrooms)

drugs$consumption_nicotine[drugs$consumption_nicotine == "never used"] <- "used over a decade ago"
drugs$consumption_nicotine[drugs$consumption_nicotine == "used in last day"] <- "used in last week"
drugs$consumption_nicotine <- droplevels(drugs$consumption_nicotine)


# We also checked relationship between the dependend variable (consumption_cocaine_last_month) and different consumptions of drugs and other products using Cramer V. 
# We noticed that the Cramer's V is very low (around) for caffeine. Model estimated later with thisvariable had indicated that caffeine have very low Information Values (< 0.01) and is not statistically significant. Additionaly, caffeine is a common drug which everybody uses so it likely have no linkedge to the consumption of cocaine. Based on that, we decided to drop it from the model.
DescTools::CramerV(drugs$consumption_cocaine_last_month,
                   drugs$consumption_caffeine)
drugs <- subset(drugs, select = -c(consumption_caffeine))


###### Numeric variables ######

# All numeric variables except sensation and impulsiveness have pretty normal distribution. We checked their distribution using histograms and boxplots. 

hist(drugs$personality_sensation)
hist(drugs$personality_impulsiveness)
par( mfrow= c(3,3) )
boxplot(personality_agreeableness ~ consumption_cocaine_last_month, 
        data = drugs)
boxplot(personality_sensation ~ consumption_cocaine_last_month,  
        data = drugs)
boxplot(personality_neuroticism ~ consumption_cocaine_last_month,  
        data = drugs)
boxplot(personality_extraversion ~ consumption_cocaine_last_month, 
        data = drugs)
boxplot(personality_conscientiousness ~ consumption_cocaine_last_month,  
        data = drugs)
boxplot(personality_impulsiveness ~ consumption_cocaine_last_month,  
        data = drugs)
boxplot(personality_openness ~ consumption_cocaine_last_month,  
        data = drugs)

# To reduce possible overfitting of the model, we decided to transform numerical variables related to personality. The reason of that is the fact those personality traits are generally hard to measure and the fact that somebody got let's say 42.5 and not 47.9 might not be statistically important. Therefore we decided to transform those variables based cut-off points: under 35 point would indicate that this personality trait is weak, between 35 and 65 points that character trait is neutral, and above 65 that it is strong.
# We also tried out other options, such as transforming those variables to 2 or 5 levels based on quantiles or transforming to different number of levels based on hard cut off points. However, we found out that these cut-off points give the best results in terms of ROC, Sensibility, Accuracy and Kappa.

drugs$personality_neuroticism <- ifelse(drugs$personality_neuroticism < 35, 0, ifelse(drugs$personality_neuroticism < 65, 1, 2))
drugs$personality_openness <- ifelse(drugs$personality_openness < 35, 0, ifelse(drugs$personality_openness < 65, 1, 2))
drugs$personality_agreeableness <- ifelse(drugs$personality_agreeableness < 35, 0, ifelse(drugs$personality_agreeableness < 65, 1, 2))
drugs$personality_extraversion <- ifelse(drugs$personality_extraversion < 35, 0, ifelse(drugs$personality_extraversion < 65, 1, 2))
drugs$personality_conscientiousness <- ifelse(drugs$personality_conscientiousness < 35, 0, ifelse(drugs$personality_conscientiousness < 65, 1, 2))
drugs$personality_impulsiveness <- ifelse(drugs$personality_impulsiveness < 35, 0, ifelse(drugs$personality_impulsiveness < 65, 1, 2))
drugs$personality_sensation <- ifelse(drugs$personality_sensation < 35, 0, ifelse(drugs$personality_sensation < 65, 1, 2))
drugs[, 5:11] <- lapply(drugs[, 5:11], factor)
glimpse(drugs)




#### Balancing sample ####

# One of the problems with the sample is that it is unbalanced - our dependend variable, consuption of cocaine in the last month, takes value "Yes" in only 9.24% of observations. It means that prediction of this value will be a subject to much bigger error.
table(drugs$consumption_cocaine_last_month)

# This is why we decided to balance the sample. We compared 3 different methods: weighning observations, upsampling and SMOTE.

fiveStats <- function(...) c(twoClassSummary(...), 
                             defaultSummary(...))
source("functions/F_own_summary_functions.R")


# For model validation we are splitting the data into train and test samples. Additionaly, we are using cross-validation on the training sample. Here in each case we are using 5-fold cross-validation repeated 3 times.

set.seed(987654321)
drugs_which_training <- createDataPartition(drugs$consumption_cocaine_last_month,
                                            p = 1099/1500, 
                                            list = FALSE) 

drugs_train <- drugs[c(drugs_which_training),]
drugs_test <- drugs[-c(drugs_which_training),]

ctrl_cv5 <- trainControl(method = "repeatedcv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = mySummary,
                         repeats = 3)

set.seed(0)

# Basic model
drugs_logit_train <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5)
summary(drugs_logit_train)
drugs_logit_train



###### Weighning observations ######

(freqs <- table(drugs_train$consumption_cocaine_last_month))

myWeights <- ifelse(drugs_train$consumption_cocaine_last_month == "Yes",
                    0.5/freqs[2], 
                    0.5/freqs[1]) * nrow(drugs_train)

tabyl(myWeights)

sum(myWeights) == nrow(drugs_train)

set.seed(987654321)

drugs_logit_train_weighted <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5,
        weights = myWeights)

drugs_logit_train_weighted

# Specisifity drastically improved, as well as ROC and Kappa but Accuracy and Sensibility lower


###### Upsampling ######
# We do not use downsampling as the number of observations is not large

ctrl_cv5$sampling <- "up"

set.seed(987654321)

drugs_logit_train_up <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5
        )

drugs_logit_train_up


###### SMOTE model #####

ctrl_cv5$sampling <- "smote"

set.seed(987654321)

drugs_logit_train_smote <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5
  )

drugs_logit_train_smote


###### Comparison ######

models_all <- ls(pattern = "drugs_logit_train")

sapply(models_all,
       function(x) (get(x))$results[,2:6]) %>% 
  t()

source("functions/F_summary_binary_class.R")

models_all %>% 
  sapply(function(x) get(x) %>% 
           predict(newdata = drugs_train) %>% 
           summary_binary_class(level_positive = "Yes",
                                level_negative = "No",
                                real = drugs_train$consumption_cocaine_last_month)) %>% 
  t()

# Choosing upsampling model based on the highest Accuracy and relatively high other
# performance measures on the train data.


#### Logit and probit ####

# Logit and probit are two main regression models for classification. 

# Let's focus on the logistic regression model we estimated with upsampling.
ctrl_cv5_logit <- trainControl(method = "repeatedcv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = mySummary,
                         repeats = 3)
ctrl_cv5_logit$sampling <- "up"

# Basic model
set.seed(987654321)
drugs_logit_train_final <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5_logit)

# We see that most variables are statistically significant. We decided not to delete or add variables at this point.

summary(drugs_logit_train_final)
drugs_logit_train_final

# We also tried the probit model.  

set.seed(987654321)
drugs_probit_train_final <- 
  train(consumption_cocaine_last_month ~ ., 
        data = drugs_train,
        method = "glm",
        family = binomial(link = "probit"),
        trControl = ctrl_cv5_logit)
drugs_probit_train_final

summary(drugs_probit_train_final)
drugs_probit_train_final


#### KNN ####

# KNN (k nearest neighbours) is a supervised learning method in which (in case of classification) the object is being assigned to the class most common among its k nearest neighbors. Here we try out different values of k from 1 to 50 by 2 and select the best one.

ctrl_cv5_knn <- trainControl(method = "repeatedcv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = mySummary,
                         repeats = 3)
ctrl_cv5_knn$sampling <- "up"

different_k = data.frame(k = seq(1, 40, 2) )

set.seed(987654321)
drugs_knn_train <- train(consumption_cocaine_last_month ~ ., 
        drugs_train,        
        method = "knn",
        metric = "ROC",
        trControl = ctrl_cv5_knn,
        tuneGrid = different_k,
        preProcess = c("range"))

drugs_knn_train
plot(drugs_knn_train)

# Taking ROC into account, the best model is with k = 35. It has also relatively high Balanced Accuracy.

k_value <- data.frame(k = 39)

set.seed(987654321)
drugs_knn_train_final <- 
  train(consumption_cocaine_last_month ~ .,
        drugs_train,        
        method = "knn",
        metric = "ROC",
        trControl = ctrl_cv5_knn,
        tuneGrid = k_value,
        preProcess = c("range"))

drugs_knn_train_final


#### SVM ####

# SVM (support-vector machine) is a supervised learning model which maps training examples to points in space so as to maximise the width of the gap between the two categories. 

# Here we are running SVM with radial basis kernel (gaussian).

ctrl_cv5_svm <- trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE)
ctrl_cv5_svm$sampling <- "up"

parametersC_sigma <- 
  expand.grid(C = c(0.01, 0.1, 1, 5, 10),
              sigma = c(0.01, 0.1, 1, 5, 10))
set.seed(987654321)
drugs_svm_train <- train(consumption_cocaine_last_month ~ ., 
                           data = drugs_train, 
                           method = "svmRadial",
                           tuneGrid = parametersC_sigma,
                           trControl = ctrl_cv5_svm)
drugs_svm_train
# The best accuracy for sigma = 10 and C = 0.01, but Kappa is 0.11. Therefore we would probably choose C = 1 and sigma = 0.01 which has Accuracy almost as high but Kappa of 0.23. 

# We wanted to also try out polynomial kernel.
svm_parametersPoly <- expand.grid(C = c(0.01, 1),
                                  degree = 2:5, 
                                  scale = c(1,10))
set.seed(987654321)
drugs_svm_train2 <- train(consumption_cocaine_last_month ~ ., 
                         data = drugs_train, 
                         method = "svmPoly",
                         tuneGrid = svm_parametersPoly,
                         trControl = ctrl_cv5_svm)
drugs_svm_train2

# Here Accuracy is similiarly high as with the gaussian kernel but all Kappa are quite low. We would probably want to reject those cases. Therefore, our final SVM model is with gaussian kernel with parameters C = 1 and sigma = 0.01.

parametersC_sigma_final <- 
  expand.grid(C = c(1), sigma = c(0.01))
set.seed(987654321)
drugs_svm_train_final <- train(consumption_cocaine_last_month ~ ., 
                          data = drugs_train, 
                          method = "svmRadial",
                          tuneGrid = parametersC_sigma_final,
                          trControl = ctrl_cv5_svm)
drugs_svm_train_final



#### Selecting the best model ####

# To select the best model, we compared performance of logit, probit, KNN and SVM models on training, test and general (combined training and test) samples using different performance measures.  


logit_fitted = predict(drugs_logit_train_final, drugs_train)
logit_results = summary_binary_class(predicted_classes = logit_fitted,
                                     real = drugs_train$consumption_cocaine_last_month)
logit_fitted_test = predict(drugs_logit_train_final, drugs_test)
logit_results_test = summary_binary_class(predicted_classes = logit_fitted_test,
                                     real = drugs_test$consumption_cocaine_last_month)
logit_fitted_all = predict(drugs_logit_train_final, drugs)
logit_results_all = summary_binary_class(predicted_classes = logit_fitted_all,
                                       real = drugs$consumption_cocaine_last_month)

probit_fitted = predict(drugs_probit_train_final, drugs_train)
probit_results = summary_binary_class(predicted_classes = probit_fitted,
                                     real = drugs_train$consumption_cocaine_last_month)
probit_fitted_test = predict(drugs_probit_train_final, drugs_test)
probit_results_test = summary_binary_class(predicted_classes = probit_fitted_test,
                                      real = drugs_test$consumption_cocaine_last_month)
probit_fitted_all = predict(drugs_probit_train_final, drugs)
probit_results_all = summary_binary_class(predicted_classes = probit_fitted_all,
                                       real = drugs$consumption_cocaine_last_month)

knn_fitted = predict(drugs_knn_train_final, drugs_train)
knn_results = summary_binary_class(predicted_classes = knn_fitted,
                                      real = drugs_train$consumption_cocaine_last_month)
knn_fitted_test = predict(drugs_knn_train_final, drugs_test)
knn_results_test = summary_binary_class(predicted_classes = knn_fitted_test,
                                   real = drugs_test$consumption_cocaine_last_month)
knn_fitted_all = predict(drugs_knn_train_final, drugs)
knn_results_all = summary_binary_class(predicted_classes = knn_fitted_all,
                                       real = drugs$consumption_cocaine_last_month)

svm_fitted = predict(drugs_svm_train_final, drugs_train)
svm_results = summary_binary_class(predicted_classes = svm_fitted,
                                   real = drugs_train$consumption_cocaine_last_month)
svm_fitted_test = predict(drugs_svm_train_final, drugs_test)
svm_results_test = summary_binary_class(predicted_classes = svm_fitted_test,
                                   real = drugs_test$consumption_cocaine_last_month)
svm_fitted_all = predict(drugs_svm_train_final, drugs)
svm_results_all = summary_binary_class(predicted_classes = svm_fitted_all,
                                        real = drugs$consumption_cocaine_last_month)

comparison <- matrix(12:7, nrow = 12, ncol = 7)
rownames(comparison) <- c("logit_train", "probit_train", "knn_train", "svm_train", "logit_test", "probit_test", "knn_test", "svm_test", "logit_all", "probit_all", "knn_all", "svm_all") 
colnames(comparison) <- c("Accuracy", "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "F1", "BALANCED ACCURACY")
comparison[1, ] <- logit_results
comparison[2, ] <- probit_results
comparison[3, ] <- knn_results
comparison[4, ] <- svm_results
comparison[5, ] <- logit_results_test
comparison[6, ] <- probit_results_test
comparison[7, ] <- knn_results_test
comparison[8, ] <- svm_results_test
comparison[9, ] <- logit_results_all
comparison[10, ] <- probit_results_all
comparison[11, ] <- knn_results_all
comparison[12, ] <- svm_results_all
comparison

# SVM
# Overall, we found out that SVM quality heavily depends on the parameters which are hard to specify (due to, among others, bigger computational power needed to estimate different combinations of parameters). SVM model validation on training and test samples is drastically different and much worse on the test sample. Therefore we do not consider this algorithm. 
# Logit and Probit
# Logit performance is almost identical to the Probit one. However, logit performs slightly better on the training sample in terms of balanced accuracy (which will be eveluated) so out of those two we would choose Logit. Therefore, the choice boils down to Logit VS KNN.
# Logit VS KNN
# The choice of Logit vs KNN is hard as in different cases one performs better than the other. For example, KNN performs worse than Logit on the training sample in terms of Balanced Accuracy, but better on the test sample. As test sample performance should be more reliable, we would like to go with KNN as our final model. 


# Prediction on test sample 

drugs_to_test <- read_csv("drugs_test.csv")
any(is.na(drugs_to_test))
glimpse(drugs_to_test)
drugs_to_test <- subset(drugs_to_test, select = -c(id) )

drugs_to_test$gender = factor(drugs_to_test$gender, levels = c("male", "female"), ordered = TRUE)

drugs_to_test$country[drugs_to_test$country %in% c("Canada","Ireland", "UK", "New Zealand")] <- "Other"
drugs_to_test$country <- as.factor(drugs_to_test$country)

drugs_to_test <- subset(drugs_to_test, select = -c(ethnicity) )

table(drugs_to_test$age)
drugs_to_test$age[drugs_to_test$age == "65+"] <- "55-64"
drugs_to_test$age[drugs_to_test$age == "55-64"] <- "55+"
drugs_to_test$age <- factor(drugs_to_test$age, 
                    levels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                    ordered = TRUE)
drugs_to_test$age <- droplevels(drugs_to_test$age)

drugs_to_test$education[drugs_to_test$education == "Left school before 16 years"] <- "Left school at/before 18 years"
drugs_to_test$education[drugs_to_test$education == "Left school at 16 years"] <- "Left school at/before 18 years"
drugs_to_test$education[drugs_to_test$education == "Left school at 17 years"] <- "Left school at/before 18 years"
drugs_to_test$education[drugs_to_test$education == "Left school at 18 years"] <- "Left school at/before 18 years"
drugs_to_test$education[drugs_to_test$education == "Masters degree"] <- "Masters degree or higher"
drugs_to_test$education[drugs_to_test$education == "Doctorate degree"] <- "Masters degree or higher"
drugs_to_test$education <- factor(drugs_to_test$education,
                          levels = c("Left school at/before 18 years",
                                     "Some college or university, no certificate or degree",
                                     "Professional certificate/ diploma",
                                     "University degree",
                                     "Masters degree or higher"),
                          ordered = TRUE)
drugs_to_test$education <- droplevels(drugs_to_test$education)
drugs_to_test$consumption_alcohol <- factor(drugs_to_test$consumption_alcohol,
                                    levels = c("never used",
                                               "used over a decade ago",
                                               "used in last decade",
                                               "used in last year",
                                               "used in last month",
                                               "used in last week", 
                                               "used in last day"),
                                    ordered = TRUE)
drugs_to_test$consumption_amphetamines <- factor(drugs_to_test$consumption_amphetamines,
                                         levels = c("never used",
                                                    "used over a decade ago",
                                                    "used in last decade",
                                                    "used in last year",
                                                    "used in last month",
                                                    "used in last week", 
                                                    "used in last day"),
                                         ordered = TRUE)
drugs_to_test$consumption_caffeine <- factor(drugs_to_test$consumption_caffeine,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)
drugs_to_test$consumption_cannabis <- factor(drugs_to_test$consumption_cannabis,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)
drugs_to_test$consumption_chocolate <- factor(drugs_to_test$consumption_chocolate,
                                      levels = c("never used",
                                                 "used over a decade ago",
                                                 "used in last decade",
                                                 "used in last year",
                                                 "used in last month",
                                                 "used in last week", 
                                                 "used in last day"),
                                      ordered = TRUE)
drugs_to_test$consumption_mushrooms <- factor(drugs_to_test$consumption_mushrooms,
                                      levels = c("never used",
                                                 "used over a decade ago",
                                                 "used in last decade",
                                                 "used in last year",
                                                 "used in last month",
                                                 "used in last week", 
                                                 "used in last day"),
                                      ordered = TRUE)
drugs_to_test$consumption_nicotine <- factor(drugs_to_test$consumption_nicotine,
                                     levels = c("never used",
                                                "used over a decade ago",
                                                "used in last decade",
                                                "used in last year",
                                                "used in last month",
                                                "used in last week", 
                                                "used in last day"),
                                     ordered = TRUE)

drugs_to_test$consumption_alcohol[drugs_to_test$consumption_alcohol == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_alcohol[drugs_to_test$consumption_alcohol == "used in last day"] <- "used in last week"
drugs_to_test$consumption_alcohol <- droplevels(drugs_to_test$consumption_alcohol)

drugs_to_test$consumption_amphetamines[drugs_to_test$consumption_amphetamines == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_amphetamines[drugs_to_test$consumption_amphetamines == "used in last day"] <- "used in last week"
drugs_to_test$consumption_amphetamines <- droplevels(drugs_to_test$consumption_amphetamines)

drugs_to_test$consumption_caffeine[drugs_to_test$consumption_caffeine == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_caffeine[drugs_to_test$consumption_caffeine == "used in last day"] <- "used in last week"
drugs_to_test$consumption_caffeine <- droplevels(drugs_to_test$consumption_caffeine)

drugs_to_test$consumption_cannabis[drugs_to_test$consumption_cannabis == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_cannabis[drugs_to_test$consumption_cannabis == "used in last day"] <- "used in last week"
drugs_to_test$consumption_cannabis <- droplevels(drugs_to_test$consumption_cannabis)

drugs_to_test$consumption_chocolate[drugs_to_test$consumption_chocolate == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_chocolate[drugs_to_test$consumption_chocolate == "used in last day"] <- "used in last week"
drugs_to_test$consumption_chocolate <- droplevels(drugs_to_test$consumption_chocolate)

drugs_to_test$consumption_mushrooms[drugs_to_test$consumption_mushrooms == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_mushrooms[drugs_to_test$consumption_mushrooms == "used in last day"] <- "used in last week"
drugs_to_test$consumption_mushrooms <- droplevels(drugs_to_test$consumption_mushrooms)

drugs_to_test$consumption_nicotine[drugs_to_test$consumption_nicotine == "never used"] <- "used over a decade ago"
drugs_to_test$consumption_nicotine[drugs_to_test$consumption_nicotine == "used in last day"] <- "used in last week"
drugs_to_test$consumption_nicotine <- droplevels(drugs_to_test$consumption_nicotine)

drugs_to_test <- subset(drugs_to_test, select = -c(consumption_caffeine))

drugs_to_test$personality_neuroticism <- ifelse(drugs_to_test$personality_neuroticism < 35, 0, ifelse(drugs_to_test$personality_neuroticism < 65, 1, 2))
drugs_to_test$personality_openness <- ifelse(drugs_to_test$personality_openness < 35, 0, ifelse(drugs_to_test$personality_openness < 65, 1, 2))
drugs_to_test$personality_agreeableness <- ifelse(drugs_to_test$personality_agreeableness < 35, 0, ifelse(drugs_to_test$personality_agreeableness < 65, 1, 2))
drugs_to_test$personality_extraversion <- ifelse(drugs_to_test$personality_extraversion < 35, 0, ifelse(drugs_to_test$personality_extraversion < 65, 1, 2))
drugs_to_test$personality_conscientiousness <- ifelse(drugs_to_test$personality_conscientiousness < 35, 0, ifelse(drugs_to_test$personality_conscientiousness < 65, 1, 2))
drugs_to_test$personality_impulsiveness <- ifelse(drugs_to_test$personality_impulsiveness < 35, 0, ifelse(drugs_to_test$personality_impulsiveness < 65, 1, 2))
drugs_to_test$personality_sensation <- ifelse(drugs_to_test$personality_sensation < 35, 0, ifelse(drugs_to_test$personality_sensation < 65, 1, 2))
drugs_to_test[, 5:11] <- lapply(drugs_to_test[, 5:11], factor)
glimpse(drugs_to_test)

knn_prediction_on_test_data = predict(drugs_knn_train_final, drugs_to_test)
write.csv(knn_prediction_on_test_data,"knn_test_predictions.csv", row.names = FALSE)

