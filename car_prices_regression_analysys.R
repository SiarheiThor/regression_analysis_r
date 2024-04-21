
library(ggplot2)
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)

# original data exploration

data <- read.csv('./data_preprocessed/cleaned_data_elbilar.csv')
summary(data)
names(data)


plot(Modellår~Miltal, data=data)
plot(Pris~I(Modellår*Hästkrafter), data=data)
plot(Pris~I(Modellår), data=data)
plot(Pris~I(Hästkrafter), data=data)
plot(Pris~I(Miltal), data=data)


base_model_el = lm(Pris ~., data = data)
summary(fit_base_model)


par(mfrow=c(2,2))
plot(fit_base_model)
vif(fit_base_model)

# model_el = lm(Pris ~+Miltal+Modellår+Miltal:Modellår+Hästkrafter:Modellår+Hästkrafter, data = data)
# summary(model_el)
# 
# vif(model_el)


# Clenaing the data based on initial finding and retraining the model

clean_data <- subset(data, select = -c(Datum.i.trafik, Märke, Modell))

#Fit the model using the cleaned dataset
model_el <- lm(Pris ~ ., data = clean_data)
summary(model_el)

par(mfrow=c(2,2))
plot(model_el)
vif(model_el)

### Feature engineering
#Fit the model using the cleaned dataset
int_model_el <- lm(Pris ~ .-Färg-Län , data = clean_data) #+Miltal:Modellår+Hästkrafter:Modellår+Hästkrafter
summary(int_model_el)

par(mfrow=c(2,2))
plot(int_model_el)
vif(int_model_el)

data[159, ]
data[193, ]
data[75, ]
data[49, ]
data[81, ]

# no_outliers_data <- clean_data[-c(81,198), ]
# 
# int_model_el_outl <- lm(Pris ~ .-Färg-Län +Miltal:Modellår+Hästkrafter:Modellår+Hästkrafter, data = no_outliers_data)
# summary(int_model_el_outl)
# 
# par(mfrow=c(2,2))
# plot(int_model_el_outl)
# vif(int_model_el_outl)

### Alternative dataset
data_l <- read.csv('./data_preprocessed/skrapning_v3_cleaned.csv')
summary(data_l)
names(data_l)
data_l_filtered <- subset(data_l, Price > 20000)
data_l_clean <- na.omit(data_l_filtered)
summary(data_l_clean)


hist(data_l_clean$Price)
hist(data_l_clean$Horsepower)
hist(data_l_clean$Year)
hist(data_l_clean$Miles)

frequency_table <- table(data_l_clean$Location)
print(frequency_table)

model_l = lm(Price ~.-Brand-Model-Engine.Volume-Name-Company, data = data_l_clean) #+Horsepower:Year+Miles:Year
summary(model_l)
par(mfrow=c(2,2))
plot(model_l)
vif(model_l)

### adjusting model
model_l_int = lm(Price ~.-Brand-Model-Engine.Volume-Name-Company+Horsepower:Year+Miles:Year, data = data_l_clean) #
summary(model_l_int)
par(mfrow=c(2,2))
plot(model_l_int)
vif(model_l_int)


model_l_log = lm(I(log(Price)) ~.-X-Brand-Model-Engine.Volume-Name-Company+Miles:Year, data = data_l_clean) #
summary(model_l_log)

plot(model_l_log)
vif(model_l_log)

data_l[10736,]
par(mfrow=c(1,2))
hist(data_l_clean$Price)
hist(log(data_l_clean$Price))

### Adding variables from external dataset

external_data <- read.csv("medeinkomst.csv")
external_data
medel_inkomst_lan <- external_data[grepl("län", external_data$region), ]
medel_inkomst_lan

unique(data_l_clean$Location)


medeinkomst <- list(
  Stockholm = 415.4,
  Dalarna = 313.3,
  Västmanland = 325.1,
  Skåne = 333.2,
  Örebro = 312.1,
  Kronoberg = 325.9,
  Västernorrland = 314.0,
  Göteborg = 337.6,
  Kalmar = 309.8,
  Södermanland = 318.5,
  Halland = 363.8,
  Blekinge = 313.1,
  Älvsborg = 337.6,
  Östergötland = 320.0,
  Uppsala = 326.1,
  Västerbotten = 317.4,
  Jämtland = 325.9,
  Skaraborg = 337.6,
  Jönköping = 325.0,
  Värmland = 316.6,
  Norrbotten = 328.2,
  Gävleborg = 312.2
)


head(data_l_clean)
library(dplyr)
# Converting the list to a named vector for easier mapping
medeinkomst_vector <- unlist(medeinkomst)

# Adding the Mean_income column
data_l_clean_1 <- data_l_clean %>%
  mutate(Mean_income = medeinkomst_vector[Location])

# Print the updated dataframe to check
print(data_l_clean_1)
summary(data_l_clean_1)

# with numerical variable instead of categorical for Location

model_l_log_ext = lm(I(log(Price)) ~.-X-Brand-Model-Engine.Volume-Name-Company-Location+Miles:Year, data = data_l_clean_1) #
summary(model_l_log_ext)
par(mfrow=c(2,2))
plot(model_l_log_ext)
vif(model_l_log_ext)






### Ridge
# Load necessary library
library(glmnet)

# Prepare data
data_modified <- data_l_clean[, !(names(data_l_clean) %in% c("Brand", "Model", "Engine.Volume", "Name", "Company"))]
data_modified$Horsepower_Year <- data_l_clean$Horsepower * data_l_clean$Year
data_modified$Miles_Year <- data_l_clean$Miles * data_l_clean$Year
X <- model.matrix(~ . - Price, data = data_modified)
Y <- data_l_clean$Price

# Apply Ridge Regression with cross-validation
set.seed(123)
cv_model <- cv.glmnet(X, Y, alpha = 0, type.measure = "mse")

# Best lambda value
best_lambda <- cv_model$lambda.min
print(best_lambda)

# Predictions
predictions <- predict(cv_model, s = best_lambda, newx = X)

######## Comparing the two models

# Split the data into training and testing sets
set.seed(123)  # Ensure reproducibility
sample_size <- floor(0.8 * nrow(data_l_clean))
train_indices <- sample(seq_len(nrow(data_l_clean)), size = sample_size)

train_data <- data_l_clean[train_indices, ]
test_data <- data_l_clean[-train_indices, ]

# Removing unnecessary columns from both train_data and test_data
unnecessary_columns <- c("Brand", "Model", "Engine.Volume", "Name", "Company")
train_data <- train_data[, !(names(train_data) %in% unnecessary_columns)]
test_data <- test_data[, !(names(test_data) %in% unnecessary_columns)]

# Adding interaction terms
train_data$Horsepower_Year <- train_data$Horsepower * train_data$Year
train_data$Miles_Year <- train_data$Miles * train_data$Year
test_data$Horsepower_Year <- test_data$Horsepower * test_data$Year
test_data$Miles_Year <- test_data$Miles * test_data$Year

# Fit the Linear Model on Training Data
model_l <- lm(Price ~ . + Horsepower:Year + Miles:Year, data = train_data)

# Prepare matrix for Ridge Model (training)
X_train <- model.matrix(~ . -Price, data = train_data)
Y_train <- train_data$Price

# Apply Ridge Regression with cross-validation
cv_model <- cv.glmnet(X_train, Y_train, alpha = 0, type.measure = "mse")

# Predictions for the Linear Model
predictions_l <- predict(model_l, newdata = test_data)

# Prepare matrix for Ridge Model (testing)
X_test <- model.matrix(~ . -Price, data = test_data)

# Predictions for the Ridge Model
predictions_ridge <- predict(cv_model, s = cv_model$lambda.min, newx = X_test)

# Calculate and Compare RMSE
rmse_l <- sqrt(mean((test_data$Price - predictions_l)^2))
rmse_ridge <- sqrt(mean((test_data$Price - predictions_ridge)^2))

# Output RMSE values for comparison
cat("RMSE for Linear Model:", rmse_l, "\n")
cat("RMSE for Ridge Model:", rmse_ridge, "\n")















set.seed(123) # For reproducibility
sample_size <- floor(0.8 * nrow(data_l_clean))
train_indices <- sample(seq_len(nrow(data_l_clean)), size = sample_size)

train_data <- data_l_clean[train_indices, ]
test_data <- data_l_clean[-train_indices, ]

model_l <- lm(Price ~ . -Brand -Model -Engine.Volume -Name -Company + Horsepower:Year + Miles:Year, data = train_data)



data_modified <- train_data[, !(names(train_data) %in% c("Brand", "Model", "Engine.Volume", "Name", "Company"))]
data_modified$Horsepower_Year <- train_data$Horsepower * train_data$Year
data_modified$Miles_Year <- train_data$Miles * train_data$Year
X_train <- model.matrix(~ . - Price, data = data_modified)
Y_train <- train_data$Price

# Apply Ridge Regression with cross-validation
set.seed(123)
cv_model <- cv.glmnet(X_train, Y_train, alpha = 0, type.measure = "mse")


predictions_l <- predict(model_l, newdata = test_data)


data_modified_test <- test_data[, !(names(test_data) %in% c("Brand", "Model", "Engine.Volume", "Name", "Company"))]
data_modified_test$Horsepower_Year <- test_data$Horsepower * test_data$Year
data_modified_test$Miles_Year <- test_data$Miles * test_data$Year
X_test <- model.matrix(~ . - Price, data = data_modified_test)

predictions_ridge <- predict(cv_model, s = cv_model$lambda.min, newx = X_test)


rmse_l <- sqrt(mean((test_data$Price - predictions_l)^2))
rmse_ridge <- sqrt(mean((test_data$Price - predictions_ridge)^2))

# Print RMSE values for comparison
cat("RMSE for Linear Model:", rmse_l, "\n")
cat("RMSE for Ridge Model:", rmse_ridge, "\n")


#########
x <- model.matrix(Salary ~ ., Hitters)[, -1]  
y <- Hitters$Salary

### (1) Ridge Regression

library(glmnet)
grid <- 10^seq(10, -2, length = 100)
# Ridge: alpha=0, Lasso: alpha=1
ridge_reg <- glmnet(x, y, alpha = 0, lambda = grid)

# For each value of lambda (100 in total), there is a vector of
# ridge regression coefficients.
dim(coef(ridge_reg))

# Demonstrating that higher lambda yields smaller coefficients
ridge_reg$lambda[50]
coef(ridge_reg)[, 50]
sqrt(sum(coef(ridge_reg)[-1, 50]^2))

ridge_reg$lambda[60]
coef(ridge_reg)[, 60]
sqrt(sum(coef(ridge_reg)[-1, 60]^2))

### (2) Creating train and test data to evaluate model
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y_test <- y[test]

# Below we first choose lambda (s) arbitrarily, then 
# we will demonstrate cross-validation.

# Choosing lambda (s) = 4 arbitrarily
ridge_reg <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge_pred <- predict(ridge_reg, s = 4, newx = x[test, ]) 
mean((ridge_pred - y_test)^2)

# Benchmarking our ridge model with a "naive estimate" of using the mean
mean((mean(y[train]) - y_test)^2)

ridge_pred <- predict(ridge_reg, s = 1e10, newx = x[test, ])
mean((ridge_pred - y_test)^2)

ridge_pred <- predict(ridge_reg, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge_pred - y_test)^2)

# Lambda = 0 is simply least squares regression
lm(y ~ x, subset = train)
# Examining coefficients with the predict function
predict(ridge_reg, s = 0, exact = T, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]

# Use CV instead of arbitrarily choosing lambda
set.seed(1)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10)
plot(cv_out)
bestlam <- cv_out$lambda.min
bestlam

ridge_pred <- predict(ridge_reg, s = bestlam, newx = x[test, ])
mean((ridge_pred - y_test)^2)

# Refitting our ridge regression on the full data set
# and examine the coefficient estimates.
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]



### Plot the data, to check different assumptions. 

#Predict the values using the fitted model
predicted_values <- predict(base_model, clean_data)

# Step 4: Create a new dataframe for plotting
plot_data <- data.frame(True_Pris = clean_data$Pris, Predicted_Pris = predicted_values, Län = clean_data$Län)

# Step 5: Plot the true values vs. the predicted values
library(ggplot2)
ggplot(plot_data, aes(x = True_Pris, y = Predicted_Pris, color = Län)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "True Prices", y = "Predicted Prices", title = "True vs. Predicted Prices by Län") +
  theme_minimal()








