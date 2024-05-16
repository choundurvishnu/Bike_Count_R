library(car)
library(pls)
library(caret)
library(randomForest)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(leaps)
library(glmnet)
library(gbm)
library(keras)
library(tidyverse)


data <- read.csv("C:/Users/choun/OneDrive/Desktop/ASL_II/hour.csv")
View(data)
dim(data)   # 17379,17
str(data)

summary(data)

#####
### Checking relationship between the dteday and yr
data$Year <- format(as.Date(data$dteday),"%Y")
data$Category <- ifelse(data$Year == "2011", 0, 1)
identical_columns <- identical(data$yr, as.integer(data$Category))
print(identical_columns)

### Checking relationship between the dteday and mnt 
data$month <- format(as.Date(data$dteday),"%m")
identical_columns <- identical(data$mnth, as.integer(data$month))
print(identical_columns)

### Checking relationship between the casual, registered and cnt
data$cnt1 <- data$casual+data$registered
identical_columns <- identical(data$cnt, data$casual+data$registered)
print(identical_columns)

### So we can remove the dteday and other columns we have added for identification
columns_to_remove <- c("dteday", "instant","month","Category","Year","casual","registered","cnt1")
data <- data[, !names(data) %in% columns_to_remove]


######
## Duplicate Data
# Find and display duplicate rows based on all columns
duplicate_rows <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]

# Display the duplicate rows
print("Duplicate Rows in the Dataset:")
print(duplicate_rows)

## Remove duplicate Data
data <- unique(data)
##Missing or Null Values
# Check for missing values using is.na() function
missing_values <- is.na(data)

# Count missing values in each column
missing_counts <- colSums(missing_values)

# Display the missing values in the dataset
print("Missing Values in the Dataset:")
print(missing_values)

# Display the count of missing values in each column
print("Missing Value Counts by Column:")
print(missing_counts)

####Summary of Data
summary(data)

### Unique values
# Assuming you have a data frame named "hour_df"
for (col_name in names(data)) {
  if (length(unique(data[[col_name]])) < 25) {
    cat(paste("Unique ", col_name,"'s count: ", length(unique(data[[col_name]])), "\n"))
    cat(paste("Unique values: ", paste(unique(data[[col_name]]), collapse = ", "), "\n\n"))
  }
}

cat_columns <- c("season","yr","mnth","hr","holiday","weekday","workingday","weathersit")
num_columns <- c("temp","atemp","hum","windspeed","cnt")

## Converting the categorical variable into factors
## For hours
data[cat_columns] <- lapply(data[cat_columns], as.factor)
str(data)


# Plot all bar plots in a 2x4 layout

gg_list <- lapply(colnames(data), function(var) {
  ggplot(data, aes(x = data[[var]])) +
    geom_bar(fill = "skyblue") +
    labs(title = paste("histogram for", var),
         x = var)
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

# Create a list of ggplot objects for each column
# Create a list of ggplot objects for each column
gg_list <- lapply(colnames(data), function(var) {
  if (is.numeric(data[[var]])) {
    ggplot(data, aes(x = data[[var]])) +
      geom_histogram(fill = "skyblue", color = "black", bins = 20) +
      labs(title = paste("Histogram for", var),
           x = var, y = "Frequency")
  } else {
    ggplot(data, aes(x = factor(data[[var]]))) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = paste("histogram plot for", var),
           x = var, y = "Frequency")
  }
})


# Combine the plots into a grid layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

# Plot all bar graphs in a 2x4 layout
gg_list <- lapply(cat_columns, function(var) {
  ggplot(data, aes(x = data[[var]], y = cnt)) +
    geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
    labs(title = paste("Bar Plot for", var),
         x = var, y = "cnt")
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

# Plot all box plots in a 2x4 layout
gg_list <- lapply(cat_columns, function(var) {
  ggplot(data, aes(x = data[[var]], y = cnt)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = paste("Box Plot for", var),
         x = var, y = "cnt")
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

#### Scatter Plots
# List of scatter plot combinations
scatter_combinations <- list(c("atemp", "cnt"), c("temp", "cnt"), c("hum", "cnt"), c("windspeed", "cnt"))

# Plot all scatter plots in a 2x2 layout
gg_list <- lapply(scatter_combinations, function(vars) {
  ggplot(data, aes_string(x = vars[1], y = vars[2])) +
    geom_point(color = "skyblue") +
    labs(title = paste("Scatter Plot for", vars[1], "vs", vars[2]),
         x = vars[1], y = vars[2])
})

# Combine the plots into a 2x2 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 2)

#-------------------------------------------------------------------------------------------------


cleaned_data <- data

# Visualize the cleaned data
ggplot(cleaned_data, aes(x = seq_along(cnt), y = cnt)) +
  geom_point(aes(color = 'Data')) +
  labs(title = 'Bike Sharing Count', x = 'Index', y = 'Count') +
  scale_color_manual(values = c('Data' = 'blue'))

##################
ggplot(cleaned_data, aes(y = cnt)) +
  geom_boxplot() +
  labs(title = 'Boxplot of Bike Sharing Count', y = 'Count')

ggplot(cleaned_data, aes(y = hum)) +
  geom_boxplot() +
  labs(title = 'Boxplot of humidity', y = 'Count')

ggplot(cleaned_data, aes(y = windspeed)) +
  geom_boxplot() +
  labs(title = 'Boxplot of Wind Speed', y = 'Count')

#####--------------------------------------------------------------------------------------------------------------

# Plot all bar graphs in a 2x4 layout
gg_list <- lapply(cat_columns, function(var) {
  ggplot(cleaned_data, aes(x = cleaned_data[[var]], y = cnt)) +
    geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
    labs(title = paste("Bar Plot for", var),
         x = var, y = "cnt")
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

#### Box Plot

# Plot all box plots in a 2x4 layout
gg_list <- lapply(cat_columns, function(var) {
  ggplot(data, aes(x = data[[var]], y = cnt)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = paste("Box Plot for", var),
         x = var, y = "cnt")
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)

#######Histogram
### Box Plot












#### Scatter Plots
# List of scatter plot combinations
scatter_combinations <- list(c("atemp", "cnt"), c("temp", "cnt"), c("hum", "cnt"), c("windspeed", "cnt"))

# Plot all scatter plots in a 2x2 layout
gg_list <- lapply(scatter_combinations, function(vars) {
  ggplot(data, aes_string(x = vars[1], y = vars[2])) +
    geom_point(color = "skyblue") +
    labs(title = paste("Scatter Plot for", vars[1], "vs", vars[2]),
         x = vars[1], y = vars[2])
})

# Combine the plots into a 2x2 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 2)





# Plot all bar plots in a 2x4 layout
gg_list <- lapply(cat_columns, function(var) {
  ggplot(data, aes(x = data[[var]])) +
    geom_bar(fill = "skyblue") +
    labs(title = paste("Box Plot for", var),
         x = var)
})

# Combine the plots into a 2x4 layout
gridExtra::grid.arrange(grobs = gg_list, ncol = 4)


###---------------------------------------------------------------------------------------------------------------
## Chi Square test
# Load necessary libraries


chi_square_heatmap <- function(data) {
  # Identify categorical variables
  cat_vars <- sapply(data, is.factor)
  
  # Filter data to include only categorical variables
  cat_data <- data[, cat_vars, drop = FALSE]
  
  # Create a matrix to store p-values
  p_values <- matrix(NA, nrow = ncol(cat_data), ncol = ncol(cat_data), dimnames = list(names(cat_data), names(cat_data)))
  
  # Perform Chi-square test for all pairs of categorical variables
  for (i in 1:(ncol(cat_data) - 1)) {
    for (j in (i + 1):ncol(cat_data)) {
      contingency_table <- table(cat_data[[i]], cat_data[[j]])
      chi_square_result <- chisq.test(contingency_table)
      p_values[i, j] <- chi_square_result$p.value
      p_values[j, i] <- chi_square_result$p.value
    }
  }
  
  # Create a melted data frame for ggplot
  p_values_melted <- reshape2::melt(p_values)
  
  # Create a heatmap of p-values with annotations
  ggplot(p_values_melted, aes(Var1, Var2, fill = value, label = round(value, 4))) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(!is.na(value), sprintf("%.4f", value), "")), vjust = 1) +
    scale_fill_gradient(low = "red", high = "green") +
    labs(title = "Chi-square Test Heatmap",
         x = "Categorical Variables",
         y = "Categorical Variables",
         fill = "P-value") +
    theme_minimal()
}

heatmap_plot <- chi_square_heatmap(cleaned_data)


# Display the heatmap
print(heatmap_plot)
##############################################################################################
# Create a function to perform chi-square tests and extract p-values
chi_square_test <- function(x, y) {
  contingency_table <- table(data[, x], data[, y])
  chi_square_result <- chisq.test(contingency_table)
  return(chi_square_result$p.value)
}

# Compute p-values for all pairs of categorical variables
variable_names <- cat_columns
p_values <- matrix(NA, nrow = length(variable_names), ncol = length(variable_names))
for (i in 1:length(variable_names)) {
  for (j in 1:length(variable_names)) {
    if (i != j) {
      p_values[i, j] <- chi_square_test(i, j)
    }
  }
}

# Create a heatmap of p-values
colnames(p_values) <- rownames(p_values) <- variable_names
melted_p_values <- melt(p_values)

# Plot heatmap
ggplot(melted_p_values, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.4f", value)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Chi-Square Test P-Values Heatmap", x = "Variable", y = "Variable") +
  theme_minimal()

###########################################################################################
############################################################################################
# Calculate the correlation matrix

#cor_matrix <- cor(data[,c("temp","atemp","hum","windspeed","casual","registered","cnt")])
cor_matrix <- cor(cleaned_data[,c("temp","atemp","hum","windspeed","cnt")])


# Display the correlation matrix
print("Correlation Matrix:")
print(cor_matrix)


ggcorrplot(cor_matrix, hc.order = TRUE, lab = TRUE)

#############################################################################################################


#vif(model.matrix(cnt ~., data = cleaned_data))
lm_model <- lm(cnt ~., data = cleaned_data)
aliases <- alias(lm_model)
print(aliases)
cleaned_data <- data.frame(cleaned_data[-7])
vif(lm(cnt ~., data = cleaned_data))

######################################################################################################
#df0 <- cleaned_data
#df0 <- lapply(df0[,!(names(df0) %in% num_columns)], as.numeric)
#df0 <- cbind(df0,cleaned_data[,num_columns])
#df000 <- df0[,-ncol(df0)]
#scale_data <- scale(df000, center = apply(df000, 2, mean), scale = apply(df000, 2, sd))
#cnt <- cleaned_data$cnt
#scale_data <- data.frame(scale_data,cnt)
#dff <- scale_data
#############################################################################################################
df0 <- cleaned_data
df0 <- model.matrix( ~., data = cleaned_data)
df0 <- data.frame(df0[,-1])
lm_model00 <- lm(cnt ~., data = df0)
aliases <- alias(lm_model00)
print(aliases)
vif(lm(cnt ~., data = df0))
df000 <- df0[,-ncol(df0)]
scale_data <- scale(df000, center = apply(df000, 2, mean), scale = apply(df000, 2, sd))
cnt <- cleaned_data$cnt
scale_data <- data.frame(scale_data,cnt)
dff <- scale_data


#######################################################################################################################

###### Linear Regression
set.seed(123)


trainIndex <- createDataPartition(dff$cnt, p = 0.8, list = FALSE)
ctrl <- trainControl(method = "cv", number = 5)
lm.fit <- train(cnt ~., data = dff[trainIndex, ], method = "lm",  trControl = ctrl)

#lm.fit <- lm(cnt~.,train_data)
lm.summary <- summary(lm.fit)
predictions = predict(lm.fit,dff[-trainIndex, ])
plot(dff[-trainIndex, ]$cnt, predictions, pch = 16, col = "red", main = "Actual vs Predicted for Linear Regression Model", xlab = "Actual", ylab = "Predicted")

abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

actual_values <- dff[-trainIndex, ]$cnt  
r_squared_lm <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2)
mse_lm <- mean((actual_values - predictions)^2)
rmse_lm <- sqrt(mean((actual_values - predictions)^2))
mae_lm <- mean(abs(actual_values-predictions))
cat("Mean Average Error (MAE): ", mae_lm, "\n")

# Display the results
cat("R-squared: ", r_squared_lm, "\n")
cat("Mean Square Error (MsE): ", mse_lm, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_lm, "\n")
cat("Mean Average Error (MAE): ", mae_lm, "\n")

####################################################################################################################################

qplot(atemp, cnt, data = cleaned_data, colour = hr) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  stat_smooth(method = "lm") 
#weathersit*hum
qplot(windspeed, cnt, data = cleaned_data, colour = weathersit) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  stat_smooth(method = "lm")
qplot(atemp, cnt, data = cleaned_data, colour = holiday) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  stat_smooth(method = "lm") 
qplot(windspeed, cnt, data = cleaned_data, colour = holiday) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  stat_smooth(method = "lm") 
qplot(windspeed, cnt, data = cleaned_data, colour =yr ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  stat_smooth(method = "lm") 
library(gridExtra)
#grid.arrange(p2)
##### Interaction terms
# Create interaction terms for all variables
interaction_terms <- model.matrix(~.^2, data = cleaned_data[,-12])

# Fit a linear regression model with interaction terms
model <- lm(cnt ~ . -1, data = data.frame(interaction_terms))


alias_terms <- alias(model)$Complete
alias_vars <- rownames(alias_terms)
nnn <- names(data.frame(interaction_terms))
result_col <- setdiff(nnn, alias_vars)
#result_df <- data.frame(interaction_terms[, result])

#data_filtered <- interaction_terms %>%
#  drop(alias_vars)


#print(interaction_terms1)
# Print coefficients and their significance
summary_table <- summary(model)
significant_terms <- summary_table$coefficients
#significant_terms <- summary_table$coefficients[summary_table$coefficients[, "Pr(>|t|)"] < 0.05, ]
# Print significant terms
print(significant_terms)

view(significant_terms)



ctrl <- trainControl(method = "cv", number = 5)
lm.fit_it <- train(cnt ~.+(holiday*atemp)+(temp*atemp)+(season*yr)+(yr*atemp)+(hr*holiday)+(season*hr)+
                     (yr*hr)+(mnth*hr)+(yr*hr)+(mnth*hr)+(yr*hr)+(mnth*hum)+(yr*mnth)+(hr*weekday), 
                   data = cleaned_data[trainIndex, ],method="lm",trControl = ctrl)

lm.summary_it <- summary(lm.fit_it)


predictions = predict(lm.fit_it,cleaned_data[-trainIndex, ])
plot(cleaned_data[-trainIndex, ]$cnt, predictions, pch = 16, col = "red", main = "Actual vs Predicted for Linear Regression with Interaction Terms", xlab = "Actual", ylab = "Predicted")

abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

actual_values <- cleaned_data[-trainIndex, ]$cnt  
r_squared_it <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2)
mse_it <- mean((actual_values - predictions)^2)
rmse_it <- sqrt(mean((actual_values - predictions)^2))
mae_it <- mean(abs(actual_values-predictions))
cat("Mean Average Error (MAE): ", mae_it, "\n")
# Display the results
cat("R-squared: ", r_squared_it, "\n")
cat("Mean Square Error (MsE): ", mse_it, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_it, "\n")








###########################################################################################################################################
### Polynomial degree 2
predictors <- num_columns[-c(5)]
#rc <- c(3,9)
poly_de2 <- data.frame(cleaned_data)
for(predictor in predictors) {
  poly_de2[paste(predictor, "de-2", sep = "-")] <- cleaned_data[predictor]^2
}
poly_de2 <- data.frame(poly_de2) 
df02 <- model.matrix( ~., data = poly_de2)
df02 <- data.frame(df02[,-1])
lm_model00 <- lm(cnt ~., data = data.frame(df02))
aliases <- alias(lm_model00)
print(aliases)
vif(lm(cnt ~., data = data.frame(df02)))
df000 <- df02[,-53]
scale_data <- scale(df000, center = apply(df000, 2, mean), scale = apply(df000, 2, sd))
cnt <- cleaned_data$cnt
#cnt <- transformed_data
scale_data <- data.frame(scale_data,cnt)
poly_de2 <- scale_data

lm.fit_2 <- train(cnt ~ ., data = poly_de2[trainIndex, ], method = "lm",  trControl = ctrl)
lm.summary_2 <- summary(lm.fit_2)
predictions = predict(lm.fit_2,poly_de2[-trainIndex,])
plot(poly_de2[-trainIndex,]$cnt, predictions, pch = 16, col = "red", main = "Actual vs Predicted for Linear Regression with degree 2", xlab = "Actual", ylab = "Predicted")

abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

actual_values <- poly_de2[-trainIndex,]$cnt  
r_squared_lm2 <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2)
mse_lm2 <- mean((actual_values - predictions)^2)
rmse_lm2 <- sqrt(mean((actual_values - predictions)^2))
mae_lm2 <- mean(abs(actual_values-predictions))

# Display the results
cat("R-squared: ", r_squared_lm2, "\n")
cat("Mean Square Error (MSE): ", mse_lm2, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_lm2, "\n")

cat("Mean Average Error (MAE): ", mae_lm2, "\n")

####################################################################################################################################
### Polynomial degree 3
predictors <- num_columns[-c(5)]
#rc <- c(3,9)
poly_de3 <- data.frame(cleaned_data)
for(predictor in predictors) {
  #poly_de2[paste(predictor, "de-2", sep = "-")] <- cleaned_data[predictor]^2
  poly_de3[[paste(predictor, "de-2", sep = "-")]] <- cleaned_data[[predictor]]^2
  poly_de3[[paste(predictor, "de-3", sep = "-")]] <- cleaned_data[[predictor]]^3
}
poly_de3 <- data.frame(poly_de3) 
df03 <- model.matrix( ~., data = poly_de3)
df03 <- data.frame(df03[,-1])
lm_model00 <- lm(cnt ~., data = data.frame(df03))
aliases <- alias(lm_model00)
print(aliases)
vif(lm(cnt ~., data = data.frame(df03)))
df000 <- df03[,-53]
scale_data <- scale(df000, center = apply(df000, 2, mean), scale = apply(df000, 2, sd))
cnt <- cleaned_data$cnt
#cnt <- transformed_data
scale_data <- data.frame(scale_data,cnt)
poly_de3 <- scale_data

lm.fit_3 <- train(cnt ~ ., data = poly_de3[trainIndex, ], method = "lm",  trControl = ctrl)
lm.summary_3 <- summary(lm.fit_3)
predictions = predict(lm.fit_3,poly_de3[-trainIndex,])
plot(poly_de3[-trainIndex,]$cnt, predictions, pch = 16, col = "red", main = "Actual vs Predicted for Linear Regression with degree 3", xlab = "Actual", ylab = "Predicted")

abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

actual_values <- poly_de3[-trainIndex,]$cnt  
r_squared_lm3 <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2)
mse_lm3 <- mean((actual_values - predictions)^2)
rmse_lm3 <- sqrt(mean((actual_values - predictions)^2))
mae_lm3 <- mean(abs(actual_values-predictions))

# Display the results
cat("R-squared: ", r_squared_lm3, "\n")
cat("Mean Square Error (MsE): ", mse_lm3, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_lm3, "\n")
cat("Mean Average Error (MAE): ", mae_lm3, "\n")

####################################################################################################################################
### Polynomial degree 4
predictors <- num_columns[-c(5)]
#rc <- c(3,9)
poly_de4 <- data.frame(cleaned_data)
for(predictor in predictors) {
  #poly_de2[paste(predictor, "de-2", sep = "-")] <- cleaned_data[predictor]^2
  poly_de4[[paste(predictor, "de-2", sep = "-")]] <- cleaned_data[[predictor]]^2
  poly_de4[[paste(predictor, "de-3", sep = "-")]] <- cleaned_data[[predictor]]^3
  poly_de4[[paste(predictor, "de-4", sep = "-")]] <- cleaned_data[[predictor]]^4
}
poly_de4 <- data.frame(poly_de4) 
df04 <- model.matrix( ~., data = poly_de4)
df04 <- data.frame(df04[,-1])
lm_model00 <- lm(cnt ~., data = data.frame(df04))
aliases <- alias(lm_model00)
print(aliases)
vif(lm(cnt ~., data = data.frame(df04)))
df000 <- df04[,-53]
scale_data <- scale(df000, center = apply(df000, 2, mean), scale = apply(df000, 2, sd))
cnt <- cleaned_data$cnt
#cnt <- transformed_data
scale_data <- data.frame(scale_data,cnt)
poly_de4 <- scale_data
lm.fit_4 <- train(cnt ~ ., data = poly_de4[trainIndex, ], method = "lm",  trControl = ctrl)
#lm.fit_4 <- train(cnt ~ .-atemp.de.4 -atemp.de.3-atemp.de.2-windspeed.de.4-windspeed.de.3-windspeed.de.2 , data = poly_de4[trainIndex, ], method = "lm",  trControl = ctrl)
lm.summary_4 <- summary(lm.fit_4)
predictions = predict(lm.fit_4,poly_de4[-trainIndex,])
plot(poly_de4[-trainIndex,]$cnt, predictions, pch = 16, col = "red", main = "Actual vs Predicted for Linear Regression with degree 4", xlab = "Actual", ylab = "Predicted")

abline(a = 0, b = 1, col = "black",lty=1,lwd=4)

actual_values <- poly_de4[-trainIndex,]$cnt  
r_squared_lm4 <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2)
mse_lm4 <- mean((actual_values - predictions)^2)
rmse_lm4 <- sqrt(mean((actual_values - predictions)^2))
mae_lm4 <- mean(abs(actual_values-predictions))
cat("Mean Average Error (MAE): ", mae_lm4, "\n")
# Display the results
cat("R-squared: ", r_squared_lm4, "\n")
cat("Mean Square Error (MsE): ", mse_lm4, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_lm4, "\n")
cat("Mean Average Error (MAE): ", mae_lm4, "\n")

##################################################################################################################################################

########## Forward
regfit.fwd=regsubsets(cnt ~ ., data=dff[trainIndex,],nvmax=ncol(dff)-1,method="forward")
reg.summary.fwd <- summary(regfit.fwd)

cv.errors.fwd <- rep(NA,51)
mae.fwd <- rep(NA,51)
cv.r2.fwd <- rep(NA,51)
test.mat=model.matrix(cnt~.,data=dff[-trainIndex,])

best.fit.fwd <- regsubsets(cnt ~., data = dff[trainIndex,], nvmax = ncol(dff)-1,method = "forward")
for (i in 1:51) {
  
  coefi=coef(best.fit.fwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  cv.errors.fwd[i]=mean((dff$cnt[-trainIndex]-pred)^2)
  cv.r2.fwd[i] = 1 - sum((pred-dff$cnt[-trainIndex])^2) / sum((dff$cnt[-trainIndex] - mean(dff$cnt[-trainIndex]))^2)
  mae.fwd[i] <- mean(abs(dff$cnt[-trainIndex]-pred))
}
rmse_fwd <- sqrt(cv.errors.fwd)
cv.errors.fwd
rmse_fwd
cv.r2.fwd
mae.fwd


which.min(reg.summary.fwd$bic)
plot(reg.summary.fwd$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(38,reg.summary.fwd$bic[38],col="red",cex=2,pch=20)
plot(regfit.fwd,scale="bic")
coef(regfit.fwd,38)

coefi=coef(best.fit.fwd,id=38)
pred.fwd=test.mat[,names(coefi)]%*%coefi
plot(dff$cnt[-trainIndex], pred.fwd, pch = 16, col = "red", main = "Actual vs Predicted for Forward Selection", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)
mse_fw <- cv.errors.fwd[38]
rmse_fw <-rmse_fwd[38]
r_squared_fw<-cv.r2.fwd[38]
mae_fw <- mae.fwd[38]
mae_fw
# Display the results
cat("R-squared: ", r_squared_fw, "\n")
cat("Mean Square Error (MsE): ", mse_fw, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_fw, "\n")
cat("Mean Average Error (MAE): ", mae_fw, "\n")

###################################################################################################################################################

################ Backward

regfit.bwd=regsubsets(cnt ~ ., data=dff[trainIndex,],nvmax=ncol(dff)-1,method="backward")
reg.summary.bwd <- summary(regfit.bwd)

cv.errors.bwd <- rep(NA,51)
cv.r2.bwd <- rep(NA,51)
mae.bwd <- rep(NA,51)
test.mat=model.matrix(cnt~.,data=dff[-trainIndex,])

best.fit.bwd <- regsubsets(cnt ~., data = dff[trainIndex,], nvmax = ncol(dff)-1,method = "backward")
for (i in 1:51) {
  
  coefi=coef(best.fit.bwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  cv.errors.bwd[i]=mean((dff$cnt[-trainIndex]-pred)^2)
  mae.bwd[i] <- mean(abs(dff$cnt[-trainIndex]-pred))
  cv.r2.bwd[i] = 1 - sum((pred-dff$cnt[-trainIndex])^2) / sum((dff$cnt[-trainIndex] - mean(dff$cnt[-trainIndex]))^2)
}

rmse_bwd <- sqrt(cv.errors.bwd)
cv.errors.bwd
rmse_bwd
cv.r2.bwd
mae.bwd


which.min(reg.summary.bwd$bic)
plot(reg.summary.bwd$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(37,reg.summary.bwd$bic[37],col="red",cex=2,pch=20)
plot(regfit.bwd,scale="bic")
coef(regfit.bwd,37)

coefi=coef(best.fit.bwd,id=37)
pred.bwd=test.mat[,names(coefi)]%*%coefi
plot(dff$cnt[-trainIndex], pred.bwd, pch = 16, col = "red", main = "Actual vs Predicted for Backward Selection", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)
mse_bw <- cv.errors.bwd[37]
rmse_bw <-rmse_bwd[37]
r_squared_bw<-cv.r2.bwd[37]
mae_bw <- mae.bwd[37]
# Display the results
cat("R-squared: ", r_squared_bw, "\n")
cat("Mean Square Error (MsE): ", mse_bw, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_bw, "\n")
cat("Mean Absolute Error (RMSE): ", mae_bw, "\n")

#######################################################################################################################################################
###### Lasso and ridge Regression
X <- model.matrix(cnt~.,dff)[,-1]
y <- dff$cnt


# The Lasso
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(X[trainIndex,],y[trainIndex],alpha=1,lambda=grid,nfolds=5)
plot(lasso.mod)
set.seed(1)
cv.out.lasso=cv.glmnet(X[trainIndex,],y[trainIndex],alpha=1,nfolds=5)
plot(cv.out.lasso)
bestlam=cv.out.lasso$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=X[-trainIndex,])
mse_lr <- mean((lasso.pred-y[-trainIndex])^2)
mae_lr <- mean(abs(lasso.pred-y[-trainIndex]))
rmse_lr <- sqrt(mse_lr)
r_squared_lr <- 1 - sum((lasso.pred-y[-trainIndex])^2) / sum((y[-trainIndex] - mean(y[-trainIndex]))^2)
mae_lr
mse_lr
rmse_lr
r_squared_lr
plot(dff$cnt[-trainIndex], lasso.pred, pch = 16, col = "red", main = "Actual vs Predicted for Lasso Regression", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

out=glmnet(X,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(X),]
lasso.coef
lasso.coef[lasso.coef!=0]
# Display the results
cat("R-squared: ", r_squared_lr, "\n")
cat("Mean Square Error (MsE): ", mse_lr, "\n")
cat("Mean Absolute Error (MAE): ", mae_lr, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_lr, "\n")

## Ridge

ridge.mod=glmnet(X[trainIndex,],y[trainIndex],alpha=0,lambda=grid,nfold = 5)
plot(ridge.mod)
set.seed(1)
cv.out.ridge=cv.glmnet(X[trainIndex,],y[trainIndex],alpha=0,nfold = 5)
plot(cv.out.ridge)
bestlam=cv.out.ridge$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=X[-trainIndex,])
mse_rr <- mean((ridge.pred-y[-trainIndex])^2)
mae_rr <- mean(abs(ridge.pred-y[-trainIndex]))
rmse_rr <- sqrt(mse_rr)
r_squared_rr <- 1 - sum((ridge.pred-y[-trainIndex])^2) / sum((y[-trainIndex] - mean(y[-trainIndex]))^2)
mse_rr
mae_rr
rmse_rr
r_squared_rr
plot(dff$cnt[-trainIndex], ridge.pred, pch = 16, col = "red", main = "Actual vs Predicted for Ridge regression", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

out=glmnet(X,y,alpha=0,lambda=grid)
ridge.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(X),]
ridge.coef
ridge.coef[ridge.coef!=0]
# Display the results
cat("R-squared: ", r_squared_rr, "\n")
cat("Mean Square Error (MsE): ", mse_rr, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_rr, "\n")
cat("Mean Absolute Error (MAE): ", mae_rr, "\n")


###############################################################################################################################################
#### Random Forest
#library(reprtree)

set.seed(1)

#ctrl <- trainControl(method = "cv", number = 5)

#rf.model=train(cnt ~ ., data = dff[trainIndex,], method = "rf", trControl = ctrl, ntree=100)#ntree = 100
#rf.model=train(cnt ~ ., data = dff[trainIndex,], method = "rf", trControl = ctrl, ntree=100,importance=TRUE)
rf.model=randomForest(cnt~.,data=dff,subset=trainIndex,importance=TRUE)
#plot(rf.model)

yhat.rf = predict(rf.model,newdata=dff[-trainIndex,])
mse_rf = mean((yhat.rf-dff$cnt[-trainIndex])^2)
mae_rf = mean(abs(yhat.rf-dff$cnt[-trainIndex]))
rmse_rf = sqrt(mse_rf)
r_squared_rf = 1 - sum((yhat.rf-dff$cnt[-trainIndex])^2) / sum((dff$cnt[-trainIndex] - mean(dff$cnt[-trainIndex]))^2)

plot(dff$cnt[-trainIndex], yhat.rf, pch = 16, col = "red", main = "Actual vs Predicted for Random Forest", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)
# Display the results
cat("R-squared: ", r_squared_rf, "\n")
cat("Mean Square Error (MsE): ", mse_rf, "\n")
cat("Mean Absolute Error (MAE): ", mae_rf, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_rf, "\n")
importance(rf.model)
varImpPlot(rf.model)

###############################################################################################################################################
### Boosting

set.seed(1)
boost.model = gbm(cnt~.,data=dff[trainIndex,],distribution="gaussian",n.trees=5000,cv.folds = 5,interaction.depth=6)
summary(boost.model)
par(mfrow=c(1,2))
plot(boost.model,i="atemp")
plot(boost.model,i="hum")
yhat.boost=predict(boost.model,newdata=dff[-trainIndex,],n.trees=5000)
mse_gbm <- mean((yhat.boost-dff$cnt[-trainIndex])^2)
mae_gbm <- mean(abs(yhat.boost-dff$cnt[-trainIndex]))
rmse_gbm <- sqrt(mse_gbm)
r_squared_gbm = 1 - sum((yhat.boost-dff$cnt[-trainIndex])^2) / sum((dff$cnt[-trainIndex] - mean(dff$cnt[-trainIndex]))^2)

plot(dff$cnt[-trainIndex], yhat.boost, pch = 16, col = "red", main = "Actual vs Predicted for Boosting", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 01, col = "black",lty=1,lwd=3)
# Display the results
cat("R-squared: ", r_squared_gbm, "\n")
cat("Mean Square Error (MSE): ", mse_gbm, "\n")
cat("Mean Absolute Error (MAE): ", mae_gbm, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_gbm, "\n")


##################################################################################################################################################################
########################################################################################################################################################

####################################################################################################################################################################
#################################################################################################################################################################################
####  Neural Network

X <- scale(model.matrix(cnt~. -1, data=df0))
#y <- transformed_data
y <- df0$cnt
#features <- dff[-53]
#target <- dff$cnt
x_train <- X[trainIndex, ]
y_train <- y[trainIndex]
x_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Assuming you want to use L2 regularization with lambda = 0.001
l2_lambda <- 0.001

# Build a neural network model 
model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(X), kernel_regularizer = regularizer_l2(l2_lambda)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 64, activation = 'relu', kernel_regularizer = regularizer_l2(l2_lambda)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = 'relu', kernel_regularizer = regularizer_l2(l2_lambda)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'  # For regression tasks
)

# Train the model
model %>% fit(
  x_train,y_train,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)


#plot(model)
npred <- predict(model , x_test)

plot(y_test, npred, pch = 16, col = "red", main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "black",lty=1,lwd=3)

mse_nn <- mean((npred-y_test)^2)
mae_nn <- mean(abs(npred-y_test))
rmse_nn <- sqrt(mse_nn)
r_squared_nn <-  1 - sum((npred-y_test)^2) / sum((y_test - mean(y_test))^2)
# Display the results
cat("R-squared: ", r_squared_nn, "\n")
cat("Mean Square Error (MsE): ", mse_nn, "\n")
cat("Mean Absolute Error (MAE): ", mae_nn, "\n")
cat("Root Mean Square Error (RMSE): ", rmse_nn, "\n")
















###################################################################################################################################################################

###################################################################################################################################################
### Evaluation Metrics
# Create a sample data frame (replace this with your actual data)
model_data <- data.frame(
  Model = c("linear Regression","poly 2","poly 3","poly 4","LR_InterTerms","forward","backward","lasso","ridge","random forest","boosting","NN"),
  RSquare = c(r_squared_lm,r_squared_lm2,r_squared_lm3,r_squared_lm4,r_squared_it,r_squared_fw,r_squared_bw,r_squared_lr,r_squared_rr,r_squared_rf,r_squared_gbm,r_squared_nn)*100,
  MAE = c(mae_lm,mae_lm2,mae_lm3,mae_lm4,mae_it,mae_fw,mae_bw,mae_lr,mae_rr,mae_rf,mae_gbm,mae_nn),
  RMSE = c(rmse_lm,rmse_lm2,rmse_lm3,rmse_lm4,rmse_it,rmse_fw,rmse_bw,rmse_lr,rmse_rr,rmse_rf,rmse_gbm,rmse_nn)
)

# Melt the data frame for easy plotting

melted_data <- melt(model_data, id.vars = "Model")
#melted_data$variable <- melted_data$variable * 100

melted_data$Model <- reorder(melted_data$Model, melted_data$value, FUN = function(x) min(x))

# Function to generate a color palette
get_color_palette <- function(n) {
  hcl(h = seq(15, 375, length.out = n + 1), l = 65, c = 100)[1:n]
}

# Separate plots for MSE, RMSE, and RSquare with different colors and labels
mae_plot <- ggplot(subset(melted_data, variable == "MAE"), aes(x = Model, y = value, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = get_color_palette(nrow(model_data))) +
  geom_text(aes(label = round(value,4)), vjust = -0.5) +
  labs(title = "MAE Plot",
       x = "Models",
       y = "MAE") +
  theme_minimal()

rmse_plot <- ggplot(subset(melted_data, variable == "RMSE"), aes(x = Model, y = value, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = get_color_palette(nrow(model_data))) +
  geom_text(aes(label = round(value,4)), vjust = -0.5) +
  labs(title = "RMSE Plot",
       x = "Models",
       y = "RMSE") +
  theme_minimal()

rsquare_plot <- ggplot(subset(melted_data, variable == "RSquare"), aes(x = Model, y = value, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = get_color_palette(nrow(model_data))) +
  geom_text(aes(label = round(value, 2)), vjust = -0.5) +
  labs(title = "R Square Plot",
       x = "Models",
       y = "R Square") +
  theme_minimal()

# Print the plots
print(mae_plot)
print(rmse_plot)
print(rsquare_plot)
model_data

###############################################


################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################
###############################################################################################################################################################################################################################################################################################


