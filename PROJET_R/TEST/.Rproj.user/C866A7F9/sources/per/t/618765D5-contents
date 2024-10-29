library(car) # vérifier la multicollinéarité
library(ggplot2) # nuage de points
library(QuantPsyc) # lm.beta
library(dplyr)



data <- read.csv("Advertising.csv")
head(data)

# Checking datatype and null count of all features
str(data)
sapply(data, class)

# Printing the number of rows and columns in ad_data
cat("The data has", nrow(data), "rows and", ncol(data), "columns.\n")
unique_influencers <- unique(data$Influencer)


# Creating new column named InfluencerNew to store school's data in binary form
data <-data %>%
  mutate(InfluencerNew = case_when(
    Influencer == "Mega" ~ 3,
    Influencer == "Micro" ~ 2,
    Influencer == "Nano" ~ 1,
    Influencer == "Macro" ~ 0,
    TRUE ~ NA_integer_  # Keep other values as NA
  ))


# Shows the data inside new InfluencerNew column
print(data$InfluencerNew)
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")


# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the sum of data inside each column
    cat("Sum of", col, "=", sum(data[[col]], na.rm = TRUE), "\n")
    print("_")
  }
}

# Specify the columns of interest
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")

# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the mean of data inside each column
    cat("Mean of", col, "=", mean(data[[col]], na.rm = TRUE), "\n")
    print("_")
  }
}

# Specify the columns of interest
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")

# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the median of data inside each column
    cat("Median of", col, "=", median(data[[col]], na.rm = TRUE), "\n")
    print("_")
  }
}


# Specify the columns of interest
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")

# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the standard deviation of data inside each column
    cat("Standard Deviation of", col, "=", sd(data[[col]], na.rm = TRUE), "\n")
    print("_")
  }
}


# Specify the columns of interest
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")


# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the maximum value of data inside each column
    cat("Maximum of", col, "=", max(data[[col]], na.rm = TRUE), "\n")
    print("_")
  }
}


# Specify the columns of interest
columns_of_interest <- c("TV", "Radio", "Social Media", "InfluencerNew", "Sales")


# Loop through each column
for (col in names(data)) {
  # Check if the column is in the specified columns_of_interest
  if (col %in% columns_of_interest) {
    # Printing the column name
    print(col)
    # Printing the minimum of data inside each column
    cat("Minimum of", col, "=", min(data[[col]], na.rm = TRUE), "\n")
    cat("_\n")
  }
}
summary(data)


# Displaying the original data frame
cat("Original DataFrame:\n")
print(data)


# Shift the column 'InfluencerNew' to the third position
third_column <- data[["InfluencerNew"]]
data <- data[, -which(names(data) == "InfluencerNew")] # Remove the column
data <- cbind(data[, 1:2],third_column, data[, 3:ncol(data)]) # Insert the column at the desired position


# Displaying the final data frame after shifting the column
cat("\nFinal DataFrame:\n")
print(data)


# Checking null values
null_values <- is.na(data)


# Displaying the null values
print(null_values)


# Checking null values and computing the sum for each column
null_sum <- colSums(is.na(data))


# Displaying the sum of null values for each column
print(null_sum)


# Calculate the median of 'Sales' column
medSales <- median(data$Sales, na.rm = TRUE)
print(medSales)

# Replace missing values in 'Sales' column with the median
data$Sales <- ifelse(is.na(data$Sales), medSales,data$Sales)

# Calculate the median of 'Radio' column
medRadio <- median(data$Radio, na.rm = TRUE)
print(medRadio)

# Replace missing values in 'Radio' column with the median
data$Radio <- ifelse(is.na(data$Radio), medRadio,data$Radio)

# Calculate the median of 'Social Media' column
medSocial <- median(data$Social.Media, na.rm = TRUE)
print(medSocial)

# Replace missing values in 'Social Media' column with the median
data$Social.Media <- ifelse(is.na(data$Social.Media), medSocial,data$Social.Media)
# Remove the 'Influencer' column
data <- subset(data, select = -c(Influencer))
# Find the sum of missing values for each column
missing_values <- colSums(is.na(data))
# Print the sum of missing values for each column
print(missing_values)

par(mfrow=c(4,1), mar=c(4,4,2,1))  # 4 rows, 1 column, and margin sizes


# Define columns to remove
remove <- c("Social Media", "Influencer")

# Create clean dataframe by removing specified columns
clean_data <- data[, !names(data) %in% remove]
print(clean_data)
# Print clean dataframe



names(clean_data) = c( "TV","Radio","InfluencerNew ","Social_Media","Sales")
head(clean_data)
summary(clean_data)

# nuage de points

nuage = ggplot(data = clean_data, aes(TV,Sales))
nuage + geom_point() + geom_smooth(method = "lm")

nuage = ggplot(data = clean_data, aes(Radio,Sales))
nuage + geom_point() + geom_smooth(method = "lm")

nuage = ggplot(data = clean_data, aes(Social_Media,Sales))
nuage + geom_point() + geom_smooth(method = "lm")


# modèle linéaire multiple

model = lm(Sales~ Radio + TV + Social_Media  , data = clean_data)
summary(model)

# test de multicollinéarité

vif(model) # variance inflation factor 
1/vif(model)
cor(clean_data[,])



# l'importance des coefficients de régression

lm.beta(model) # coefficients centré réduits
confint(model) # 95% intervalle de confiance 

# test de l'auto-corrélation

durbinWatsonTest(model)

# analyse des résidus

clean_data$résidus = rstandard(model)
hist(clean_data$résidus)
clean_data$prévisions = fitted(model)
nuage = ggplot(clean_data, aes(prévisions, résidus))
nuage + geom_point() + geom_smooth(method = "lm" ,  colour= "Blue") + labs( x= "Valeurs prédites" ,  y = "Résidus normalisés")




# Predicting sales using model
predicted_sales <- predict(model, newdata = clean_data)

# Calculating Mean Squared Error (MSE)
mse <- mean((clean_data$Sales - predicted_sales)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculating Mean Absolute Error (MAE)
mae <- mean(abs(clean_data$Sales - predicted_sales))
cat("Mean Absolute Error (MAE):", mae, "\n")



# Get the summary of the regression model
summary(model)

# Extract R-squared value
r_squared <- summary(model)$r.squared

# Calculate adjusted R-squared
n <- nrow(clean_data)  # Number of observations
p <- length(model$coefficients) - 1  # Number of predictors
r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))

# Print the results
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", r_squared_adj, "\n")


