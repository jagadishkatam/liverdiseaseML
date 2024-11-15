# profvis({

logisticmodel <- function(csv_file) {
  # Read the data from CSV file
  # csv_file <- 'Liver_disease_data.csv'
  data1 <<- read.csv(csv_file)
  data <- read.csv(csv_file)
  # Display the structure of the data
  str(data)
  
  # Check for missing values
  sum(is.na(data))
  
  data$Gender <- as.factor(data$Gender)
  data$Smoking <- as.factor(data$Smoking)
  data$Diabetes <- as.factor(data$Diabetes)
  data$Hypertension <- as.factor(data$Hypertension)
  data$GeneticRisk <- as.factor(data$GeneticRisk)
  data$Diagnosis <- as.factor(data$Diagnosis)
  
  str(data)
  
  # Generate boxplots for numeric columns with facet wrap
  numeric_columns <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_columns]
  
  # Reshape the data using pivot_longer
  long_data <<- data %>%
    pivot_longer(cols = names(numeric_data), names_to = "variable", values_to = "value") %>% 
    mutate(vars=case_when(
      variable == 'AlcoholConsumption' ~ 'Alcohol Consumption',
      variable == 'PhysicalActivity' ~ 'Physical Activity',
      variable == 'LiverFunctionTest' ~ 'Liver Function Test',
      TRUE ~ variable
    ))
  
  custom_colors <- c("0" = "orange", "1" = "darkorange")
  # Plot boxplots using facet wrap
  boxplot_figure <- ggplot(long_data, aes(x = Diagnosis, y = value, fill=Diagnosis)) +
    geom_boxplot() +
    facet_wrap(~ vars,   nrow = 3,ncol = 2,scales = "free") +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 360, hjust = 0.5),
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          panel.background = element_blank(),
          axis.line = element_line(color = "black")) 
  
  
  
  # Identify factor columns
  factor_columns <- sapply(data %>% select(-Diagnosis), is.factor)
  factor_data <- data[, factor_columns]
  
  # Reshape the data using pivot_longer
  long_factor_data <- data %>%
    pivot_longer(cols = names(factor_data), names_to = "variable", values_to = "value")
  
  # Plot bar charts using facet wrap
  barplot_figure <- ggplot(long_factor_data, aes(x = value, fill = Diagnosis)) +
    geom_bar(position = "dodge") +
    facet_wrap(~ variable, scales = "free") +
    ggtitle("Bar Charts of Factor Columns by Diagnosis") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(barplot_figure)
  
  
  
  # Split the data into training and testing sets
  set.seed(42)
  trainIndex <- caTools::sample.split(data, SplitRatio = 0.8)
  trainData <- base::subset(data, trainIndex=='TRUE')
  testData <- base::subset(data, trainIndex=='FALSE')
  
  
  # Train a logistic regression model
  model <- stats::glm(Diagnosis ~ Age + Gender + BMI + AlcoholConsumption + Smoking + GeneticRisk + Hypertension + LiverFunctionTest + PhysicalActivity + Diabetes, data = trainData, family = binomial)
  
  model_Age <- stats::glm(Diagnosis ~ Age, data = trainData, family = binomial)
  model_Gender <- stats::glm(Diagnosis ~ Gender, data = trainData, family = binomial)
  model_BMI <- stats::glm(Diagnosis ~ BMI, data = trainData, family = binomial)
  model_AlcoholConsumption <- stats::glm(Diagnosis ~ AlcoholConsumption, data = trainData, family = binomial)
  model_Smoking <- stats::glm(Diagnosis ~ Smoking, data = trainData, family = binomial)
  model_GeneticRisk <- stats::glm(Diagnosis ~ GeneticRisk, data = trainData, family = binomial)
  model_PhysicalActivity <- stats::glm(Diagnosis ~ PhysicalActivity, data = trainData, family = binomial)
  model_Diabetes <- stats::glm(Diagnosis ~ Diabetes, data = trainData, family = binomial)
  model_Hypertension <- stats::glm(Diagnosis ~ Hypertension, data = trainData, family = binomial)
  model_LiverFunctionTest <- stats::glm(Diagnosis ~ LiverFunctionTest, data = trainData, family = binomial)
  
  # Print the model summary
  summary(model)
  
  # Create a named list of the models
  models <- list(model_Age = model_Age, 
                 model_Gender = model_Gender, 
                 model_BMI=model_BMI,
                 model_AlcoholConsumption=model_AlcoholConsumption,
                 model_Smoking=model_Smoking,
                 model_GeneticRisk=model_GeneticRisk,
                 model_PhysicalActivity=model_PhysicalActivity,
                 model_Diabetes=model_Diabetes,
                 model_Hypertension=model_Hypertension,
                 model_LiverFunctionTest=model_LiverFunctionTest)
  
  # Use lapply to dynamically create a data frame for each model
  final_df <- do.call(rbind, lapply(names(models), function(model_name) {
    data.frame(
      list_name = model_name,       # List name (dynamic)
      aic_value = models[[model_name]]$aic  # AIC values from each model
    )
  })) %>% arrange(aic_value)
  
  # Print the final combined data frame
  print(final_df)
  
  
  # Make predictions on the test data
  predictions <<- stats::predict(model, newdata = testData, type = "response")
  predicted_status <<- ifelse(predictions >= 0.5, 1, 0)
  accuracy <- mean(predicted_status == as.factor(testData$Diagnosis))
  
  # Evaluate the model
  cm <- caret::confusionMatrix(as.factor(predicted_status), as.factor(testData$Diagnosis), positive = '1', mode='everything')
  
  par(pty='s')
  roc <- pROC::roc(trainData$Diagnosis, model$fitted.values, plot=TRUE)
  
  
  predicted.data <- data.frame(probability=model$fitted.values, Diagnosis=trainData$Diagnosis)
  predicted.data <- predicted.data[order(predicted.data$probability, decreasing=FALSE),]
  predicted.data$rank <- 1:nrow(predicted.data)
  
  pp <- ggplot(data=predicted.data, aes(x=rank, y=probability)) +
    geom_point(aes(color=Diagnosis), alpha=1, shape=2, stroke=2) +
    xlab('Index') + ylab('Predicted Probability of Getting Liver Disease') + 
    theme_minimal() +
    theme(panel.grid = element_blank(),  # Add border around plot panel
          axis.line = element_line(color = "black"))
  
  
  #   # Prepare output
  output <- list(
    table=data,
    boxplot_figure = boxplot_figure,
    barplot_figure = barplot_figure,
    trainData=trainData,
    model=model,
    model_summary = summary(model),
    accuracy = accuracy,
    confusion_matrix = cm,
    roc_curve = roc,
    predicted_probabilities = pp
  )
  
  return(output)
}

# debugonce(logisticmodel)
logisticmodel('data/Liver_disease_data.csv')
# })

# Sample data
data <- data1 %>% mutate(Diagnosis=ifelse(Diagnosis==1, 'Liver Disease', 'No Liver Disease'))

# Step 1: Count the frequency of each diagnosis
diagnosis_freq <- table(data$Diagnosis)

percentages <- round(100 * diagnosis_freq / sum(diagnosis_freq), 1)

# Step 3: Create labels with percentages
labels <- paste(names(diagnosis_freq), percentages, "%")

# Step 4: Generate the pie chart with percentage labels
pie(diagnosis_freq, labels = labels, main = "Diagnosis Distribution", col = rainbow(length(diagnosis_freq)))
