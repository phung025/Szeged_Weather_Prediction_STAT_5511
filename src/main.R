# Predict the temperature of Szeged, Hungary based on the humidity, wind 
# speed, etc

# Required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("plyr")
usePackage("PerformanceAnalytics")
usePackage("rpart")
usePackage("rpart.plot")
usePackage("arules")
usePackage("caret")
usePackage("e1071")
usePackage("DMwR")
usePackage("plot3D")
usePackage("randomForest")
usePackage("kknn")
usePackage("leaps")

# Variables contain the dataframe
# data - processed dataframe used for regression problem
# data.discretized - discretized dataframe based on k-cluster used for classification problem (5 groups)
# data.discretized2 - discretized dataframe based on interval used for classification problem (15 groups)

# Read the weather data
data = read.csv('WeatherHistory.csv', sep=',')

#################################################################
# DATA PREPROCESS
#################################################################

# Extract day
data$Day = as.numeric(sapply(data$Formatted.Date, substring, 9, 10))

# Extract month
data$Month = as.numeric(sapply(data$Formatted.Date, substring, 6, 7))

# Extract year (used for plotting heatmap)
data$Year = as.numeric(sapply(data$Formatted.Date, substring, 1, 4))

# Extract hour
data$Hour = as.numeric(sapply(data$Formatted.Date, substring, 12, 13))

# Drop unused columns
data = data[, !(colnames(data) %in% c("Formatted.Date", "Loud.Cover", "Daily.Summary"))]

# Take out rows with null class label in column Precip.Type
# Required packages:
#   + plyr
#   + PerformanceAnalytics
data = data[data$Precip.Type != 'null', ]

# Map categorical variable in Precip.Type to numerical value
# 1 = rain
# 2 = snow
data$Precip.Type <- mapvalues(data$Precip.Type, 
                                        from=c("rain","snow"), 
                                        to=c(1, 2))

# Map categorical variable in Summary to numerical value
data$Summary <- mapvalues(data$Summary, 
                                        from=as.vector(unique(data$Summary)), 
                                        to=seq(1, length(as.vector(unique(data$Summary))), 1))
# 1=clear, 2=partly cloudy, 3=mostly cloudy, 4=overcast, 5=foggy, 6=others
data$Processed.Summary <- mapvalues(data$Summary, 
                                    from=seq(1, length(as.vector(unique(data$Summary))), 1), 
                                    to=c(2, 3, 4, 5, 3, 1, 3, 4, 3, 2, 5, 4, 5, 2, 6, 2, 3, 2, 6, 6, 4, 6, 6, 6, 3, 6, 6))

# Convert variables to numeric variables
columns = names(data)
for (column in columns) {
  data[[column]] = as.numeric(as.character(data[[column]]))
}
data = data

# Standardize all columns except response column & columns with categorical
# values
excluding_columns = c("Precip.Type", "Day", "Month", "Year", "Hour", "Summary", "Processed.Summary", "Temp")
tmp = as.data.frame(scale(data[, !names(data) %in% excluding_columns]))
tmp[excluding_columns] = data[excluding_columns]
data = tmp
rm(tmp)
rm(excluding_columns)

# Discretize the temperature column & turn into a classification problem
# Discretize temperature column by breaking into 8 groups with same interval
data.discretized = data
data.discretized$Temp = discretize(data.discretized$Temp,
                                   method = "interval",
                                   breaks = 8)
data.discretized.targets = unique(as.vector(data.discretized$Temp))

#################################################################
# PLOTTING
#################################################################

data.plot = data[sample(nrow(data), 1000), ]

# Do a correlation plot & examine correlations among variables (only 
# use first 500 data entries)
chart.Correlation(data.plot, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)

# Scatter plot of humidity vs temperature
plot(data.plot$Temp~data.plot$Humidity,
     main="Scatter Plot of Humidity vs Temperature",
     xlab="Humidity",
     ylab="Temperature")

# Scatter plot of month vs temperature
plot(data.plot$Temp~data.plot$Month,
     main="Scatter Plot of Month vs Temperature",
     xlab="Month",
     ylab="Temperature")

# Scatter plot of summary vs temperature
plot(data.plot$Temp~data.plot$Summary,
     main="Scatter Plot of Summary vs Temperature",
     xlab="Summary",
     ylab="Temperature")

# Scatter plot of summary vs temperature
plot(data.plot$Temp~data.plot$Processed.Summary,
     main="Scatter Plot of Processed.Summary vs Temperature",
     xlab="Processed.Summary",
     ylab="Temperature",
     pch = 18)

year = 2014
plots = list() 
for (month in 1:12) {
  tmp = data[data$Year == year & data$Month == month, ][, c("Day", "Temp")]
  tmp = tmp[order(tmp$Day),]
  tmp = matrix(as.matrix(tmp$Temp), nrow = 31, ncol=24, byrow = TRUE)
  
  breaks = seq(-20, 40, by=0.01)
  x = levelplot(t(tmp), xlab="Time",
                ylab="Day", 
                main=paste(c("Temperature in", month.abb[month], year), collapse = " "), 
                col.regions = colorRampPalette(c("deeppink", "blue", "cyan", "lightgreen", "yellow", "orange", "red"))(length(breaks)-1),
                at= seq(-15, 35, length.out=120))
  plots = append(plots, list(x))
}
rm(year)
for (i in 1:12) {
  print(plots[i])
}

scatter3D(x=data.plot$Humidity, y=data.plot$Pressure, z=data.plot$Temp, 
          clab = c("Temperature"),
          xlab="Humidity",
          ylab="Pressure",
          zlab="Temperature", col.panel ="white", pch = 18, bty = "u",
          col.grid = "black",
          main = "3D plot of Temp, Humidity, Pressure")

scatter3D(x=data.plot$Humidity, y=data.plot$Visibility, z=data.plot$Temp, 
          clab = c("Temperature"),
          xlab="Humidity",
          ylab="Visibility",
          zlab="Temperature", col.panel ="white", pch = 18, bty = "u",
          col.grid = "black",
          main = "3D plot of Temp, Humidity, Visibility")

scatter3D(x=data$Processed.Summary, y=data$Precip.Type, z=data$Temp, 
          clab = c("Temperature"),
          xlab="Processed.Summary",
          ylab="Precip.Type",
          zlab="Temperature", col.panel ="white", pch = 18, bty = "u",
          col.grid = "black",
          main = "3D plot of Temp, Processed.Summary,\n Precip.Type")

scatter3D(x=data.plot$Day, y=data.plot$Month, z=data.plot$Temp, 
          clab = c("Temperature"),
          xlab="Day",
          ylab="Month",
          zlab="Temperature", col.panel ="white", pch = 18, bty = "u",
          col.grid = "black",
          main = "3D plot of Temp, Day, Month")

scatter3D(x=data.plot$Hour, y=data.plot$Day, z=data.plot$Temp, 
          clab = c("Temperature"),
          xlab="Hour",
          ylab="Day",
          zlab="Temperature", col.panel ="white", pch = 18, bty = "u",
          col.grid = "black",
          main = "3D plot of Temp, Hour, Day")

# Plot to find optimal k value for knn models
k = c()
accuracy = c()
for (i in 1:20) {
  t = sample(nrow(data.discretized), floor(dim(data.discretized)[1]*0.01))
  training_data = data.discretized[t, ]
  test_data = data.discretized[-t, ]
  
  # Fit a KNN model with k=5
  attributes = c("Temp", "Humidity", "Pressure", "Precip.Type", "Processed.Summary", "Visibility", "Day", "Month", "Hour")
  knn_model1 <- kNN(Temp~.,
                    train = training_data[attributes], 
                    test = test_data[attributes],
                    k=i)
  
  # Prepare a confusion matrix to compute the error rate of the model on training
  # data
  conf_matrix = confusionMatrix(reference=test_data$Temp, data=knn_model1)
  
  # Append values to vector for later plot
  accuracy = append(accuracy, conf_matrix$overall[1])
  k = append(k, i)
  
  # Cleanup
  rm(conf_matrix)
}

# Plot the k against accuracy to find optimal k value for KNN model
plot(x=k, y=accuracy, xlab="k", ylab="Accuracy", type="l", main="Performance of KNN model using different k values")
rm(k)
rm(accuracy)

# Frequency plot of discretized temperature
par(las=2)
barplot(count(data.discretized, 'Temp')$freq, 
        names.arg = count(data.discretized, 'Temp')$Temp, 
        horiz=FALSE, 
        cex.names=0.8,
        col=c("deeppink","darkblue","deepskyblue","cyan","chartreuse","yellow","orange","red"))

#################################################################
# KNN MODEL FITTING
#
# Instead of keeping this as a regression problem, we discretize
# the response variable & turn it into a classification problem.
# We match the temperatures into group of temperature range and
# apply KNN classification algorithm to fit the model
# and compute the accuracy of the model
#################################################################

# Classification KNN Model 1
# Use 80% of the input data for training & 20% of data for testing
# Perform KNN algorithm with the optimal k obtained from the graph
t = sample(nrow(data.discretized), floor(dim(data.discretized)[1]*0.8))
training_data = data.discretized[t, ]
test_data = data.discretized[-t, ]

# Fit a KNN model with k=8
attributes = c("Temp", "Humidity", "Wind.Bearing.deg", "Pressure", "Precip.Type", "Month", "Day", "Hour")
knn_model1 <- kNN(Temp~.,
                  train = training_data[attributes], 
                  test = test_data[attributes],
                  k=8)

# Prepare a confusion matrix to compute the error rate of the model on training
# data
conf_matrix_knn = confusionMatrix(reference=test_data$Temp, data=knn_model1)

#################################################################
# Ensemble Model Fitting
#
# Instead of using only 1 model to predict the data, we combine
# multiple classification models to produce model with better
# performance
# Models used in the ensemble model include: KNN models, Random
# forest model
#################################################################
attributes2 = c("Temp", "Humidity", "Wind.Bearing.deg", "Processed.Summary","Pressure", "Precip.Type", "Month", "Day", "Hour")
rand_forest <- randomForest(Temp ~.,
                            method="class", 
                            data=training_data[attributes2])

rand_forest_pred = predict(rand_forest, newdata = test_data[attributes2])
conf_matrix_ensemble = confusionMatrix(reference=test_data$Temp, data=rand_forest_pred)

#################################################################
# MORE PLOTTING
#################################################################

# Level plot of the confusion matrices to show performance of
# 2 classification models through graphical method
confusion_plot = function(data, main, sub) {
  
  normalized_data = data
  for (i in 1:dim(data)[1]) {
    for (j in 1:dim(data)[2]) {
      normalized_data[i,j] = data[i,j] / sum(as.matrix(data)[,j])
    }
  }
  
  levelplot(normalized_data, 
            xlab="Prediction",
            ylab="Reference", 
            main=main,
            sub=sub,
            col.regions = colorRampPalette(c("red2", "red", "orange2", "orange", "goldenrod1", "yellow", "greenyellow", "chartreuse", "green3"))(length(breaks)-1),
            at= seq(0, 1, length.out=120),
            panel=function(...) {
              arg <- list(...)
              panel.levelplot(...)
              panel.text(arg$x, arg$y, round(arg$z,1))})
}

confusion_plot(data=conf_matrix_knn$table, 
               main="Confusion Matrix Plot of The KNN Model with K=8",
               sub=paste("Accuracy", conf_matrix_knn$overall[1]))

confusion_plot(data=conf_matrix_ensemble$table, 
               main="Confusion Matrix Plot of The \n Random Forest Model",
               sub=paste("Accuracy", conf_matrix_ensemble$overall[1]))

# Plot accuracy of 2 models when training against different amount of data
performance_plot = function() {
  accuracy_knn = c()
  accuracy_rforest = c()
  proportion = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  for (p in proportion) {
    # Sample 40% of the rows in dataframe for training and the rest for testing
    # the model
    t = sample(nrow(data.discretized), floor(dim(data.discretized)[1]*p))
    training_data = data.discretized[t, ]
    test_data = data.discretized[-t, ]
    
    #################################################################
    # KNN Model Fitting
    #################################################################
    attributes = c("Temp", "Humidity", "Wind.Bearing.deg", "Pressure", "Precip.Type", "Month", "Day", "Hour")
    knn_model1 <- kNN(Temp~.,
                      train = training_data[attributes], 
                      test = test_data[attributes],
                      k=12)
    
    # Prepare a confusion matrix to compute the error rate of the model on training
    # data
    conf_matrix_knn = confusionMatrix(reference=test_data$Temp, data=knn_model1)
    
    #################################################################
    # Ensemble Model Fitting
    #################################################################
    attributes2 = c("Temp", "Humidity", "Wind.Bearing.deg", "Processed.Summary","Pressure", "Precip.Type", "Month", "Day", "Hour")
    rand_forest <- randomForest(Temp ~.,
                                method="class", 
                                data=training_data[attributes2])
    
    rand_forest_pred = predict(rand_forest, newdata = test_data[attributes2])
    conf_matrix_ensemble = confusionMatrix(reference=test_data$Temp, data=rand_forest_pred)
    
    accuracy_knn = c(accuracy_knn, conf_matrix_knn$overall[1])
    accuracy_rforest = c(accuracy_rforest, conf_matrix_ensemble$overall[1])
  }
  
  # Plot KNN line
  plot(x=proportion, 
       y=accuracy_knn, 
       xlab="Amount of training data", 
       ylab="Accuracy", 
       type="o", col="blue", pch=18, lty=1, ylim=c(0,1),
       main="Performance of classification models when \n training with different amount of data")
  
  # Add random forest line
  points(proportion, accuracy_rforest, col="red", pch=19)
  lines(proportion, accuracy_rforest, col="red",lty=2)
  
  # Add a legend
  legend("bottomright", legend=c("Random Forest Model", "KNN Model"),
         col=c("red", "blue"), lty=2:1, cex=0.8,
         title="Models")
}
performance_plot()

# Variable selection plot
varImpPlot(rand_forest, main = "Variable Importance for Random Forest Model")

# KNN Model Performance plot with different k
## LOOCV cross validation
set.seed(0)
t = sample(nrow(data.discretized), floor(dim(data.discretized)[1]*0.5))
plot_data = data.discretized[t, ]
CV.kknn.interactions <- train.kknn(Temp~., data=plot_data[attributes], 
                                   distance = 2, kmax = 30, kernel = "optimal")
rm(plot_data)
rm(t)

# plot MSE and elbow method to determine K
par(mfrow = c(1,1))
plot(CV.kknn.interactions, main = "Optimizing for K") #, xlab = "Numnber of Clusters",ylab = "RSE")
number.clusters = 5
points(number.clusters, CV.kknn.interactions$MEAN.SQU[number.clusters], col = "red", pch = 20)