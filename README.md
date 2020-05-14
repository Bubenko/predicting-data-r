# Predicting Data using R


**1 Data Preparation**

```{r}
#1.1 Load the dataset data.csv into memory.
rm(list = ls()) #clear memory
data <- read.csv("insurance.csv") #1,338 rows and 7 columns 

str(data) 
```

```{r}
#1.2.In the data frame, transform the variable charges by setting data$charges = log(data$charges).
data$charges = log(data$charges)
```

```{r}
#1.3.Using the data set from 1.2, use the model.matrix() function to create another data set that uses dummy variables in place of categorical variables. Verify that the first column only has ones (1) as values, and then discard the column only after verifying it has only ones as values.

#TEST 01________
#data.dummy <- data.frame(data[,! colnames(data) %in% "sex smoker region"],
  #model.matrix(~ sex + smoker + region , data))
#TEST 02________
data.dummy <- model.matrix(~.-1,data = data)
#dummy variables in place of categorical variables

#We need to delete all factor features
#data.dummy$sex <- NULL
#data.dummy$smoker <- NULL
#data.dummy$region <- NULL

#data.dummy$X.Intercept. <- NULL 
#Verify that the first column only has ones (1) as values, and then discard the column only after verifying it has only ones as values.
```


```{r}
#1.4.Use the sample() function with set.seed equal to 1 to generate row indexes for your training and tests sets, with 2/3 of the row indexes for your training set and 1/3 for your test set. Do not use any method other than the sample() function for splitting your data.

set.seed(1)

size <- floor(0.66 * nrow(data.dummy)) #2/3 of the row indexes

index <- sample(seq_len(nrow(data.dummy)), size = size) #sample() function for splitting data.

dummy.train <- data.dummy[index, ]
dummy.test <- data.dummy[-index, ]
```

```{r}
#1.5. Create a training and test data set from the data set created in 1.2 using the training and test row indexes created in 1.4. Unless otherwise stated, only use the training and test data sets created in this step.

size <- floor(0.66 * nrow(data)) #2/3 of the row indexes

index <- sample(seq_len(nrow(data)), size = size) #sample() function for splitting your data.

data.train <- data[index, ] #training data set from the data set created in 1.2
data.test <- data[-index, ] #test data set from the data set created in 1.2
```

```{r}
#1.6. Create a training and test data set from data set created in 1.3 using the training and test row indexes created in 1.4
index.dummy <- sample(1:nrow(data.dummy), 0.66*nrow(data.dummy))

data.dummy.train <- data.dummy[index.dummy,]
data.dummy.test <- data.dummy[-index.dummy,]
```

**2 Build a multiple linear regression model.**

```{r}
#load all nesecarly librarys
library(MASS)
library(caret)

#2.1. Perform multiple linear regression with charges as the response and the predictors are age, sex, bmi, children, smoker, and region. Print out the results using the summary() function. Use the training data set created in step 1.5 to train your model
mod_lm <- lm(charges ~ age + sex + bmi + children + smoker + region, data = data)

summary(mod_lm) # show results
```

```{r}
#2.2. Is there a relationship between the predictors and the response?

#We can see from our results that almost all variables (age, bmi, children, smokeryes, regionsoutheast, regionsouthwest) have high significance. Only (regionnorthwest) shows the lowest significance. to the predictor. (sexmale) shows mediocre significance
```

```{r}
#2.3. Does sex have a statistically significant relationship to the response?

#(sex) - shows mediocre significance. As a result we can exclude it for our final model. To make our model easier to interpret.
```


```{r}
#2.4. Perform best subset selection using the stepAIC() function from the MASS library, choose best model based on AIC. For the "direction" parameter in the stepAIC() method, set direciton="backward"

full <-  glm(charges ~., data = data.train) #run the linear model with all potential predictors in r using glm () function
null <- lm(charges ~ 1, data = data.train) #have the lower point

lm.bwd <-  stepAIC(full, direction = "backward",
                   scope = list(lower = null, upper = full)) #run stepwise slection calling function stepAIC with three arguments: model results, direction and stop point (lower point) 

lm.bwd #print out the best model based backward selection

# From the results we can see that (formula = charges ~ age + sex + bmi + children + smoker + region) is a good model
```


```{r}
#2.5 Compute the test error of the best model in #2.4 based on AIC using LOOCV using trainControl() and train() from the caret library. Report the MSE by squaring the reported RMSE.

library(caret) #load the library

train_control <- trainControl(method="LOOCV") # define training control by specifying LOOCV

model <- train(charges ~ age + sex + bmi + children + smoker + 
    region, data = data.train, trControl=train_control, method="lm") # train the model

summary(model) #results
```


```{r}
#2.6 Calculate the test error of the best model in #2.4 based on AIC using 10-fold Cross-Validation. Use train and trainControl from the caret library. Refer to model selected in #2.4 based on AIC. Report the MSE.
#CV

train_control_2 <- trainControl(method = "CV", number=10) #define training control by specifying LOOCV

model_2 <- train(charges ~ age + sex + bmi + children + smoker + 
    region, data = data.train, trControl = train_control_2, method="lm") #train the model

summary(model_2) #results
```

```{r}
#2.7 Calculate and report the test MSE using the best model from 2.4 and test data set created in step 1.5.

prediction <- predict(lm.bwd, data.test)

mse.multiLinear <- sum((data.test$charges - predict(lm.bwd, data.frame(data.test)))^2) #MSE is 87.52 - witch is good.
mse.multiLinear
```

```{r}
#2.8 Compare the test MSE calculated in step 2.6 using 10-fold cross-validation with the test MSE calculated in step 2.7. How similar are they?

#The MSE in model_2 (step 2.6) is 0.445 but in step 2.7 MSE is 87.5, whitch is higher! And that is better for our moder

#data(cars)
#MSE(prediction, model_2) #not working
#table(prediction, model_2)
```


**3 Build a regression tree model.**

```{r}
#3.1 Build a regression tree model using function tree(), where charges is the response and the predictors are age, sex, bmi, children, smoker, and region.

#install tree model pakege
#install.packages("tree")
library(tree)

tree.data <-  tree(charges ~ age + sex + bmi + children + smoker + region, data =  data) #build a tree model. We need to minimize the mean square errors (MSE).

summary(tree.data) #We have 7 nodes in the tree. Used variables smoker, age, children, bmi
```

```{r}
#3.2 Find the optimal tree by using cross-validation and display the results in a graphic. Report the best size.

cv.data <-  cv.tree(tree.data) #cross-validation to prune the decision trees to get a simpler model.

plot(cv.data$size, cv.data$dev) #We can see that the best size would be 3-4 for our model
```

```{r}
#3.3 Justify the number you picked for the optimal tree with regard to the principle of variance-bias trade-off.

justified.tree.data <-  tree(charges ~ smoker + age + children + bmi, data =  data)
summary(justified.tree.data)
```


```{r}
#3.4 Prune the tree using the optinal size found in 3.2
prune.data = prune.tree(tree.data, best = 4) #decreases the error of the tree model
prune.data
```

```{r}
#3.5 Plot the best tree model and give labels.

besttree <-  predict(prune.data, newdata = data.test) # We use numerical method to generates the best tree model for the training set

plot(besttree) #We can see some patternt in our model
```

```{r}
#3.6 Calculate the test MSE for the best model.
mse.regression <- mean((besttree - data.test$charges)^2) #MSE - 0.2197944, Witch is not good.
mse.regression
```

**4 Build a random forest model.**

```{r}
#4.1 Build a random forest model using function randomForest(), where charges is the response and the predictors are age, sex, bmi, children, smoker, and region.

#install.packages("randomForest")
library(randomForest) #installing all neccesarly libraries

index <- sample(1:nrow(data), 0.67*nrow(data)) #dividing data into training and test data set

random.train <- data[index, ]
random.test <- data[-index, ]

forestmod.data <-  randomForest(charges ~ age + sex + bmi + children + smoker + region, data = data.train, importance = TRUE) #random forest model
forestmod.data # We have 500 trees
```

```{r}
#4.2 Compute the test error using the test data set.
foresttest <-  predict(forestmod.data, newdata = data.test)

mse.forest <- mean((foresttest - data.test$charges)^2) #test error - 0.16.
mse.forest
```

```{r}
#4.3 Extract variable importance measure using the importance() function.
importance(forestmod.data) #importance  of variable
#We can see that (smoker) has the highest importance measure (145.282067)
```

```{r}
#4.4 Plot the variable importance using the function, varImpPlot(). Which are the top 3 important predictors in this model?

varImpPlot(forestmod.data)
# From our graph we can see that smoker, age and children has the gretest importance 
```

**5 Build a support vector machine model**

```{r}
#5.1 The response is charges and the predictors are age, sex, bmi, children, smoker, and region. Please use the svm() function with radial kernel and gamma=5 and cost = 50.

#load the library
library(e1071)

# support vector machine to predict am: 0 or 1
svm.fit <-  svm(charges ~ age + sex + bmi + children + smoker+ region, data = data.train, kernel = "radial", gamma = 5, cost =50) #the larger the charge, the smaller the training error.

summary(svm.fit)
```


```{r}
#5.2 Perform a grid search to find the best model with potential cost: 1, 10, 50, 100 and potential gamma: 1,3 and 5 and potential kernel: "linear","radial" and "sigmoid". And use the training set created in step 1.5.

tune.out <-  tune(svm, charges ~ age + sex + bmi + children + smoker+ region, data = data.train, kernel = list("linear", "radial", "sigmoid"),  ranges = list(cost = c(1, 10, 50, 100), gamma= c(1,3, 5))) #we train many models for the different combination of cost and gamma, and choose the best model

summary(tune.out) #best performance at 0.2175325
```

```{r}
#5.3 Print out the model results. What are the best model parameters?

summary(svm.fit) #5.2
summary(tune.out) #5.3 
#The best model parameters (5.3) is when the potential cost = 1
```


```{r}
#5.4 Forecast charges using the test dataset and the best model found in 5.3).
pred1 <-  predict(tune.out$best.model, newdata = data.test)

summary(pred1)

trueObservation1 <-  data.test$charges #true observation of am of  the test dataset
```

```{r}
#5.5 Compute the MSE (Mean Squared Error) on the test data.

mse.supportVector <- mean((pred1 - data.test$charges)^2) #test error - 0.20
mse.supportVector
```

**6 Perform the k-means cluster analysis.**
```{r}
#6.1 Use the training data set created in step 1.6 and standardize the inputs using the scale() function.

#loading all nececarly libraries
library(factoextra)
library(cluster)

#Standardize the data
scaled.data <-   scale(data.dummy.train[, -1]) #The scale only works on numeric data
```

```{r}
#6.2 Convert the standardized inputs to a data frame using the as.data.frame() function
scaled.data <-  as.data.frame(scaled.data) #the output of a scale function is a matrix, so we need to convert it into a dataframe.
```

```{r}
#6.3 Determine the optimal number of clusters, and use the gap_stat method and set iter.max=20. Justify your answer. It may take longer running time since it uses a large dataset.
fviz_nbclust(data.dummy.train, kmeans, method = "gap_stat") # To obtain the optimal number of clusters. 
#As a result the optimum number of clusters would be 2-3
```

```{r}
#6.4 Perform k-means clustering using the optimal number of clusters found in step 6.3. Set parameter nstart = 25.

# the optimal number of clusters is 2
#number of random sets - 25 
km.res <- kmeans(data.dummy.train, 2, nstart = 25)  
km.res
```

```{r}
#6.5 Visualize the clusters in different colors, setting parameter geom="point".
fviz_cluster(km.res, data = data.dummy.train)
#We can see some patterns in our clustering. Red - Blue -Red - Blue
```

**7 Build a neural networks model**

```{r}
#7.1 Using the training data set created in step 1.6, create a neural network model where the response is charges and the predictors are age, sexmale, bmi, children, smokeryes, regionnorthwest, regionsoutheast, and regionsouthwest. Please use 1 hidden layer with 1 neuron. Do not scale the data.

#load the library
library(neuralnet)

neuralnetwork <- neuralnet(charges ~ age + sexmale + bmi + children + smokeryes + regionnorthwest + regionsoutheast + regionsouthwest, data = data.dummy.train, hidden = 1)
```

```{r}
#7.2 Plot the neural network.
plot(neuralnetwork)
#We can see 8 input nodes and one hidden layer
```

```{r}
#7.3 Forecast the charges in the test dataset.

#data.dummy.test$age <- as.integer(data.dummy.test$age)
#data.dummy.test$sexmale <- as.numeric(data.dummy.test$sexmale)

#data.dummy.test <- scale(data.dummy.test)
#data.dummy.test <- as.data.frame(data.dummy.test) #Convert the standardized inputs to a data frame using the as.data.frame() function.

#predict <- compute(neuralnetwork, data.dummy.test[, c("age", "sexmale", "bmi", "children", "smokereyes", "regionnorthwest", "regionsoutheast", "regionsouthwest")]) #I have a wierd erorr. Error in "age" + "sexmale" : non-numeric argument to binary operator. But prir to that i converted it to numberic

#predict <- compute(network, test[, c("volatile.acidity", "density","pH", "alcohol")])

predict <- compute(neuralnetwork, data.dummy.test)
summary(predict)
```

```{r}
#7.4 Compute test error (MSE).
mse.network <- mean((data.dummy.test[1:455] - predict$net.result)^2)
mse.network
#MSE - 1187.882. Have some issues.
```

**8 Putting it all together**
```{r}
#8.1 For predicting insurance charges, your supervisor asks you to choose the best model among the multiple regression regression tree, random forest, support vector machine, and neural network models. Compare the test MSEs of the models generated in steps 2.7, 3.6, 4.2, 5.5, and 7.4. Display the names for these types of these models, using these labels: "Multiple Linear Regression", "Regression Tree", "Random Forest", "Support Vector Machine", and "Neural Network" and their corresponding test MSEs in a data.frame. Label the column in your data frame with the labels as "Model.Type", and label the column with the test MSEs as "Test.MSE" and round the data in this column to 4 decimal places. Present the formatted data to your supervisor and recommend which model is best and why.

mse.multiLinear
mse.regression
mse.forest
mse.supportVector
mse.network

Test.MSE <- c(mse.multiLinear, mse.regression, mse.forest, mse.supportVector, mse.network)

Model.Type <- c("Multiple Linear Regression", "Regression Tree", "Random Forest", "Support Vector Machine", "Neural Network")

mse.table <- data.frame(Model.Type, Test.MSE)
mse.table
```


```{r}
#8.2 Another supervisor from the sales department has requested your help to create a predictive model that his sales representatives can use to explain to clients what the potential costs could be for different kinds of customers, and they need an easy and visual way of explaining it. What model would you recommend, and what are the benefits and disadvantages of your recommended model compared to other models?

#______________

#From the analyzed data I would advise my supervisor to use Random Forest model. Accourding to the data it has the lowest MSE - 0.1598438. As a result, predicted data shows that our data values are dispersed closely to its central moment.

# After analyzing the graph and data the best features would be smoker, age and children (they have the gretest importance on the predictor). I would recomment to use this graph as a evidence:

varImpPlot(forestmod.data)

# Over all the best advantage of this model, that is is relatively easy to illustrate and interpret. Moreover, due to choosiong only important predictors we simplify our model.

```

**9 Final**

```{r}
#9.1 The supervisor from the sales department likes your regression tree model. But she says that the sales people say the numbers in it are way too low and suggests that maybe the numbers on the leaf nodes predicting charges are log transformations of the actual charges. You realize that in step 1.2 of this project that you had indeed transformed charges using the log function. And now you realize that you need to reverse the transformation in your final output. The solution you have is to reverse the log transformation of the variables in the regression tree model you created and re-display the result.

data$charges = log(data$charges) #original

data$charges = 1/log(data$charges) #reverse the log transformation

tree.data <-  tree(charges ~ age + sex + bmi + children + smoker + region, data =  data) #updated model

cv.data <-  cv.tree(tree.data) 
plot(cv.data$size, cv.data$dev) # We can see vividly that 2 leaf nodes are the best solution

prune.data = prune.tree(tree.data, best = 2) #decreases the error of the tree model
prune.data

final.pred <-  predict(prune.data, newdata = data.test)
mse.regression <- mean((final.pred - data.test$charges)^2)
mse.regression

```
