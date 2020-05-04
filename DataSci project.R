library(tidyr)

replace.date = function(df, pred_name) {
  names = c(sprintf("%s.Month", pred_name), sprintf("%s.Day", pred_name), sprintf("%s.Year", pred_name))
  
  splited = separate(df, pred_name, names, "/", convert=TRUE)
  
  return(splited)
}

omit.cols = function(df, names) {
  return(df[, !(names(df) %in% names)])
}

omitted_cols = c(
  "Declaration.Number",
  "Declaration.Type",
  "Disaster.Title",
  "County",
  "Disaster.Title",
  "Start.Date",
  "End.Date",
  "Individual.Assistance.Program",
  "Individuals.Households.Program",
  "Public.Assistance.Program",
  "Hazard.Mitigation.Program",
  "Close.Date"
)

data = read.csv("C:/Users/jonat/Desktop/MATH 4323/database.csv")
data = omit.cols(data, omitted_cols)
data = replace.date(data, "Declaration.Date")

n <- nrow(data)
RNGkind(sample.kind = "Rounding")
set.seed(1)

train <- sample(1:n, 36948)

library(class)

data$`Disaster.Type` <- as.factor(data$`Disaster.Type`)
data$`Disaster.Type` <- as.numeric(data$`Disaster.Type`)

data$State <- as.factor(data$State)
data$State <- as.numeric(data$State)

####################### testing #######################

#unscaled test and train data
x.train <- data[train,]
x.test <- data[-train,]
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]

#scaled train and test data
x.train <- scale(data[train,])
x.test <-  scale(data[-train,],
                 center = attr(x.train, "scaled:center"),
                 scale = attr(x.train, "scaled:scale"))
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]


for (K in c(1,3,10)){
  set.seed(1)
  knn.pred=knn(x.train,
               x.test,
               y.train,
               k=K)
  print(mean(knn.pred != y.test))
}

#uses LOOCV to predict data
set.seed(1)
knn.cv.pred <- knn.cv(train = data,
                cl = data$`Disaster.Type`,
                k=1)

mean(knn.cv.pred != y.test)

