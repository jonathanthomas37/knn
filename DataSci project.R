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

x.train <- data[train,]
x.test <- data[-train,]
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]

set.seed(1)
knn.pred <- knn(train = x.train,
                test = x.test,
                cl = y.train,
                k=3)

mean(knn.pred != y.test)

