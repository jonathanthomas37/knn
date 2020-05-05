library(tidyr)

replace.date = function(df, pred_name) {
  names = c(sprintf("%s.Month", pred_name), sprintf("%s.Day", pred_name), sprintf("%s.Year", pred_name))
  
  splited = separate(df, pred_name, names, '/', convert=TRUE)
  
  return(splited)
}

Date.season = function(DATES) {
  winter = as.Date("2012-12-15") # Winter Solstice
  spring = as.Date("2012-3-15") # Spring Equinox
  summer = as.Date("2012-6-15") # Summer Solstice
  fall = as.Date("2012-9-15") # Fall Equinox
  
  d = as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse(winter <= d | d < spring, 1,
         ifelse(spring <= d & d < summer, 2,
                ifelse(summer <= d & d < fall, 3, 4)))
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

#makes the seasons columns using numaric values
data$Declaration.Date2 <- as.Date(data$Declaration.Date, tryFormats="%m/%d/%Y")
data$Start.Season = as.integer(Date.season(data$Declaration.Date2))
data <- data[,-6]

#splits the date column into 3 colums of month day and year
data$Declaration.Date <- as.factor(data$Declaration.Date)
data = replace.date(data, "Declaration.Date")

#changes disaster type to numeric value
data$`Disaster.Type` <- as.factor(data$`Disaster.Type`)
data$`Disaster.Type` <- as.numeric(data$`Disaster.Type`)

#changes state to numeric value
data$State <- as.factor(data$State)
data$State <- as.numeric(data$State)

####################### testing #######################

library(class)

n <- nrow(data)
RNGkind(sample.kind = "Rounding")
set.seed(1)

train <- sample(1:n, 36948)

#unscaled test and train data (with Declaration.Date)
x.train <- data[train,-6]
x.test <- data[-train,-6]
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]

#unscaled test and train data (with Start.Season)
x.train <- data[train,c(-1,-2,-3)]
x.test <- data[-train,c(-1,-2,-3)]
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]

#scaled train and test data (with Declaration.Date)
x.train <- scale(data[train,-6])
x.test <-  scale(data[-train,-6],
                 center = attr(x.train, "scaled:center"),
                 scale = attr(x.train, "scaled:scale"))
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]

#scaled train and test data (with Start.Season)
x.train <- scale(data[train,c(-1,-2,-3)])
x.test <-  scale(data[-train,c(-1,-2,-3)],
                 center = attr(x.train, "scaled:center"),
                 scale = attr(x.train, "scaled:scale"))
y.train <- data$`Disaster.Type`[train]
y.test <- data$`Disaster.Type`[-train]


for (K in c(1,2,3,5,10,20,40,50)){
  set.seed(1)
  knn.pred=knn(x.train,
               x.test,
               y.train,
               k=K)
  print(paste("K =", K, "error =", mean(knn.pred != y.test)))
}

#uses LOOCV to predict data
set.seed(1)
knn.cv.pred <- knn.cv(train = data[,-6],
                cl = data$`Disaster.Type`,
                k=1)

mean(knn.cv.pred != y.test)

