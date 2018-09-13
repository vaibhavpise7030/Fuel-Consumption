##install.packages("lsa")
library(lsa) # Latent semantic analysis,  is, that text do have a higher order (=latent semantic) structure which, however, 
# is obscured by word usage 

library(dplyr) #  A Grammar of Data Manipulation

options(warn=-1) # Allow the user to set and examine a variety of global options t

# reading fuel consumption data 
hyundai.creta <- read.csv("C:/Users/Siddhesh/Documents/HyundaiCreta.csv")
# to list the names of variables
colnames(hyundai.creta)

# arraging the observation based on data using arrange function 
hyundai.creta <- hyundai.creta %>% arrange(date)
# removing observations that determine no travelling based on Odo_counter(odo meter- measuring the distance travelled by a vehicle) variable having Zero  
hyundai.creta <- hyundai.creta[!(hyundai.creta$Odo_counter==0),]
head(hyundai.creta)

# Renaming certain columns in dataset
colnames(hyundai.creta)[29]="assetName" 
colnames(hyundai.creta)[1]="date"
colnames(hyundai.creta)[8]="altitude"
colnames(hyundai.creta)[7]="heading"
colnames(hyundai.creta)[4]="lat"
colnames(hyundai.creta)[5]="lon"
colnames(hyundai.creta)[6]="speed"
colnames(hyundai.creta)[26]="Odometer"
colnames(hyundai.creta)[16]="ignition"
colnames(hyundai.creta)[34]="airflow"
colnames(hyundai.creta)[36]="engine"
colnames(hyundai.creta)[28]="idle_counter"
colnames(hyundai.creta)[39]="RPM"
colnames(hyundai.creta)[60]="Engine.Temp"
colnames(hyundai.creta)[37]="Fuel.Used"
colnames(hyundai.creta)[62]="moving"
colnames(hyundai.creta)[32]="overspeed"
colnames(hyundai.creta)[20]="power_voltage"
colnames(hyundai.creta)[64]="rsq"
colnames(hyundai.creta)[27]="hours_00_counter"

# Taking required columns into further analysis
hyundai.creta.required.columns <- hyundai.creta[,c("date","time",
                                                   "altitude",
                                                   "heading","lat",
                                                   "lon","speed",
                                                   "Odometer","ignition","airflow",
                                                   "engine","idle_counter","RPM",
                                                   "Engine.Temp","Fuel.Used",
                                                   "moving",
                                                   "overspeed",
                                                   "power_voltage",
                                                   "rsq",
                                                   "hours_00_counter")]

# Merging date and time column to make another column
hyundai.creta.required.columns$date = paste(hyundai.creta.required.columns$date,hyundai.creta.required.columns$time)
head( hyundai.creta.required.columns[,c(2)])

# We need to remove time column from dataset
hyundai.creta.required.columns <- hyundai.creta.required.columns[,-c(2)]

# Taking only that data where ignition was on i.e. 1
hyundai.creta.required.columns <- hyundai.creta.required.columns[hyundai.creta.required.columns$ignition ==1,]

# Changing the new date column into appropriate format
hyundai.creta.required.columns$date <- as.POSIXct(hyundai.creta.required.columns$date,format= "%m/%d/%Y %H:%M")
class(hyundai.creta.required.columns$date)

# Analyzing structure of columns of data
str( hyundai.creta.required.columns)

hyundai.creta.required.columns$Engine.Temp <- as.numeric(hyundai.creta.required.columns$Engine.Temp)
hyundai.creta.required.columns$moving <- as.numeric(hyundai.creta.required.columns$moving)
hyundai.creta.required.columns$rsq <- as.numeric(hyundai.creta.required.columns$rsq)

# Analyzing cosine similarity between some columns like engine and ignition and moving
cosine(as.numeric(hyundai.creta.required.columns$engine),y = as.numeric(hyundai.creta.required.columns$ignition))
cosine(as.numeric(hyundai.creta.required.columns$ignition),y = as.numeric(hyundai.creta.required.columns$moving))

# Ignoring data where fuel used is 0
hyundai.creta.required.columns <- hyundai.creta.required.columns[!hyundai.creta.required.columns$Fuel.Used==0,]

# Arranging data with respect to date,odometer reading and fuel used
hyundai.creta.required.columns <- hyundai.creta.required.columns %>% arrange(date,Odometer,Fuel.Used)
#anyNA(hyundai.creta.required.columns)
# Ignoring data where odometer reading is na
hyundai.creta.required.columns <- hyundai.creta.required.columns[!is.na(hyundai.creta.required.columns$Odometer),]

# Giving row names to data
rownames(hyundai.creta.required.columns) <- 1: nrow(hyundai.creta.required.columns)

head(hyundai.creta.required.columns)

# Calculating lead metrics of fuel used
qwe  = lead(hyundai.creta.required.columns$Fuel.Used[1016:21439]) - hyundai.creta.required.columns$Fuel.Used[1016:21439]
qwe1  = lead(hyundai.creta.required.columns$Fuel.Used[1:1016]) - hyundai.creta.required.columns$Fuel.Used[1:1016]

unique(qwe)
unique(qwe1)

hyundai.creta.required.columns <- hyundai.creta.required.columns[1016:nrow(hyundai.creta.required.columns),]

length(is.na(hyundai.creta.required.columns)==TRUE)

# Since the sensor data gathering often 
# encounters some processing issues the 
# data gathered may not be uniform throughout.
# The mechanism to define a trip here is 
# to split the data per 10 km and assume it 
# as a trip keeping in mind other possibilties
# as well.

p = 1
q = 1
newdf = data.frame()
newdf1 = data.frame()

# Staring loop from 2nd row till last second row of data
for(n in 2:(nrow(hyundai.creta.required.columns))-1){
  
  # Steps to do iteratively when at any stage the distance between records become equal to 10 km
  # This needs to get considered as one trip and then within this 10 km range data custom paramters are calculated
  if(hyundai.creta.required.columns$Odometer[n] - hyundai.creta.required.columns$Odometer[p]==10){
    if(hyundai.creta.required.columns$Odometer[n] == hyundai.creta.required.columns$Odometer[n+1]){
      #       print("next11")
      #       break()
      next()
    }
    else{
      #       distance = hyundai.creta.required.columns$Odometer[n] - hyundai.creta.required.columns[p]
      avg.engine.temp = sum(hyundai.creta.required.columns$Engine.Temp[p:n],na.rm = TRUE)/(n-p)
      avg.RPM = sum(hyundai.creta.required.columns$RPM[p:n],na.rm = TRUE)/(n-p)
      avg.airflow = sum(hyundai.creta.required.columns$airflow[p:n],na.rm = TRUE)/(n-p)
      #       total.time.taken = as.numeric(hyundai.creta.required.columns$date[n] - hyundai.creta.required.columns$date[p])
      total.hours.travelled <- hyundai.creta.required.columns$hours_00_counter[n]- hyundai.creta.required.columns$hours_00_counter[p]
      #       avg.intake.temp = sum(hyundai.creta.required.columns$intake_temp[p:n],na.rm = TRUE)/(n-p)
      avg.power.vltg = sum(hyundai.creta.required.columns$power_voltage[p:n],na.rm = TRUE)/(n-p)
      avg.rsq = sum(hyundai.creta.required.columns$rsq[p:n],na.rm = TRUE)/(n-p)
      #       avg.throttle = sum(hyundai.creta.required.columns$throttle,na.rm = TRUE)/(n-p)
      ctr = 1
      idle.time  <- na.omit(hyundai.creta.required.columns$idle_counter[p:n])
      total.idle.time= c()
      for(i in 1:length(idle.time)){
        
        if(idle.time[i]==0 ){
          #           print("next12")
          #           break()
          next()
        }
        else{
          
          if(i<length(idle.time) & idle.time[i+1]==0){
            total.idle.time[ctr] <- idle.time[i]
            ctr = ctr +1
            
          }
          else{
            total.idle.time[ctr] <- idle.time[i]
          }
          
        }
        
        
      }
      total.idle.time = sum(total.idle.time)
      
      diffSpeed = lag(hyundai.creta.required.columns$speed[p:n]) - hyundai.creta.required.columns$speed[p:n]
      hrsh.accleration = length(diffSpeed[diffSpeed >= 40])
      hrsh.brake = length(diffSpeed[diffSpeed <= -40])
      max.speed = max(hyundai.creta.required.columns$speed[p:n])
      min.speed = min(hyundai.creta.required.columns$speed[p:n])
      avg.speed = sum(hyundai.creta.required.columns$speed[p:n],na.rm = TRUE)/(n-p)
      fuel.consumed = (hyundai.creta.required.columns$Fuel.Used[n] - hyundai.creta.required.columns$Fuel.Used[p])/1.6
      newdf = c(#distance,
        avg.engine.temp,
        avg.RPM,
        avg.airflow,
        total.hours.travelled,
        #         avg.intake.temp,
        avg.power.vltg,
        avg.rsq,
        #         avg.throttle,
        total.idle.time,
        hrsh.accleration,
        hrsh.brake,
        max.speed,
        min.speed,
        avg.speed,
        fuel.consumed,
        p,
        n)
      
      newdf1 = rbind(newdf1,newdf)
      p = n
    }
    
  }
  else{
    # If the distance between one record and other record is more than 10 km then we need to reinitialize the counters
    if((hyundai.creta.required.columns$Odometer[n] - hyundai.creta.required.columns$Odometer[p])>10){
      p = n
    }else{
      next()
    }
    #     print("next13")
    #     break()
    next()
  }
}

# Renaming columns in dataset
colnames(newdf1) <- c("avg.engine.temp",
                      "avg.RPM",
                      "avg.airflow",
                      "total.hours.travelled",
                      #   "avg.intake.temp",
                      "avg.power.vltg",
                      "avg.rsq",
                      #   "avg.throttle",
                      "total.idle.time",
                      "hrsh.accleration",
                      "hrsh.brake",
                      "max.speed",
                      "min.speed",
                      "avg.speed",
                      "fuel.consumed",
                      "p",
                      "n")

newdf1$p <- NULL
newdf1$n <- NULL
data <- newdf1

# Loading required libraries
##install.packages("plotly")
library(plotly)
##install.packages("lmtest")
library(lmtest)
##install.packages("usdm")
library(usdm)
##install.packages("dplyr")
library(dplyr)
##install.packages("reshape2")
library(reshape2)
##install.packages("corrplot")
library(corrplot)
#install.packages("randomForest")
library(randomForest)
options(warn=-1)

# Seeing top records of data;head() returns top 5 records by default
head(data)

# Checking dimension of data
dim(data)

# Checking number of missing values in data
colMeans(is.na(data))

# Analyzing distribution of fuel consumption 
plot_ly(x = data$fuel.consumed, type = "histogram") %>% 
  layout(xaxis =x <- list(title = "Fuel Consumed") ,
         yaxis = list(title = "Count"))

# Analyzing distributions of each variable
d <- melt(data[,c(1:8)])
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

# By visual analysis we see that the column - "avg.airflow" has no variance at all and
# hence we remove this column from further analysis
data$avg.airflow <- NULL

corrmat <- cor(data,use =  "pairwise.complete.obs")
corrplot(corrmat, method="circle")
#install.packages("GGally")
library(GGally)
ggpairs(data)

options(warn=-1)

# Adding a column called - "harsh_break_harsh_acc" which shows the interaction between harsh breaking and harsh acceleration
data$harsh_break_harsh_acc <- data$hrsh.accleration * data$hrsh.brake 

data[,colnames(data)[!colnames(data) %in% "fuel.consumed"]] <- apply(data[,colnames(data)[!colnames(data) %in% "fuel.consumed"]],2, function(x) (x-min(x))/(max(x)-min(x)))

# dividing data into train and test
smp_size <- floor(0.70 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]
#install.packages("caret")
library(caret)
set.seed(110)  
id <- createDataPartition(data$fuel.consumed, p=0.7, list = FALSE)
dim(id)
train <- data[id,]
dim(train)

test <- data[-id,]
dim(test)

dim(train)

# Building model with all the variables
set.seed(63)
model_lm <- lm(formula =fuel.consumed ~. ,data = train )
summary(model_lm)
predtest_lm <- predict(model_lm,newdata = test)
rmse_lm <- sqrt( sum((test$fuel.consumed - predtest_lm)^2 , na.rm = TRUE) / nrow(test))
print(paste0("Rmse of linear regression model is : ",rmse_lm))

rsquare_traindata.model_lm  = summary(model_lm)$adj.r.squared
rsquare_traindata.model_lm

SSE.model_lm.Test <- sum((test$fuel.consumed - predtest_lm)^2)
SST.Test <- sum((test$fuel.consumed - mean(test$fuel.consumed))^2)
rsquare.testdata.model_lm <- 1- SSE.model_lm.Test/SST.Test
print(paste0("R square of linear regression model for test data is :",rsquare.testdata.model_lm))
##install.packages("car")
library(car)
multicol <- car::vif(model_lm)
multicol <- as.data.frame(multicol)
colnames(multicol)[which(names(multicol) == "multicol")] <- "VIF"
multicol

options(warn=-1)

all_independent_vars <- colnames(data)[!colnames(data) %in% "fuel.consumed"]
all_independent_vars

# Removing variables with vif coeffficient higher than 5 one by one 
iteration <- 1
variance_cont_vars1 <- data[,all_independent_vars]
while((max(multicol$VIF,na.rm = TRUE) >= 5)){
  iteration <- iteration + 1
  varmaxcollinear <- multicol$Variables[which(multicol$VIF == max(multicol$VIF,na.rm = TRUE))]
  variance_cont_vars1 <- variance_cont_vars1[!(colnames(variance_cont_vars1) %in% varmaxcollinear)]
  multicol <- usdm::vif(variance_cont_vars1)
  print(iteration)
  print(multicol)
}

print("Variables left after removing multicollinearity")
multicol$Variables

# Taking variables with no multicollinearity
selected_features <- c("avg.engine.temp" ,"avg.RPM", "avg.power.vltg", "avg.rsq" ,"total.idle.time" ,"hrsh.accleration", "hrsh.brake"
                       , "max.speed", "min.speed")

# Building model with all the variables
set.seed(64)
model_lm_1 <- lm(formula =fuel.consumed ~. ,data =train[,c(selected_features,'fuel.consumed')] )
summary(model_lm_1)
predtest_lm_1 <- predict(model_lm_1,newdata = test)
rmse_lm_1 <- sqrt( sum((test$fuel.consumed - predtest_lm_1)^2 , na.rm = TRUE) / nrow(test))
print(paste0("Rmse of linear regression model is : ",rmse_lm_1))

rsquare_traindata.model_lm_1  = summary(model_lm_1)$adj.r.squared
rsquare_traindata.model_lm_1

SSE.model_lm.Test_1 <- sum((test$fuel.consumed - predtest_lm_1)^2)
SST.Test <- sum((test$fuel.consumed - mean(test$fuel.consumed))^2)
Rsquare.testdata.model_lm_1 <- 1- SSE.model_lm.Test_1/SST.Test
print(paste0("R square of linear regression model(after dealing with multicollinearity) for test data is :",Rsquare.testdata.model_lm_1))

step(model_lm,direction="both")

# Building model based on stepwise regression
set.seed(64)
model_lm_2 <- lm(formula = fuel.consumed ~ avg.RPM + total.hours.travelled + 
                   avg.power.vltg + total.idle.time + hrsh.brake + max.speed + 
                   avg.speed, data = train)
summary(model_lm_2)
predtest_lm_2 <- predict(model_lm_2,newdata = test)
rmse_lm_2 <- sqrt( sum((test$fuel.consumed - predtest_lm_2)^2 , na.rm = TRUE) / nrow(test))
print(paste0("Rmse of linear regression model is : ",rmse_lm_2))

rsquare_traindata.model_lm_2  = summary(model_lm_2)$adj.r.squared
rsquare_traindata.model_lm_2

SSE.model_lm.Test_2 <- sum((test$fuel.consumed - predtest_lm_2)^2)
SST.Test <- sum((test$fuel.consumed - mean(test$fuel.consumed))^2)
Rsquare.testdata.model_lm_2 <- 1- SSE.model_lm.Test_2/SST.Test
print(paste0("R square of linear regression model(after stepwise regression) for test data is :",Rsquare.testdata.model_lm_2))

# Iteration 2-random forest
set.seed(59)
model_rf <- randomForest(fuel.consumed~.,data = train[,c(all_independent_vars,'fuel.consumed')],ntree = 7)
model_rf
predtest_rf <- predict(model_rf,test)
rmse_rf <- sqrt( sum((test$fuel.consumed - predtest_rf)^2 , na.rm = TRUE) / nrow(test))
print(paste0("Rmse(test data) of random Forest model is : ",rmse_rf))

# R square 
predtest_rf_train <- predict(model_rf,train)
SSE.model_rf.Train <- sum((train$fuel.consumed - predtest_rf_train)^2)
SST.Train <- sum((train$fuel.consumed - mean(train$fuel.consumed))^2)
rsquare_traindata_model_rf <- 1- SSE.model_rf.Train/SST.Train
print(paste0("R square of random forest model for train data is :",rsquare_traindata_model_rf))

# Checking important variables in random forest model
varImpPlot(model_rf)

# R square
SSE.model_rf.Test <- sum((test$fuel.consumed - predtest_rf)^2)
SST.Test <- sum((test$fuel.consumed - mean(test$fuel.consumed))^2)
rsquare_testdata_model_rf <- 1- SSE.model_rf.Test/SST.Test
print(paste0("R square of random forest model for test data is :",rsquare_testdata_model_rf))

Metric_1 <- c("Linear regression model",rsquare_traindata.model_lm,rsquare.testdata.model_lm,rmse_lm)
Metric_2 <- c("Linear regression model after multicollinearity treatment",rsquare_traindata.model_lm_1,Rsquare.testdata.model_lm_1,rmse_lm_1)
Metric_3 <- c("Linear regression model after stepwise Regression",rsquare_traindata.model_lm_2,Rsquare.testdata.model_lm_2,rmse_lm_2)
Metric_4 <- c("Random Forest",rsquare_traindata_model_rf,rsquare_testdata_model_rf,rmse_rf)

modelstats <- rbind(Metric_1, Metric_2, Metric_3, Metric_4)
colnames(modelstats) <- c("Model","R_square_training_data","R_square_testing_data", "RMSE_testing_data")
#row.names(modelstats) <- NULL
modelstats

