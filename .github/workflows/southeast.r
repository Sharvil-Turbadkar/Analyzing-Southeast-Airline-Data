################################################
# IST 687, Standard Homework Heading
#
# Project 
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor and internet resources.

vector<-scan("C:/Users/yshobhav.AD.012/Desktop/Data.txt",character(0),sep = "\n")
# Maually assigning dataset to a json Class
class(vector)<-"json"
jsonlite::prettify(vector)

install.packages("jsonlite")
require(jsonlite)

# Converting to data frame fro JSON format
df<-jsonlite::fromJSON(vector)

View(df)
# Calculating NA values across all attributes
sapply(df,function(x)sum(is.na(x)))

# The following attributes have NA values 

# Likelihood to recommend
# Flight Time in Minutes
# Freetext
# Departure.Delay.in.Minutes 
# Arrival.Delay.in.Minutes 

#6531
head(df$Likelihood.to.recommend)

which(is.na(df$Likelihood.to.recommend))
# 6531 index value is na 

table(df$Class)
str(df$Class)

# Replacing NA values

################# Departure.Delay.in.Minutes and Arrival.Delay.in.Minutes #########
View(df$Departure.Delay.in.Minutes)


#Replacing  na values by MODE which is zero as there are many cases where there are no delays 
?cor

cor(df$Arrival.Delay.in.Minutes,df$Departure.Delay.in.Minutes,use = "pairwise.complete.obs")
# High Correlation of 95.9% is there

lm(df$Arrival.Delay.in.Minutes~df$Departure.Delay.in.Minutes,data = df)
lm(df$Departure.Delay.in.Minutes~df$Arrival.Delay.in.Minutes,data = df)
table(is.na(df$Departure.Delay.in.Minutes))
# 
# Temporaray data frame
df1<-data.frame(df)

for(i in 1:nrow(df1))
{
  if((is.na(df1$Arrival.Delay.in.Minutes[i])=="TRUE")&&(is.na(df1$Departure.Delay.in.Minutes[i]))=="TRUE")
    # If both arrival and departure times are not mentioned
  {
    df1$Arrival.Delay.in.Minutes[i]=0
    df1$Departure.Delay.in.Minutes[i]=0
  }
  if(is.na(df1$Arrival.Delay.in.Minutes[i])=="TRUE")
    # If Arrival time is NA
  {
    df1$Arrival.Delay.in.Minutes[i]=0.8066+0.9881 *df1$Departure.Delay.in.Minutes[i] 
  }
  if(is.na(df1$Departure.Delay.in.Minutes[i])=="TRUE")
    # If Departure Time is NA
  {
    df1$Departure.Delay.in.Minutes[i]=0.4808+0.9314*df1$Arrival.Delay.in.Minutes[i]
  }
}
# To check whether all na values are removed 

sum(is.na(df1$Arrival.Delay.in.Minutes))
sum(is.na(df1$Departure.Delay.in.Minutes))


# df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes))]<- 0
# df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes))]<-0
mean(df1$Departure.Delay.in.Minutes)
# Mean Departure Delay is 15.22 minutes
mean(df1$Arrival.Delay.in.Minutes)
# Mean Arrival Delay is 15.84 minutes

######### Flight Time in Minutes##########
table(is.na(df$Flight.time.in.minutes))
which(is.na(df$Flight.time.in.minutes))

# Correlation between Flight.time.in.minutes and Flight.Distance is very high 
# thus we can make use of linear regression

cor(df1$Flight.time.in.minutes,df1$Flight.Distance,use = "pairwise.complete.obs")
m<- lm(df1$Flight.time.in.minutes~df1$Flight.Distance,data = df1)

#Intercept->17.6625  
# SLope->0.1176 

for(i in 1:nrow(df1))
{
  if(is.na(df1$Flight.time.in.minutes[i])=="TRUE")
  {
    df1$Flight.time.in.minutes[i]=17.6625+0.1176*df1$Flight.Distance[i] 
  }
}

# To check NA values 
sum(is.na(df1$Flight.time.in.minutes))
mean(df1$Flight.time.in.minutes)


#Creating a Numeric attribute for FLight Cancellation 
for(i in 1:nrow(df1))
{
  if(df1$Flight.cancelled[i]=="Yes")
  {
    df1$Flight.cancelled.num[i]=1
  }
  if(df1$Flight.cancelled[i]=="No")
  {
    df1$Flight.cancelled.num[i]=0
  }
}

table(df1$Flight.cancelled.num)

################ Likelihood to recommend#########
# No strong correlation could be found
cor(df1$Likelihood.to.recommend,df1$Type.of.Travel,use = "pairwise.complete.obs")

str(df1$Type.of.Travel)

# Replacing NA value of Likelihood to recommnd by mean value
df1$Likelihood.to.recommend[which(is.na(df1$Likelihood.to.recommend))]<-mean(df1$Likelihood.to.recommend,na.rm=T)
View(df1$Likelihood.to.recommend)
##Inferences  by attributes 
# Age
df.age<-df1%>% group_by(df1$Age_Buckets)%>%
  summarize(mean=mean(CustomerType_num),count=n())

df.age
#Loyalty
df.loyalty<-df1%>% group_by(df1$Loyalty_Buckets)%>%
  summarize(mean=mean(CustomerType_num))

df.age
View(df.age)
View(df.loyalty)

NPS_Score

install.packages("ggplot2")
require(ggplot2)
require(dplyr)

ageplot<-ggplot(df.age,aes(x=df.age$`df1$Age_Buckets`,y=df.age$mean,fill=df.age$`df1$Age_Buckets`))+geom_bar(stat = "identity",color="white")
ageplot<-ageplot+ggtitle("Distribution of scores by age ")+xlab("Age")+ylab("Score")
ageplot


# Age dot Plot
df.agedot<-df1%>%group_by(Age)%>%summarise(count=n(),mean=mean(Likelihood.to.recommend))
df.agedot

# Plots how age  affects  Likelihood to Recommend by using a  dot plot with a line to capture the effect of trend 
agedotplot<-ggplot(df.agedot,aes(x=Age,y=mean))
agedotplot<-agedotplot +geom_line()+geom_point(aes(size=count))
agedotplot<-agedotplot+ggtitle("Distribution of scores by age ")+xlab("Age")+ylab("Likelihood to recommend")
agedotplot


# Plots how loyalty affects the Likelihood to Recommend

loyalty_plot<-ggplot(df.loyalty,aes(x=df.loyalty$`df1$Loyalty_Buckets`,y=df.loyalty$mean,fill=df.loyalty$`df1$Loyalty_Buckets`))+geom_bar(stat = "identity",color="white")
loyalty_plot<-loyalty_plot+ggtitle("Distribution of scores by Loyalty")+xlab("Loyalty")+ylab("Score")
loyalty_plot

# NPS score is higehest for people from 25-58 years
# Else Likelihood to recommend is less

df.flight_Cancelled<-df1 %>%group_by(Flight.cancelled)%>%summarize(mean=mean(Likelihood.to.recommend))
df.flight_Cancelled

# How cancelling a flight affects Lieklihood to recommend 
flight.cancel.plot<-ggplot(df1,aes(y=Likelihood.to.recommend,x=Flight.cancelled,fill=Flight.cancelled))+geom_boxplot()
flight.cancel.plot<-flight.cancel.plot+ ggtitle("Effect of Fight Cancellation on Likelihood to recommend")
flight.cancel.plot


# To plot airline status and how passengers aboard every airline recommend 
df.AirlineStatus<-df1 %>%group_by(Airline.Status)%>%summarize(mean=mean(Likelihood.to.recommend))
df.AirlineStatus

AirlineStatus.plot<-ggplot(df.AirlineStatus,aes(y=df.AirlineStatus$mean,x=df.AirlineStatus$Airline.Status,fill=Airline.Status))+geom_bar(stat="identity")
AirlineStatus.plot<-AirlineStatus.plot+ ggtitle("Airline Status effect on Likelihood to recommend")+xlab("Airline Status")+ylab("Likelihood to Recommend")
AirlineStatus.plot
# 
# How Price Sensitivity affects passengers and tehir lieklihood to recommend

df.Price<-df1 %>%group_by(Price.Sensitivity)%>%summarize(mean=mean(Likelihood.to.recommend))
df.Price  

Price.plot<-ggplot(df.Price,aes(y=df.Price$mean,x=df.Price$Price.Sensitivity,fill=df.Price$Price.Sensitivity))+geom_bar(stat="identity")
Price.plot<-Price.plot+ ggtitle("Price Sensitvity effect ")+xlab("Price Sensitivity")+ylab("Likelihood to Recommend")
Price.plot


View(df1)
?geom_boxplot
View(df1)
#No clear Trend

###Association Rules#########
install.packages("kernlab")
library(kernlab)
library(dplyr)

##Support Vector Machine
unique(df1$Likelihood.to.recommend)

# Bucketed the customers into Detractors and Attractors 
df1$CustomerType<-(cut(df1$Likelihood.to.recommend,breaks = c(0.5,floor(mean(df1$Likelihood.to.recommend)),max(df1$Likelihood.to.recommend)),labels = c("Detractors","Promoters")))
table(df1$CustomerType)
table(is.na(df1$CustomerType))

install.packages("dplyr")
library(dplyr)

# Calculating NPS Score
No_Detractors<-df1%>%filter(CustomerType=="Detractors")%>%summarise(count=n())
No_Detractors
No_Promoters<-df1%>%filter(CustomerType=="Promoters")%>%summarise(count=n())

NPS_Score<-(No_Detractors/length(df1$CustomerType))-(No_Promoters/length(df1$CustomerType))
NPS_Score
# There are more Promoters than detractors as NPS score is -0.11



# Dividing into train and test datasets
?sample_n


str(df1)

df1$Origin.City<-as.factor(df1$Origin.City)
df1$Origin.State<-as.factor(df1$Origin.State)
df1$Destination.City<-as.factor(df1$Destination.City)
df1$Destination.State<-as.factor(df1$Destination.State)
df1$Partner.Code<-as.factor(df1$Partner.Code)
df1$Airline.Status<-as.factor(df1$Airline.Status)
df1$Flight.cancelled<-as.factor(df1$Flight.cancelled)
df1$Type.of.Travel<-as.factor(df1$Type.of.Travel)

ageplot
#Bucketing Age into different Buckets
table(df1$Age_Buckets)

df1$Age_Buckets<-cut(df1$Age,breaks = c((min(df1$Age)-1),23,61,(max(df1$Age))),labels = c("Young","Adult","Old"));
df1$Loyalty_Buckets<-cut(df1$Loyalty,breaks = c((min(df1$Loyalty)-0.1),mean(df1$Loyalty),(max(df1$Loyalty)+0.1)),labels = c("Not Loyal","Loyal"));

mean(df1$Loyalty)
##SVM Creating a Random index
rand<-sample_n(df1,10282,replace = F)

View(rand)
# Cutoff point between Training and Test Dataset
cutpoint<-round(dim(rand)[1]*(3/4))
df1$Departure.Delay.in.Minutes
df1$Loyalty

mean(df1$Departure.Delay.in.Minutes)
library()
df1$Airline.Status
View(df1)
# Training and test datasets have been made
trainset<- rand[1:cutpoint,               c("Airline.Status","Flight.cancelled","Price.Sensitivity",
                                            "Arrival.Delay.in.Minutes","Departure.Delay.in.Minutes",
                                            "CustomerType","Loyalty","Age_Buckets",
                                            "Class","Type.of.Travel","Flights.Per.Year")]
testset<-rand[(cutpoint+1):(dim(rand)[1]),c("Airline.Status","Flight.cancelled","Price.Sensitivity",
                                            "Arrival.Delay.in.Minutes","Departure.Delay.in.Minutes",
                                            "CustomerType","Loyalty","Age_Buckets",
                                            "Class","Type.of.Travel","Flights.Per.Year")]
install.packages("kernlab")
library(kernlab)

svmOutput<-ksvm(CustomerType~.,data=trainset,kpar="automatic",kernal = "rbfdot", C = 5, cross = 5,
                prob.model = TRUE)

print(svmOutput)
str(trainset)
str(testset)

svmOutput
# Cross Validation error is 22.6%
# Now Predicting for test dataset
svmpred<-predict(svmOutput,testset,type="votes")
# Creating a confusion Matrix

confusionMatrix<- data.frame(testset$CustomerType,svmpred[2,])
confusionMatrixTable<- table(confusionMatrix)

confusionMatrixTable
errorRate<-((confusionMatrixTable[1,2]+confusionMatrixTable[2,1])/sum(confusionMatrixTable))
errorRate

accuracyRate<-((confusionMatrixTable[1,1]+confusionMatrixTable[2,2])/sum(confusionMatrixTable))
accuracyRate
# Accuracy is 77.8%
#plotting Confusion Matrix
CustomerType <- factor(c(0, 0, 1, 1))
CustomerType_Predict <- factor(c(0, 1, 0, 1))

confusionMatrixTable[2,1]
Y      <- c(
  confusionMatrixTable[1,1], 
  confusionMatrixTable[2,1],
  confusionMatrixTable[1,2], 
  confusionMatrixTable[2,2])

confusionMatrixPlot <- data.frame(CustomerType,CustomerType_Predict, Y)

confusionMatrixPlot

svmPlot<- ggplot(data = confusionMatrixPlot, mapping = aes(x = CustomerType, y = CustomerType_Predict)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_color_gradientn (colors="green")+
  theme_bw() + theme(legend.position = "none")


# Model has correctly predicted 1292 customers as Promoters and falsely predicted 436 Promoters as detractors
# Model has correctly predicted 708 customers as Detractors and falsely predicted 134 Detractors as Promoters

svmPlot

svmOutput


# Building a correlation matrix
install.packages("corrr")
require(corrr)
require(dplyr)
table(df1$CustomerType)

for(i in 1:nrow(df1)){
  if(df1$CustomerType[i]=="Detractors")
  {
    df1$CustomerType_num[i]=0
  }
  if(df1$CustomerType[i]=="Promoters")
  {
    df1$CustomerType_num[i]=1
  }
}
df_num<- df1[,sapply(df1, function(x) is.numeric(x))]

View(df_num)

install.packages("corrplot")
library(corrplot)
cor<-cor(df_num)

corrplot(cor,method = "circle")
View(df1)
##Converting numeric to non numeric data
sapply(df1,function(x)is.numeric(x))

head(df1$Age)

df1$Destination.City
df1$Destination.City<-as.numeric(df1$Destination.City)
df1$Origin.City<-as.numeric(df1$Origin.City)
df1$Airline.Status<-as.numeric(df1$Airline.Status)
df1$Gender<-as.numeric(df1$Gender)
df1$Type.of.Travel<-as.numeric(df1$Flight.date)
df1$Partner.Code<-as.numeric(df1$Partner.Code)
df1$Partner.Name<-as.numeric(df1$Partner.Name)
df1$Class<-as.numeric(df1$Class)
df1$Origin.State<-as.numeric(df1$Origin.State)
df1$CustomerType<-as.numeric(df1$CustomerType)
df1$Destination.State<-as.numeric(df1$Destination.State)

View(df1)

# 
# -----------------Random Forest------------

# install package Random Forest 
install.packages("randomForest")
library(randomForest)

#Select required attributes for using in Random Forest Model
Random_Forest<- df1[c(3,4,5,6,7,9,10,13,14,22,23,27)]

str(Random_Forest) 

#Convert all attributes to numeric and factors 
Random_Forest$Gender<- as.factor(Random_Forest$Gender)
Random_Forest$Age<- as.numeric(Random_Forest$Age)
Random_Forest$Loyalty<- as.numeric(Random_Forest$Loyalty)
Random_Forest$Price.Sensitivity<- as.numeric(Random_Forest$Price.Sensitivity)
Random_Forest$Departure.Delay.in.Minutes<- as.numeric(Random_Forest$Departure.Delay.in.Minutes)
Random_Forest$Year.of.First.Flight<- as.numeric(Random_Forest$Year.of.First.Flight)
Random_Forest$Eating.and.Drinking.at.Airport<- as.numeric(Random_Forest$Eating.and.Drinking.at.Airport)
Random_Forest$Class<- as.factor(Random_Forest$Class)

#Use likelihood to recommend as condition
#data as Random_Forest
#Ntree: Number of trees to grow. We use ntree as 100
#Mtry: Number of variables randomly sampled as candidates at each split. We use Mtry as 3

randomforest_classifier = randomForest(Likelihood.to.recommend ~ ., data=Random_Forest, ntree=100, mtry=3, importance=TRUE)
randomforest_classifier

#Accuracy=53.7%

-----------------------------------------------------
  #new test for code 
  
  #select client data 
  ClientData <- df1[df1$Partner.Name == 'Southeast Airlines Co.', ]
ClientData <-subset(ClientData, select = -c(Partner.Name,Partner.Code))
#sample data function 

SamF <- function(data, a) {
  smp_size <- floor(a * nrow(data))
  train_ind <- sample(nrow(data), size = smp_size)
  train <- data[train_ind,]
  test <- data[-train_ind,]
  return(list(train, test))
}

#Remove SATISFACTION



a <- data.frame(lapply(ClientData, as.factor))

a <- a[, -1]



## Fit MCA


install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)


res.mca <- MCA(a)

res = dimdesc(res.mca, axes = 1:2, proba = 0.05)

res$`Dim 1`$quali



#

###

ind <- get_mca_ind(res.mca)

var <- get_mca_var(res.mca)

# Coordinates

head(var$coord)

# Cos2: quality on the factore map

head(var$cos2)

# Contributions to the principal components

head(var$contrib)



summary.MCA(res.mca)



## eigvalue

eig.val <- res.mca$eig

barplot(
  
  eig.val[, 2],
  
  names.arg = 1:nrow(eig.val),
  
  main = "Variances Explained by Dimensions ",
  
  xlab = "Principal Dimensions",
  
  ylab = "Percentage of variances",
  
  col = "saddlebrown"
  
)

# Add connected line segments to the plot

lines(
  
  x = 1:nrow(eig.val),
  
  eig.val[, 2],
  
  type = "b",
  
  pch = 19,
  
  col = "red"
  
)





fviz_eig(res.mca)

fviz_mca_biplot(res.mca)

fviz_mca_ind(res.mca)

fviz_mca_var(res.mca)

fviz_mca_var(res.mca, choice = "mca.cor",
             
             repel = TRUE, # Avoid text overlapping (slow)
             
             ggtheme = theme_minimal())

#these two functions have a loading error they do not load and generates an error

fviz_mca_var(res.mca,
             
             repel = TRUE, # Avoid text overlapping (slow)
             
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2",
             
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             
             repel = TRUE, # Avoid text overlapping
             
             ggtheme = theme_minimal())



# Contributions of rows to dimension 1

fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)

# Contributions of rows to dimension 2

fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)



########################**********association rule minig********################



library(arules)

library(arulesViz)



ARData <- ClientData



ARData$CustomerType<-(cut(ARData$Likelihood.to.recommend,breaks = c(0.5,floor(mean(ARData$Likelihood.to.recommend)),max(ARData$Likelihood.to.recommend)),labels = c("Detractors","Promoters")))

vBuckets <- replicate(length(ARData$CustomerType), "Detractors")

vBuckets[ARData$CustomerType >3] <- "Y"

ARData$CustomerType <- vBuckets



for (i in 1:ncol(ARData)) {
  
  ARData[, i] <- factor(ARData[, i])
  
}

train <- SamF(ARData, 0.7)[[1]]

test <- SamF(ARData, 0.7)[[2]]

ARDataX <- as(train, "transactions")

inspect(ARDataX[1:5])

summary(ARDataX)

itemFrequencyPlot(ARDataX, topN = 10, horiz = T)

image(sample(ARDataX, 100))



RuleDF <-apriori(ARDataX ,parameter = list(support = 0.1, confidence = 0.8),appearance = list(rhs = c('CustomerType=Detractors', 'CustomerType=Promoters'),default = 'lhs'))

RuleDF <-apriori(train ,parameter = list(support = 0.2, confidence = 0.8),appearance = list(rhs = c('CustomerType=Detractors', 'CustomerType=Promoters'),default = 'lhs'))



inspect(RuleDF [1:20])



ordered_rules <- sort(RuleDF, by = "lift")

inspect(ordered_rules[1:20])
# Creating dataset for circular boxplot
df1$Partner.Name
unique(df1$Partner.Name)

df.Airline <- df1 %>%group_by(Partner.Name)
df1$Likelihood.to.recommend
z <- summarize(df.Airline, mediansatisfaction = median(Likelihood.to.recommend), count = n())
#b <- z[order(-z$count),]
b <- z
b$Partner.Name
b$Partner.Name<- as.character(b$Partner.Name)
b$Partner.Name[b$Partner.Name == "Cheapseats Airlines Inc."] <- c("Cheapseat")
b$Partner.Name[b$Partner.Name == "Northwest Business Airlines Inc."] <- c("Northwest Business")
b$Partner.Name<- as.factor(b$Partner.Name)

#unique(b$AirlineName)

data = data.frame(id=seq(1,14), individual = paste(b$Partner.Name, sep=""), value= b$count)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data=data

# calculate the ANGLE of the labels
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #

# Start the plot
p = ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", aes(fill= b$mediansatisfaction))+ guides(fill = guide_legend(title = "Satisfaction"))+    #alpha("skyblue", 0.7)) +
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-1000,3000) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data$angle, inherit.aes = FALSE )
p

# Circular Boxplot
# Thus we conclude that Cheapset airline has most number of customers
# Flyfast Airways has customers with most promoters 
