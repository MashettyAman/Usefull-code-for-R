# useful codes for Analysis in R

#reshape2() for long to wide/ wide to long format data purpose
library(reshape2)
person<-c("Sankar","Aiyar","Singh")
age<-c(26,24,25)
weight<-c(70,60,65)

wide<-data.frame(person,age,weight)
wide

melt(wide)
melt(wide,id.vars="person")

melt(wide,id.vars="person",variable.names="Demographics",value.name ="Demo_Value" )
melted<-melt(wide,id.vars="person",variable.names="Demographics",value.name ="Demo_Value" )

dcast(melted,person~variable,value.var = "Demo_Value")


#String manipulation
a<-"Batman"
substr(a,start=2,stop=6)
nchar(a)
tolower(a)
toupper(a)
b<-"Bat-Man"
strsplit(b,split="-")
c<-"Bat/Man"
strsplit(c,split="/")
paste(b,split=c)
grep("-",b)
grepl("/",c)
sub("-","/",b)
d<-"Bat-Ma-n"
sub("-","/",d)
gsub("-","/",d)


# to convert all the charater into factor
data[sapply(data,is.character)] <- 
  lapply(data[sapply(data,is.character)],as.factor)


# for convert all int into factor
cols <- c("season","weather","holiday","workingday")

for (i in cols) {
  data[,i] <- as.factor(data[,i])
  
}



# Features analysis
# Let's represent the features. We will convert the factor data in numeric data.

m.data = mushrooms[,2:22]
m.class = mushrooms[,1]
m.data <- sapply( m.data, function (x) as.numeric(as.factor(x)))



# removing the duplicate values based on the columns
df3 <-  ipl[!duplicated(ipl$user_name),]


# removing redununt columns
data <- data[,-which(names(data) %in% c(""))]

# or
# droping the dependent var, for checking the multicolinarity
mtcars_a <- subset(mtcars,select = -(mpg))

# select only the numeric variables
numericData <- mtcars_a[sapply(mtcars_a,is.numeric)]

# select only factor variable
factVar <- carp[sapply(carp,is.factor)]



# removing the extra special chr/r word in the perticular col
data$Value <- sapply(strsplit(as.character(data$Value),"â,¬"),tail,1)



# to missing value imputation with next column
n <- length(data$which_variable_u_impute)
for (i in 1:n) {
if(is.na(data$which_variable_u_impute[i]))
  data$which_variable_u_impute[i] <- data$from_which_u_import[i]
}



# changing the row value imputation with mode
# data$x[data$x=="previous name"] <- "the current name"
carp$drive.wheels[carp$drive.wheels=="4wd"] <- "fwd"

# Mode imputation
data$var[data$var==""] <- "ModeValue"
carp$engine.location[carp$engine.location==''] <- 'front'



# Mean imputation
data$var[is.na(data$var)] <- mean(data$var,na.rm = T)
carp$bore[is.na(carp$bore)] <- mean(carp$bore,na.rm = T)



# check the missing values
sum(is.na(car))
sapply(car,function(x) sum(x==""))
sapply(car,function(x) sum(is.na(x)))

colSums(is.na(car))


Percentage_missing=
  round(colSums(is.na(data[,colnames(zom)
                          [colSums(is.na(data))>0]]))/nrow(data)*100,2)
Percentage_missing

#if their remove with complete.cases
zom <- zom[complete.cases(zom),]

#Random sampling
sample_n(df,5)



aggregate(oj$price,by=list(oj$brand),mean)
tapply(oj$price,oj$brand,mean)
xtabs(oj$INCOME~oj$brand+oj$feat)

# data cleaning

# the select function choose specific
# columns and put htem in a iven order

library(tidyverse)

# seperate based on the character "-"

dat <- dat %>% mutate(no_of_mugs = 
                        case_when(date_cleaned == as.date("2018-07-23") ~
                                    separate(no_of_mugs,c("lower","upper"),"_") %>% 
                                    NA_character_,TRUE ~ gsub("\\(.*","",no_of_mugs))) %>% 
  mutate(no_of_mugs = case_when(!is.na(upper) == TRUE ~ (as.numeric(lower)+as.numeric(upper))/2,TRUE~as.numeric(lower))) 


# mutate missing values, and  modify the dataframe 

df <- df %>% mutate(newcharges = replace(monthlycharges,
                                         is.na(monthlycharges),
                                         median(monthlycharges,na,rm = T)))


# counting unique, missing and median values
df %>% summarise(n=n_distinct(monthlycharges),
                 na = sum(is.na(monthlycharges)),
                 med = median(monthlycharges,na.rm = T))


# replacing with standard missingvlaue type, NA
df <- df %>% mutate(totalchar = 
                      replace(totalchar,total-charges =='na',NA)) %>% 
  mutate(totalchar = 
           replace(totalchar, totalcharges == 'N/A',NA))  


# the 'parse_date_time' function converts date written in different form 
# converts string into UTC

dat <- dat %>% 
  mutate(date_cleaned = parse_date_time(date_cleaned,"dmy"))



# the str detect function allows choosen words to be deteccted
# within a string the is.na function identifies if a values is na

dat <- dat %>% 
  mutate(clean_sides = 
           case_when(str_detect(sides_and_sink, c("side|both")) ==
                       TRUE~TRUE,
                     is.na(sides_and_sink)==TRUE~NA,
                     TRUE~FALSE),
         clean_sink=case_when(str_detect(sides_and_sink,c(sink|both))==
                                TRUE~TRUE,
                              is.na(sides_and_sink)==TRUE~NA,
                              TRUE~FALSE))


# the str_replace_all function with the option removes special chrs

dat <- dat %>% mutate(new_c = str_replace_all(toupper(col), "[[:punct:]]",""))


# making all uppercase and lower case
data$col <- toupper(data$col)

data$col <- tolower(data$col)

# trimmig the white space
data$col <- str_trim(data$col)

# replace the "not provided' with "not available'

data$col <- str_replace(data$col,'not_provided',
                        "not_available")

# replacing the outlier with perticular column with median
vec1 <- boxplot.stats(data$col)$out

data$col[data$col %in% vec1] <- median(data$col)


negs <- subset(mydata,mydata[11]>0)
noOutliers <- subset(noNegs,noNegs[11]<1500)

noOutliers["Age"] <- as.numeric(noOutliers["Age"])


Mydata$date <- paste(substr(mydata$date,6,7), 
                     substr(mydata$date,9,10),
                     substr(mydata$date,1,4),sep = "/")
# class(mydata$date)
"Character"

# convert the gbp values to USD

Mydata$val <- mydata$val*1.4


# function
getRate <- function(arg){
  if(arg=="GPB"){
    myRate <- 1.4
  }
  if (arg=="CAD") {
    myRate <- 1.34
  }
  return(myRate)
}

mydata$val <- mydata$val * getRate("GPB")



df %>% distinct(monthlycharges)


# check the outlier in the data by visualising
boxplot(data,horizontal = F,axes = T,outline = TRUE,las =2, col = c("grey","blue","yellow",
                                                                    "green","orange","gold","red","white"))


Variables <- names(audit)
Miss <- 1:ncol(audit)
Percentage<-1:ncol(audit)
Class<-names(audit)
Min<-1:ncol(audit)
Max<-1:ncol(audit)
Average<-1:ncol(audit)
UniqueL<-1:ncol(audit)


Missing_Count <- list(Variables,Miss,Percentage,Class,Min,Max,Average,UniqueL)

for(count in 1:ncol(audit))
{
  Missing_Count[[2]][count]<- sum(is.na(audit[count]))
  Missing_Count[[3]][count]<-sum(is.na(audit[count]))/nrow(audit[count])
  Missing_Count[[4]][count]<-class(audit[,count])
  Missing_Count[[5]][count]<-ifelse(class(audit[,count])=="factor",0,min(audit[,count],na.rm=TRUE))
  Missing_Count[[6]][count]<-ifelse(class(audit[,count])=="factor",0,max(audit[,count],na.rm=TRUE))
  Missing_Count[[7]][count]<-ifelse(class(audit[,count])=="factor",0,mean(audit[,count],na.rm=TRUE))
  Missing_Count[[8]][count]<-ifelse(class(audit[,count])=="factor",length(levels(audit[,count])),0)
}
test <- data.frame(Missing_Count)
names(test) <- c("Variable_Name","Missing_Count","Percentage","Class","Min","Max","Average","Unique Levels")
test[order(test$Variable_Name),]
#write.csv(test,"dataQ")
audit$churn<-audit$TARGET_Adjusted


# missing value imputation with regression for ex

# ind <- function(t)
# {
#   x <- dim(length(t))
#   x[which(!is.na(t))]=1
#   x[which(is.na(t))]=0
#   return(x)
# }
# carp$I <- ind(carp$peak.rpm)
# symnum(cor(numericData,use = "complete.obs"))
# 
# 
# lm(peak.rpm ~ horsepower,data = carp)
# summary(lm(peak.rpm ~ horsepower,data = carp))
# 
# # missing value imputation with regression
# for (i in 1:nrow(carp)) 
#   {
#   if(carp$I[i]==0)
#   {
#     carp$peak.rpm[i]= 4994.58 + 54.31  * carp$horsepower[i]
#   }
# }
# 
# summary(carp$peak.rpm)

sum(is.na(carp))


# Knn imputation for missing value
# for that we need VIM library
library(VIM)

# create a new dataset 
df1 <- kNN(df,variable = c("var1","var2"),k=6)

summary(df1)
# now check the summary no missing value if more than 10 col has missing value

df1 <- kNN(df)

summary(df1)

# we see all the col have duplicated to rm that 
df1 <- subset(df1,select = var:var) #done


# print the blank row number
which(carp$num.of.doors=="",)


# remove  the entire rows of blank value or NA's bcoz 
# they both had same row with slight difference in it also both from renault
carp<- carp[-which(is.na(carp$horsepower)), ]


#without converting them as factor

carp$Car_Price_category <- ifelse(carp$Car_Price_category== "Lowprice","1","0")
carp$Car_Price_category <- as.factor(carp$Car_Price_category)



# recode car_price as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))



# for outlier detection
boxplot(x=car$dist,main = 'Distance',sub= paste('outlier row : ',
                                                boxplot.stats(car$dist)$out))


# OR


outlier_dete <- function(x)
{
  mu <- mean(x,na.rm = T)
  q1 <- quantile(x,0.25)[[1]]
  q3 <- quantile(x,0.75)[[1]]
  iqr1 <- IQR(x)
  ll <- q1-1.5*iqr1
  ul <- q3+1.5*iqr1
  out <- ifelse(x<ll | x>ul,"outlier","No Outlier")
  return(out)
}


boston$CRIM_out <- outlier_dete(boston$CRIM)
prop.table(table(boston$CRIM_out))


# find the deviation for mean
data$std <- sweep(data[,3],2,mean(data$Aver))
data


data <- transform(data,new = std/2.92)
data


# How to create a week 1,2,3 and 4 for any dataset baes on the days col
library(lubridate)
# here D means Dataset


w1 = d[d['Day']<=7]
w2 = d[(d['Day']>7) & (d['Day']<=14)]
w3 = d[(d['Day']>14) &(d['Day']<=21)]
w4 = d[(d['Day']>21) &(d['Day']<=31)]

d5 <- d %>% select(Day) %>% filter(Day <=7)
d6 <- d %>% select(Day) %>% filter(Day > 7 & Day<=14)
d7 <- d %>% select(Day) %>% filter(Day > 14 & Day<=21)
d8 <- d %>% select(Day) %>% filter(Day > 21 & Day<=31)

d5$weeksday <- d5[d5$weeksday<=7] <- "w1"
d6$weeksday <- d6[d6$weeksday<=14] <- "w2"
d7$weeksday <- d7[d7$weeksday<=21] <- "w3"
d8$weeksday <- d8[d8$weeksday<=31] <- "w4"



# after creating the all the columns we need rowbind it 
dd <- rbind(d5,d6,d7,d8)

# now we can check the  min value
sapply(split(DD$Day,DD$weeksday),min)


# for seperate the one column into two
# ex col has ground and state same col m
Ground_Data <- separate(Ground_Data,Ground,into = c("Ground","State"),sep = "-")

# remove white space in the columns
Ground_Data$State <- gsub(" ","",Ground_Data$State)


indground$Ground
Wc_venue_pitches <- data.frame(Country = c("Eden Gardens Kolkata ","Delhi"))

# merging the ODI scores data with england stadium
WC_Grounds_History <- merge(Data,Wc_venue_pitches,by.x = "Ground",
                            by.y = "Country",all.x = F)

# if have blank values in the blank values use grep function
# WC_Grounds_History<- WC_Grounds_History[!grep("-","",WC_Grounds_History$Result),]

# let us divided by the pitches and see the history
win_by_pitches <- WC_Grounds_History %>% 
  group_by(Inns,Ground) %>% 
  filter(Result == 'won') %>% summarise(n=n())

win_by_pitches <- spread(win_by_pitches,Inns,n)


# to create/replace the col names
cols <- c("Ground",'inn1','inn2')
colnames(win_by_pitches) <- cols

# or
names(df2) <- c('date','pure_gold_24k','standard_gold_22k')


# to divide the row wise percentage
win_by_pitches <- WC_Grounds_History %>% 
  group_by(Inns,Ground) %>% 
  filter(Result == 'won') %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n/rowSums(win_by_pitches[2:3]))*100) %>% 
  arrange(desc(n))



# for each carrier calculates the which two days of year had their longest
# departure delay
flights %>% 
  group_by(UniqueCarrier) %>% 
  select(Month,DayofMonth,DepDelay) %>% 
  filter(min_rank(desc(DepDelay))<=2) %>% 
  arrange(UniqueCarrier,desc(DepDelay))
# or
flights %>% 
  group_by(UniqueCarrier) %>% 
  select(Month,DayofMonth,DepDelay) %>% 
  top_n(2) %>% 
  arrange(UniqueCarrier,desc(DepDelay))

# for each month calculate the no of flights and change from the previous month
flights %>% 
  group_by(Month) %>% 
  summarise(flights_count=n()) %>% 
  mutate(change=flights_count-lag(flights_count))


# for each carrier calculated the min and max arrival and departure delay
flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(min(.,na.rm = T),max(.,na.rm = T)),matches("Delay"))

#for each carrier, calculate the percentage of flights cancelled and diverted
flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(min(na.rm = T),max(na.rm = T),mean()),mpg,hp)



train <-
  train %>%
  mutate(Year  = factor(year(Dates), levels=2003:2015),
         Month = factor(month(Dates), levels=1:12),
         Day   = day(Dates),
         Hour  = factor(hour(Dates), levels=0:23),
         dayDate = as.POSIXct(round(Dates, units = "days")),
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
  )



library(lubridate)

# converting the date col into a date format
milk$Date <- dmy(milk$Date)

# adding a month name into a column
milk$month <- months(milk$Date)

# checking unique months
unique(months(milk$Date))

# Adding a weekdays column
milk$week <- weekdays(milk$Date)


ggplotly(Actor %>% group_by(actorName,movieCount) %>% 
           arrange(desc(movieCount)) %>% head(10) %>% 
           ungroup() %>% mutate(actorName = reorder(actorName,-movieCount)) %>% 
           ggplot(aes(x=actorName,y=movieCount))+
           geom_bar(stat = 'identity',color = 'black',fill = 'blue')+
           theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+labs(x="Actor Name",y='movie_Name')
)


##Graph: Pickups Changing by month
ggplot(DatePickups,aes(x=Month,y=Pickups))+
  stat_summary(fun.y="mean",geom="bar",fill="royalblue4")+
  ggtitle("Ave.daily Pickups Changing by Month")+ylab(" Ave.daily Pickups")


#Total Pickups Changing by date
ggplot(DatePickups,aes(x=Date,y=Pickups))+
  geom_point(color="royalblue4")+geom_smooth(method=lm, fill='yellowgreen', 
                                             color='yellow1',se=FALSE,size=1, formula = y~x)+
  stat_poly_eq(formula = y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
               parse = TRUE) +     
  ggtitle("Total Pickups Changing by Date")+ylab("Total Pickups")
summary(lm(DatePickups$Pickups~DatePickups$Date))


ggplot(df,aes(x=reorder(make,-total),y = total,fill = make))+
  geom_bar(stat = 'identity')+theme_bw()+
  geom_text(aes(label = total),vjust = 0.7,size = 3)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+
  labs(x="Car Maker",y='Total')



# to build a pie graph 
df2 <- df %>% group_by(winner) %>% summarise(count = n()) %>%
mutate(prop = round(count/sum(count)*100,digits = 2))
df2$winner <- as.factor(df2$winner)

ggplot(df2,aes(x="", y=prop,fill = winner))+
  geom_bar(stat = 'identity',width = 3,
           color = 'black')+
  coord_polar("y", start = 0)+
  theme_void()+
  theme(legend.position = "right")+
  geom_text(aes(y=prop,label= prop),color ='white',size=5,vjust=-1.7)+
  scale_fill_brewer(palette = 'Dark2')


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# *********************************#
 # visualise the correlation
#check the correlation
descor <- cor(numericData)
descor
print(descor,digits = 3)
symnum(descor)


# visualise the correlation
library(corrplot)
corrplot(descor,order = "FPC",method = "color",type = "lower",
         cl.cex = 0.7,tl.col = rgb(0,0,0))

# finding out the highcorrelated variables
library(caret)
highlycorreleted <- findCorrelation(descor,cutoff = 0.7)
highlycorreleted

# identifying the names which is highly correlated variable
highcorcol <- colnames(numericData)[highlycorreleted]

# so their is a multicolinarity
print(highcorcol)
# carb variable have only 0.6 correlation with others

# remove highly correlated values
dat1 <- mtcars[,-which(colnames(mtcars)%in%highcorcol)]
dim(dat1)

#to build a model split the data
set.seed(123)
samp <- sample.int(n=nrow(df),size = 0.7*nrow(df))
train <- df[samp,]
test <- df[-samp,]

# Root-mean squared error
rmse.lm <- sqrt(mean((pred.lm-testing$MEDV)^2))

c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared)

# mean absolute error
MAE.lm <- mean(abs(pred.lm-testing$MEDV))
MAE.lm

#predict on test set
pred.lm2 <- predict(fit.lm2, newdata = testing)

# Root-mean squared error
rmse.lm2 <- sqrt(sum((exp(pred.lm2)-testing$MEDV)^2)/
                   length(testing$MEDV))



rmse.lm2 <- sqrt(mean((exp(pred.lm2)-testing$MEDV)^2))
c(RMSE = rmse.lm2, R2 = summary(fit.lm2)$r.squared)

MAE.lm2 <- mean(abs(exp(pred.lm2)-testing$MEDV))
MAE.lm2

# Random forest
rmse.rf <- sqrt(sum((pred.rf - testing$MEDV)^2)/
                  length(testing$MEDV))
rmse.rf <- sqrt(mean((pred.rf-testing$MEDV)^2))

c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))


## BAseline Model
best.guess <- mean(train$MEDV)
RMSE.baseline <- sqrt(mean((best.guess-test$MEDV)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$MEDV))
MAE.baseline


# Predict on full data: p
p <- predict(fit_reg)
head(p)
p <- ppredict(fit_reg,train)
head(p)

# Compute errors: error
error <- p - test[["MEDV"]]
head(error)

# Calculate RMSE
sqrt(mean(error^2))

library(caret)
library(car)
vif(fit_reg)
dwt(fit_reg)
plot(fit_reg)


##Prune Tree
printcp(fit)
min.xerror <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

fit.pruned <- prune(fit,cp = min.xerror)
fancyRpartPlot(fit.pruned)




#cross validation

# Fit lm model using 5-fold CV: model
model <- train(
  MEDV ~., 
  boston,
  method = "glmnet",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5*5-fold CV: model
model <- train(MEDV~.,
               boston,
               method ='lm',
               trcontrol = trainControl(method = 'repetedcv',
                                        number = 5,
                                        repeats = 5,
                                        verboseIter = T))

print(model)
p<- predict(model)
p

sqrt(mean((p-train$MEDV)^2))

#10-fold cross-validation
# Fit lm model using 10-fold CV: model
model <- train(
  MEDV ~ ., boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  ))

# Print model to console
summary(model)
model


# Fit random forest: model
model <- train(
  MEDV~.,
  tuneLength = 1,
  boston, 
  method = 'ranger',
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
summary(model)
model

library(randomForest)
rf <- randomForest(MEDV~., data = train)
summary(rf)




# Let's quickly understand the structure of ggplot code:
#   
#   aes - refers to aesthetics. It includes variable names used to create plots.
# geom_point - ggplot offers many 'geoms' which are used to represent data. 
#Since, we are interested here in scatter plot, we used geom_points.
# scale_x_continuous - x variable is continuous. This parameter is used 
#to alter information represented at x axis.
# scale_y_continuous - It performs the same task as scale_x_continuous but for y axis.
# theme_bw - It refers to setting the background of plots. I used the grid version.
# We can also add a categorical variable (Item_Type) in the current plot.
#Do check the data to get familiar with the available in the data set.

ggplot(train, aes(Item_Visibility, Item_MRP)) + 
  geom_point(aes(color = Item_Type)) +
  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
  scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+
  theme_bw() + labs(title="Scatterplot")

# We can even make it better by creating separate scatter plot for separate Item_Type.

ggplot(train, aes(Item_Visibility, Item_MRP)) + 
  geom_point(aes(color = Item_Type)) +
  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
  scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ 
  theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ Item_Type)


# Q2 . How to create a Histogram ?
# When to use: Histogram is used when we want to plot one continuous variable.

ggplot(train, aes(Item_MRP)) + geom_histogram(binwidth = 2)+
  scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 20))+
  labs(title = "Histogram")


# Q3. How to create a  Bar Chart ?
#   When to use: Bar Chart is used when we want to plot a categorical variable or a combination of continuous and categorical variable.

#Bar chart with one variable
ggplot(train, aes(Outlet_Establishment_Year)) + geom_bar(fill = "red")+theme_bw()+
  scale_x_continuous("Establishment Year", breaks = seq(1985,2010)) +
  scale_y_continuous("Count", breaks = seq(0,1500,150)) +
  coord_flip()+ labs(title = "Bar Chart") + theme_gray()

# #Bar Chart with 2 variables
ggplot(train, aes(Item_Type, Item_Weight)) + geom_bar(stat = "identity", fill = "darkblue") + scale_x_discrete("Outlet Type")
+ scale_y_continuous("Item Weight", breaks = seq(0,15000, by = 500))
+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
+ labs(title = "Bar Chart")

# Q4. How to create a Stack Bar Chart ?
# When to use: It's an advanced version of a Bar Chart. It used when we wish to visualize a combination of categorical variables.

ggplot(train, aes(Outlet_Location_Type, fill = Outlet_Type)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "Outlet Location Type", y = "Count of Outlets")



# Q5. How to create a Box Plot ?
# When to use: Box Plots are used to plot a combination of categorical and continuous variables. This plot helps us to identify data distribution and detect outliers.

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_boxplot(fill = "red")+
  scale_y_continuous("Item Outlet Sales", breaks= seq(0,15000, by=500))+
  labs(title = "Box Plot", x = "Outlet Identifier")

# Q6. How to create an Area Chart ?
# When to use: Area chart is used to show continuity across a variable or data set. It's quite similar to a line chart. It is commonly used for time series plots. Alternatively, it is used to plot continuous variables and analyze the underlying trends.

ggplot(train, aes(Item_Outlet_Sales)) + geom_area(stat = "bin", bins = 30, fill =          "steelblue") +
  scale_x_continuous(breaks = seq(0,11000,1000))+
  labs(title = "Area Chart", x = "Item Outlet Sales", y = "Count")

# Q7. How to create a heat map ?
# When to use: Heat Map uses intensity (density) of colors to display relationship between two or three or many variables in a two dimensional image.

ggplot(train, aes(Outlet_Identifier, Item_Type))+
  geom_raster(aes(fill = Item_MRP))+
  labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
  scale_fill_continuous(name = "Item MRP")

# You can zoom this plot at your end for a better visual. The dark portion indicates Item MRP is close 50. The brighter portion indicates Item MRP is close to 250.

# Heat Maps can also produce visuals used for image recognition. This can be done by adding a parameter as interpolate = TRUE .



ggplot(train, aes(Outlet_Identifier, Item_Type))+
  geom_raster(aes(fill = Item_MRP), interpolate = T)+
  labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
  scale_fill_continuous(name = "Item MRP")


# Q8. How to create a Correlogram ?
#   When to use: Correlogram is used to test the level of correlation among the variable available in the data set.
# 
# To create a correlogram, I've used corrgram package instead of ggplot. I realized creating correlogram using its dedicated package is much easier than using ggplot.

install.packages("corrgram")
library(corrgram)
corrgram(train, order=NULL,
         panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")


# It's quite easy to interpret too. Darker the color, higher the correlation between variables. Blue indicates positive correlation. Red indicates negative correlation. Color intensity indicates the magnitude of correlation.


























