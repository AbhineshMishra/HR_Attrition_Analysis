##loading libraries used
library(tidyr)
library(dplyr)
library(lubridate)
library(caTools)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(GGally)
library(ggplot2)
library(rlang)


######################### DATA PREPARATION & EDA ##############################

# read files
employee <- read.csv("general_data.csv",stringsAsFactors = F)

emp_survey <- read.csv("employee_survey_data.csv")

mngr_survey <- read.csv("manager_survey_data.csv")

in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

###checking if all files have same number of records
length(unique(employee$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(emp_survey$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(mngr_survey$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(in_time$X)) # 4410, confirming X is EmployeeID and key
length(unique(out_time$X)) # 4410, confirming X is EmployeeID and key

###
setdiff(employee$EmployeeID,emp_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,mngr_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,in_time$X) # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,out_time$X) # Identical EmployeeID across these datasets


#arrange by employee ID so both in time and out time are in same order
in_time <- arrange(in_time,X)
out_time <- arrange(out_time,X)

#remove id and convert character to date and time format 
intime <- in_time[,-1]
intime <- as.data.frame(lapply(intime, as.POSIXct,origin="%Y-%m-%d %H:%M:%S"))

outtime <-out_time[,-1]
outtime <- as.data.frame(lapply(outtime, as.POSIXct, origin = "%Y-%m-%d %H:%M:%S"))

#calculate total time in office by finding difference in in time and out time
totaltime <- as.data.frame(sapply(1:ncol(intime), function(i) difftime(time1 = outtime[,i], time2 = intime[,i], units = "hours")))

##to remove any columns with all NA's

for(i in 1:ncol(totaltime))
    {
  if(TRUE == (t = all(is.na(totaltime[,i])))) {
    totaltime <- subset(totaltime, select = -i)
  }
  }


#calculate Hours worked by employeeID
HoursWorked <- rowSums(totaltime[1:ncol(totaltime)],na.rm = TRUE)
AverageHoursWorked <- rowMeans(totaltime[1:ncol(totaltime)],na.rm = TRUE)

#calculate Total Leaves by employeeID by finding NA's
TotalLeaves <- apply(totaltime, 1, function(x) sum(is.na(x)))

#combine Employee ID, Hours worked and total leaves 
time<- cbind(in_time$X,HoursWorked,AverageHoursWorked,TotalLeaves)
time <- as.data.frame(time)

#rename the column X to employee ID
colnames(time)[colnames(time)=="V1"] <- "EmployeeID"

#merge all files to 1 data source
emp <- merge(employee,emp_survey,by="EmployeeID")

emp <- merge(emp,mngr_survey,by="EmployeeID")

emp <- merge(emp,time,by="EmployeeID")

#remove NA's
emp<-na.omit(emp)



#create Age_Group from Age
Age_group <- vector()

for (num in 1:nrow(emp))
{
Age_group[num] <- if(emp[num,"Age"]>=18 & emp[num,"Age"]<=30) 
              {"18-30"} else if (emp[num,"Age"]>30 & emp[num,"Age"]<=36) 
                {"31-36"} else if (emp[num,"Age"]>36 & emp[num,"Age"]<=43) 
                {"37-43"} else  
                {">43"}
}

#add age_group to emp
emp <- cbind(emp,Age_group)

emp$Age_group <- as.character(emp$Age_group)

#convert numbers to Education string values
for (num in 1:nrow(emp))
{
  emp[num,"Education"] <- if (emp[num,"Education"]==1) 
  {"Below College"} else if (emp[num,"Education"]==2) 
  {"College"} else if (emp[num,"Education"]==3) 
  {"Bachelor"} else if (emp[num,"Education"]==4)  
  {"Master"} else if (emp[num,"Education"]==5)
  {"Doctor"}
}

#replace numbers to Environment Satisfaction level
for (num in 1:nrow(emp))
{
  emp[num,"EnvironmentSatisfaction"] <- if (emp[num,"EnvironmentSatisfaction"]==1) 
  {"Low"} else if (emp[num,"EnvironmentSatisfaction"]==2) 
  {"Medium"} else if (emp[num,"EnvironmentSatisfaction"]==3) 
  {"High"} else if (emp[num,"EnvironmentSatisfaction"]==4)  
  {"Very High"} 
}

#replace numbers to Job Satisfaction level
for (num in 1:nrow(emp))
{
  emp[num,"JobSatisfaction"] <- if (emp[num,"JobSatisfaction"]==1) 
  {"Low"} else if (emp[num,"JobSatisfaction"]==2) 
  {"Medium"} else if (emp[num,"JobSatisfaction"]==3) 
  {"High"} else if (emp[num,"JobSatisfaction"]==4)  
  {"Very High"} 
}

#replace JobInvolment score to strings
for (num in 1:nrow(emp))
{
  emp[num,"JobInvolvement"] <- if (emp[num,"JobInvolvement"]==1) 
  {"Low"} else if (emp[num,"JobInvolvement"]==2) 
  {"Medium"} else if (emp[num,"JobInvolvement"]==3) 
  {"High"} else if (emp[num,"JobInvolvement"]==4)  
  {"Very High"} 
}

#replace WorkLifeBalance score to strings
for (num in 1:nrow(emp))
{
  emp[num,"WorkLifeBalance"] <- if (emp[num,"WorkLifeBalance"]==1) 
  {"Bad"} else if (emp[num,"WorkLifeBalance"]==2) 
  {"Good"} else if (emp[num,"WorkLifeBalance"]==3) 
  {"Better"} else if (emp[num,"WorkLifeBalance"]==4)  
  {"Best"} 
}

#replace Performance Rating score to strings
for (num in 1:nrow(emp))
{
  emp[num,"PerformanceRating"] <- if (emp[num,"PerformanceRating"]==1) 
  {"Low"} else if (emp[num,"PerformanceRating"]==2) 
  {"Good"} else if (emp[num,"PerformanceRating"]==3) 
  {"Excellent"} else if (emp[num,"PerformanceRating"]==4)  
  {"Outstanding"} 
}

#convert Job Level from num to character
emp$JobLevel <- as.character(emp$JobLevel)

#convert StockOptionLevel to character
emp$StockOptionLevel <- as.character(emp$StockOptionLevel)

###########################Attrition data and variables on which it depenends##########################
emp_attrition <- subset(emp,Attrition=='Yes')
plot_grid(ggplot(emp_attrition,aes(x=Age_group))+geom_bar()
          ,ggplot(emp_attrition,aes(x=BusinessTravel))+geom_bar()
          ,ggplot(emp_attrition,aes(x=Department))+geom_bar()
          ,ggplot(emp_attrition,aes(x=DistanceFromHome))+geom_line(stat="count")
          
)

### 18-30 age group employees leave most often
### employees who Travel Rarely leave most often
### employees who Travel Rarely leave most often
### employee who live near generally leave

ggplot(emp_attrition,aes(x=JobRole))+geom_bar()
###JobRole Sales Executive are the ones who are mostly leaving

plot_grid(ggplot(emp_attrition,aes(x=PercentSalaryHike))+geom_histogram(binwidth=4,color="black",fill="gray")
          ,ggplot(emp_attrition,aes(x=TrainingTimesLastYear))+geom_histogram(binwidth=1,color="black",fill="gray")
          ,ggplot(emp_attrition,aes(x=TotalWorkingYears))+geom_histogram(binwidth=2,color="black",fill="gray")
          ,ggplot(emp_attrition,aes(x=YearsAtCompany))+geom_histogram(binwidth=2,color="black",fill="gray")
          
)

### Employees who get hike like than 15 are most likely to leave
### employees who get training between 2 and 4 hrs might leave
### employee who are freshers or have Total experience of 6-8 years are higly risky
### employees who have recently joined can leave early

plot_grid(ggplot(emp_attrition,aes(x=EnvironmentSatisfaction))+geom_bar()
          ,ggplot(emp_attrition,aes(x=JobSatisfaction))+geom_bar()
          ,ggplot(emp_attrition,aes(x=WorkLifeBalance))+geom_bar()
          ,ggplot(emp_attrition,aes(x=JobInvolvement))+geom_bar()
          ,ggplot(emp_attrition,aes(x=PerformanceRating))+geom_bar()
 )

##Employees who find Evnivorment Satisfaction Low or Job Satisfaction High 
##or work life balance Better or are highly involved in Job or get Excellent in Performance Rating
## are the ones who mostly leave

ggplot(emp_attrition,aes(x=AverageHoursWorked))+geom_histogram(binwidth=2,color="black",fill="gray")
ggplot(emp_attrition,aes(x=TotalLeaves))+geom_histogram(binwidth=2,color="black",fill="gray")
## no trends can be found from Average Hours worked or from total leaves

########################### Changing column to Dummy variables ##############

#remove EmployeeID and Age column (use Age Group)
emp <- emp[,-c(1,2)]

#convert levels to 0 and 1
emp$Attrition <-factor(emp$Attrition)
levels(emp$Attrition) <- c(0,1)

emp$Attrition <- as.numeric(levels(emp$Attrition))[emp$Attrition]

##dummy variable BusinessTravel
summary(factor(emp$BusinessTravel))

dummy1 <- data.frame(model.matrix( ~BusinessTravel, data = emp))
dummy1 <- dummy1[,-1]
emp <- cbind(emp,dummy1)

emp <- emp[,-2]

##dummy variable Department
summary(factor(emp$Department))

dummy2 <- data.frame(model.matrix( ~Department, data = emp))
dummy2 <- dummy2[,-1]

emp <- cbind(emp,dummy2)
emp <- emp[,-2]

##dummy variable Education
summary(factor(emp$Education))

dummy3 <- data.frame(model.matrix( ~Education, data = emp))
dummy3 <- dummy3[,-1]

emp <- cbind(emp,dummy3)
emp <- emp[,-3]

##dummy variable Education
summary(factor(emp$EducationField))

dummy4 <- data.frame(model.matrix( ~EducationField, data = emp))
dummy4 <- dummy4[,-1]

emp <- cbind(emp,dummy4)
emp <- emp[,-3]

#convert Gender levels to 0 and 1
summary(factor(emp$Gender))

emp$Gender <-factor(emp$Gender)

levels(emp$Gender) <- c(0,1)

emp$Gender <- as.numeric(levels(emp$Gender))[emp$Gender]


##dummy variable JobLevel
summary(factor(emp$JobLevel))

dummy5 <- data.frame(model.matrix( ~JobLevel, data = emp))
dummy5 <- dummy5[,-1]

emp <- cbind(emp,dummy5)
emp <- emp[,-5]


##dummy variable JobRole
summary(factor(emp$JobRole))

dummy6 <- data.frame(model.matrix( ~JobRole, data = emp))
dummy6 <- dummy6[,-1]

emp <- cbind(emp,dummy6)
emp <- emp[,-5]

##dummy variable MaritalStatus
summary(factor(emp$MaritalStatus))

dummy7 <- data.frame(model.matrix( ~MaritalStatus, data = emp))
dummy7 <- dummy7[,-1]

emp <- cbind(emp,dummy7)
emp <- emp[,-5]

##check variable Over18
summary(factor(emp$Over18))

##only one value
emp <- emp[,-7]

##check variable StandardHours
summary(factor(emp$StandardHours))

##only one value
emp <- emp[,-8]

##dummy variable EnvironmentSatisfaction
summary(factor(emp$EnvironmentSatisfaction))

dummy8 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp))
dummy8 <- dummy8[,-1]

emp <- cbind(emp,dummy8)
emp <- emp[,-14]

##dummy variable JobSatisfaction
summary(factor(emp$JobSatisfaction))

dummy9 <- data.frame(model.matrix( ~JobSatisfaction, data = emp))
dummy9 <- dummy9[,-1]

emp <- cbind(emp,dummy9)
emp <- emp[,-14]


##dummy variable WorkLifeBalance
summary(factor(emp$WorkLifeBalance))

dummy10 <- data.frame(model.matrix( ~WorkLifeBalance, data = emp))
dummy10 <- dummy10[,-1]

emp <- cbind(emp,dummy10)
emp <- emp[,-14]

##dummy variable JobInvolvement
summary(factor(emp$JobInvolvement))

dummy11 <- data.frame(model.matrix( ~JobInvolvement, data = emp))
dummy11 <- dummy11[,-1]

emp <- cbind(emp,dummy11)
emp <- emp[,-14]

##dummy variable PerformanceRating
summary(factor(emp$PerformanceRating))

emp$PerformanceRating<- factor(emp$PerformanceRating)
#convert levels to 0 and 1
levels(emp$PerformanceRating) <- c(0,1)

emp$PerformanceRating <- as.numeric(levels(emp$PerformanceRating))[emp$PerformanceRating]

##dummy variable Age_group
summary(factor(emp$Age_group))

dummy12 <- data.frame(model.matrix( ~Age_group, data = emp))
dummy12 <- dummy12[,-1]

emp <- cbind(emp,dummy12)

emp <- emp[,-18]

##dummy variable StockOptionLevel
summary(factor(emp$StockOptionLevel))
emp$StockOptionLevel <- as.character(emp$StockOptionLevel)

dummy13 <- data.frame(model.matrix( ~StockOptionLevel, data = emp))
dummy13 <- dummy13[,-1]

emp <- cbind(emp,dummy13)
emp <- emp[,-8]

#remove employee count 
emp <- emp[,-3]

###################### Outlier treatment for continous variables############################
box <- boxplot.stats(emp$DistanceFromHome)
out <- box$out
emp1 <- emp[ !emp$DistanceFromHome %in% out, ]
emp <- emp1


box <- boxplot.stats(emp$MonthlyIncome)
out <- box$out
emp1 <- emp[ !emp$MonthlyIncome %in% out, ]
emp <- emp1


box <- boxplot.stats(emp$YearsAtCompany)
out <- box$out

emp1 <- emp[ !emp$YearsAtCompany %in% out, ]

emp <- emp1

box <- boxplot.stats(emp$HoursWorked)
out <- box$out
emp1 <- emp[ !emp$HoursWorked %in% out, ]

emp <- emp1

box <- boxplot.stats(emp$AverageHoursWorked)
out <- box$out
emp1 <- emp[ !emp$AverageHoursWorked %in% out, ]

emp <- emp1

box <- boxplot.stats(emp$TotalLeaves)
out <- box$out
emp1 <- emp[ !emp$TotalLeaves %in% out, ]
emp <- emp1

################### scaling the continous values ##########################
emp$DistanceFromHome <- scale(emp$DistanceFromHome)
emp$MonthlyIncome <- scale(emp$MonthlyIncome)
emp$NumCompaniesWorked <- scale(emp$NumCompaniesWorked)
emp$PercentSalaryHike <- scale(emp$PercentSalaryHike)
emp$TotalWorkingYears <- scale(emp$TotalWorkingYears)
emp$TrainingTimesLastYear <- scale(emp$TrainingTimesLastYear)
emp$YearsAtCompany<- scale(emp$YearsAtCompany)
emp$YearsSinceLastPromotion <- scale(emp$YearsSinceLastPromotion)
emp$YearsWithCurrManager <- scale(emp$YearsWithCurrManager)
emp$HoursWorked <- scale(emp$HoursWorked)
emp$AverageHoursWorked <- scale(emp$AverageHoursWorked)
emp$TotalLeave <- scale(emp$TotalLeave)

###########divide data into train and test
set.seed(100)

indices <- sample.split(emp$Attrition, SplitRatio = 0.7)

train <- emp[indices,]

test <- emp[!(indices),]

############### Creating model using training data

model_1 = glm(Attrition ~ ., data = train, family = "binomial")

summary(model_1)

################## Variable selection using stepwise AIC algorithm
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

############## Checking VIF

vif(model_2)

#######removing YearsAtCompany with highest p value

model_3<- glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                DepartmentSales + EducationBelow.College + EducationCollege + 
                JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Representative + MaritalStatusSingle + EnvironmentSatisfactionLow + 
                EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                Age_group18.30 + Age_group37.43 + StockOptionLevel1, family = "binomial", 
              data = train)

summary(model_3)
vif(model_3)

####### removing EducationCollege with high p value

model_4 <-  glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + EducationBelow.College +  
                  JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Representative + MaritalStatusSingle + EnvironmentSatisfactionLow + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                  Age_group18.30 + Age_group37.43 + StockOptionLevel1, family = "binomial", 
                data = train)

summary(model_4)

vif(model_4)

#removing Age_group18.30 with high P value

model_5 <-   glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                   PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                   DepartmentSales + EducationBelow.College +  
                   JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                   JobRoleSales.Representative + MaritalStatusSingle + EnvironmentSatisfactionLow + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                    Age_group37.43 + StockOptionLevel1, family = "binomial", 
                 data = train)

summary(model_5)

vif(model_5)

#removing JobRoleSales.Representative with high P value

model_6 <-   glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                   PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                   DepartmentSales + EducationBelow.College +  
                   JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                    MaritalStatusSingle + EnvironmentSatisfactionLow + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 + StockOptionLevel1, family = "binomial", 
                 data = train)

summary(model_6)

vif(model_6)

#removing BusinessTravelTravel_Rarely with high p value

model_7 <-   glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                   PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + 
                   DepartmentSales + EducationBelow.College +  
                   JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 + StockOptionLevel1, family = "binomial", 
                 data = train)

summary(model_7)

vif(model_7)

#removing EducationBelow.College with high p value

model_8 <-  glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales +   
                  JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionLow + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                  Age_group37.43 + StockOptionLevel1, family = "binomial", 
                data = train)

summary(model_8)

vif(model_8)

#removing JobRoleManufacturing.Director

model_9 <-  glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                  PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales +   
                  JobLevel5 + JobRoleResearch.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionLow + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                  Age_group37.43 + StockOptionLevel1, family = "binomial", 
                data = train)

summary(model_9)

vif(model_9)

#removing PercentSalaryHike

model_10 <-  glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                    TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + 
                   DepartmentSales + JobLevel5 + JobRoleResearch.Director + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 + StockOptionLevel1, family = "binomial", 
                 data = train)

summary(model_10)

vif(model_10)

#removing StockOptionLevel1
model_11 <-  glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                   TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + 
                   DepartmentSales + JobLevel5 + JobRoleResearch.Director + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_11)

vif(model_11)

#removing DistanceFromHome
model_12 <- glm(formula = Attrition ~  NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales + JobLevel5 + JobRoleResearch.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionLow + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                  Age_group37.43 , family = "binomial", 
                data = train)
summary(model_12)

vif(model_12)

#removing EnvironmentSatisfactionVery.High
model_13 <-  glm(formula = Attrition ~  NumCompaniesWorked + 
                   TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + 
                   DepartmentSales + JobLevel5 + JobRoleResearch.Director + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow  + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_13)

vif(model_13)


#removing JobLevel5
model_14 <-  glm(formula = Attrition ~  NumCompaniesWorked + 
                   TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + 
                   DepartmentSales + JobRoleResearch.Director + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow  + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)
summary(model_14)

vif(model_14)

#removing JobRoleResearch.Director
model_15 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked + TotalLeaves + BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + DepartmentSales +  
                   MaritalStatusSingle + EnvironmentSatisfactionLow  + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_15)

vif(model_15)

#removing TotalLeaves
model_16 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked +  BusinessTravelTravel_Frequently + 
                   DepartmentResearch...Development + DepartmentSales +  
                   MaritalStatusSingle + EnvironmentSatisfactionLow  + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_16)

vif(model_16)

#removing DepartmentResearch...Development
model_17 <-    glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                     YearsSinceLastPromotion + YearsWithCurrManager + 
                     HoursWorked +  BusinessTravelTravel_Frequently + 
                      DepartmentSales + MaritalStatusSingle + EnvironmentSatisfactionLow  
                      + JobSatisfactionLow + JobSatisfactionVery.High + 
                     WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                     Age_group37.43 , family = "binomial", 
                   data = train)
summary(model_17)

vif(model_17)

# removing DepartmentSales

model_18 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked +  BusinessTravelTravel_Frequently + 
                    MaritalStatusSingle + EnvironmentSatisfactionLow  
                 + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_18)

vif(model_18)

#removing TrainingTimesLastYear
model_19 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears +  
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked +  BusinessTravelTravel_Frequently + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow  
                 + JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)
summary(model_19)

vif(model_19)

#removing JobSatisfactionLow 
model_20 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears +  
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked +  BusinessTravelTravel_Frequently + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow  
                    + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_20)
vif(model_20)

#removing WorkLifeBalanceBest
model_21 <-  glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears +  
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   HoursWorked +  BusinessTravelTravel_Frequently + 
                   MaritalStatusSingle + EnvironmentSatisfactionLow  
                 + JobSatisfactionVery.High +  WorkLifeBalanceBetter + WorkLifeBalanceGood + 
                   Age_group37.43 , family = "binomial", 
                 data = train)

summary(model_21)

vif(model_21)

#removing WorkLifeBalanceGood
model_22 <-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears +  
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 HoursWorked +  BusinessTravelTravel_Frequently + 
                 MaritalStatusSingle + EnvironmentSatisfactionLow  
               + JobSatisfactionVery.High +  WorkLifeBalanceBetter +  
                 Age_group37.43 , family = "binomial", 
               data = train)

summary(model_22)

vif(model_22)

#removing WorkLifeBalanceBetter
model_23 <-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears +  
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 HoursWorked +  BusinessTravelTravel_Frequently + 
                 MaritalStatusSingle + EnvironmentSatisfactionLow  
               + JobSatisfactionVery.High + Age_group37.43 , family = "binomial", 
               data = train)
summary(model_23)

vif(model_23)

#predicting the values
test_actual_attrition <- factor(ifelse(test$Attrition==1,'Yes','No'))

test_predict <- predict(model_23,type = "response", newdata= test[,-1])
summary(test_predict)


############ calculate Accuracy, sensitivity, Specificity for values ranging from 0 to 1
Accuracy <- vector()
Sensitivity <- vector()
Specificity <- vector()
var_constant <- vector()
var = 0.01

for (num in 1:100)
{
  test_predict_attrition <- ifelse(test_predict>=var,'Yes','No')
  
  yes_yes <- sum(test_actual_attrition=='Yes' & test_predict_attrition=='Yes')

  yes_no <-sum(test_actual_attrition=='Yes' & test_predict_attrition=='No')
  no_no<-sum(test_actual_attrition=='No' & test_predict_attrition=='No')
  no_yes<-sum(test_actual_attrition=='No' & test_predict_attrition=='Yes')
  
  Accuracy[num] <- (yes_yes+no_no)/(yes_no+yes_yes+no_no+no_yes)
  Sensitivity[num] <- yes_yes/(yes_yes+yes_no)
  Specificity[num] <- no_no/(no_no+no_yes)
  var_constant[num] <- var
  var <- var+0.01
}

########## merging data to create the plot to see where the 3 values intersect
prob <- as.data.frame(cbind(Accuracy,Sensitivity,Specificity,var_constant))

####### creating the plot 
ggplot(data=prob,aes(x=var_constant,label=var_constant))+geom_line(aes(y=Accuracy,colour="Accuracy"))+geom_line(aes(y=Sensitivity,colour="Sensitivity"))+geom_line(aes(y=Specificity,colour="Specificity"))+geom_vline(aes(xintercept = mean(test_predict)),linetype = "dashed", size = 0.6)
ggplot(data=prob,aes(x=var_constant,label=var_constant))+geom_line(aes(y=Accuracy,colour="Accuracy"))+geom_line(aes(y=Sensitivity,colour="Sensitivity"))+geom_line(aes(y=Specificity,colour="Specificity"))+geom_point(aes(y=Sensitivity))+geom_vline(aes(xintercept = 0.153),linetype = "dashed", size = 0.6)

###using table function to see the values predicted with 0.153
test_actual_attrition <- factor(ifelse(test$Attrition==1,'Yes','No'))
test_predict_attrition <- factor(ifelse(test_predict>=0.153,'Yes','No'))

### calculating 
yes_yes <- sum(test_actual_attrition=='Yes' & test_predict_attrition=='Yes')
yes_no <-sum(test_actual_attrition=='Yes' & test_predict_attrition=='No')
no_no<-sum(test_actual_attrition=='No' & test_predict_attrition=='No')
no_yes<-sum(test_actual_attrition=='No' & test_predict_attrition=='Yes')

Accuracy <- (yes_yes+no_no)/(yes_no+yes_yes+no_no+no_yes)
Sensitivity <- yes_yes/(yes_yes+yes_no)
Specificity <- no_no/(no_no+no_yes)

Accuracy
Sensitivity
Specificity

## view table 
table(test_predict_attrition,test_actual_attrition )

##view confusionMatrix 
confusionMatrix(test_predict_attrition,test_actual_attrition ,positive='Yes')

##################################################################################################
### KS -statistic - Test Data ######

test_actual_attrition_val <- test$Attrition
test_predict_attrition_val <- as.numeric(ifelse(test_predict_attrition=='Yes',1,0))

library(ROCR)

pred_object_emp<- prediction(test_predict_attrition_val, test_actual_attrition_val)

performance_measures_emp<- performance(pred_object_emp, "tpr", "fpr")

ks_table_emp <- attr(performance_measures_emp, "y.values")[[1]] - 
  (attr(performance_measures_emp, "x.values")[[1]])

max(ks_table_emp)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition_val, test_predict_attrition_val, groups = 10)

## view Atrrition buckets
Attrition_decile

##create plots for Gain and lift
  ggplot(data=Attrition_decile,aes(x=bucket,y=Gain))+geom_line()+geom_point()+geom_text(label=round(Attrition_decile$Gain,2))
  
  ggplot(data=Attrition_decile,aes(x=bucket,y=Cumlift))+geom_line()+geom_point()+geom_text(label=round(Attrition_decile$Cumlift,2))
  
  