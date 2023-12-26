################################################################################
################ ----- Data cleaning to test Hypothesis 1 ----- ################
################################################################################

#inspect the data
info32

#create new data with only a few columns of info32 dataset
dataHyp1Sub <- info32[,c('Goal_USD','Pledge_USD','Launched_at','Deadline', 'Creator_nb_projects', 'Creator_nb_backed')]
#dataHyp1Sub

#make a subdataset where deadline is before week 32 in 2019 in unix time
dataHyp1 <- dataHyp1Sub[dataHyp1Sub$Deadline < 1565474400,]
#dataHyp1

#You drop only 1850 records out of 75000

#count the number of rows in dataHyp1sub and dataHyp1
nrow(dataHyp1Sub)
nrow(dataHyp1)


#columns to integer
dataHyp1$Pledge_USD <- as.integer(dataHyp1$Pledge_USD)
dataHyp1$Goal_USD <- as.integer(dataHyp1$Goal_USD)

#drop the rows where Goal_USD is 0 -> then you might get inf percentage
dataHyp1 <- dataHyp1[dataHyp1$Goal_USD != 0,]
#dataHyp1

#Create a new column with percentage of goal reached
dataHyp1$Goal_percentage <- dataHyp1$Pledge_USD/dataHyp1$Goal_USD
dataHyp1$Goal_percentage <- round(dataHyp1$Goal_percentage, digits = 3)
dataHyp1$Goal_percentage[dataHyp1$Goal_percentage>100] = 100
print(dataHyp1$Goal_percentage)
#dataHyp1

#Get from Creator_nb_backed only the number with a regex
dataHyp1$Creator_nb_backed <- gsub("[^0-9]", "", dataHyp1$Creator_nb_backed) #Seeing if this works better as a string rather than
#dataHyp1

#drop redundant columns
dataHyp1 <- dataHyp1[,c('Creator_nb_projects', 'Creator_nb_backed', 'Goal_percentage')]


#Drop the rows where nan values occur
dataHyp1 <- dataHyp1[complete.cases(dataHyp1),]


#dataHyp1#Columns as integers
#dataHyp1$Creator_nb_backed <- as.integer(dataHyp1$Creator_nb_backed)


#to find an appropriate cutoff point:
table(dataHyp1$Creator_nb_projects)
table(dataHyp1$Creator_nb_backed)

# we will only use values with at least 50 records, because for normal distribution you need a minimum of 50 values
# So then you have an substantial amount of values


#iterate over the values of the following table: table(dataHyp1$Creator_nb_projects)
quan_table1 = table(dataHyp1$Creator_nb_projects)
quan_table2 = table(dataHyp1$Creator_nb_backed)

#in quan_table, drop the records where the value is < 50
quan_table1 <- quan_table1[quan_table1 >= 50]
quan_table2 <- quan_table2[quan_table2 >= 50]

#clean the data such that only records with over 50 occurences are in there
dataHyp1_2 <- dataHyp1[dataHyp1$Creator_nb_projects %in% names(quan_table1),]
dataHyp1_2 <- dataHyp1[dataHyp1$Creator_nb_backed %in% names(quan_table2),]

nrow(dataHyp1_2)

#dataHyp1_2

#make a table of the values of Creator_nb_projects
table(dataHyp1_2$Creator_nb_projects)
#dataHyp1_2
class(dataHyp1_2$Creator_nb_projects)
#drop nan and inf values in dataHyp1_2
dataHyp1_2 <- dataHyp1_2[complete.cases(dataHyp1_2),]

#give summary statistics of dataHyp1_2
summary(dataHyp1_2)

x<-dataHyp1_2$Goal_percentage
##check the distribution of the outcome variable
hist(x)
table(x)
#looks exponential, so we try log transform
y<-log(dataHyp1_2$Goal_percentage+1)
qqnorm(y)
qqline(y)
#still skewed, so we try to include sqrt transform
y<-sqrt(log(dataHyp1_2$Goal_percentage+1))
qqnorm(y)
qqline(y)

##since there is no improvement, and the distribution seems exponential, we use
##poisson distribution

modelPoisson <- glm(Goal_percentage ~ Creator_nb_projects + Creator_nb_backed, data = dataHyp1_2,family = poisson(link = 'log'))
summary(modelPoisson)

##try Gamma distribution

dataHyp1_2$nonzero_percentage <- dataHyp1_2$Goal_percentage + 1e-6
modelGamma  <- glm(nonzero_percentage ~ Creator_nb_projects + Creator_nb_backed, data = dataHyp1_2,family = poisson(link = 'log'))
summary(modelGamma)

# trying zero inflation model
##can't use factors, need to one hot encode



hot_encode <- model.matrix(~  Creator_nb_projects + Creator_nb_backed  - 1, data = dataHyp1_2)

hot_encode<-as.data.frame(hot_encode)

##can't use non integer values for the dependent variable

inflDS <- cbind(dataHyp1_2$Goal_percentage, hot_encode)

#don't run this if the second zeroinfl will be runned (it will cause issues with removing values to try to avoid skeweness)
inflDS$percent_int<- as.integer(round(inflDS$`dataHyp1_2$Goal_percentage`,0))

null_values_per_column <- sapply(inflDS, function(x) any(is.na(x)))

# Print the result

##doesn't converge
modelZeroINFL_1 <- pscl::zeroinfl(percent_int ~ . - `dataHyp1_2$Goal_percentage` | . - `dataHyp1_2$Goal_percentage`, dist = "geometric", data = inflDS, control = pscl::zeroinfl.control(maxit = 100000))

##try to remove skeweness by ignoring certain values (between 0 an 1, to avoid having too much data)

inflDS<- inflDS[!(inflDS$`dataHyp1_2$Goal_percentage` > 0 & inflDS$`dataHyp1_2$Goal_percentage` < 1), ]
inflDS$percent_int<- as.integer(round(inflDS$`dataHyp1_2$Goal_percentage`,0))


##doesn't converge either
modelZeroINFL_2 <- pscl::zeroinfl(percent_int ~ . - `dataHyp1_2$Goal_percentage` | . - `dataHyp1_2$Goal_percentage`, dist = "geometric", data = inflDS, control = pscl::zeroinfl.control(maxit = 100000))
summary(modelZeroINFL)

#Perform anova test on dataHyp1
anova(modelPoisson)
anova(modelGamma)

#visualize the model
plot(modelHyp1) #-> from this you get the QQ test and stuff. The bookcamp shizzle to check whether the model is correct

summary(modelHyp1)



#plot
#coefficientsHyp1 <- summary(modelHyp1)$coefficients
#coefficientsHyp1
# From coefficientsHyp1, only keep the column estimate and the rownames in a table
#coefficientsHyp1 <- coefficientsHyp1[,1]
#coefficientsHyp1 to table 
#coefficientsHyp1 <- data.frame(coefficientsHyp1)
#coefficientsHyp1

#from the rownames, use only the number in the name with regex, when there is no number in the name, drop the row
#coefficientsHyp1$projects <- gsub("Creator_nb_projects", "", rownames(coefficientsHyp1))

#drop the rows where projects is not a number
#coefficientsHyp1 <- coefficientsHyp1[!is.na(as.numeric(coefficientsHyp1$projects)),]

#coefficientsHyp1 <- coefficientsHyp1[grep('projects', rownames(coefficientsHyp1)),]

#order the table based on the number of projects
#coefficientsHyp1 <- coefficientsHyp1[order(as.numeric(coefficientsHyp1$projects)),]

#coefficientsHyp1

#make a barplot of coefficientsHyp1. Projects should be the x-axis and the estimate the y-axis
#barplot(coefficientsHyp1$coefficientsHyp1, names.arg = coefficientsHyp1$projects, xlab = "Number of projects", ylab = "Estimate", main = "Estimate of Goal_percentage based on number of projects")

#make a barplot of coefficientsHyp1. Projects should be the x-axis and the estimate the y-axis each bar should have a label
#barplot(coefficientsHyp1$coefficientsHyp1, names.arg = coefficientsHyp1$projects, xlab = "Number of projects", ylab = "Estimate", main = "Estimate of Goal_percentage based on number of projects", las = 2, cex.names = 0.8)


#plot(coefficientsHyp1$projects, coefficientsHyp1$coefficientsHyp1, type = "l", col = "red", xlab = "Number of projects", ylab = "Estimate", main = "Estimate of Goal_percentage based on number of projects")










