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


#make a new column with boolean if goal_USD >= Pledge_USD
dataHyp1$Goal_met <- dataHyp1$Goal_USD <= dataHyp1$Pledge_USD
#dataHyp1

#Get from Creator_nb_backed only the number with a regex
dataHyp1$Creator_nb_backed <- as.integer(gsub("[^0-9]", "", dataHyp1$Creator_nb_backed))

#drop na values in dataHyp1
dataHyp1 <- dataHyp1[complete.cases(dataHyp1),]
dataHyp1 <- dataHyp1[,c('Creator_nb_projects', 'Creator_nb_backed', 'Goal_met')]
#dataHyp1


#change Goal_met to binary
dataHyp1$Goal_met <- as.integer(dataHyp1$Goal_met)
#dataHyp1


#Columns as integers
dataHyp1$Creator_nb_backed <- as.integer(dataHyp1$Creator_nb_backed)


#to find an appropriate cutoff point:
table(dataHyp1$Creator_nb_projects)

# we will only use values with at least 50 records, because for normal distribution you need a minimum of 50 values
# So then you have an substantial amount of values


#iterate over the values of the following table: table(dataHyp1$Creator_nb_projects)
quan_table = table(dataHyp1$Creator_nb_projects)

#in quan_table, drop the records where the value is < 50
quan_table <- quan_table[quan_table >= 50]

#clean the data such that only records with over 50 occurences are in there
dataHyp1_2 <- dataHyp1[dataHyp1$Creator_nb_projects %in% names(quan_table),]

#make a table of the values of Creator_nb_projects
table(dataHyp1_2$Creator_nb_projects)




#make a logreg model to predict goal_met with Creator_nb_projects and Creator_nb_backed
modelHyp1Log <- glm(Goal_met ~ Creator_nb_projects + Creator_nb_backed, data = dataHyp1_2, family = "binomial")
summary(modelHyp1Log)

#Perform anova test on dataHyp1
anova(modelHyp1)




#return the values of the loop in a vector
vec_val <- c()
vec_ind <- c()
vec_amount <- c()

for (i in 1:as.integer(max(dataHyp1$Creator_nb_projects))){
  vec_val[i] <- sum(dataHyp1$Goal_met[dataHyp1$Creator_nb_projects == i])/length(dataHyp1$Goal_met[dataHyp1$Creator_nb_projects == i])
  vec_ind[i] <- i
  vec_amount[i] <- length(dataHyp1$Goal_met[dataHyp1$Creator_nb_projects == i])
}




#create a table of vec_val and vec_ind as two columns with headers: index and percentage
tableHyp1 <- data.frame(vec_ind, vec_val, vec_amount)
tableHyp1

#make a barplot of the column vec_amount
barplot(tableHyp1$vec_amount, names.arg = tableHyp1$vec_ind, xlab = "Number of projects", ylab = "Number of projects of the creator", main = "Number of projects of the creator by number of projects of the creator")


#drop columns where nan exists
tableHyp1 <- tableHyp1[complete.cases(tableHyp1),]
tableHyp1


#make a barchart of tableHyp1
barplot(tableHyp1$vec_val, names.arg = tableHyp1$vec_ind, xlab = "Number of projects", ylab = "Percentage of projects that met their goal", main = "Percentage of projects that met their goal by number of projects of the creator")

#make a barchart of the number of projects per number of projects
barplot(tableHyp1$vec_amount, names.arg = tableHyp1$vec_ind, xlab = "Number of projects", ylab = "Number of projects of the creator", main = "Number of projects of the creator by number of projects of the creator")


barplot(dataHyp1$Creator_nb_projects,  names.arg = tableHyp1$vec_ind, xlab = "x", ylab = "y", main = "title")


dataHyp1
dataHyp1$Creator_nb_projects
tableHyp1$vec_ind
