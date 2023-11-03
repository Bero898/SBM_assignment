################################################################################
################ ----- Data cleaning to test Hypothesis 1 ----- ################
################################################################################

#inspect the data
info32

#create new data with only a few columns of info32 dataset
dataHyp1Sub <- info32[,c('Goal_USD','Pledge_USD','Launched_at','Deadline', 'Creator_nb_projects')]
dataHyp1Sub

#make a subdataset where deadline is before week 32 in 2019 in unix time
dataHyp1 <- dataHyp1Sub[dataHyp1Sub$Deadline < 1565474400,]
dataHyp1

#You drop only 1850 records out of 75000

#count the number of rows in dataHyp1sub and dataHyp1
nrow(dataHyp1Sub)
nrow(dataHyp1)


#make a new column with boolean if goal_USD >= Pledge_USD
dataHyp1$Goal_met <- dataHyp1$Goal_USD <= dataHyp1$Pledge_USD
dataHyp1

#drop na values in dataHyp1
dataHyp1 <- dataHyp1[complete.cases(dataHyp1),]
dataHyp1


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

