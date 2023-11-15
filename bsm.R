#inspect the data
info32 = info


#create a dataset with only creator_slug that appear more than once in the dataset
dataHyp2Double <- info32[info32$creator_slug %in% names(which(table(info32$creator_slug) > 1)),]
dataHyp2Double

dataHyp2sub<- dataHyp2Double[,c('creator_slug','Goal_USD','Pledge_USD','Launched_at','Deadline', 'Project_description')]
dataHyp2sub

#make a subdataset where deadline is before week 32 in 2019 in unix time
dataHyp2 <- dataHyp2sub[dataHyp2sub$Deadline < 1565474400,]
dataHyp2

#You drop only 1850 records out of 75000

#count the number of rows in dataHyp1sub and dataHyp1
#nrow(dataHyp2Sub)
#nrow(dataHyp2)


#make a new column with boolean if goal_USD >= Pledge_USD
dataHyp2$Goal_met <- dataHyp2$Goal_USD <= dataHyp2$Pledge_USD
dataHyp2

#drop na values in dataHyp1
dataHyp2 <- dataHyp2[complete.cases(dataHyp2),]

#make a new column with order which orders the projects per creator_slug based on launched_at. So the first project of a creator should get 1, the second 2 ..
dataHyp2$Order <- ave(dataHyp2$Launched_at, dataHyp2$creator_slug, FUN = seq_along)

#new subset of the data
dataHyp2<- dataHyp2[,c('creator_slug','Project_description', 'Goal_met', 'Order')]
dataHyp2
