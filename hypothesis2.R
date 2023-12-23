# Install and load required packages
# install.packages(c("tidytext", "dplyr", "ggplot2"))
library(tidytext)
library(dplyr)
library(ggplot2)

#inspect the data
info_filename <- file.choose()
info <- readRDS(info_filename)


#create a dataset with only creator_slug that appear more than once in the dataset
dataHyp2Double <- info[info$creator_slug %in% names(which(table(info$creator_slug) > 1)),]

dataHyp2sub<- dataHyp2Double[,c('creator_slug','Goal_USD','Pledge_USD','Launched_at','Deadline','Project_description','Creator_nb_projects','Creator_nb_backed', 'Category')]

dataHyp2filtered <- na.omit(dataHyp2sub)

#make a subdataset where deadline is before week 32 in 2019 in unix time
dataHyp2 <- dataHyp2filtered[dataHyp2filtered$Deadline < 1565474400,]


#make a new column with boolean if goal_USD >= Pledge_USD
dataHyp2$Pledge_USD <- log(as.numeric(dataHyp2$Pledge_USD) + 1)
dataHyp2$Goal_USD <- log(as.numeric(dataHyp2$Goal_USD) + 1)
dataHyp2$success_rate <- dataHyp2$Pledge_USD / dataHyp2$Goal_USD

dataHyp2 <- subset(dataHyp2, is.finite(dataHyp2$Pledge_USD) & is.finite(dataHyp2$Goal_USD) & is.finite(dataHyp2$success_rate))

# make a new column with order which orders the projects per creator_slug based on launched_at.
# So the first project of a creator should get 1, the second 2 ..
dataHyp2 <- dataHyp2 %>%
  arrange(Launched_at) %>%
  group_by(creator_slug) %>%
  mutate(Order = row_number())

dataHyp2$duration <- as.numeric(dataHyp2$Deadline) - as.numeric(dataHyp2$Launched_at)

# Define the vector of words
word_dict <- c(
  "ad lib",
  "adroit",
  "adroitness",
  "bright idea",
  "clever",
  "cleverness",
  "conceive",
  "concoct",
  "concoction",
  "concoctive",
  "conjure up",
  "creative",
  "creativity",
  "develop",
  "developed",
  "dream",
  "dream up",
  "expert",
  "formulation",
  "freethinker",
  "genesis",
  "genius",
  "gifted",
  "hit upon",
  "imagination",
  "imaginative",
  "improvise",
  "ingenious",
  "ingenuity",
  "innovate",
  "innovated",
  "innovates",
  "innovating",
  "innovation",
  "innovations",
  "innovative",
  "innovativeness",
  "introduced",
  "introducing",
  "introduction",
  "introductions",
  "invent",
  "invented",
  "invention",
  "inventive",
  "inventiveness",
  "inventor",
  "launch",
  "launched",
  "launching",
  "master stroke",
  "mastermind",
  "metamorphose",
  "metamorphosis",
  "neoteric",
  "neoterism",
  "neoterize",
  "new capabilities",
  "new capability",
  "new compounds",
  "new content",
  "new core areas",
  "new course",
  "new directions",
  "new family",
  "new features",
  "new generation",
  "new generations",
  "new idea",
  "new ideas",
  "new line of business",
  "new medicine",
  "new medicines",
  "new molecular entities",
  "new pharmaceuticals",
  "new platform",
  "new process",
  "new processes",
  "new product",
  "new products",
  "new solutions",
  "new systems",
  "new technique",
  "new techniques",
  "new technologies",
  "new technology",
  "new therapies",
  "new thinking",
  "new tools",
  "new treatments",
  "new ways",
  "new wrinkle",
  "new-generation",
  "new-product",
  "next generation",
  "next-generation",
  "novation",
  "novel",
  "novelty",
  "patent",
  "patented",
  "patents",
  "process development",
  "product development",
  "product launch",
  "product launches",
  "proprietary",
  "prototype",
  "prototyping",
  "push the envelope",
  "R&D",
  "radical",
  "re-engineering",
  "reformulated",
  "refreshed",
  "reinvent",
  "re-invent",
  "reinvented",
  "reinventing",
  "reinvention",
  "reinvents",
  "released",
  "renewal",
  "renewing",
  "research",
  "reshape",
  "reshaped",
  "reshapes",
  "reshaping",
  "resourceful",
  "resourcefulness",
  "restyle",
  "restyling",
  "revolutionary",
  "revolutionize",
  "revolutionized",
  "roll out",
  "rolled out",
  "see things",
  "technologically advanced",
  "think up",
  "trademark",
  "transform",
  "transformation",
  "transformed",
  "transforming",
  "visualize"
)

innovation_ratios <- vector(mode='numeric', length=nrow(dataHyp2))
for (i in 1:nrow(dataHyp2)) {
  innovation_counter <- 0
  for (word in word_dict) {
    innovation_counter <- innovation_counter + stringr::str_count(dataHyp2$Project_description[i], paste("(?i)", word, sep=''))
  }
  innovation_ratios[i] <- innovation_counter/stringr::str_count(dataHyp2$Project_description[i], ' ')
}

dataHyp2$innovation <- innovation_ratios * 1000

dataHyp2$orderFactor <- as.factor(dataHyp2$Order)

dataHyp2 <- dataHyp2 %>%
  arrange(creator_slug, Order) %>%  # Sort the dataframe by 'id' and 'order'
  group_by(creator_slug) %>%
  mutate(previousSuccess = lag(success_rate, default = 1))

#### make a lm
mod0 <- lm(innovation ~ Order, data=dataHyp2)
mod1 <- lm(innovation ~ orderFactor, data=dataHyp2)
mod2 <- lm(innovation ~ Goal_USD, data=dataHyp2)
mod3 <- lm(innovation ~ duration, data=dataHyp2)
mod4 <- lm(innovation ~ previousSuccess, data=dataHyp2)
mod5 <- lm(innovation ~ Category, data=dataHyp2)

modA <- lm(innovation ~ Order + Goal_USD + duration, data=dataHyp2)
modB <- lm(innovation ~ orderFactor + Goal_USD + duration, data=dataHyp2)
modC <- lm(innovation ~ Order + Goal_USD + duration + previousSuccess, data=dataHyp2)
modD <- lm(innovation ~ orderFactor + Goal_USD + duration + previousSuccess, data=dataHyp2)
modE <- lm(innovation ~ Order + Goal_USD + duration + previousSuccess + previousSuccess * Order, data=dataHyp2)
modF <- lm(innovation ~ orderFactor + Goal_USD + duration + previousSuccess * orderFactor, data=dataHyp2)
modG <- lm(innovation ~ Order + Goal_USD + duration + previousSuccess + previousSuccess * Order + Category, data=dataHyp2)
modH <- lm(innovation ~ orderFactor + Goal_USD + duration + previousSuccess * orderFactor + Category, data=dataHyp2)

texreg::screenreg(list(mod0, mod1, mod2, mod3, mod4, mod5, modA, modB, modC, modD, modE, modF, modG, modH))
texreg::screenreg(list(mod0, mod2, mod3, mod4, modA, modC, modE))

summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(modA)
summary(modB)
summary(modC)
summary(modD)
summary(modE)
summary(modF)
summary(modG)
summary(modH)

#### make graph of innovation across different orders

dataHyp2 <- dataHyp2 %>%
  arrange(creator_slug, Order) %>%  # Sort the dataframe by 'id' and 'order'
  group_by(creator_slug) %>%
  mutate(change = innovation - lag(innovation, default = first(innovation)))

dataHyp2 %>% ggplot(aes(x=factor(Order), y=change)) +
                      geom_boxplot() +
                      geom_line(stat="summary", fun="mean", aes(group=1, y=innovation), color="red", size=1) +
                      labs(title="Change innovation over time",
                           x="Time",
                           y="Change in innovation compared to time-1")

ggplot(dataHyp2, aes(x = Order, y = change)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean innovation by Order", x = "Order", y = "Innovation")

dataHyp2 %>% ggplot(aes(x=factor(Order), y=innovation)) +
  geom_boxplot() +
  labs(title="Change innovation over time",
       x="Time",
       y="Change in innovation compared to time-1")

ggplot(dataHyp2, aes(x = Order, y = innovation)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean innovation by Order", x = "Order", y = "Innovation")

pval_data <- data.frame(x=1:51, y=c(0.558542,0.000444,0.001761,0.019599,0.060136,0.280711,0.019939,0.628639,0.053100,0.031022,0.124358,0.132697,0.041261,0.044061,0.015593,0.042814,0.043052,0.023550,0.207842,0.037092,0.063320,0.062987,0.093064,0.090276,0.157054,0.154718,0.186388,0.167106,0.306590,0.264606,0.243928,0.393743,0.370298,0.382906,0.378965,0.309810,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284,0.441284))

ggplot(pval_data, aes(x=x, y=y)) +
  geom_point() +
  geom_hline(yintercept=0.05, color='red') +
  labs(title="P-value for the project per creator", x="number of previous projects", y="p-value")









dataHyp2 <- dataHyp2 %>%
  arrange(creator_slug, Order) %>%  # Sort the dataframe by 'id' and 'order'
  group_by(creator_slug) %>%
  mutate(change = innovation - lag(innovation, default = first(innovation)))

dataHyp2 %>% ggplot(aes(x=factor(Order), y=change)) +
  geom_boxplot() +
  geom_line(stat="summary", fun="mean", aes(group=1, y=innovation), color="red", size=1) +
  labs(title="Change in innovation for number of projects",
       x="Number of projects",
       y="Change in innovation compared to time-1")

ggplot(dataHyp2, aes(x = Order, y = change)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean change in innovation for number of projects", x = "Number of projects", y = "Innovation")

dataHyp2 %>% ggplot(aes(x=factor(Order), y=innovation)) +
  geom_boxplot() +
  labs(title="Innovation for number of projects",
       x="Number of projects",
       y="Innovation")

ggplot(dataHyp2, aes(x = Order, y = innovation)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean innovation for number of projects", x = "Number of projects", y = "Innovation")

plot(dataHyp2$duration, dataHyp2$innovation)
