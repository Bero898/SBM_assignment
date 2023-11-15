# Install and load required packages
install.packages(c("tidytext", "dplyr", "ggplot2"))
library(tidytext)
library(dplyr)
library(ggplot2)

#inspect the data
info_filename <- file.choose()
info <- readRDS(info_filename)


#create a dataset with only creator_slug that appear more than once in the dataset
dataHyp2Double <- info[info$creator_slug %in% names(which(table(info$creator_slug) > 1)),]

dataHyp2sub<- dataHyp2Double[,c('creator_slug','Goal_USD','Pledge_USD','Launched_at','Deadline','Project_description','Creator_nb_projects','Creator_nb_backed')]

dataHyp2filtered <- na.omit(dataHyp2sub)

#make a subdataset where deadline is before week 32 in 2019 in unix time
dataHyp2 <- dataHyp2filtered[dataHyp2filtered$Deadline < 1565474400,]


#make a new column with boolean if goal_USD >= Pledge_USD
dataHyp2$Pledge_USD <- as.numeric(dataHyp2$Pledge_USD)
dataHyp2$Goal_USD <- as.numeric(dataHyp2$Goal_USD)
dataHyp2$success_rate <- dataHyp2$Pledge_USD / dataHyp2$Goal_USD


# make a new column with order which orders the projects per creator_slug based on launched_at.
# So the first project of a creator should get 1, the second 2 ..
dataHyp2 <- dataHyp2 %>%
  arrange(Launched_at) %>%
  group_by(creator_slug) %>%
  mutate(Order = row_number())


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

dataHyp2$innovation <- innovation_ratios

#### make a 



#### make graph of innovation across different orders

#This is not a good plot, because we're aggregating. We should look at each creator individually.Ï
ggplot(dataHyp2, aes(x = Order, y = innovation)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean innovation by Order", x = "Order", y = "Innovation")


