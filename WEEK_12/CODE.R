library(tidyverse)

# Load data 
Owls <- read.csv("Owls.csv")  # Create variables used in the analysis
Owls$NCalls     <- Owls$SiblingNegotiation 
Owls$LBroodSize <- log(Owls$BroodSize) 
Owls$NestNight  <- factor(ifelse(Owls$FoodTreatment == "Deprived",
                                 paste(Owls$Nest, ".Dep", sep=""),
                                 paste(Owls$Nest, ".Sat", sep="")))


# Scatter plot: NCalls vs ArrivalTime
ggplot(Owls, aes(x = ArrivalTime, y = NCalls)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Number of Calls vs Arrival Time",
       x = "Arrival Time (hours)",
       y = "Number of Calls") +
  theme_minimal()

# Box + violin plot: NCalls by FoodTreatment
ggplot(Owls, aes(x = FoodTreatment, y = NCalls, fill = FoodTreatment)) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.color = "red", outlier.shape = 16) +
  labs(title = "Number of Calls by Food Treatment",
       x = "Food Treatment",
       y = "Number of Calls") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Fit Poisson GLM
glm_poisson <- glm(NCalls ~ SexParent * FoodTreatment + SexParent * ArrivalTime + offset(LBroodSize),
                   family = poisson(link = "log"),
                   data = Owls)

# Summary of model
summary(glm_poisson)

