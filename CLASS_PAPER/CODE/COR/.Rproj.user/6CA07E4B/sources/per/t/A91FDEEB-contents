##install packages-----
install.packages("ggthemes")
install.packages('RColorBrewer')
install.packages("cowplot")
install.packages("scales")
install.packages("smplot2")
#install.packages("installr")
install.packages('gridExtra')
install.packages('grid')
install.packages("plotly")
devtools::install_github("ropensci/plotly")
install.packages(c('tidyverse','devtools','gghalves'))
install.packages("Hmisc")#rcorr function, gives you correlation coefficient and p-value
install.packages("corrplot")#plot the correlations
install.packages("kim")
install.packages("psych")

library(ggplot2) # to create data visualizations 
library(ggthemes) # use themes to clean up the data visualizations 
library(RColorBrewer) # for multiple color palettes
library(dplyr)
library(cowplot)
library(scales)
library(gridExtra)
library(grid)
library(tidyr) # v. 0.8.3
library(plotly)
library(Hmisc)
library(corrplot)
library(kim)
library(psych)
###=I37&" , "&J37&","&K37
correl <- corr.test(data$OBSERVED_LWMA, data$X.cw, use = "pairwise", method="pearson")

########----
data <- read.csv("FIG.csv")


###PEARSON WITHOUT LOGGING-----


data <- read.csv("FIG.csv")

#####convert the data frame to numeric
df2 <- data.frame(sapply(data, function(x) as.numeric(as.character(x))))
str(df2)
df2 <- abs(df2)


correl <- corr.test(as.matrix(df2), use = "pairwise", method="pearson")


P <- corr.p(correl$r, correl$n)

R <- round(correl$r,2) #Extract the correlation coefficients
write.csv(R, "R_PEARSON_NOLOG_regression.csv")

R <- correl$p #Extract the p-values
write.csv(R,"P_PEARSON_N0LOG_regression.csv")


#Plot of relationships
pairs(ftdata)







###PEARSON WITH LOGGING-----

data <- read.csv("FIG.csv")
str(data)
#data <- na.omit(data)
data <- data |>
  filter(TAX !="G") 


data = subset(data, select = -c(ID,TAX, LOC) )

data <- data.frame(sapply(data, function(x) as.numeric(as.character(x))))
str(data)

data <- data |>
  mutate(Po = abs(Po)) |>
  mutate(YTLP = abs(YTLP)) |>
  mutate(af = abs(af)) 
#|>
  #summarise(across(everything(), mean))


data1 = abs(data)

data1 = log(data1)
#data1 = abs(data1)

data1[is.na(data) | data1 == "inf" = NA ]
#correl <- cor(data1)
#Correlation matrix
#correl <- rcorr(as.matrix(data1),type="pearson")

correl <- corr.test(as.matrix(data1), use = "pairwise", method="pearson")


P <- corr.p(correl$r, correl$n)



R <- round(correl$r,2) #Extract the correlation coefficients
write.csv(R, "R_PEARSON_LOG_regression.csv")

R <- correl$p #Extract the p-values
write.csv(R,"P_PEARSON_LOG_regression.csv")

#Plot correlation
#M<-cor(data1)
#corrplot(M, method="number")

#Plot of relationships
pairs(ftdata)





###SPEARMAN CORRELATION-----
data <- read.csv("FIG.csv")
#data <- na.omit(data)

data <- data |>
  filter(TAX !="G") 
  #group_by( ID) %>%
  #summarise(across(everything(), mean))

data1 = subset(data, select = -c(ID,TAX,LOC) )


data1 <- data.frame(sapply(data, function(x) as.numeric(as.character(x))))
str(data1)

data1 <- abs(data1)
#correl <- cor(data1)
#Correlation matrix
#correl <- rcorr(as.matrix(data1),type="spearman")

correl <- corr.test(as.matrix(data1), use = "pairwise", method="spearman")


P <- corr.p(correl$r, correl$n)




#correl <- cor(data1, use = "pairwise.complete.obs", method="spearman")

R <- round(correl$r,2) #Extract the correlation coefficients and rounds to 2 decimals
write.csv(R, "R_SPEARMAN_regression.csv")

R <- correl$p #Extract the p-values
write.csv(R,"P_SPEARMAN_regression.csv")

#Plot correlation
M<-cor(data1, use = "complete.obs", method="spearman")
corrplot(M, method="number")

#Plot of relationships
pairs(ftdata)



RNO <- read.csv("R_NOLOG_regression.csv")
RLOG <- read.csv("RLOG_regression.csv")
RSPEAR <- read.csv("R_SPEARMAN_regression.csv")

all_of_the_marbles <- rbind(c("RNO", "RLOG", "RSPEAR"))




Hmisc::rcorr

correlation_matrix(data1)

R <- correlation_madata1R <- correlation_matrix (data1,
                               type = "pearson",
                               digits = 2,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = "")

