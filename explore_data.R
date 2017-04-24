# Import dataset (comm_prop)
library(readr)
library(tidyverse)
comm_prop <- tbl_df(read_csv("comm_prop.csv"))
comm_prop_matrix <- read_csv("comm_prop.csv")

# Explore dataset - Functions{View, head, str}
dim(comm_prop)
names(comm_prop)
head(comm_prop)
summary(comm_prop)


# The variable 'W2MiDT' is a factor. Convert it into a factor
comm_prop$W2MiDT <- factor(comm_prop$W2MiDT, labels = c("No", 'Yes'))
show(comm_prop$W2MiDT)





# Examine the distribution of each variable (Histogram)

#                               --------- RentRate -----------
hist(comm_prop$RentRate, xlab = "Rent Rate", main = paste("Histogram of", "Rent Rate (In Thousands)"), col = "indianred")

#                               --------- Age _______________
hist(comm_prop$Age, xlab = "Age", main = paste("Histogram of", "Age"), col = "indianred")

#                               --------- Total Monthly Operating Expenses -------------
hist(comm_prop$OperExp, xlab = "Total Monthly Operating Expenses", main = paste("Histogram of", "Total Monthly Operating Expenses (In Thousands)"), col = "indianred")

#                             ----------- Vacancy Rate ---------------
hist(comm_prop$VacRate, xlab = "Vacancy Rate", main = paste("Histogram of", "Vacancy Rate"), col = "indianred")

#                              ----------- Total Square Footage --------------
hist(comm_prop$SqFt, xlab = "Total Square Footage", main = paste("Histogram of", "Total Square Footage"), col = "indianred") 
  
#                             ------------- Taxes -----------------------------
hist(comm_prop$Taxes, xlab = "Taxes", main = paste("Histogram of", "Total Monthly Tax Expense (In Thousands)"), col = "indianred")


#                            ------------ Building Location ------------------
 #     ****** 'Building Location' is a factor and thus requires a BoxPlot to view it's distribution *********

ggplot(comm_prop$W2MiDT) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() +
  coord_flip() + # Makes regions names easier to read
  labs(x = "Region", y = "Birth Rate")
hist(comm_prop$W2MiDT, xlab = "Building Located Within 2 Miles of Downtown", main = paste("Histogram of", "Age"), col = "indianred")






# Examine the Correlation amongst  nmerical variables (Scatterplot Matrix & Correlation Matrix)

 # Scatterplot Matrix - Function{pairs}
pairs(comm_prop[,1:6], pch = 16)

 # Correlation Matrix - Function{cor}
cor(comm_prop[,1:6])
