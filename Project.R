# Import dataset (comm_prop)
library(readr)
comm_prop <- read_csv("C:/Users/steal/Data Science/Stats/MSDS 5043/Data Sets/comm_prop.csv")

# Explore dataset - Functions{View, head, str}
View(comm_prop)
head(comm_prop)
ls.str(comm_prop)
summary(comm_prop)

# The variable 'W2MiDT' is a factor. Convert it into a factor
comm_prop$W2MiDT <- factor(comm_prop$W2MiDT, levels = c(0, 1), labels = c("No", "Yes"))
show(comm_prop$W2MiDT)


##########################################################################################


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
plot(comm_prop$W2MiDT, xlab="Building Located within 2 Miles of Downtown", ylab ="Frequency")


####################################################################################


# Examine the Correlation amongst  nmerical variables (Scatterplot Matrix & Correlation Matrix)
library(corrplot)
 # Scatterplot Matrix - Function{pairs}
pairs(comm_prop[,1:6], pch = 16)

 # Correlation Matrix - Function{cor}
cor(comm_prop[,1:6])

corrplot(cor(comm_prop[,1:6]), method = "ellipse")




##########################################################################################
library(car)
# Fit full model to a Linear Model
mod.lm <- lm(RentRate ~ ., data = comm_prop)
summary(mod.lm)


# Examine model's Residual Plots 
library(ggplot2)
mod.lm.df <- fortify(mod.lm)
ggplot(mod.lm.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")

# Check for Normality
qqPlot(mod.lm$residuals, pch=16)
shapiro.test(mod.lm$residuals)

# Check for Non-Cnstant Varience (NCV)
ncvTest(mod.lm)

# Check for Multicollinearity
vif(mod.lm)

####################################################################################

# Iterate Manually & Drop Significant Variables

mod1 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt  + W2MiDT, data = comm_prop)   # Remove Taxes
summary(mod1)                  

mod2 <- lm(RentRate ~ Age + OperExp + SqFt + W2MiDT, data = comm_prop)   # Remove VacRate
summary(mod2)



# Plot the fit
mod2.df <- fortify(mod2)
ggplot(mod2.df, aes(x=.fitted, y=RentRate)) +
  geom_point(size=2) +
  geom_smooth(method="lm", color="red", lwd=1.5) +
  labs(x="Sales Predicted", y="Sales Actual")

# Examine Models' Residuals's Variance & Distribution
ggplot(mod2.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")

shapiro.test(mod2$residuals)
pairs(comm_prop[-c(4,6)], pch=16)

ncvTest(mod2)

vif(mod2)

###################################################
# Other variables that could have been included:
# Neighborhood (Restuarants, etc)


pred.RentRate <- data.frame(Age = 9, OperExp = 13, SqFt = 40, Taxes = 540, W2MiDT = "No")
predict(mod2, newdata = pred.RentRate, interval = "predict")



model_all <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + 
                  Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:SqFt + 
                  OperExp:W2MiDT + VacRate:SqFt + VacRate:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_all)


# Create Model and then run a 'All Substs Regression' (ASR)
library(leaps)
subsets <-regsubsets(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + 
             Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:SqFt + 
             OperExp:W2MiDT + VacRate:SqFt + VacRate:W2MiDT + SqFt:W2MiDT, data=comm_prop)
# Plot model and evaluate recommended variables
plot(subsets, scale = 'adjr2' )

# Eliminate: VacRate, SqFt, Age:W2MiDT, OperExp:VacRate, OperExp:SqFt, VacRate:SqFt,
# VacRate:W2MiDT + Interate another ASR Model (Include Lower Order Terms)

# Create liniar model from recommended variables
mod_lm_subset1 <- lm(RentRate ~ Age + OperExp + W2MiDT + VacRate + SqFt + Age:OperExp + 
                       Age:VacRate + Age:SqFt + OperExp:W2MiDT+ SqFt:W2MiDT, data = comm_prop)
summary(mod_lm_subset1)
# The following variables would be eliminated due to possessing insignificant P-values. 
# Even though the Adjusted R-Squared increased (.8414),  the Mult. R=Squared valued increased (.8574)
# as well as the F-Statistic decreasing concluding that there are variables present 
# that we do not want to include. Thus we would recommend using our origional linear model.
#                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Eliminate: VacRate, SqFt, Age:W2MiDT, OperExp:VacRate, OperExp:SqFt, VacRate:SqFt,
# VacRate:W2MiDT + Interate another ASR Model (Include Lower Order Terms)



# Stepwise Regression
library(MASS)
mod_step <- lm(RentRate ~ .*., data = comm_prop)
stepAIC(mod_step, direction = 'backward')

# Suggested lm model:
# lm(formula = RentRate ~ Age + OperExp + VacRate + Taxes + W2MiDT + Age:OperExp + 
# Age:VacRate + OperExp:VacRate + OperExp:W2MiDT + Taxes:W2MiDT, data = comm_prop)