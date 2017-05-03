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
mod_lm_subset1 <- lm(RentRate ~ Age + OperExp + W2MiDT + Age:OperExp + 
                       Age:VacRate + Age:SqFt + OperExp:W2MiDT+ SqFt:W2MiDT, data = comm_prop)
summary(mod_lm_subset1)


# Visually & Formally test stepwise model's Variance and Distributuion of Residuals
mod_sub.df <- fortify(mod_lm_subset1)
ggplot(mod_step.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Sales Predicted", y="Sales Actual")
# Good Variance


shapiro.test(mod_lm_subset1$residuals)

ncvTest(mod_lm_subset1)

vif(mod_lm_subset1)



# The following variables would be eliminated due to possessing insignificant P-values. 
# Even though the Adjusted R-Squared increased (.8414),  the Mult. R=Squared valued increased (.8574)
# as well as the F-Statistic decreasing concluding that there are variables present 
# that we do not want to include. Thus we would recommend using our origional linear model.
#                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Eliminate: VacRate, SqFt, Age:W2MiDT, OperExp:VacRate, OperExp:SqFt, VacRate:SqFt,
# VacRate:W2MiDT + Interate another ASR Model (Include Lower Order Terms)