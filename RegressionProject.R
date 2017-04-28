library(tidyverse)
library(corrplot)

df.full <- lm(RentRate ~ ., data = comm_prop)
summary(df.full)
cor(comm_prop)
corrplot(cor(comm_prop), method="ellipse")

#removed the 3 variables that don't seem to have much impact
dfLessMin <- lm(RentRate ~ Age + OperExp + W2MiDT, data = comm_prop)
summary(dfLessMin)

#Adjust for multicollinearity and add back in SqFt
df_model <- lm(RentRate ~ Age + OperExp + W2MiDT + SqFt, data = comm_prop)
summary(df_model)

#residual stuff
full.df <- fortify(df_model)
ggplot(full.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")

# Check for normality
qqPlot(df_model$residuals, pch=16)
shapiro.test(df_model$residuals)
pairs(comm_prop[-c(4,6)], pch=16)

# Check for NCV
ncvTest(df_model)

# Check for multicollinearity
vif(df_model)

# Plot of the fit
ggplot(full.df, aes(x=.fitted, y=RentRate)) +
  geom_point(size=2) +
  geom_smooth(method="lm", color="red", lwd=1.5) +
  labs(x="Rent Predicted", y="Rent Actual")

new_RentRate <- data.frame(Age=9, OperExp=13, SqFt=40, Taxes=540, W2MiDT='No')
predict(df_model, newdata=new_RentRate, interval="predict")
