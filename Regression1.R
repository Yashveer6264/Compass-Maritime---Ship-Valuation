library(readxl)
shipdata <- read_excel("E:/Stat 610/Case Study/Part2/RegressionData - Copy.xlsx")

dfdata = data.frame(shipdata)

dfdata

plot(dfdata$Age.at.Sale..Years.,dfdata$Sale.Price...US.millions.)

reg <- lm(Sale.Price...US.millions.~Age.at.Sale..Years., data=dfdata)
plot(dfdata$Age.at.Sale..Years.,dfdata$Sale.Price...US.millions.)
lines(dfdata$Age.at.Sale..Years.,predict(reg),col="red")
summary(reg)
-- R Squared : It means sale price can expalin 61% of the variation in age

plot(dfdata$Dead.Weight.Tons...000.,dfdata$Sale.Price...US.millions.)


reg2 <- lm(Sale.Price...US.millions.~ Dead.Weight.Tons...000., data=dfdata)
plot(dfdata$Dead.Weight.Tons...000.,dfdata$Sale.Price...US.millions.)
summary(reg2)
lines(dfdata$Dead.Weight.Tons...000.,predict(reg2),col="blue")

reg3 <- lm(Sale.Price...US.millions.~Trailing.1.Year.Average.Monthly.Baltic.Dry.Capesize.Index, data=dfdata)
plot(dfdata$Trailing.1.Year.Average.Monthly.Baltic.Dry.Capesize.Index,dfdata$Sale.Price...US.millions.)
lines(dfdata$Trailing.1.Year.Average.Monthly.Baltic.Dry.Capesize.Index,predict(reg3),col="blue")
