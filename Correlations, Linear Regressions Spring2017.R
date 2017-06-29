#3.6
cash_ast = c( 76, 70, 68, 52, 48, 46)
noncash = 100-cash_ast
names(noncash) = c( "97", "98", "99","00", "01", "02")
names(cash_ast) = c( "97", "98", "99","00", "01", "02")
total.spending = rbind(noncash, cash_ast)
par(mfrow = c(2, 1))
barplot(cash_ast, xlab = "Year",
        ylab = "Percentage of Cash Assitance",
        main = "Cash Assistance from '97 to '02")
barplot(noncash, xlab = "Year",
        ylab = "Percentage of Non-Cash Assitance",
        main = "Non-Cash Assistance from '97 to '02")
par(mfrow = c(1, 1))
barplot(total.spending, main="Cash Assistance vs. Non-Cash",
        xlab="Years", col=c("darkblue","red"),
        legend.text = TRUE, 
        args.legend = list(x = "topright", bty = "n"), beside=TRUE)

#3.10

data("stud.recs")
attach(stud.recs)
par(mfrow = c(1, 2))
plot(density(sat.v),ylim=c(0,0.0075), main="Densityplots of Verbal and Math SAT Scores")
lines(density(sat.m), lty=2)
legend(150,0.0068, legend=c("Verbal Scores", "Math"), lty = c(1, 2))
boxplot(sat.m, sat.v, names= c("Sat Math","SAT Verbal"),main = "Sat Score for Incoming Students", xlab = "Test", ylab="SAt Score")
density (sat.v)
density(sat.m)

summary(sat.v)
summary(sat.m)
qqnorm (sat.v, main = "Normal Q-Q Plot of SAT Verbal Scores", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",plot.it = TRUE, datax = FALSE)
qqline(sat.v)
shapiro.test(sat.v)
qqnorm (sat.m, main = "Normal Q-Q Plot of SAT Math Scores", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",plot.it = TRUE, datax = FALSE)
qqline(sat.m)
shapiro.test(sat.m)
par(mfrow = c(1, 1))

detach(stud.recs)

#3.18



x77 = data.frame(state.x77)
attach(x77)
plot (Population, Frost, main="Scatterplot of Population vs. Frost", 
      xlab="Population ", ylab="Frost ")

plot (Population, Murder, main="Scatterplot of Population vs. Murder", 
      xlab="Population ", ylab="Murder ")

plot (Population, Area, main="Scatterplot of Population vs. Area", 
      xlab="Population ", ylab="Area ")

plot (Income, HS.Grad, main="Scatterplot of Income vs. HS Graduation Rate", 
      xlab="Income ", ylab="High School Graduation Rate ")
abline(lm(HS.Grad~Income), col="red") # regression line (y~x) 

detach (x77)
#3.26


x77 = data.frame(state.x77)
attach(x77)

plot (Illiteracy, HS.Grad, main="Scatterplot of Illiteracy vs. HS Graduation Rate", 
      xlab="Illiteracy Rate ", ylab="High School Graduation Rate ")
abline(lm(HS.Grad~Illiteracy), col="red") # regression line (y~x) 


plot (Life.Exp, Murder, main="Scatterplot of Life Expectancy vs. Murder Rate", 
      xlab="Life Expectancy Rate ", ylab="Murder Rate Rate ")
abline(lm(Murder~Life.Exp), col="red") # regression line (y~x) 


plot (Income, Illiteracy, main="Scatterplot of Income vs. Illiteracy rate", 
      xlab="Income ", ylab="Illiteracy Rate ")
abline(lm(Illiteracy~Income), col="red") # regression line (y~x) 


detach(x77)

#3.27

library(UsingR)
data("batting")
attach(batting)

hr.reg =  lm(RBI~HR)
summary(hr.reg)

plot (HR, RBI, main="Scatterplot of RBIs vs. Home Runs", 
      xlab="Home Runs ", ylab="RBIs")
abline(lm(RBI~HR), col="red") # regression line (y~x) 


predict(hr.reg, data.frame(HR=33))

reg.rbi.quad=lm(RBI ~ HR + I(HR^2))
summary(reg.rbi.quad)
predict(reg.rbi.quad, data.frame(HR = 33))
plot(RBI ~ HR)
polynomial = function (x, coefs) {
   tot = 0
   for(i in 1:length(coefs)) tot = tot+coefs[i]*x^(i-1)
   tot
}
curve(polynomial(x, coef(reg.rbi.quad)), add = T)
anova(reg.rbi.quad, hr.reg)
detach(batting)
#10.6

house.price = c(300, 250, 400, 550, 317, 389, 425, 289, 389 )
num.bedrooms = c(3, 3, 4, 5, 4, 3, 6, 3, 4 )
house.data = cbind.data.frame(house.price, num.bedrooms)
house.data
bedr.reg = lm(num.bedrooms~house.price)
summary(bedr.reg)

plot (num.bedrooms, house.price, main="Scatterplot of Housing Price vs. Number of Bedrooms", 
      ylab="Housing Price ", xlab="Number of Bedrooms")
abline(lm(house.price~num.bedrooms), col="red") # regression line (y~x) 

house.reg = lm(house.price~num.bedrooms)
summary(house.reg)

#10.11 

data("homedata")
attach(homedata)
price.reg = lm(y2000~y1970)
summary(price.reg)

predict(price.reg, data.frame(y1970=80000))
par(mfrow = c(2, 2))
plot(price.reg)
par(mfrow = c(1, 1))

#10.24

data("deflection")
attach(deflection)

deflect.reg = lm(Deflection ~ Load + I(Load^2))
summary(deflect.reg)

plot(Load, Deflection, main = "Load vs. Deflection", xlab = "Load", ylab = "Deflection")
abline(deflect.reg)
polynomial = function(x, coefs) {
  tot = 0
  for(i in 1:length(coefs)) tot = tot + coefs[i]*x^(i-1)
   tot}
curve(polynomial(x, coef(deflect.reg)), add = T, lty = 2)
legend(0,1.09,legend=c("Linear","Quadratic"), lty=c(1,2))
plot(deflect.reg)