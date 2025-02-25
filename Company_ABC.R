library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

set.seed(123)

years <- 2010:2025

employees <- numeric(length(years))
profit <- numeric(length(years))

employees[1] <- 1050
profit[1] <- 1e5

for (i in 2:length(years)){
  if ( years[i] < 2020){
   employees[i] <- employees[i - 1] *1.2
   profit[i] <- profit[i - 1]* 1.2
  }
  else if (years[i] == 2020){
    employees[i] <- employees[i - 1] * 0.75
    profit[i] <- profit[i - 1] * 0.80
  }
  else{
    layoff <- runif(1, 0.08, 0.12)
    employees[i] <- employees[i - 1] * (1 - layoff)
    profit[i] <- profit[i - 1] * 1.1
  }
}

abc <- data.frame(
  years = years,
  employees=round(employees),
  profit=round(profit)
)

ggplot(abc, aes(x = years), size = 1)+
  geom_line(aes(y = employees, color = "Employees"), size = 1)+
  geom_line(aes(y = profit/1e5, color = "profit(in thousands"), size = 1)+
  scale_y_continuous(
    name = "Employees",
    sec.axis = sec_axis(~. * 1e5, name = "profit(in Rs.)", label = comma)
  )+
  scale_color_manual(values = c("Employees" = "yellow", "profit(in thousands)" = "red"))+
  labs(
    title = "Company's Growth: Profit&Employees over the time",
    x = "Year",
    color = "Legend"
  )+
  theme_economist()


summary(abc)
sd(abc$profit)
sd(abc$employees)

before_2020 <- subset(abc, years < 2020)
after_2020 <- subset(abc, years >= 2020)

t.test(before_2020$employees, after_2020$employees, var.equal = TRUE)
t.test(before_2020$profit, after_2020$profit, var.equal = TRUE)

cor(abc$profit, abc$employees)

model = lm(profit ~ employees, data = abc)

summary(abc)

anova(model)
