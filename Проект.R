library(haven)
library(plm)
library(clubSandwich)
library(car)
library(stargazer)
library(lmtest)
library(texreg)

dd <- read_sav("data.sav", col_select = c("idind","year", "educ", "status", "j13.2" ,"age", "j72.172", "j65", "j66.1", "j721632", "l5", "m3", "h5", "region", "m80", "j132.1", "marst"))
# возраст
dd <- dd[abs(dd$age) > 16,]
# образование
dd <- dd[abs(dd$educ) <9999999,]
# тип местности
dd <- dd[abs(dd$status) <999999,]
# кол-во детей
dd <- dd[abs(dd$j72.172) <999999,]
# средняя зарплата
dd <- dd[abs(dd$j13.2) <999999,]
dd <- dd[abs(dd$j13.2) >0,]
# удовлетворённость жизнью
dd <- dd[abs(dd$j65) <999999,]
# можете ли обеспечить детям походы в доп секции?
dd <- dd[abs(dd$j721632) <999999,]
# удовлетворённость финансовым положением
dd <- dd[abs(dd$j66.1) <999999,]
# проблемы со здоровьем в последнее время
dd <- dd[abs(dd$l5) <999999, ]
# оценка здоровья
dd <- dd[abs(dd$m3) <999999, ]
# употребление алкоголя в последнее время
dd <- dd[abs(dd$m80) <999999, ]
# помогает ли религия?
dd <- dd[abs(dd$j132.1) <999999, ]
# семейное положение
dd <- dd[abs(dd$marst) <999999, ]

dd <- na.omit(dd)
dd <- dd[abs(dd$year) > 2014,]

dd_panel <- pdata.frame(dd, index = c("idind", "year"), Education = dd$educ, Status = dd$status, Age = dd$age, Sex = dd$h5, 
                        AverageSalary = dd$j13.2, NumberOfChildren = dd$j72.172, Region = dd$region, 
                        FinancialSatisfaction = dd$j66.1, LifeSatisfaction = dd$j65, AdditionalSections = dd$j721632,
                        HealthProblems = dd$l5, Health = dd$m3, Alcohol = dd$m80, Religion = dd$j132.1, Marst = dd$marst)
m1 <- plm(LifeSatisfaction ~ Education + Status + log(AverageSalary) + I(log(AverageSalary)^2) + Age + I(Age^2) + NumberOfChildren 
          + FinancialSatisfaction + AdditionalSections + HealthProblems + Health + Alcohol + Region + Sex + Marst, 
          data = dd_panel,
          effect = "time",
          model = "within")
summary(m1, vcov = vcovHC(m1))
stargazer(m1, se = list(sqrt(diag(vcovHC(m1)))), type = "html", out = "md.tex")

m.pooled <- plm(log(j65) ~ Education + Status + j13.2 + age + j72.172 + j66.1 + j721632 + l5 + m3 + m80 + region + h5 + j132.1, 
          data = dd_panel,
          model = "pooling")

m.re <- plm(log(j65) ~ educ + status + j13.2 + age + j72.172 + j66.1 + j721632 + l5 + m3 + m80 + region + h5, 
          data = dd_panel,
          model = "random")

m.fe <- plm(log(j65) ~ educ + status + j13.2 + age + j72.172 + j66.1 + j721632 + l5 + m3 + m80 + region + h5, 
          data = dd_panel,
          model = "within")

summary(m1, vcov = vcovHC(m1))
summary(m.pooled, vcov = vcovHC(m.pooled))
summary(m.re, vcov = vcovHC(m.re))
summary(m.fe, vcov = vcovHC(m.fe))


htmlreg(list(m.pooled, m.re, m.fe), 
        custom.model.names = c("Pooling", "RE", "FE"))
vif(m.fe)
vif(m1)


pFtest(m.fe, m.pooled)
phtest(m.fe, m.re)


plmtest(m.re, type = "bp")
stargazer(m1, se = list(sqrt(diag(vcovHC(m1)))), type = "html", out = "md.doc")



# попытка сделать взвешенный МНК
bptest(m1)
m1$residuals
m1$fitted.values
beta_hat <- coef(m1)
eps_hat <- residuals(m1)
sqrt( deviance (m1)/df.residual (m1)) # = 0.6733828

