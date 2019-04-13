---
title: "Case Study 2"
author: "Brady Arendale"
date: "April 11, 2019"
output: 
  html_document:
    keep_md: true
---
# T-test

```r
# Read in data
workers <- read.csv("CaseStudy2-data.csv")
names(workers)[1] <- "Age"
workers2 <- workers
# Create dummy variables
businesstravel <- model.matrix(~0+BusinessTravel, data = workers)
department <- model.matrix(~0+Department, data = workers)
educationfield <- model.matrix(~0+EducationField, data = workers)
gender <- model.matrix(~0+Gender, data = workers)
jobrole <- model.matrix(~0+JobRole, data = workers)
maritalstatus <- model.matrix(~0+MaritalStatus, data = workers)
overtime <- model.matrix(~0+OverTime, data = workers)
# Add dummy variables to data frame
workers <- data.frame(workers, businesstravel, department, educationfield, gender, jobrole, maritalstatus, overtime)
# Run t-test for each variable on attrition
workers <- workers[,-c(3, 5, 8, 9, 10, 12, 16, 18, 22, 23, 27)]
for (i in 1:52) {
  print(names(workers)[i])
  if(class(workers[,i]) != "factor") {
    print(t.test(workers[,i] ~ Attrition, data = workers))
  } else {
    print("factor")
  }
}
```

```
## [1] "Age"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 5.828, df = 316.93, p-value = 1.38e-08
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.618930 5.288346
## sample estimates:
##  mean in group No mean in group Yes 
##          37.56123          33.60759 
## 
## [1] "Attrition"
## [1] "factor"
## [1] "DailyRate"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 2.1789, df = 333.76, p-value = 0.03004
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##    6.040083 118.243100
## sample estimates:
##  mean in group No mean in group Yes 
##          812.5045          750.3629 
## 
## [1] "DistanceFromHome"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -2.8882, df = 322.72, p-value = 0.004137
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.8870025 -0.5475146
## sample estimates:
##  mean in group No mean in group Yes 
##          8.915653         10.632911 
## 
## [1] "Education"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.2177, df = 336.95, p-value = 0.2242
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.05374319  0.22843290
## sample estimates:
##  mean in group No mean in group Yes 
##          2.927007          2.839662 
## 
## [1] "EnvironmentSatisfaction"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.7513, df = 316.62, p-value = 0.0002092
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.146056 0.468253
## sample estimates:
##  mean in group No mean in group Yes 
##          2.771290          2.464135 
## 
## [1] "HourlyRate"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 0.26477, df = 335.98, p-value = 0.7914
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.432272  3.188891
## sample estimates:
##  mean in group No mean in group Yes 
##          65.95215          65.57384 
## 
## [1] "JobInvolvement"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 4.6602, df = 312.81, p-value = 4.681e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.1453097 0.3576727
## sample estimates:
##  mean in group No mean in group Yes 
##          2.770479          2.518987 
## 
## [1] "JobLevel"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 7.3859, df = 376.25, p-value = 9.845e-13
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.3733861 0.6443231
## sample estimates:
##  mean in group No mean in group Yes 
##          2.145985          1.637131 
## 
## [1] "JobSatisfaction"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.9261, df = 328.59, p-value = 0.0001052
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.1547890 0.4656797
## sample estimates:
##  mean in group No mean in group Yes 
##          2.778589          2.468354 
## 
## [1] "MonthlyIncome"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 7.4826, df = 412.74, p-value = 4.434e-13
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1508.244 2583.050
## sample estimates:
##  mean in group No mean in group Yes 
##          6832.740          4787.093 
## 
## [1] "MonthlyRate"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -0.5755, df = 330.1, p-value = 0.5653
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1296.8656   709.8084
## sample estimates:
##  mean in group No mean in group Yes 
##          14265.78          14559.31 
## 
## [1] "NumCompaniesWorked"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -1.5747, df = 317.14, p-value = 0.1163
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.66437603  0.07367926
## sample estimates:
##  mean in group No mean in group Yes 
##          2.645580          2.940928 
## 
## [1] "PercentSalaryHike"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 0.50424, df = 326.11, p-value = 0.6144
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.3890709  0.6572652
## sample estimates:
##  mean in group No mean in group Yes 
##          15.23114          15.09705 
## 
## [1] "PerformanceRating"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -0.10999, df = 331.22, p-value = 0.9125
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.05350780  0.04784086
## sample estimates:
##  mean in group No mean in group Yes 
##          3.153285          3.156118 
## 
## [1] "RelationshipSatisfaction"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.7019, df = 323.54, p-value = 0.08973
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02102367  0.29067575
## sample estimates:
##  mean in group No mean in group Yes 
##          2.733982          2.599156 
## 
## [1] "StockOptionLevel"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 5.2442, df = 329.67, p-value = 2.812e-07
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.1985054 0.4368288
## sample estimates:
##  mean in group No mean in group Yes 
##         0.8450933         0.5274262 
## 
## [1] "TotalWorkingYears"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 7.0192, df = 350.88, p-value = 1.16e-11
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.604401 4.632019
## sample estimates:
##  mean in group No mean in group Yes 
##         11.862936          8.244726 
## 
## [1] "TrainingTimesLastYear"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 2.3305, df = 339.56, p-value = 0.02036
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03251776 0.38439273
## sample estimates:
##  mean in group No mean in group Yes 
##          2.832928          2.624473 
## 
## [1] "WorkLifeBalance"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 2.1742, df = 302.49, p-value = 0.03047
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.01165453 0.23393357
## sample estimates:
##  mean in group No mean in group Yes 
##          2.781022          2.658228 
## 
## [1] "YearsAtCompany"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 5.2826, df = 338.21, p-value = 2.286e-07
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.404805 3.071629
## sample estimates:
##  mean in group No mean in group Yes 
##          7.369019          5.130802 
## 
## [1] "YearsInCurrentRole"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 6.8471, df = 366.57, p-value = 3.187e-11
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.127107 2.035355
## sample estimates:
##  mean in group No mean in group Yes 
##          4.484185          2.902954 
## 
## [1] "YearsSinceLastPromotion"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.2879, df = 338.49, p-value = 0.1987
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1525043  0.7309843
## sample estimates:
##  mean in group No mean in group Yes 
##          2.234388          1.945148 
## 
## [1] "YearsWithCurrManager"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 6.6334, df = 365.1, p-value = 1.185e-10
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.065929 1.964223
## sample estimates:
##  mean in group No mean in group Yes 
##          4.367397          2.852321 
## 
## [1] "BusinessTravelNon.Travel"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.6345, df = 446.56, p-value = 0.0003109
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.02814850 0.09442996
## sample estimates:
##  mean in group No mean in group Yes 
##        0.11192214        0.05063291 
## 
## [1] "BusinessTravelTravel_Frequently"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -3.8949, df = 300.46, p-value = 0.0001211
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1843103 -0.0605797
## sample estimates:
##  mean in group No mean in group Yes 
##         0.1686942         0.2911392 
## 
## [1] "BusinessTravelTravel_Rarely"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.8298, df = 322.28, p-value = 0.06821
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.004598931  0.126910469
## sample estimates:
##  mean in group No mean in group Yes 
##         0.7193836         0.6582278 
## 
## [1] "DepartmentHuman.Resources"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -0.60362, df = 314.97, p-value = 0.5465
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.03948760  0.02094684
## sample estimates:
##  mean in group No mean in group Yes 
##        0.04136253        0.05063291 
## 
## [1] "DepartmentResearch...Development"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.1561, df = 322.12, p-value = 0.00175
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.04156424 0.17913858
## sample estimates:
##  mean in group No mean in group Yes 
##         0.6715328         0.5611814 
## 
## [1] "DepartmentSales"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -2.952, df = 318.69, p-value = 0.003392
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.16844892 -0.03371314
## sample estimates:
##  mean in group No mean in group Yes 
##         0.2871046         0.3881857 
## 
## [1] "EducationFieldHuman.Resources"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -1.1485, df = 288.39, p-value = 0.2517
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.036133764  0.009503234
## sample estimates:
##  mean in group No mean in group Yes 
##        0.01622060        0.02953586 
## 
## [1] "EducationFieldLife.Sciences"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.2683, df = 336.66, p-value = 0.2056
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02411779  0.11166797
## sample estimates:
##  mean in group No mean in group Yes 
##         0.4193025         0.3755274 
## 
## [1] "EducationFieldMarketing"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -1.9126, df = 304.34, p-value = 0.05674
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.095583507  0.001360299
## sample estimates:
##  mean in group No mean in group Yes 
##         0.1005677         0.1476793 
## 
## [1] "EducationFieldMedical"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.8736, df = 345.55, p-value = 0.06182
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.002954913  0.121755410
## sample estimates:
##  mean in group No mean in group Yes 
##         0.3252230         0.2658228 
## 
## [1] "EducationFieldOther"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 0.73397, df = 356.12, p-value = 0.4634
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01875882  0.04109807
## sample estimates:
##  mean in group No mean in group Yes 
##        0.05758313        0.04641350 
## 
## [1] "EducationFieldTechnical.Degree"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -2.2879, df = 296.37, p-value = 0.02285
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.100296396 -0.007539797
## sample estimates:
##  mean in group No mean in group Yes 
##         0.0811030         0.1350211 
## 
## [1] "GenderFemale"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 1.1421, df = 336.66, p-value = 0.2542
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02834014  0.10681499
## sample estimates:
##  mean in group No mean in group Yes 
##         0.4063260         0.3670886 
## 
## [1] "GenderMale"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -1.1421, df = 336.66, p-value = 0.2542
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.10681499  0.02834014
## sample estimates:
##  mean in group No mean in group Yes 
##         0.5936740         0.6329114 
## 
## [1] "JobRoleHealthcare.Representative"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 4.0453, df = 487.81, p-value = 6.074e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03135690 0.09058506
## sample estimates:
##  mean in group No mean in group Yes 
##        0.09894566        0.03797468 
## 
## [1] "JobRoleHuman.Resources"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -1.2017, df = 297.84, p-value = 0.2304
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.04798283  0.01159941
## sample estimates:
##  mean in group No mean in group Yes 
##        0.03244120        0.05063291 
## 
## [1] "JobRoleLaboratory.Technician"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -3.3437, df = 301.99, p-value = 0.0009309
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.16176015 -0.04190078
## sample estimates:
##  mean in group No mean in group Yes 
##         0.1597729         0.2616034 
## 
## [1] "JobRoleManager"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 4.7592, df = 607.41, p-value = 2.431e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03381561 0.08133012
## sample estimates:
##  mean in group No mean in group Yes 
##        0.07866991        0.02109705 
## 
## [1] "JobRoleManufacturing.Director"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 4.2528, df = 484.71, p-value = 2.534e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03620370 0.09838622
## sample estimates:
##  mean in group No mean in group Yes 
##        0.10948905        0.04219409 
## 
## [1] "JobRoleResearch.Director"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 5.9974, df = 969.02, p-value = 2.827e-09
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03688336 0.07275969
## sample estimates:
##  mean in group No mean in group Yes 
##       0.063260341       0.008438819 
## 
## [1] "JobRoleResearch.Scientist"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 0.013768, df = 332.89, p-value = 0.989
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.05534907  0.05612930
## sample estimates:
##  mean in group No mean in group Yes 
##         0.1987024         0.1983122 
## 
## [1] "JobRoleSales.Executive"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -0.73955, df = 325.98, p-value = 0.4601
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.08176396  0.03708545
## sample estimates:
##  mean in group No mean in group Yes 
##         0.2181671         0.2405063 
## 
## [1] "JobRoleSales.Representative"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -4.2491, df = 266.07, p-value = 2.971e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.14441844 -0.05295958
## sample estimates:
##  mean in group No mean in group Yes 
##         0.0405515         0.1392405 
## 
## [1] "MaritalStatusDivorced"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.8754, df = 386.63, p-value = 0.000125
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.04887418 0.14953045
## sample estimates:
##  mean in group No mean in group Yes 
##         0.2384428         0.1392405 
## 
## [1] "MaritalStatusMarried"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 3.6006, df = 342.03, p-value = 0.0003645
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.05592823 0.19060436
## sample estimates:
##  mean in group No mean in group Yes 
##         0.4776967         0.3544304 
## 
## [1] "MaritalStatusSingle"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -6.3584, df = 313.8, p-value = 7.186e-10
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.2913092 -0.1536280
## sample estimates:
##  mean in group No mean in group Yes 
##         0.2838605         0.5063291 
## 
## [1] "OverTimeNo"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = 8.7046, df = 304.63, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.2333247 0.3696299
## sample estimates:
##  mean in group No mean in group Yes 
##         0.7656123         0.4641350 
## 
## [1] "OverTimeYes"
## 
## 	Welch Two Sample t-test
## 
## data:  workers[, i] by Attrition
## t = -8.7046, df = 304.63, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.3696299 -0.2333247
## sample estimates:
##  mean in group No mean in group Yes 
##         0.2343877         0.5358650
```

# Logistic regression model

```r
model.full <- glm(Attrition ~ ., data = workers, family = binomial)
model.step <- step(model.full)
```

```
## Start:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + EducationFieldTechnical.Degree + GenderFemale + 
##     GenderMale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     JobRoleSales.Representative + MaritalStatusDivorced + MaritalStatusMarried + 
##     MaritalStatusSingle + OverTimeNo + OverTimeYes
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + EducationFieldTechnical.Degree + GenderFemale + 
##     GenderMale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     JobRoleSales.Representative + MaritalStatusDivorced + MaritalStatusMarried + 
##     MaritalStatusSingle + OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + EducationFieldTechnical.Degree + GenderFemale + 
##     GenderMale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     JobRoleSales.Representative + MaritalStatusDivorced + MaritalStatusMarried + 
##     OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + EducationFieldTechnical.Degree + GenderFemale + 
##     GenderMale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + EducationFieldTechnical.Degree + GenderFemale + 
##     JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + DepartmentSales + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + GenderFemale + JobRoleHealthcare.Representative + 
##     JobRoleHuman.Resources + JobRoleLaboratory.Technician + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     BusinessTravelTravel_Rarely + DepartmentHuman.Resources + 
##     DepartmentResearch...Development + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + GenderFemale + JobRoleHealthcare.Representative + 
##     JobRoleHuman.Resources + JobRoleLaboratory.Technician + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
## 
## Step:  AIC=949.01
## Attrition ~ Age + DailyRate + DistanceFromHome + Education + 
##     EnvironmentSatisfaction + HourlyRate + JobInvolvement + JobLevel + 
##     JobSatisfaction + MonthlyIncome + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + DepartmentResearch...Development + 
##     EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - Education                         1   859.01  947.01
## - MonthlyIncome                     1   859.03  947.03
## - DepartmentResearch...Development  1   859.03  947.03
## - EducationFieldHuman.Resources     1   859.06  947.06
## - HourlyRate                        1   859.07  947.07
## - JobLevel                          1   859.08  947.08
## - PerformanceRating                 1   859.11  947.11
## - MonthlyRate                       1   859.16  947.16
## - PercentSalaryHike                 1   859.32  947.32
## - JobRoleLaboratory.Technician      1   859.38  947.38
## - JobRoleHuman.Resources            1   859.61  947.61
## - DepartmentHuman.Resources         1   860.12  948.12
## - DailyRate                         1   860.64  948.64
## - StockOptionLevel                  1   860.75  948.75
## - EducationFieldMarketing           1   860.81  948.81
## <none>                                  859.01  949.01
## - JobRoleResearch.Scientist         1   861.19  949.19
## - JobRoleManufacturing.Director     1   861.72  949.72
## - JobRoleManager                    1   861.92  949.92
## - JobRoleHealthcare.Representative  1   862.37  950.37
## - TotalWorkingYears                 1   863.40  951.40
## - EducationFieldOther               1   863.69  951.69
## - GenderFemale                      1   863.73  951.73
## - JobRoleResearch.Director          1   863.79  951.79
## - JobRoleSales.Executive            1   864.38  952.38
## - Age                               1   864.38  952.38
## - YearsAtCompany                    1   864.70  952.70
## - TrainingTimesLastYear             1   865.95  953.95
## - YearsWithCurrManager              1   867.10  955.10
## - BusinessTravelNon.Travel          1   867.49  955.49
## - WorkLifeBalance                   1   867.95  955.95
## - EducationFieldLife.Sciences       1   868.01  956.01
## - RelationshipSatisfaction          1   868.79  956.79
## - EducationFieldMedical             1   869.37  957.37
## - YearsInCurrentRole                1   869.71  957.71
## - MaritalStatusMarried              1   870.14  958.14
## - MaritalStatusDivorced             1   870.56  958.56
## - BusinessTravelTravel_Frequently   1   875.88  963.88
## - YearsSinceLastPromotion           1   876.37  964.37
## - DistanceFromHome                  1   876.56  964.56
## - JobInvolvement                    1   877.70  965.70
## - NumCompaniesWorked                1   884.03  972.03
## - JobSatisfaction                   1   885.34  973.34
## - EnvironmentSatisfaction           1   887.62  975.62
## - OverTimeNo                        1   975.46 1063.46
## 
## Step:  AIC=947.01
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + 
##     MonthlyIncome + MonthlyRate + NumCompaniesWorked + PercentSalaryHike + 
##     PerformanceRating + RelationshipSatisfaction + StockOptionLevel + 
##     TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
##     YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + DepartmentResearch...Development + 
##     EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - MonthlyIncome                     1   859.03  945.03
## - DepartmentResearch...Development  1   859.04  945.04
## - EducationFieldHuman.Resources     1   859.07  945.07
## - HourlyRate                        1   859.07  945.07
## - JobLevel                          1   859.08  945.08
## - PerformanceRating                 1   859.11  945.11
## - MonthlyRate                       1   859.16  945.16
## - PercentSalaryHike                 1   859.32  945.32
## - JobRoleLaboratory.Technician      1   859.38  945.38
## - JobRoleHuman.Resources            1   859.61  945.61
## - DepartmentHuman.Resources         1   860.12  946.12
## - DailyRate                         1   860.64  946.64
## - StockOptionLevel                  1   860.75  946.75
## - EducationFieldMarketing           1   860.81  946.81
## <none>                                  859.01  947.01
## - JobRoleResearch.Scientist         1   861.19  947.19
## - JobRoleManufacturing.Director     1   861.72  947.72
## - JobRoleManager                    1   861.92  947.92
## - JobRoleHealthcare.Representative  1   862.37  948.37
## - TotalWorkingYears                 1   863.40  949.40
## - EducationFieldOther               1   863.70  949.70
## - GenderFemale                      1   863.73  949.73
## - JobRoleResearch.Director          1   863.79  949.79
## - JobRoleSales.Executive            1   864.38  950.38
## - Age                               1   864.44  950.44
## - YearsAtCompany                    1   864.70  950.70
## - TrainingTimesLastYear             1   865.95  951.95
## - YearsWithCurrManager              1   867.10  953.10
## - BusinessTravelNon.Travel          1   867.51  953.51
## - WorkLifeBalance                   1   867.96  953.96
## - EducationFieldLife.Sciences       1   868.03  954.03
## - RelationshipSatisfaction          1   868.79  954.79
## - EducationFieldMedical             1   869.37  955.37
## - YearsInCurrentRole                1   869.71  955.71
## - MaritalStatusMarried              1   870.17  956.17
## - MaritalStatusDivorced             1   870.59  956.59
## - BusinessTravelTravel_Frequently   1   875.91  961.91
## - YearsSinceLastPromotion           1   876.44  962.44
## - DistanceFromHome                  1   876.65  962.65
## - JobInvolvement                    1   877.70  963.70
## - NumCompaniesWorked                1   884.14  970.14
## - JobSatisfaction                   1   885.36  971.36
## - EnvironmentSatisfaction           1   887.68  973.68
## - OverTimeNo                        1   975.48 1061.48
## 
## Step:  AIC=945.03
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + 
##     MonthlyRate + NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
##     RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
##     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
##     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
##     BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + DepartmentResearch...Development + 
##     EducationFieldHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - DepartmentResearch...Development  1   859.05  943.05
## - EducationFieldHuman.Resources     1   859.08  943.08
## - HourlyRate                        1   859.09  943.09
## - JobLevel                          1   859.09  943.09
## - PerformanceRating                 1   859.12  943.12
## - MonthlyRate                       1   859.18  943.18
## - PercentSalaryHike                 1   859.33  943.33
## - JobRoleLaboratory.Technician      1   859.40  943.40
## - JobRoleHuman.Resources            1   859.63  943.63
## - DepartmentHuman.Resources         1   860.14  944.14
## - DailyRate                         1   860.66  944.66
## - StockOptionLevel                  1   860.77  944.77
## - EducationFieldMarketing           1   860.82  944.82
## <none>                                  859.03  945.03
## - JobRoleResearch.Scientist         1   861.20  945.20
## - JobRoleManufacturing.Director     1   861.72  945.72
## - JobRoleManager                    1   862.30  946.30
## - JobRoleHealthcare.Representative  1   862.37  946.37
## - TotalWorkingYears                 1   863.41  947.41
## - EducationFieldOther               1   863.76  947.76
## - GenderFemale                      1   863.77  947.77
## - JobRoleResearch.Director          1   864.01  948.01
## - JobRoleSales.Executive            1   864.41  948.41
## - Age                               1   864.46  948.46
## - YearsAtCompany                    1   864.73  948.73
## - TrainingTimesLastYear             1   865.98  949.98
## - YearsWithCurrManager              1   867.23  951.23
## - BusinessTravelNon.Travel          1   867.54  951.54
## - WorkLifeBalance                   1   867.97  951.97
## - EducationFieldLife.Sciences       1   868.06  952.06
## - RelationshipSatisfaction          1   868.81  952.81
## - EducationFieldMedical             1   869.41  953.41
## - YearsInCurrentRole                1   869.71  953.71
## - MaritalStatusMarried              1   870.18  954.18
## - MaritalStatusDivorced             1   870.60  954.60
## - BusinessTravelTravel_Frequently   1   875.91  959.91
## - YearsSinceLastPromotion           1   876.58  960.58
## - DistanceFromHome                  1   876.66  960.66
## - JobInvolvement                    1   877.72  961.72
## - NumCompaniesWorked                1   884.26  968.26
## - JobSatisfaction                   1   885.41  969.41
## - EnvironmentSatisfaction           1   887.69  971.69
## - OverTimeNo                        1   975.64 1059.64
## 
## Step:  AIC=943.05
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + 
##     MonthlyRate + NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
##     RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
##     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
##     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
##     BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldHuman.Resources + 
##     EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
##     EducationFieldOther + GenderFemale + JobRoleHealthcare.Representative + 
##     JobRoleHuman.Resources + JobRoleLaboratory.Technician + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - EducationFieldHuman.Resources     1   859.11  941.11
## - HourlyRate                        1   859.11  941.11
## - JobLevel                          1   859.12  941.12
## - PerformanceRating                 1   859.15  941.15
## - MonthlyRate                       1   859.20  941.20
## - PercentSalaryHike                 1   859.37  941.37
## - JobRoleHuman.Resources            1   859.74  941.74
## - DepartmentHuman.Resources         1   860.35  942.35
## - DailyRate                         1   860.69  942.69
## - StockOptionLevel                  1   860.80  942.80
## - EducationFieldMarketing           1   860.87  942.87
## <none>                                  859.05  943.05
## - JobRoleLaboratory.Technician      1   861.06  943.06
## - JobRoleManager                    1   863.42  945.42
## - TotalWorkingYears                 1   863.48  945.48
## - EducationFieldOther               1   863.76  945.76
## - GenderFemale                      1   863.81  945.81
## - JobRoleSales.Executive            1   864.42  946.42
## - Age                               1   864.50  946.50
## - YearsAtCompany                    1   864.74  946.74
## - TrainingTimesLastYear             1   866.00  948.00
## - YearsWithCurrManager              1   867.25  949.25
## - BusinessTravelNon.Travel          1   867.56  949.56
## - WorkLifeBalance                   1   867.99  949.99
## - EducationFieldLife.Sciences       1   868.06  950.06
## - RelationshipSatisfaction          1   868.81  950.81
## - EducationFieldMedical             1   869.41  951.41
## - YearsInCurrentRole                1   869.73  951.73
## - JobRoleResearch.Director          1   870.09  952.09
## - MaritalStatusMarried              1   870.18  952.18
## - JobRoleManufacturing.Director     1   870.45  952.45
## - MaritalStatusDivorced             1   870.60  952.60
## - JobRoleHealthcare.Representative  1   873.17  955.17
## - JobRoleResearch.Scientist         1   874.78  956.78
## - BusinessTravelTravel_Frequently   1   875.96  957.96
## - DistanceFromHome                  1   876.81  958.81
## - YearsSinceLastPromotion           1   876.87  958.87
## - JobInvolvement                    1   877.72  959.72
## - NumCompaniesWorked                1   884.33  966.33
## - JobSatisfaction                   1   885.46  967.46
## - EnvironmentSatisfaction           1   887.72  969.72
## - OverTimeNo                        1   975.64 1057.64
## 
## Step:  AIC=941.11
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + 
##     MonthlyRate + NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
##     RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
##     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
##     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
##     BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - HourlyRate                        1   859.17  939.17
## - JobLevel                          1   859.17  939.17
## - PerformanceRating                 1   859.21  939.21
## - MonthlyRate                       1   859.25  939.25
## - PercentSalaryHike                 1   859.43  939.43
## - JobRoleHuman.Resources            1   859.79  939.79
## - DepartmentHuman.Resources         1   860.55  940.55
## - DailyRate                         1   860.71  940.71
## - StockOptionLevel                  1   860.83  940.83
## - EducationFieldMarketing           1   860.87  940.87
## <none>                                  859.11  941.11
## - JobRoleLaboratory.Technician      1   861.13  941.13
## - JobRoleManager                    1   863.47  943.47
## - TotalWorkingYears                 1   863.54  943.54
## - EducationFieldOther               1   863.81  943.81
## - GenderFemale                      1   863.84  943.84
## - JobRoleSales.Executive            1   864.48  944.48
## - Age                               1   864.51  944.51
## - YearsAtCompany                    1   864.78  944.78
## - TrainingTimesLastYear             1   866.04  946.04
## - YearsWithCurrManager              1   867.28  947.28
## - BusinessTravelNon.Travel          1   867.67  947.67
## - WorkLifeBalance                   1   867.99  947.99
## - EducationFieldLife.Sciences       1   868.38  948.38
## - RelationshipSatisfaction          1   868.94  948.94
## - YearsInCurrentRole                1   869.77  949.77
## - EducationFieldMedical             1   869.83  949.83
## - JobRoleResearch.Director          1   870.14  950.14
## - MaritalStatusMarried              1   870.40  950.40
## - JobRoleManufacturing.Director     1   870.51  950.51
## - MaritalStatusDivorced             1   870.75  950.75
## - JobRoleHealthcare.Representative  1   873.24  953.24
## - JobRoleResearch.Scientist         1   874.84  954.84
## - BusinessTravelTravel_Frequently   1   875.97  955.97
## - DistanceFromHome                  1   876.86  956.86
## - YearsSinceLastPromotion           1   876.96  956.96
## - JobInvolvement                    1   877.74  957.74
## - NumCompaniesWorked                1   884.33  964.33
## - JobSatisfaction                   1   885.49  965.49
## - EnvironmentSatisfaction           1   887.76  967.76
## - OverTimeNo                        1   975.64 1055.64
## 
## Step:  AIC=939.17
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobLevel + JobSatisfaction + MonthlyRate + 
##     NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
##     RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
##     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
##     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
##     BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - JobLevel                          1   859.23  937.23
## - PerformanceRating                 1   859.26  937.26
## - MonthlyRate                       1   859.31  937.31
## - PercentSalaryHike                 1   859.49  937.49
## - JobRoleHuman.Resources            1   859.85  937.85
## - DepartmentHuman.Resources         1   860.61  938.61
## - DailyRate                         1   860.75  938.75
## - StockOptionLevel                  1   860.89  938.89
## - EducationFieldMarketing           1   860.92  938.92
## <none>                                  859.17  939.17
## - JobRoleLaboratory.Technician      1   861.17  939.17
## - JobRoleManager                    1   863.48  941.48
## - TotalWorkingYears                 1   863.61  941.61
## - GenderFemale                      1   863.86  941.86
## - EducationFieldOther               1   863.86  941.86
## - JobRoleSales.Executive            1   864.51  942.51
## - Age                               1   864.52  942.52
## - YearsAtCompany                    1   864.89  942.89
## - TrainingTimesLastYear             1   866.09  944.09
## - YearsWithCurrManager              1   867.36  945.36
## - BusinessTravelNon.Travel          1   867.69  945.69
## - WorkLifeBalance                   1   868.05  946.05
## - EducationFieldLife.Sciences       1   868.41  946.41
## - RelationshipSatisfaction          1   869.01  947.01
## - YearsInCurrentRole                1   869.85  947.85
## - EducationFieldMedical             1   869.92  947.92
## - JobRoleResearch.Director          1   870.20  948.20
## - MaritalStatusMarried              1   870.41  948.41
## - JobRoleManufacturing.Director     1   870.51  948.51
## - MaritalStatusDivorced             1   870.84  948.84
## - JobRoleHealthcare.Representative  1   873.24  951.24
## - JobRoleResearch.Scientist         1   874.85  952.85
## - BusinessTravelTravel_Frequently   1   875.99  953.99
## - DistanceFromHome                  1   876.94  954.94
## - YearsSinceLastPromotion           1   876.96  954.96
## - JobInvolvement                    1   877.74  955.74
## - NumCompaniesWorked                1   884.36  962.36
## - JobSatisfaction                   1   885.89  963.89
## - EnvironmentSatisfaction           1   887.85  965.85
## - OverTimeNo                        1   975.66 1053.66
## 
## Step:  AIC=937.23
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - PerformanceRating                 1   859.33  935.33
## - MonthlyRate                       1   859.36  935.36
## - PercentSalaryHike                 1   859.55  935.55
## - JobRoleHuman.Resources            1   859.91  935.91
## - DepartmentHuman.Resources         1   860.69  936.69
## - DailyRate                         1   860.81  936.81
## - StockOptionLevel                  1   860.98  936.98
## - EducationFieldMarketing           1   860.99  936.99
## <none>                                  859.23  937.23
## - JobRoleLaboratory.Technician      1   861.24  937.24
## - GenderFemale                      1   863.89  939.89
## - EducationFieldOther               1   863.97  939.97
## - Age                               1   864.57  940.57
## - YearsAtCompany                    1   864.91  940.91
## - TotalWorkingYears                 1   865.31  941.31
## - TrainingTimesLastYear             1   866.14  942.14
## - JobRoleManager                    1   866.98  942.98
## - JobRoleSales.Executive            1   867.00  943.00
## - YearsWithCurrManager              1   867.37  943.37
## - BusinessTravelNon.Travel          1   867.80  943.80
## - WorkLifeBalance                   1   868.16  944.16
## - EducationFieldLife.Sciences       1   868.47  944.47
## - RelationshipSatisfaction          1   869.06  945.06
## - YearsInCurrentRole                1   869.85  945.85
## - EducationFieldMedical             1   869.93  945.93
## - MaritalStatusMarried              1   870.41  946.41
## - MaritalStatusDivorced             1   870.84  946.84
## - JobRoleManufacturing.Director     1   873.74  949.74
## - JobRoleResearch.Scientist         1   874.86  950.86
## - JobRoleResearch.Director          1   875.83  951.83
## - BusinessTravelTravel_Frequently   1   876.00  952.00
## - JobRoleHealthcare.Representative  1   876.70  952.70
## - DistanceFromHome                  1   876.95  952.95
## - YearsSinceLastPromotion           1   877.01  953.01
## - JobInvolvement                    1   877.79  953.79
## - NumCompaniesWorked                1   884.51  960.51
## - JobSatisfaction                   1   885.97  961.97
## - EnvironmentSatisfaction           1   887.97  963.97
## - OverTimeNo                        1   975.79 1051.79
## 
## Step:  AIC=935.33
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + MonthlyRate + NumCompaniesWorked + 
##     PercentSalaryHike + RelationshipSatisfaction + StockOptionLevel + 
##     TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
##     YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - MonthlyRate                       1   859.45  933.45
## - PercentSalaryHike                 1   859.59  933.59
## - JobRoleHuman.Resources            1   860.02  934.02
## - DepartmentHuman.Resources         1   860.79  934.79
## - DailyRate                         1   860.90  934.90
## - EducationFieldMarketing           1   861.09  935.09
## - StockOptionLevel                  1   861.11  935.11
## - JobRoleLaboratory.Technician      1   861.28  935.28
## <none>                                  859.33  935.33
## - GenderFemale                      1   863.96  937.96
## - EducationFieldOther               1   864.10  938.10
## - Age                               1   864.72  938.72
## - YearsAtCompany                    1   864.98  938.98
## - TotalWorkingYears                 1   865.37  939.37
## - TrainingTimesLastYear             1   866.27  940.27
## - JobRoleManager                    1   867.03  941.03
## - JobRoleSales.Executive            1   867.03  941.03
## - YearsWithCurrManager              1   867.47  941.47
## - BusinessTravelNon.Travel          1   868.00  942.00
## - WorkLifeBalance                   1   868.25  942.25
## - EducationFieldLife.Sciences       1   868.58  942.58
## - RelationshipSatisfaction          1   869.21  943.21
## - YearsInCurrentRole                1   869.88  943.88
## - EducationFieldMedical             1   870.07  944.07
## - MaritalStatusMarried              1   870.53  944.53
## - MaritalStatusDivorced             1   870.88  944.88
## - JobRoleManufacturing.Director     1   873.76  947.76
## - JobRoleResearch.Scientist         1   874.87  948.87
## - JobRoleResearch.Director          1   875.89  949.89
## - BusinessTravelTravel_Frequently   1   876.18  950.18
## - JobRoleHealthcare.Representative  1   876.78  950.78
## - DistanceFromHome                  1   876.96  950.96
## - YearsSinceLastPromotion           1   877.15  951.15
## - JobInvolvement                    1   877.90  951.90
## - NumCompaniesWorked                1   884.53  958.53
## - JobSatisfaction                   1   886.08  960.08
## - EnvironmentSatisfaction           1   888.05  962.05
## - OverTimeNo                        1   976.58 1050.58
## 
## Step:  AIC=933.45
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + PercentSalaryHike + 
##     RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
##     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
##     YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + 
##     BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - PercentSalaryHike                 1   859.72  931.72
## - JobRoleHuman.Resources            1   860.14  932.14
## - DepartmentHuman.Resources         1   860.91  932.91
## - DailyRate                         1   861.04  933.04
## - EducationFieldMarketing           1   861.23  933.23
## - StockOptionLevel                  1   861.31  933.31
## - JobRoleLaboratory.Technician      1   861.42  933.42
## <none>                                  859.45  933.45
## - GenderFemale                      1   864.03  936.03
## - EducationFieldOther               1   864.28  936.28
## - Age                               1   864.86  936.86
## - YearsAtCompany                    1   865.06  937.06
## - TotalWorkingYears                 1   865.47  937.47
## - TrainingTimesLastYear             1   866.40  938.40
## - JobRoleManager                    1   867.10  939.10
## - JobRoleSales.Executive            1   867.14  939.14
## - YearsWithCurrManager              1   867.65  939.65
## - BusinessTravelNon.Travel          1   868.18  940.18
## - WorkLifeBalance                   1   868.34  940.34
## - EducationFieldLife.Sciences       1   868.64  940.64
## - RelationshipSatisfaction          1   869.32  941.32
## - YearsInCurrentRole                1   870.00  942.00
## - EducationFieldMedical             1   870.14  942.14
## - MaritalStatusMarried              1   870.60  942.60
## - MaritalStatusDivorced             1   870.94  942.94
## - JobRoleManufacturing.Director     1   873.92  945.92
## - JobRoleResearch.Scientist         1   875.05  947.05
## - JobRoleResearch.Director          1   875.92  947.92
## - BusinessTravelTravel_Frequently   1   876.37  948.37
## - JobRoleHealthcare.Representative  1   876.84  948.84
## - DistanceFromHome                  1   877.32  949.32
## - YearsSinceLastPromotion           1   877.42  949.42
## - JobInvolvement                    1   878.01  950.01
## - NumCompaniesWorked                1   884.63  956.63
## - JobSatisfaction                   1   886.20  958.20
## - EnvironmentSatisfaction           1   888.06  960.06
## - OverTimeNo                        1   976.89 1048.89
## 
## Step:  AIC=931.72
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleHuman.Resources + 
##     JobRoleLaboratory.Technician + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - JobRoleHuman.Resources            1   860.42  930.42
## - DepartmentHuman.Resources         1   861.18  931.18
## - DailyRate                         1   861.37  931.37
## - EducationFieldMarketing           1   861.51  931.51
## - JobRoleLaboratory.Technician      1   861.59  931.59
## - StockOptionLevel                  1   861.59  931.59
## <none>                                  859.72  931.72
## - GenderFemale                      1   864.29  934.29
## - EducationFieldOther               1   864.67  934.67
## - Age                               1   865.28  935.28
## - YearsAtCompany                    1   865.39  935.39
## - TotalWorkingYears                 1   865.77  935.77
## - TrainingTimesLastYear             1   866.64  936.64
## - JobRoleSales.Executive            1   867.21  937.21
## - JobRoleManager                    1   867.30  937.30
## - YearsWithCurrManager              1   867.95  937.95
## - WorkLifeBalance                   1   868.52  938.52
## - BusinessTravelNon.Travel          1   868.69  938.69
## - EducationFieldLife.Sciences       1   868.96  938.96
## - RelationshipSatisfaction          1   869.51  939.51
## - YearsInCurrentRole                1   870.31  940.31
## - EducationFieldMedical             1   870.50  940.50
## - MaritalStatusMarried              1   870.84  940.84
## - MaritalStatusDivorced             1   871.19  941.19
## - JobRoleManufacturing.Director     1   874.11  944.11
## - JobRoleResearch.Scientist         1   875.14  945.14
## - JobRoleResearch.Director          1   875.96  945.96
## - BusinessTravelTravel_Frequently   1   876.65  946.65
## - JobRoleHealthcare.Representative  1   876.90  946.90
## - DistanceFromHome                  1   877.51  947.51
## - YearsSinceLastPromotion           1   877.78  947.78
## - JobInvolvement                    1   878.22  948.22
## - NumCompaniesWorked                1   885.10  955.10
## - JobSatisfaction                   1   886.31  956.31
## - EnvironmentSatisfaction           1   888.24  958.24
## - OverTimeNo                        1   976.96 1046.96
## 
## Step:  AIC=930.42
## Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleLaboratory.Technician + 
##     JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - DailyRate                         1   862.08  930.08
## - EducationFieldMarketing           1   862.22  930.22
## <none>                                  860.42  930.42
## - StockOptionLevel                  1   862.44  930.44
## - JobRoleLaboratory.Technician      1   862.54  930.54
## - DepartmentHuman.Resources         1   863.58  931.58
## - GenderFemale                      1   864.94  932.94
## - EducationFieldOther               1   865.35  933.35
## - Age                               1   865.92  933.92
## - YearsAtCompany                    1   865.99  933.99
## - TotalWorkingYears                 1   866.55  934.55
## - TrainingTimesLastYear             1   867.29  935.29
## - JobRoleSales.Executive            1   868.45  936.45
## - YearsWithCurrManager              1   868.46  936.46
## - WorkLifeBalance                   1   869.23  937.23
## - BusinessTravelNon.Travel          1   869.33  937.33
## - JobRoleManager                    1   869.34  937.34
## - EducationFieldLife.Sciences       1   869.61  937.61
## - RelationshipSatisfaction          1   870.13  938.13
## - YearsInCurrentRole                1   870.88  938.88
## - EducationFieldMedical             1   871.19  939.19
## - MaritalStatusMarried              1   871.32  939.32
## - MaritalStatusDivorced             1   871.67  939.67
## - JobRoleManufacturing.Director     1   875.48  943.48
## - JobRoleResearch.Scientist         1   876.77  944.77
## - JobRoleResearch.Director          1   877.03  945.03
## - BusinessTravelTravel_Frequently   1   877.36  945.36
## - DistanceFromHome                  1   878.23  946.23
## - JobRoleHealthcare.Representative  1   878.25  946.25
## - YearsSinceLastPromotion           1   878.48  946.48
## - JobInvolvement                    1   879.06  947.06
## - NumCompaniesWorked                1   885.96  953.96
## - JobSatisfaction                   1   887.23  955.23
## - EnvironmentSatisfaction           1   888.95  956.95
## - OverTimeNo                        1   977.85 1045.85
## 
## Step:  AIC=930.08
## Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleLaboratory.Technician + 
##     JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - EducationFieldMarketing           1   863.64  929.64
## - JobRoleLaboratory.Technician      1   864.00  930.00
## <none>                                  862.08  930.08
## - StockOptionLevel                  1   864.08  930.08
## - DepartmentHuman.Resources         1   865.00  931.00
## - GenderFemale                      1   866.51  932.51
## - EducationFieldOther               1   866.98  932.98
## - YearsAtCompany                    1   867.74  933.74
## - Age                               1   867.81  933.81
## - TotalWorkingYears                 1   868.36  934.36
## - TrainingTimesLastYear             1   869.10  935.10
## - JobRoleSales.Executive            1   869.94  935.94
## - YearsWithCurrManager              1   869.99  935.99
## - WorkLifeBalance                   1   870.66  936.66
## - JobRoleManager                    1   870.74  936.74
## - EducationFieldLife.Sciences       1   871.19  937.19
## - BusinessTravelNon.Travel          1   871.20  937.20
## - RelationshipSatisfaction          1   871.78  937.78
## - EducationFieldMedical             1   872.94  938.94
## - YearsInCurrentRole                1   873.16  939.16
## - MaritalStatusMarried              1   873.47  939.47
## - MaritalStatusDivorced             1   873.65  939.65
## - JobRoleManufacturing.Director     1   876.46  942.46
## - JobRoleResearch.Scientist         1   877.84  943.84
## - JobRoleResearch.Director          1   878.30  944.30
## - BusinessTravelTravel_Frequently   1   879.25  945.25
## - JobRoleHealthcare.Representative  1   879.78  945.78
## - DistanceFromHome                  1   880.22  946.22
## - YearsSinceLastPromotion           1   881.24  947.24
## - JobInvolvement                    1   881.33  947.33
## - NumCompaniesWorked                1   887.35  953.35
## - JobSatisfaction                   1   889.72  955.72
## - EnvironmentSatisfaction           1   890.60  956.60
## - OverTimeNo                        1   978.54 1044.54
## 
## Step:  AIC=929.64
## Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMedical + EducationFieldOther + GenderFemale + 
##     JobRoleHealthcare.Representative + JobRoleLaboratory.Technician + 
##     JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - JobRoleLaboratory.Technician      1   865.11  929.11
## <none>                                  863.64  929.64
## - DepartmentHuman.Resources         1   865.65  929.65
## - StockOptionLevel                  1   865.80  929.80
## - EducationFieldOther               1   867.19  931.19
## - GenderFemale                      1   868.28  932.28
## - YearsAtCompany                    1   869.02  933.02
## - TotalWorkingYears                 1   869.90  933.90
## - Age                               1   869.96  933.96
## - TrainingTimesLastYear             1   870.35  934.35
## - YearsWithCurrManager              1   871.19  935.19
## - EducationFieldLife.Sciences       1   871.69  935.69
## - WorkLifeBalance                   1   871.85  935.85
## - JobRoleManager                    1   872.04  936.04
## - BusinessTravelNon.Travel          1   872.37  936.37
## - JobRoleSales.Executive            1   872.43  936.43
## - RelationshipSatisfaction          1   873.39  937.39
## - EducationFieldMedical             1   873.51  937.51
## - YearsInCurrentRole                1   874.70  938.70
## - MaritalStatusMarried              1   874.75  938.75
## - MaritalStatusDivorced             1   874.97  938.97
## - JobRoleManufacturing.Director     1   877.03  941.03
## - JobRoleResearch.Scientist         1   878.14  942.14
## - JobRoleResearch.Director          1   879.00  943.00
## - JobRoleHealthcare.Representative  1   880.25  944.25
## - BusinessTravelTravel_Frequently   1   880.79  944.79
## - DistanceFromHome                  1   881.32  945.32
## - JobInvolvement                    1   883.15  947.15
## - YearsSinceLastPromotion           1   883.47  947.47
## - NumCompaniesWorked                1   889.93  953.93
## - JobSatisfaction                   1   891.15  955.15
## - EnvironmentSatisfaction           1   891.65  955.65
## - OverTimeNo                        1   979.20 1043.20
## 
## Step:  AIC=929.11
## Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     DepartmentHuman.Resources + EducationFieldLife.Sciences + 
##     EducationFieldMedical + EducationFieldOther + GenderFemale + 
##     JobRoleHealthcare.Representative + JobRoleManager + JobRoleManufacturing.Director + 
##     JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
##     MaritalStatusDivorced + MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - DepartmentHuman.Resources         1   866.03  928.03
## <none>                                  865.11  929.11
## - StockOptionLevel                  1   867.37  929.37
## - GenderFemale                      1   869.48  931.48
## - EducationFieldOther               1   869.60  931.60
## - YearsAtCompany                    1   870.43  932.43
## - Age                               1   871.50  933.50
## - TotalWorkingYears                 1   871.67  933.67
## - TrainingTimesLastYear             1   871.69  933.69
## - JobRoleManager                    1   872.05  934.05
## - WorkLifeBalance                   1   872.90  934.90
## - YearsWithCurrManager              1   872.90  934.90
## - JobRoleSales.Executive            1   873.86  935.86
## - BusinessTravelNon.Travel          1   873.95  935.95
## - EducationFieldLife.Sciences       1   874.95  936.95
## - RelationshipSatisfaction          1   874.95  936.95
## - YearsInCurrentRole                1   876.28  938.28
## - MaritalStatusMarried              1   876.38  938.38
## - MaritalStatusDivorced             1   876.65  938.65
## - EducationFieldMedical             1   877.14  939.14
## - JobRoleManufacturing.Director     1   878.56  940.56
## - JobRoleResearch.Director          1   879.29  941.29
## - DistanceFromHome                  1   882.35  944.35
## - BusinessTravelTravel_Frequently   1   882.45  944.45
## - JobRoleHealthcare.Representative  1   882.61  944.61
## - JobRoleResearch.Scientist         1   883.45  945.45
## - JobInvolvement                    1   884.63  946.63
## - YearsSinceLastPromotion           1   885.36  947.36
## - NumCompaniesWorked                1   890.61  952.61
## - JobSatisfaction                   1   892.35  954.35
## - EnvironmentSatisfaction           1   893.11  955.11
## - OverTimeNo                        1   980.87 1042.87
## 
## Step:  AIC=928.03
## Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
##     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     EducationFieldLife.Sciences + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## - StockOptionLevel                  1   868.01  928.01
## <none>                                  866.03  928.03
## - EducationFieldOther               1   870.22  930.22
## - GenderFemale                      1   870.34  930.34
## - YearsAtCompany                    1   870.97  930.97
## - TrainingTimesLastYear             1   872.23  932.23
## - Age                               1   872.55  932.55
## - TotalWorkingYears                 1   872.63  932.63
## - JobRoleManager                    1   872.64  932.64
## - YearsWithCurrManager              1   873.71  933.71
## - JobRoleSales.Executive            1   873.88  933.88
## - WorkLifeBalance                   1   874.02  934.02
## - BusinessTravelNon.Travel          1   874.76  934.76
## - EducationFieldLife.Sciences       1   875.09  935.09
## - RelationshipSatisfaction          1   876.33  936.33
## - YearsInCurrentRole                1   876.94  936.94
## - EducationFieldMedical             1   877.32  937.32
## - MaritalStatusMarried              1   878.31  938.31
## - MaritalStatusDivorced             1   878.46  938.46
## - JobRoleManufacturing.Director     1   878.71  938.71
## - JobRoleResearch.Director          1   879.54  939.54
## - JobRoleHealthcare.Representative  1   882.70  942.70
## - DistanceFromHome                  1   883.34  943.34
## - BusinessTravelTravel_Frequently   1   883.41  943.41
## - JobRoleResearch.Scientist         1   883.45  943.45
## - JobInvolvement                    1   885.68  945.68
## - YearsSinceLastPromotion           1   887.16  947.16
## - NumCompaniesWorked                1   891.46  951.46
## - JobSatisfaction                   1   893.44  953.44
## - EnvironmentSatisfaction           1   893.76  953.76
## - OverTimeNo                        1   982.13 1042.13
## 
## Step:  AIC=928.01
## Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
##     YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     EducationFieldLife.Sciences + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo
## 
##                                    Df Deviance     AIC
## <none>                                  868.01  928.01
## - EducationFieldOther               1   871.80  929.80
## - GenderFemale                      1   872.15  930.15
## - YearsAtCompany                    1   872.97  930.97
## - TotalWorkingYears                 1   874.36  932.36
## - TrainingTimesLastYear             1   874.40  932.40
## - Age                               1   874.74  932.74
## - JobRoleManager                    1   874.74  932.74
## - YearsWithCurrManager              1   875.90  933.90
## - JobRoleSales.Executive            1   876.22  934.22
## - WorkLifeBalance                   1   876.46  934.46
## - BusinessTravelNon.Travel          1   876.70  934.70
## - EducationFieldLife.Sciences       1   877.19  935.19
## - RelationshipSatisfaction          1   877.63  935.63
## - YearsInCurrentRole                1   878.89  936.89
## - EducationFieldMedical             1   879.90  937.90
## - JobRoleManufacturing.Director     1   881.00  939.00
## - JobRoleResearch.Director          1   881.26  939.26
## - JobRoleHealthcare.Representative  1   884.77  942.77
## - DistanceFromHome                  1   884.88  942.88
## - JobRoleResearch.Scientist         1   885.41  943.41
## - BusinessTravelTravel_Frequently   1   885.69  943.69
## - JobInvolvement                    1   887.68  945.68
## - YearsSinceLastPromotion           1   889.48  947.48
## - NumCompaniesWorked                1   893.18  951.18
## - EnvironmentSatisfaction           1   896.10  954.10
## - JobSatisfaction                   1   896.47  954.47
## - MaritalStatusMarried              1   897.91  955.91
## - MaritalStatusDivorced             1   904.34  962.34
## - OverTimeNo                        1   983.88 1041.88
```

```r
summary(model.step)
```

```
## 
## Call:
## glm(formula = Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction + 
##     JobInvolvement + JobSatisfaction + NumCompaniesWorked + RelationshipSatisfaction + 
##     TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
##     YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
##     YearsWithCurrManager + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + 
##     EducationFieldLife.Sciences + EducationFieldMedical + EducationFieldOther + 
##     GenderFemale + JobRoleHealthcare.Representative + JobRoleManager + 
##     JobRoleManufacturing.Director + JobRoleResearch.Director + 
##     JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusDivorced + 
##     MaritalStatusMarried + OverTimeNo, family = binomial, data = workers)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7586  -0.4959  -0.2583  -0.0953   3.4364  
## 
## Coefficients:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       8.25362    0.85491   9.654  < 2e-16 ***
## Age                              -0.03383    0.01329  -2.546 0.010896 *  
## DistanceFromHome                  0.04358    0.01057   4.125 3.71e-05 ***
## EnvironmentSatisfaction          -0.42654    0.08192  -5.207 1.92e-07 ***
## JobInvolvement                   -0.53319    0.12109  -4.403 1.07e-05 ***
## JobSatisfaction                  -0.42144    0.08035  -5.245 1.56e-07 ***
## NumCompaniesWorked                0.19241    0.03805   5.056 4.27e-07 ***
## RelationshipSatisfaction         -0.25082    0.08120  -3.089 0.002009 ** 
## TotalWorkingYears                -0.06442    0.02632  -2.447 0.014401 *  
## TrainingTimesLastYear            -0.18094    0.07253  -2.495 0.012600 *  
## WorkLifeBalance                  -0.35392    0.12170  -2.908 0.003635 ** 
## YearsAtCompany                    0.08499    0.03795   2.240 0.025114 *  
## YearsInCurrentRole               -0.14682    0.04456  -3.295 0.000986 ***
## YearsSinceLastPromotion           0.18849    0.04134   4.560 5.12e-06 ***
## YearsWithCurrManager             -0.13202    0.04668  -2.828 0.004683 ** 
## BusinessTravelNon.Travel         -1.02389    0.37373  -2.740 0.006150 ** 
## BusinessTravelTravel_Frequently   0.89096    0.20972   4.248 2.15e-05 ***
## EducationFieldLife.Sciences      -0.69890    0.23099  -3.026 0.002480 ** 
## EducationFieldMedical            -0.84071    0.24544  -3.425 0.000614 ***
## EducationFieldOther              -0.80125    0.42717  -1.876 0.060692 .  
## GenderFemale                     -0.36846    0.18273  -2.016 0.043753 *  
## JobRoleHealthcare.Representative -1.56131    0.42241  -3.696 0.000219 ***
## JobRoleManager                   -1.44800    0.60380  -2.398 0.016478 *  
## JobRoleManufacturing.Director    -1.33363    0.40202  -3.317 0.000909 ***
## JobRoleResearch.Director         -2.52849    0.87735  -2.882 0.003952 ** 
## JobRoleResearch.Scientist        -0.98718    0.24338  -4.056 4.99e-05 ***
## JobRoleSales.Executive           -0.69369    0.24543  -2.826 0.004708 ** 
## MaritalStatusDivorced            -1.47818    0.26079  -5.668 1.44e-08 ***
## MaritalStatusMarried             -1.05492    0.19595  -5.384 7.30e-08 ***
## OverTimeNo                       -1.96066    0.19169 -10.228  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1298.58  on 1469  degrees of freedom
## Residual deviance:  868.01  on 1440  degrees of freedom
## AIC: 928.01
## 
## Number of Fisher Scoring iterations: 6
```

# Top three contributors?

```r
workers <- workers2
# Overall three biggest contributors to attrition
lm.null <- glm(Attrition ~ 1, data = workers, family = binomial)
add1(lm.null, ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ 1
##                          Df Deviance    AIC    LRT  Pr(>Chi)    
## <none>                        1298.6 1300.6                     
## Age                       1   1259.1 1263.1 39.508 3.268e-10 ***
## BusinessTravel            2   1274.8 1280.8 23.760 6.927e-06 ***
## DailyRate                 1   1293.9 1297.9  4.727  0.029692 *  
## Department                2   1288.1 1294.1 10.490  0.005273 ** 
## DistanceFromHome          1   1290.0 1294.0  8.580  0.003399 ** 
## Education                 1   1297.1 1301.1  1.440  0.230192    
## EnvironmentSatisfaction   1   1283.1 1287.1 15.502 8.241e-05 ***
## Gender                    1   1297.3 1301.3  1.286  0.256859    
## HourlyRate                1   1298.5 1302.5  0.069  0.792976    
## JobInvolvement            1   1274.5 1278.5 24.131 9.002e-07 ***
## JobLevel                  1   1249.9 1253.9 48.691 2.996e-12 ***
## JobRole                   8   1209.7 1227.7 88.909 7.743e-16 ***
## JobSatisfaction           1   1283.0 1287.0 15.529 8.127e-05 ***
## MaritalStatus             2   1254.6 1260.6 44.000 2.790e-10 ***
## MonthlyIncome             1   1253.1 1257.1 45.487 1.537e-11 ***
## MonthlyRate               1   1298.2 1302.2  0.338  0.560818    
## NumCompaniesWorked        1   1295.9 1299.9  2.711  0.099629 .  
## OverTime                  1   1217.2 1221.2 81.402 < 2.2e-16 ***
## PercentSalaryHike         1   1298.3 1302.3  0.269  0.604063    
## PerformanceRating         1   1298.6 1302.6  0.012  0.911958    
## RelationshipSatisfaction  1   1295.5 1299.5  3.071  0.079679 .  
## StockOptionLevel          1   1267.9 1271.9 30.706 3.003e-08 ***
## TotalWorkingYears         1   1248.1 1252.1 50.507 1.187e-12 ***
## TrainingTimesLastYear     1   1293.3 1297.3  5.319  0.021098 *  
## WorkLifeBalance           1   1292.7 1296.7  5.896  0.015173 *  
## YearsAtCompany            1   1266.5 1270.5 32.083 1.477e-08 ***
## YearsInCurrentRole        1   1255.9 1259.9 42.700 6.382e-11 ***
## YearsSinceLastPromotion   1   1296.9 1300.9  1.674  0.195775    
## YearsWithCurrManager      1   1258.7 1262.7 39.867 2.719e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
add1(update(lm.null, ~ . + OverTime), ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ OverTime
##                          Df Deviance    AIC    LRT  Pr(>Chi)    
## <none>                        1217.2 1221.2                     
## Age                       1   1172.3 1178.3 44.857 2.119e-11 ***
## BusinessTravel            2   1195.7 1203.7 21.444 2.205e-05 ***
## DailyRate                 1   1211.8 1217.8  5.417  0.019936 *  
## Department                2   1206.2 1214.2 10.949  0.004191 ** 
## DistanceFromHome          1   1209.4 1215.4  7.824  0.005156 ** 
## Education                 1   1216.1 1222.1  1.064  0.302330    
## EnvironmentSatisfaction   1   1194.1 1200.1 23.056 1.573e-06 ***
## Gender                    1   1214.7 1220.7  2.490  0.114601    
## HourlyRate                1   1217.1 1223.1  0.039  0.844166    
## JobInvolvement            1   1191.9 1197.9 25.325 4.844e-07 ***
## JobLevel                  1   1165.5 1171.5 51.647 6.642e-13 ***
## JobRole                   8   1120.0 1140.0 97.186 < 2.2e-16 ***
## JobSatisfaction           1   1198.8 1204.8 18.365 1.824e-05 ***
## MaritalStatus             2   1168.7 1176.7 48.512 2.922e-11 ***
## MonthlyIncome             1   1167.8 1173.8 49.363 2.128e-12 ***
## MonthlyRate               1   1217.0 1223.0  0.153  0.695688    
## NumCompaniesWorked        1   1213.5 1219.5  3.659  0.055762 .  
## PercentSalaryHike         1   1217.0 1223.0  0.229  0.632229    
## PerformanceRating         1   1217.2 1223.2  0.005  0.942970    
## RelationshipSatisfaction  1   1211.9 1217.9  5.256  0.021874 *  
## StockOptionLevel          1   1185.1 1191.1 32.036 1.513e-08 ***
## TotalWorkingYears         1   1162.6 1168.6 54.601 1.477e-13 ***
## TrainingTimesLastYear     1   1214.5 1220.5  2.680  0.101614    
## WorkLifeBalance           1   1212.1 1218.1  5.115  0.023724 *  
## YearsAtCompany            1   1185.9 1191.9 31.249 2.270e-08 ***
## YearsInCurrentRole        1   1176.1 1182.1 41.063 1.474e-10 ***
## YearsSinceLastPromotion   1   1215.7 1221.7  1.469  0.225487    
## YearsWithCurrManager      1   1179.8 1185.8 37.399 9.627e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
add1(update(lm.null, ~ . + OverTime + JobRole), ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ OverTime + JobRole
##                          Df Deviance    AIC    LRT  Pr(>Chi)    
## <none>                        1120.0 1140.0                     
## Age                       1   1107.5 1129.5 12.460 0.0004158 ***
## BusinessTravel            2   1101.7 1125.7 18.270 0.0001078 ***
## DailyRate                 1   1112.9 1134.9  7.055 0.0079032 ** 
## Department                2   1118.6 1142.6  1.413 0.4933658    
## DistanceFromHome          1   1110.7 1132.7  9.331 0.0022531 ** 
## Education                 1   1120.0 1142.0  0.000 0.9968618    
## EnvironmentSatisfaction   1   1095.9 1117.9 24.108 9.107e-07 ***
## Gender                    1   1118.4 1140.4  1.625 0.2023814    
## HourlyRate                1   1120.0 1142.0  0.008 0.9271898    
## JobInvolvement            1   1096.6 1118.6 23.354 1.347e-06 ***
## JobLevel                  1   1118.2 1140.2  1.795 0.1803475    
## JobSatisfaction           1   1100.5 1122.5 19.510 1.001e-05 ***
## MaritalStatus             2   1080.0 1104.0 39.986 2.075e-09 ***
## MonthlyIncome             1   1119.5 1141.5  0.541 0.4621085    
## MonthlyRate               1   1119.3 1141.3  0.672 0.4124294    
## NumCompaniesWorked        1   1109.8 1131.8 10.157 0.0014379 ** 
## PercentSalaryHike         1   1119.7 1141.7  0.330 0.5654312    
## PerformanceRating         1   1120.0 1142.0  0.004 0.9476485    
## RelationshipSatisfaction  1   1115.1 1137.1  4.914 0.0266432 *  
## StockOptionLevel          1   1088.9 1110.9 31.059 2.502e-08 ***
## TotalWorkingYears         1   1112.0 1134.0  8.024 0.0046158 ** 
## TrainingTimesLastYear     1   1115.2 1137.2  4.799 0.0284783 *  
## WorkLifeBalance           1   1114.2 1136.2  5.744 0.0165497 *  
## YearsAtCompany            1   1113.6 1135.6  6.369 0.0116105 *  
## YearsInCurrentRole        1   1101.9 1123.9 18.090 2.108e-05 ***
## YearsSinceLastPromotion   1   1118.9 1140.9  1.103 0.2936849    
## YearsWithCurrManager      1   1105.0 1127.0 14.974 0.0001090 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
model.final <- update(lm.null, ~ . + OverTime + JobRole + MaritalStatus)
summary(model.final)
```

```
## 
## Call:
## glm(formula = Attrition ~ OverTime + JobRole + MaritalStatus, 
##     family = binomial, data = workers)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6865  -0.5564  -0.3805  -0.2124   3.0171  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -3.7803     0.4101  -9.217  < 2e-16 ***
## OverTimeYes                     1.5353     0.1604   9.571  < 2e-16 ***
## JobRoleHuman Resources          1.6861     0.4958   3.401 0.000673 ***
## JobRoleLaboratory Technician    1.5671     0.3879   4.040 5.35e-05 ***
## JobRoleManager                 -0.3655     0.5886  -0.621 0.534631    
## JobRoleManufacturing Director  -0.0620     0.4905  -0.126 0.899422    
## JobRoleResearch Director       -1.0657     0.8062  -1.322 0.186204    
## JobRoleResearch Scientist       0.8024     0.3926   2.044 0.040970 *  
## JobRoleSales Executive          1.0372     0.3868   2.681 0.007331 ** 
## JobRoleSales Representative     2.2003     0.4330   5.082 3.73e-07 ***
## MaritalStatusMarried            0.3051     0.2313   1.319 0.187217    
## MaritalStatusSingle             1.1908     0.2299   5.179 2.23e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1298.6  on 1469  degrees of freedom
## Residual deviance: 1080.0  on 1458  degrees of freedom
## AIC: 1104
## 
## Number of Fisher Scoring iterations: 6
```

```r
# We can't discriminate against age, gender, or marital status, so let's control for those variables
lm.base <- glm(Attrition ~ Age + Gender + MaritalStatus, data = workers, family = binomial)
add1(lm.base, ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ Age + Gender + MaritalStatus
##                          Df Deviance    AIC    LRT  Pr(>Chi)    
## <none>                        1222.6 1232.6                     
## BusinessTravel            2   1199.2 1213.2 23.325 8.610e-06 ***
## DailyRate                 1   1220.0 1232.0  2.612 0.1060432    
## Department                2   1212.4 1226.4 10.138 0.0062887 ** 
## DistanceFromHome          1   1212.0 1224.0 10.555 0.0011585 ** 
## Education                 1   1222.6 1234.6  0.007 0.9326941    
## EnvironmentSatisfaction   1   1205.9 1217.9 16.702 4.373e-05 ***
## HourlyRate                1   1222.6 1234.6  0.003 0.9595578    
## JobInvolvement            1   1201.1 1213.1 21.463 3.608e-06 ***
## JobLevel                  1   1204.1 1216.1 18.470 1.726e-05 ***
## JobRole                   8   1166.6 1192.6 55.946 2.890e-09 ***
## JobSatisfaction           1   1204.7 1216.7 17.851 2.389e-05 ***
## MonthlyIncome             1   1206.0 1218.0 16.626 4.552e-05 ***
## MonthlyRate               1   1222.3 1234.3  0.220 0.6393504    
## NumCompaniesWorked        1   1208.2 1220.2 14.356 0.0001513 ***
## OverTime                  1   1131.2 1143.2 91.422 < 2.2e-16 ***
## PercentSalaryHike         1   1222.2 1234.2  0.320 0.5713181    
## PerformanceRating         1   1222.6 1234.6  0.009 0.9230729    
## RelationshipSatisfaction  1   1219.3 1231.3  3.233 0.0721800 .  
## StockOptionLevel          1   1220.7 1232.7  1.916 0.1663436    
## TotalWorkingYears         1   1208.0 1220.0 14.523 0.0001385 ***
## TrainingTimesLastYear     1   1216.1 1228.1  6.497 0.0108036 *  
## WorkLifeBalance           1   1215.3 1227.3  7.223 0.0071971 ** 
## YearsAtCompany            1   1208.7 1220.7 13.862 0.0001967 ***
## YearsInCurrentRole        1   1197.4 1209.4 25.151 5.300e-07 ***
## YearsSinceLastPromotion   1   1222.4 1234.4  0.132 0.7160344    
## YearsWithCurrManager      1   1197.6 1209.6 24.941 5.911e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
add1(update(lm.base, ~ . + OverTime), ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ Age + Gender + MaritalStatus + OverTime
##                          Df Deviance    AIC    LRT  Pr(>Chi)    
## <none>                        1131.2 1143.2                     
## BusinessTravel            2   1111.0 1127.0 20.107 4.304e-05 ***
## DailyRate                 1   1127.7 1141.7  3.411 0.0647456 .  
## Department                2   1120.3 1136.3 10.834 0.0044394 ** 
## DistanceFromHome          1   1120.5 1134.5 10.613 0.0011228 ** 
## Education                 1   1131.0 1145.0  0.120 0.7285812    
## EnvironmentSatisfaction   1   1107.5 1121.5 23.664 1.147e-06 ***
## HourlyRate                1   1131.2 1145.2  0.004 0.9468355    
## JobInvolvement            1   1108.8 1122.8 22.363 2.257e-06 ***
## JobLevel                  1   1112.8 1126.8 18.333 1.854e-05 ***
## JobRole                   8   1069.3 1097.3 61.875 1.995e-10 ***
## JobSatisfaction           1   1109.2 1123.2 21.898 2.875e-06 ***
## MonthlyIncome             1   1113.9 1127.9 17.277 3.231e-05 ***
## MonthlyRate               1   1131.0 1145.0  0.166 0.6834899    
## NumCompaniesWorked        1   1113.6 1127.6 17.555 2.790e-05 ***
## PercentSalaryHike         1   1131.0 1145.0  0.205 0.6506802    
## PerformanceRating         1   1131.2 1145.2  0.003 0.9566348    
## RelationshipSatisfaction  1   1125.5 1139.5  5.703 0.0169333 *  
## StockOptionLevel          1   1129.6 1143.6  1.575 0.2094264    
## TotalWorkingYears         1   1116.5 1130.5 14.659 0.0001288 ***
## TrainingTimesLastYear     1   1127.9 1141.9  3.251 0.0713969 .  
## WorkLifeBalance           1   1124.2 1138.2  6.980 0.0082445 ** 
## YearsAtCompany            1   1118.7 1132.7 12.432 0.0004220 ***
## YearsInCurrentRole        1   1108.4 1122.4 22.792 1.805e-06 ***
## YearsSinceLastPromotion   1   1130.9 1144.9  0.275 0.6003242    
## YearsWithCurrManager      1   1109.0 1123.0 22.098 2.590e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
add1(update(lm.base, ~ . + OverTime + JobRole), ~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, test = "LRT")
```

```
## Single term additions
## 
## Model:
## Attrition ~ Age + Gender + MaritalStatus + OverTime + JobRole
##                          Df Deviance    AIC     LRT  Pr(>Chi)    
## <none>                        1069.3 1097.3                      
## BusinessTravel            2   1050.9 1082.9 18.3701 0.0001026 ***
## DailyRate                 1   1064.2 1094.2  5.0759 0.0242613 *  
## Department                2   1067.7 1099.7  1.6309 0.4424341    
## DistanceFromHome          1   1058.1 1088.1 11.1323 0.0008484 ***
## Education                 1   1068.9 1098.9  0.3923 0.5311104    
## EnvironmentSatisfaction   1   1045.1 1075.1 24.1794 8.777e-07 ***
## HourlyRate                1   1069.3 1099.3  0.0042 0.9484610    
## JobInvolvement            1   1048.5 1078.5 20.7580 5.211e-06 ***
## JobLevel                  1   1069.2 1099.2  0.1287 0.7197758    
## JobSatisfaction           1   1047.2 1077.2 22.0880 2.604e-06 ***
## MonthlyIncome             1   1069.2 1099.2  0.0543 0.8157382    
## MonthlyRate               1   1068.7 1098.7  0.6015 0.4380176    
## NumCompaniesWorked        1   1049.1 1079.1 20.2215 6.897e-06 ***
## PercentSalaryHike         1   1069.1 1099.1  0.2076 0.6486636    
## PerformanceRating         1   1069.3 1099.3  0.0045 0.9463998    
## RelationshipSatisfaction  1   1063.3 1093.3  5.9501 0.0147165 *  
## StockOptionLevel          1   1067.0 1097.0  2.2954 0.1297609    
## TotalWorkingYears         1   1068.2 1098.2  1.0663 0.3017853    
## TrainingTimesLastYear     1   1064.3 1094.3  4.9261 0.0264534 *  
## WorkLifeBalance           1   1062.1 1092.1  7.1352 0.0075586 ** 
## YearsAtCompany            1   1066.2 1096.2  3.0602 0.0802341 .  
## YearsInCurrentRole        1   1056.6 1086.6 12.6583 0.0003739 ***
## YearsSinceLastPromotion   1   1066.2 1096.2  3.0832 0.0791047 .  
## YearsWithCurrManager      1   1057.5 1087.5 11.7888 0.0005959 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
model.final.2 <- update(lm.base, ~ . + OverTime + JobRole + EnvironmentSatisfaction)
summary(model.final.2)
```

```
## 
## Call:
## glm(formula = Attrition ~ Age + Gender + MaritalStatus + OverTime + 
##     JobRole + EnvironmentSatisfaction, family = binomial, data = workers)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8470  -0.5715  -0.3758  -0.2000   3.0766  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -1.891518   0.601016  -3.147 0.001648 ** 
## Age                           -0.028980   0.009861  -2.939 0.003296 ** 
## GenderMale                     0.250129   0.165225   1.514 0.130058    
## MaritalStatusMarried           0.268652   0.233933   1.148 0.250799    
## MaritalStatusSingle            1.146676   0.233404   4.913 8.98e-07 ***
## OverTimeYes                    1.665300   0.166374  10.009  < 2e-16 ***
## JobRoleHuman Resources         1.516657   0.503973   3.009 0.002618 ** 
## JobRoleLaboratory Technician   1.414419   0.395790   3.574 0.000352 ***
## JobRoleManager                -0.148973   0.598496  -0.249 0.803429    
## JobRoleManufacturing Director -0.007564   0.495556  -0.015 0.987821    
## JobRoleResearch Director      -1.041467   0.812853  -1.281 0.200106    
## JobRoleResearch Scientist      0.621194   0.401381   1.548 0.121708    
## JobRoleSales Executive         0.947915   0.392021   2.418 0.015605 *  
## JobRoleSales Representative    2.008432   0.445034   4.513 6.39e-06 ***
## EnvironmentSatisfaction       -0.355094   0.072914  -4.870 1.12e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1298.6  on 1469  degrees of freedom
## Residual deviance: 1045.1  on 1455  degrees of freedom
## AIC: 1075.1
## 
## Number of Fisher Scoring iterations: 6
```
