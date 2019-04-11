---
title: "Case Study 2"
author: "Brady Arendale"
date: "April 11, 2019"
output: 
  html_document:
    keep_md: true
---

```r
# Read in data
workers <- read.csv("CaseStudy2-data.csv")
names(workers)[1] <- "Age"
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
workers <- workers[,-c(9, 10, 22, 27)]
for (i in 1:59) {
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
## [1] "BusinessTravel"
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
## [1] "Department"
## [1] "factor"
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
## [1] "EducationField"
## [1] "factor"
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
## [1] "Gender"
## [1] "factor"
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
## [1] "JobRole"
## [1] "factor"
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
## [1] "MaritalStatus"
## [1] "factor"
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
## [1] "OverTime"
## [1] "factor"
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
