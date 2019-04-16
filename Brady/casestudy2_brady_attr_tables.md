---
title: "Attrition Tables"
author: "Brady Arendale"
date: "April 12, 2019"
output: 
  html_document:
    keep_md: true
---

```r
library(knitr)
library(reshape2)
# Read in data
workers <- read.csv("CaseStudy2-data.csv")
names(workers)[1] <- "Age"
paste("Overall attrition rate: ", round(table(workers$Attrition)[2]/1470, digits = 3)*100, "%", sep = "")
```

[1] "Overall attrition rate: 16.1%"

```r
for (i in 1:35) {
  if (class(workers[,i]) == "factor") {
    attrTable <- as.data.frame(table(workers[,i], workers$Attrition))
    attrTable <- dcast(attrTable, Var1 ~ Var2, value.var = "Freq")
    names(attrTable)[1] <- names(workers)[i]
    attrTable$AttrRate <- attrTable$Yes/(attrTable$Yes+attrTable$No)
    print(kable(attrTable))
  }
}
```



Attrition      No   Yes   AttrRate
----------  -----  ----  ---------
No           1233     0          0
Yes             0   237          1


BusinessTravel        No   Yes    AttrRate
------------------  ----  ----  ----------
Non-Travel           138    12   0.0800000
Travel_Frequently    208    69   0.2490975
Travel_Rarely        887   156   0.1495686


Department                 No   Yes    AttrRate
-----------------------  ----  ----  ----------
Human Resources            51    12   0.1904762
Research & Development    828   133   0.1383975
Sales                     354    92   0.2062780


EducationField       No   Yes    AttrRate
-----------------  ----  ----  ----------
Human Resources      20     7   0.2592593
Life Sciences       517    89   0.1468647
Marketing           124    35   0.2201258
Medical             401    63   0.1357759
Other                71    11   0.1341463
Technical Degree    100    32   0.2424242


Gender     No   Yes    AttrRate
-------  ----  ----  ----------
Female    501    87   0.1479592
Male      732   150   0.1700680


JobRole                       No   Yes    AttrRate
--------------------------  ----  ----  ----------
Healthcare Representative    122     9   0.0687023
Human Resources               40    12   0.2307692
Laboratory Technician        197    62   0.2393822
Manager                       97     5   0.0490196
Manufacturing Director       135    10   0.0689655
Research Director             78     2   0.0250000
Research Scientist           245    47   0.1609589
Sales Executive              269    57   0.1748466
Sales Representative          50    33   0.3975904


MaritalStatus     No   Yes    AttrRate
--------------  ----  ----  ----------
Divorced         294    33   0.1009174
Married          589    84   0.1248143
Single           350   120   0.2553191


Over18      No   Yes    AttrRate
-------  -----  ----  ----------
Y         1233   237   0.1612245


OverTime     No   Yes    AttrRate
---------  ----  ----  ----------
No          944   110   0.1043643
Yes         289   127   0.3052885
