---
title: "Attrition Graphs"
author: "Brady Arendale"
date: "April 14, 2019"
output: 
  html_document:
    keep_md: true
---


```r
workers <- read.csv("CaseStudy2-data.csv")
names(workers)[1] <- "Age"
library(ggplot2)
ggplot(workers) + geom_bar(mapping = aes(x = BusinessTravel, fill = Attrition), position = "fill") +
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = Department, fill = Attrition), position = "fill") + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = EducationField, fill = Attrition), position = "fill")  + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = Gender, fill = Attrition), position = "fill")  + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = JobRole, fill = Attrition), position = "fill") + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = MaritalStatus, fill = Attrition), position = "fill") + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-6.png)<!-- -->

```r
ggplot(workers) + geom_bar(mapping = aes(x = OverTime, fill = Attrition), position = "fill")  + 
  geom_hline(yintercept = 0.16, size = 1) + ylab("Attrition Rate") + theme(legend.position = "none")
```

![](casestudy2_attr_graphs_files/figure-html/unnamed-chunk-1-7.png)<!-- -->
