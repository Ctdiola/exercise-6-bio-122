library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(visdat)
library(imputeR)
library(gridExtra)
library(ggpubr)
library(outliers)
library(qqplotr)
library(car)
## loading data
data <- read.csv("data6.csv", header = TRUE)
data <- select(data, -Data.source)
colnames(data) <- c("age","sex","CRT.BR","CRT.BL","CRT.FR","CRT.FL",
                    "CRT.IR","CRT.IL","Allen.RR","Allen.RU","Allen.LR",
                    "Allen.LU", "Rubor.RL","Rubor.LL","histamine",
                    "Vein.Neck","Vein.BOH","Vein.DOF","PVC")
data <- data[-1,]
data <- select(data, -histamine)
## test for CRT-------------==------------------------------------------------
CRT <- select(data, sex, CRT.BR,CRT.BL,CRT.FR,CRT.FL,
                    CRT.IR,CRT.IL)
str(CRT)
CRT <- mutate(CRT, ID = 1:39)
CRT <- mutate(CRT, CRT.BR = as.numeric(CRT.BR))
CRT <- mutate(CRT, CRT.BL = as.numeric(CRT.BL))
CRT <- mutate(CRT, CRT.FR = as.numeric(CRT.FR))
CRT <- mutate(CRT, CRT.FL = as.numeric(CRT.FL))
CRT <- mutate(CRT, CRT.IR = as.numeric(CRT.IR))
CRT <- mutate(CRT, CRT.IL = as.numeric(CRT.IL))
CRT_long <- CRT %>% 
  tidyr::gather(CRT,Seconds,-sex,-ID)
str(CRT_long)
CRT_long[CRT_long$sex == "F",]$sex <- "F"
CRT_long[CRT_long$sex == "F ",]$sex <- "F"
CRT_long[CRT_long$sex == "M",]$sex <- "M"
CRT_long <- mutate(CRT_long,Seconds = as.numeric(Seconds))
CRT_long <- mutate(CRT_long,sex = as.factor(sex))
## creating heatmap ## 
ggplot(CRT_long,aes(x = sex, y = CRT, fill = Seconds))+
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd")
## testing for parameters of parametric test ##
CRT.1 <- select(CRT,-sex)
library(PerformanceAnalytics)
chart.Correlation(CRT.1)
## normality ##
shapiro.test(CRT.1$CRT.BR)
shapiro.test(CRT.1$CRT.BL)
shapiro.test(CRT.1$CRT.FR)
shapiro.test(CRT.1$CRT.FL) # normal
shapiro.test(CRT.1$CRT.IR) # normal
shapiro.test(CRT.1$CRT.IL)# normal 
## 
library(reshape2)
CRT.v <- as.list(CRT_long)
friedman.test(CRT.v,seconds~sex)
  






