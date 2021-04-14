library(readxl)

setwd("C:/Users/Catherine/Box Sync/KapurLab/NDV/Manuscripts/Odisha")
data1 <- read_excel("Odisha datasheet.xlsx", sheet = "DATA")
names(data1)

data1$Seasons<-factor(data1$Seasons,levels = c(1,2),labels = c("Non-migratory","Migratory"))
data1$sample_date<-as.Date(data1$sample_date,format = "%m.%d.%y")


data1$Sex<-factor(data1$Sex,levels =c("F","M"),labels = c("Female", "Male"))
data1$Vacc<-factor(data1$Vacc)
data1$Zone<-factor(data1$Zone, levels = c("1","2","3"), labels=c("I","II", "III"))
data1$Migratory_birds<-factor(data1$Migratory_birds)
data1$Housing<-factor(data1$Housing)
data1$Condition<-factor(data1$Condition)
data1$Breed<-factor(data1$Breed)
data1$M_gene<-factor(data1$M_gene)
data1$status<-factor(data1$status,levels = c("0","1"),labels = c("ND Negative", "ND Positive"))
data1$Age_weeks<-as.numeric(data1$Age_weeks)
data1$Flock_Size<-as.numeric(data1$Flock_Size)
data1$Bird_Type<-factor(data1$Bird_Type)


summary(data1)
table(data1$status,data1$Bird_Type)
table(data1$status,data1$Housing)

##Loading packages
library(tidyverse)
library(survival)
library(gt)
library(rstan)
library(boot)
library(gt)
library(finalfit)


##to get the summary of the tables
library(finalfit)
library(tidyverse)
library(survival)
library(gt)
library(rstan)
library(boot)
library(gt)
library(broom)

##
explanatory = c("Sex","Vacc","Zone","Seasons","Migratory_birds","Condition","Breed","Bird_Type","Age_weeks")
dependent = "status"
data1 %>%
  summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE)->tab1
t1<-gt(tab1)
t1


data1 %>%
  summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,column = FALSE)->tab_row

tab_row<-gt(tab_row)
tab_row
gtsave(tab_row,"tab1_row.rtf")


## TO get the regression table
explanatory = c("Sex","Vacc","Zone","Seasons","Migratory_birds","Condition","Bird_Type","Age_weeks")
dependent = "status"
data1 %>%
  finalfit(dependent, explanatory,metrics = T)->reg.tabrevised
reg.tabrevised
reg.tabrevised[[1]]<-gt(reg.tabrevised[[1]])
reg.tabrevised[[2]]
gtsave(reg.tabrevised, "revised regression table.rtf")

reg.parameters<-gt(reg.tabrevised[[2]])
reg.parameters


data1 %>%
  or_plot(dependent, explanatory)-> revised.plot

gtsave
data1 %>%
  missing_pattern(dependent, explanatory)


data1 %>% 
  or_plot(dependent, explanatory, 
          breaks = c(0.5, 1, 5, 10, 20, 30))-> plotnew1



data1 %>% or_plot(dependent, explanatory)->diag1
gt


## CMH added code
#Table 3 reproduced by:
table(data1$Sex, data1$status)
table(data1$Vacc, data1$status)
table(data1$Zone, data1$status)
table(data1$Seasons, data1$status)
table(data1$Migratory_birds, data1$status)
table(data1$Condition, data1$status)
table(data1$Bird_Type, data1$status)
table(data1$Bird_Type, data1$status)
