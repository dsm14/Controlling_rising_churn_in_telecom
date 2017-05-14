getwd()
setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 13 -  Final Case Study Course Wrap up")
dt<-read.csv("telecomfinal.csv")
library(dplyr)

dim(dt)
str(dt)
unique(dt)
summary(dt)
colSums(is.na(dt))

## Count Unique values for every column in data frame
apply(dt, 2, function(x)length(unique(x)))
## Count Unique values for a single column in a dataframe
dt_uniq<- unique(dt$mou_Mean)
length(dt_uniq)


##Quantile information forall variables to be used in Data Quality Report
col_numeric<-which(sapply(dt, is.numeric))

z= sapply(col_numeric, function(y){ quantile(x = unlist(dt[, y]), c(.05, .10, .25, .5, .75, .90, .95,.99), na.rm = TRUE)})

options(scipen=999)
z1=data.frame(t(z))

# exporting this quantile data 
library(xlsx)
write.xlsx(z1, "C:\\Jig12836\\Capstone Project\\quant_info.xlsx")


### Profiling: Continuous Variable ###

#Continuos Variable #1:mou_Mean
library(dplyr)
dt%>% mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var1
var1$N<-unclass(dt%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var1$churn_perc<-var1$n/var1$N
var1$GreaterThan<-unclass(dt%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
var1$LessThan<-unclass(dt%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
var1$varname<-rep("mou_Mean",nrow(var1))

#Continuous Variable #2:totMrc_mean
dt%>% mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var2
var2$N<-unclass(dt%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var2$churn_perc<-var2$n/var2$N
var2$GreaterThan<-unclass(dt%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
var2$LessThan<-unclass(dt%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
var2$varname<-rep("totmrc_Mean",nrow(var2))

#Continuous Variable #3:rev_Range
dt%>% mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->var3
var3$N<-unclass(dt%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
var3$churn_perc<-var3$n/var3$N
var3$GreaterThan<-unclass(dt%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
var3$LessThan<-unclass(dt%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
var3$varname<-rep("rev_Range",nrow(var3))

#Continuous Variable #4:mou_Range
dt%>% mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->var4
var4$N<-unclass(dt%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
var4$churn_perc<-var4$n/var4$N
var4$GreaterThan<-unclass(dt%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
var4$LessThan<-unclass(dt%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
var4$varname<-rep("mou_Range",nrow(var4))

#Continuous Variable #5:change_mou
dt%>% mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->var5
var5$N<-unclass(dt%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
var5$churn_perc<-var5$n/var5$N
var5$GreaterThan<-unclass(dt%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
var5$LessThan<-unclass(dt%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
var5$varname<-rep("change_mou",nrow(var5))

#Continuous Variable #6:drop_blk_Mean
dt%>% mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var6
var6$N<-unclass(dt%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var6$churn_perc<-var6$n/var6$N
var6$GreaterThan<-unclass(dt%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
var6$LessThan<-unclass(dt%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
var6$varname<-rep("drop_blk_Mean",nrow(var6))

#Continuous Variable #7:drop_vce_Range
dt%>% mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->var7
var7$N<-unclass(dt%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
var7$churn_perc<-var7$n/var7$N
var7$GreaterThan<-unclass(dt%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
var7$LessThan<-unclass(dt%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
var7$varname<-rep("drop_vce_Range",nrow(var7))

#Continuous Variable #8:owylis_vce_Range
dt%>% mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->var8
var8$N<-unclass(dt%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
var8$churn_perc<-var8$n/var8$N
var8$GreaterThan<-unclass(dt%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
var8$LessThan<-unclass(dt%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
var8$varname<-rep("owylis_vce_Range",nrow(var8))

#Continuous Variable #9:mou_opkv_Range
dt%>% mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->var9
var9$N<-unclass(dt%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
var9$churn_perc<-var9$n/var9$N
var9$GreaterThan<-unclass(dt%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
var9$LessThan<-unclass(dt%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
var9$varname<-rep("mou_opkv_Range",nrow(var9))

#Continuous Variable #10:months
dt%>% mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->var10
var10$N<-unclass(dt%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
var10$churn_perc<-var10$n/var10$N
var10$GreaterThan<-unclass(dt%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
var10$LessThan<-unclass(dt%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
var10$varname<-rep("months",nrow(var10))

#Continuous Variable #11:totcalls
dt%>% mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->var11
var11$N<-unclass(dt%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
var11$churn_perc<-var11$n/var11$N
var11$GreaterThan<-unclass(dt%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
var11$LessThan<-unclass(dt%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
var11$varname<-rep("totcalls",nrow(var11))

#Continuous Variable #12:income
dt%>% mutate(dec=ntile(income,n=5))%>%count(churn,dec)%>%filter(churn==1)->var12
var12$N<-unclass(dt%>%mutate(dec=ntile(income,n=5))%>%count(dec)%>%unname())[[2]]
var12$churn_perc<-var12$n/var12$N
var12$GreaterThan<-unclass(dt%>%mutate(dec=ntile(income,n=5))%>%group_by(dec)%>%summarise(min(income)))[[2]]
var12$LessThan<-unclass(dt%>%mutate(dec=ntile(income,n=5))%>%group_by(dec)%>%summarise(max(income)))[[2]]
var12$varname<-rep("income",nrow(var12))

#Continuous Variable #13:eqpdays
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(eqpdays,n=3))%>%count(churn,dec)%>%filter(churn==1)->var13
var13$N<-unclass(dt%>%mutate(dec=ntile(eqpdays,n=3))%>%count(dec)%>%unname())[[2]]
var13$churn_perc<-var13$n/var13$N
var13$GreaterThan<-unclass(dt%>%mutate(dec=ntile(eqpdays,n=3))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
var13$LessThan<-unclass(dt%>%mutate(dec=ntile(eqpdays,n=3))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
var13$varname<-rep("eqpdays",nrow(var13))

#Continuous Variable #14:custcare_Mean
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var14
var14$N<-unclass(dt%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var14$churn_perc<-var14$n/var14$N
var14$GreaterThan<-unclass(dt%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
var14$LessThan<-unclass(dt%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
var14$varname<-rep("custcare_Mean",nrow(var14))

#Continuous Variable #15:callwait_Mean
dt%>% mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var15
var15$N<-unclass(dt%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var15$churn_perc<-var15$n/var15$N
var15$GreaterThan<-unclass(dt%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
var15$LessThan<-unclass(dt%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
var15$varname<-rep("callwait_Mean",nrow(var15))

#Continuous Variable #16:iwylis_vce_Mean
dt%>% mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->var16
var16$N<-unclass(dt%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
var16$churn_perc<-var16$n/var16$N
var16$GreaterThan<-unclass(dt%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
var16$LessThan<-unclass(dt%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
var16$varname<-rep("iwylis_vce_Mean",nrow(var16))

#Continuous Variable #17:callwait_Range
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->var17
var17$N<-unclass(dt%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(dec)%>%unname())[[2]]
var17$churn_perc<-var17$n/var17$N
var17$GreaterThan<-unclass(dt%>%mutate(dec=ntile(callwait_Range,n=4))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
var17$LessThan<-unclass(dt%>%mutate(dec=ntile(callwait_Range,n=4))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
var17$varname<-rep("callwait_Range",nrow(var17))

#Continuous Variable #18:ccrndmou_Range
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->var18
var18$N<-unclass(dt%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(dec)%>%unname())[[2]]
var18$churn_perc<-var18$n/var18$N
var18$GreaterThan<-unclass(dt%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
var18$LessThan<-unclass(dt%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
var18$varname<-rep("ccrndmou_Range",nrow(var18))

#Continuous Variable #19:adjqty
dt%>% mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->var19
var19$N<-unclass(dt%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
var19$churn_perc<-var19$n/var19$N
var19$GreaterThan<-unclass(dt%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
var19$LessThan<-unclass(dt%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
var19$varname<-rep("adjqty",nrow(var19))

#Continuous Variable #20:ovrrev_Mean
dt%>% mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var20
var20$N<-unclass(dt%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var20$churn_perc<-var20$n/var20$N
var20$GreaterThan<-unclass(dt%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
var20$LessThan<-unclass(dt%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
var20$varname<-rep("ovrrev_Mean",nrow(var20))

#Continuous Variable #21:rev_Mean
dt%>% mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var21
var21$N<-unclass(dt%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var21$churn_perc<-var21$n/var21$N
var21$GreaterThan<-unclass(dt%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
var21$LessThan<-unclass(dt%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
var21$varname<-rep("rev_Mean",nrow(var21))

#Continuous Variable #22: ovrmou_Mean
dt%>% mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var22
var22$N<-unclass(dt%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var22$churn_perc<-var22$n/var22$N
var22$GreaterThan<-unclass(dt%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
var22$LessThan<-unclass(dt%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
var22$varname<-rep("ovrmou_Mean",nrow(var22))

#Continuous Variable #23:comp_vce_Mean
dt%>% mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var23
var23$N<-unclass(dt%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var23$churn_perc<-var23$n/var23$N
var23$GreaterThan<-unclass(dt%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
var23$LessThan<-unclass(dt%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
var23$varname<-rep("comp_vce_Mean",nrow(var23))

#Continuous Variable #24:plcd_vce_Mean
dt%>% mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var24
var24$N<-unclass(dt%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var24$churn_perc<-var24$n/var24$N
var24$GreaterThan<-unclass(dt%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
var24$LessThan<-unclass(dt%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
var24$varname<-rep("plcd_vce_Mean",nrow(var24))

#Continuous Variable #25:avg3mou
dt%>% mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->var25
var25$N<-unclass(dt%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
var25$churn_perc<-var25$n/var25$N
var25$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
var25$LessThan<-unclass(dt%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
var25$varname<-rep("avg3mou",nrow(var25))

#Continuous Variable #26:avgmou
dt%>% mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->var26
var26$N<-unclass(dt%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
var26$churn_perc<-var26$n/var26$N
var26$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
var26$LessThan<-unclass(dt%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
var26$varname<-rep("avgmou",nrow(var26))

#Continuous Variable #27:avg3qty
dt%>% mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->var27
var27$N<-unclass(dt%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
var27$churn_perc<-var27$n/var27$N
var27$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
var27$LessThan<-unclass(dt%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
var27$varname<-rep("avg3qty",nrow(var27))

#Continuous Variable #28:avgqty
dt%>% mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->var28
var28$N<-unclass(dt%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
var28$churn_perc<-var28$n/var28$N
var28$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
var28$LessThan<-unclass(dt%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
var28$varname<-rep("avgqty",nrow(var28))

#Continuous Variable #29:avg6mou
dt%>% mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->var29
var29$N<-unclass(dt%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
var29$churn_perc<-var29$n/var29$N
var29$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
var29$LessThan<-unclass(dt%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
var29$varname<-rep("avg6mou",nrow(var29))

#Continuous Variable #30:avg6qty
dt%>% mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->var30
var30$N<-unclass(dt%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
var30$churn_perc<-var30$n/var30$N
var30$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
var30$LessThan<-unclass(dt%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
var30$varname<-rep("avg6qty",nrow(var30))

#Continuous Variable #31:age1
dt%>% mutate(dec=ntile(age1,n=4))%>%count(churn,dec)%>%filter(churn==1)->var31
var31$N<-unclass(dt%>%mutate(dec=ntile(age1,n=4))%>%count(dec)%>%unname())[[2]]
var31$churn_perc<-var31$n/var31$N
var31$GreaterThan<-unclass(dt%>%mutate(dec=ntile(age1,n=4))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
var31$LessThan<-unclass(dt%>%mutate(dec=ntile(age1,n=4))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
var31$varname<-rep("age1",nrow(var31))

#Continuous Variable #32:age2
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->var32
var32$N<-unclass(dt%>%mutate(dec=ntile(age2,n=4))%>%count(dec)%>%unname())[[2]]
var32$churn_perc<-var32$n/var32$N
var32$GreaterThan<-unclass(dt%>%mutate(dec=ntile(age2,n=4))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
var32$LessThan<-unclass(dt%>%mutate(dec=ntile(age2,n=4))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
var32$varname<-rep("age2",nrow(var32))

#Continuous Variable #33:models
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->var33
var33$N<-unclass(dt%>%mutate(dec=ntile(models,n=4))%>%count(dec)%>%unname())[[2]]
var33$churn_perc<-var33$n/var33$N
var33$GreaterThan<-unclass(dt%>%mutate(dec=ntile(models,n=4))%>%group_by(dec)%>%summarise(min(models)))[[2]]
var33$LessThan<-unclass(dt%>%mutate(dec=ntile(models,n=4))%>%group_by(dec)%>%summarise(max(models)))[[2]]
var33$varname<-rep("models",nrow(var33))

#Continuous Variable #34:hnd_price
dt%>% mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->var34
var34$N<-unclass(dt%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
var34$churn_perc<-var34$n/var34$N
var34$GreaterThan<-unclass(dt%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
var34$LessThan<-unclass(dt%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
var34$varname<-rep("hnd_price",nrow(var34))

#Continuous Variable #35:actvsubs
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->var35
var35$N<-unclass(dt%>%mutate(dec=ntile(actvsubs,n=4))%>%count(dec)%>%unname())[[2]]
var35$churn_perc<-var35$n/var35$N
var35$GreaterThan<-unclass(dt%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]
var35$LessThan<-unclass(dt%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]
var35$varname<-rep("actvsubs",nrow(var35))

#Continuous Variable #36:uniqsubs
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->var36
var36$N<-unclass(dt%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(dec)%>%unname())[[2]]
var36$churn_perc<-var36$n/var36$N
var36$GreaterThan<-unclass(dt%>%mutate(dec=ntile(uniqsubs,n=4))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
var36$LessThan<-unclass(dt%>%mutate(dec=ntile(uniqsubs,n=4))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
var36$varname<-rep("uniqsubs",nrow(var36))

#Continuous Variable #37:forgntvl
# Might use xtabs for var37 as it has a small unique values
dt%>% mutate(dec=ntile(forgntvl,n=2))%>%count(churn,dec)%>%filter(churn==1)->var37
var37$N<-unclass(dt%>%mutate(dec=ntile(forgntvl,n=2))%>%count(dec)%>%unname())[[2]]
var37$churn_perc<-var37$n/var37$N
var37$GreaterThan<-unclass(dt%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]
var37$LessThan<-unclass(dt%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]
var37$varname<-rep("forgntvl",nrow(var37))

#Continuous Variable #38:opk_dat_Mean
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var38
var38$N<-unclass(dt%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var38$churn_perc<-var38$n/var38$N
var38$GreaterThan<-unclass(dt%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
var38$LessThan<-unclass(dt%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
var38$varname<-rep("opk_dat_Mean",nrow(var38))

#Continuous Variable #39:mtrcycle
# Might use xtabs for var39 as it has a small unique values

#Continuous Variable #40:truck
# Might use xtabs for var40 as it has a small unique values


#Continuous Variable #41:roam_Mean
# Might use xtabs for var41 as it cant be binned into groups of atleast 4
dt%>% mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var41
var41$N<-unclass(dt%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var41$churn_perc<-var41$n/var41$N
var41$GreaterThan<-unclass(dt%>%mutate(dec=ntile(roam_Mean,n=4))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
var41$LessThan<-unclass(dt%>%mutate(dec=ntile(roam_Mean,n=4))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
var41$varname<-rep("roam_Mean",nrow(var41))

#Continuous Variable #42:recv_sms_Mean 
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var42
var42$N<-unclass(dt%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var42$churn_perc<-var42$n/var42$N
var42$GreaterThan<-unclass(dt%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
var42$LessThan<-unclass(dt%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
var42$varname<-rep("recv_sms_Mean",nrow(var42))

#Continuous Variable #43:blck_dat_Mean
# Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(blck_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var43
var43$N<-unclass(dt%>%mutate(dec=ntile(blck_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var43$churn_perc<-var43$n/var43$N
var43$GreaterThan<-unclass(dt%>%mutate(dec=ntile(blck_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
var43$LessThan<-unclass(dt%>%mutate(dec=ntile(blck_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
var43$varname<-rep("blck_dat_Mean",nrow(var43))

#Continuous Variable #44:mou_pead_Mean
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var44
var44$N<-unclass(dt%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var44$churn_perc<-var44$n/var44$N
var44$GreaterThan<-unclass(dt%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
var44$LessThan<-unclass(dt%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
var44$varname<-rep("mou_pead_Mean",nrow(var44))

#Continuous Variable #45:da_Mean
dt%>% mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var45
var45$N<-unclass(dt%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var45$churn_perc<-var45$n/var45$N
var45$GreaterThan<-unclass(dt%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
var45$LessThan<-unclass(dt%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
var45$varname<-rep("da_Mean",nrow(var45))

#Continuous Variable #46:da_Range 
dt%>% mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->var46
var46$N<-unclass(dt%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
var46$churn_perc<-var46$n/var46$N
var46$GreaterThan<-unclass(dt%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
var46$LessThan<-unclass(dt%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
var46$varname<-rep("da_Range",nrow(var46))

#Continuous Variable #47:datovr_Mean
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var47
var47$N<-unclass(dt%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var47$churn_perc<-var47$n/var47$N
var47$GreaterThan<-unclass(dt%>%mutate(dec=ntile(datovr_Mean,n=4))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
var47$LessThan<-unclass(dt%>%mutate(dec=ntile(datovr_Mean,n=4))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
var47$varname<-rep("datovr_Mean",nrow(var47))

#Continuous Variable #48:datovr_Range
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->var48
var48$N<-unclass(dt%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(dec)%>%unname())[[2]]
var48$churn_perc<-var48$n/var48$N
var48$GreaterThan<-unclass(dt%>%mutate(dec=ntile(datovr_Range,n=4))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
var48$LessThan<-unclass(dt%>%mutate(dec=ntile(datovr_Range,n=4))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
var48$varname<-rep("datovr_Range",nrow(var48))

#Continuous Variable #49:drop_dat_Mean
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var49
var49$N<-unclass(dt%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var49$churn_perc<-var49$n/var49$N
var49$GreaterThan<-unclass(dt%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
var49$LessThan<-unclass(dt%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
var49$varname<-rep("drop_dat_Mean",nrow(var49))

#Continuous Variable #50:drop_vce_Mean
dt%>% mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->var50
var50$N<-unclass(dt%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
var50$churn_perc<-var50$n/var50$N
var50$GreaterThan<-unclass(dt%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
var50$LessThan<-unclass(dt%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
var50$varname<-rep("drop_vce_Mean",nrow(var50))

#Continuous Variable #51:adjmou
dt%>% mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->var51
var51$N<-unclass(dt%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
var51$churn_perc<-var51$n/var51$N
var51$GreaterThan<-unclass(dt%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
var51$LessThan<-unclass(dt%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
var51$varname<-rep("adjmou",nrow(var51))

#Continuous Variable #52:totrev
dt%>% mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->var52
var52$N<-unclass(dt%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
var52$churn_perc<-var52$n/var52$N
var52$GreaterThan<-unclass(dt%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
var52$LessThan<-unclass(dt%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
var52$varname<-rep("totrev",nrow(var52))

#Continuous Variable #53:adjrev
dt%>% mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->var53
var53$N<-unclass(dt%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
var53$churn_perc<-var53$n/var53$N
var53$GreaterThan<-unclass(dt%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
var53$LessThan<-unclass(dt%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
var53$varname<-rep("adjrev",nrow(var53))

#Continuous Variable #54:avgrev
dt%>% mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->var54
var54$N<-unclass(dt%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
var54$churn_perc<-var54$n/var54$N
var54$GreaterThan<-unclass(dt%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
var54$LessThan<-unclass(dt%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
var54$varname<-rep("avgrev",nrow(var54))

#Continuous Variable #55:comp_dat_Mean
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var55
var55$N<-unclass(dt%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var55$churn_perc<-var55$n/var55$N
var55$GreaterThan<-unclass(dt%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]
var55$LessThan<-unclass(dt%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
var55$varname<-rep("comp_dat_Mean",nrow(var55))

#Continuous Variable #56:plcd_dat_Mean
#Output reveals this var can't be binned into atleast 4 groups, so we will exclude it from our model
dt%>% mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->var56
var56$N<-unclass(dt%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
var56$churn_perc<-var56$n/var56$N
var56$GreaterThan<-unclass(dt%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]]
var56$LessThan<-unclass(dt%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]]
var56$varname<-rep("plcd_dat_Mean",nrow(var56))



### Profiling: Categorical Variable ###

#Categorical Variable #1: crclscod
dt%>%count(churn,levels=crclscod)%>%filter(churn==1)->cat1
cat1$N<-unclass(dt%>%filter(crclscod%in%cat1$levels)%>%count(crclscod))[[2]]
cat1$ChurnPerc<-cat1$n/cat1$N
cat1$Var.Name<-rep("crclscod",nrow(cat1))

#Categorical Variable #2:asl_flag
dt%>%count(churn,levels=asl_flag)%>%filter(churn==1)->cat2
cat2$N<-unclass(dt%>%filter(asl_flag%in%cat2$levels)%>%count(asl_flag))[[2]]
cat2$ChurnPerc<-cat2$n/cat2$N
cat2$Var.Name<-rep("asl_flag",nrow(cat2))

#Categorical Variable #3:prizm_social_one
dt%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->cat3
cat3$N<-unclass(dt%>%filter(prizm_social_one%in%cat3$levels)%>%count(prizm_social_one))[[2]]
cat3$ChurnPerc<-cat3$n/cat3$N
cat3$Var.Name<-rep("prizm_social_one",nrow(cat3))

#Categorical Variable #4: area
dt%>%count(churn,levels=area)%>%filter(churn==1)->cat4
cat4$N<-unclass(dt%>%filter(area%in%cat4$levels)%>%count(area))[[2]]
cat4$ChurnPerc<-cat4$n/cat4$N
cat4$Var.Name<-rep("area",nrow(cat4))


#Categorical Variable #5:refurb_new
dt%>%count(churn,levels=refurb_new)%>%filter(churn==1)->cat5
cat5$N<-unclass(dt%>%filter(refurb_new%in%cat5$levels)%>%count(refurb_new))[[2]]
cat5$ChurnPerc<-cat5$n/cat5$N
cat5$Var.Name<-rep("refurb_new",nrow(cat5))

#Categorical Variable #6:hnd_webcap
dt%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->cat6
cat6$N<-unclass(dt%>%filter(hnd_webcap%in%cat6$levels)%>%count(hnd_webcap))[[2]]
cat6$ChurnPerc<-cat6$n/cat6$N
cat6$Var.Name<-rep("hnd_webcap",nrow(cat6))

#Categorical Variable #7:marital
dt%>%count(churn,levels=marital)%>%filter(churn==1)->cat7
cat7$N<-unclass(dt%>%filter(marital%in%cat7$levels)%>%count(marital))[[2]]
cat7$ChurnPerc<-cat7$n/cat7$N
cat7$Var.Name<-rep("marital",nrow(cat7))

#Categorical Variable #8:ethnic
dt%>%count(churn,levels=ethnic)%>%filter(churn==1)->cat8
cat8$N<-unclass(dt%>%filter(ethnic%in%cat8$levels)%>%count(ethnic))[[2]]
cat8$ChurnPerc<-cat8$n/cat8$N
cat8$Var.Name<-rep("ethnic",nrow(cat8))

#Categorical Variable #9:dwlltype
dt%>%count(churn,levels=dwlltype)%>%filter(churn==1)->cat9
cat9$N<-unclass(dt%>%filter(dwlltype%in%cat9$levels)%>%count(dwlltype))[[2]]
cat9$ChurnPerc<-cat9$n/cat9$N
cat9$Var.Name<-rep("dwlltype",nrow(cat9))

#Categorical Variable #10:dwllsize
dt%>%count(churn,levels=dwllsize)%>%filter(churn==1)->cat10
cat10$N<-unclass(dt%>%filter(dwllsize%in%cat10$levels)%>%count(dwllsize))[[2]]
cat10$ChurnPerc<-cat10$n/cat10$N
cat10$Var.Name<-rep("dwllsize",nrow(cat10))

#Categorical Variable #11: car_buy
dt%>%count(churn,levels=car_buy)%>%filter(churn==1)->cat11
cat11$N<-unclass(dt%>%filter(car_buy%in%cat11$levels)%>%count(car_buy))[[2]]
cat11$ChurnPerc<-cat11$n/cat11$N
cat11$Var.Name<-rep("car_buy",nrow(cat11))


##Exporting Profiling information for cont var to a xls file##
#Combining all profiled continuous objects in a single csv
pd<-do.call("rbind",list(var1,var2, var3, var4, var5, var6, var7, var8, var9, var10, var11, var12, var15, var16,var19,var20, var21, var22, var23, var24,var25,var26,var27,var28,var29,var30,var31,var34, var45,var46,var50,var51,var52,var53,var54))
write.csv(pd,"C:\\Jig12836\\Capstone Project\\profiling.csv" )

#Converting the csv to xls format
library(xlsx)
setwd("C:\\Jig12836\\Capstone Project")
pd1<-read.csv("profiling.csv")
write.xlsx(pd1,"C:\\Jig12836\\Capstone Project\\Profiling1.xlsx")

##Exporting Profiling information for cat var to a xls file##
#Combining all profiled continuous objects in a single csv
pd2<-do.call("rbind",list(cat1,cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, cat10, cat11))
write.csv(pd2,"C:\\Jig12836\\Capstone Project\\profiling_cat.csv" )

#Converting the csv to xls format
library(xlsx)
setwd("C:\\Jig12836\\Capstone Project")
pd3<-read.csv("profiling_cat.csv")
write.xlsx(pd3,"C:\\Jig12836\\Capstone Project\\Profiling.xlsx")

# Complete Calls Percentage
dt$comp_mean = dt$comp_vce_Mean + dt$comp_dat_Mean 
dt$comp_vcecall_perc = dt$comp_vce_Mean/dt$comp_mean
#Missing value imputation for comp_vcecall_perc 
dt$comp_vcecall_perc[is.na(dt$comp_vcecall_perc)]<-mean(dt$comp_vcecall_perc, na.rm =TRUE)
sum(is.na(dt$comp_vcecall_perc))

### ----Step 2c in Assignment Doc--Start of Final Data Preparation steps---###
## Data Preparation: Variables reduction
#Keeping only those variables in dataset that are to be included in model- Refer Profiling Variables xls
dt1<- dt%>%select(mou_Mean, totcalls, income,iwylis_vce_Mean, adjqty,rev_Mean, comp_vce_Mean,plcd_vce_Mean, avg3mou,avgmou, avg6mou,avg6qty, age1,totrev, adjrev,crclscod,hnd_webcap, churn, Customer_ID, totmrc_Mean, datovr_Mean, ovrmou_Mean,adjmou, change_mou, comp_vcecall_perc)

## Data Preparation: Missing value imputation for Continuous Variables
#Imputation logic explained in Profiling Variables xls
sum(is.na(dt1$mou_Mean))
dt1$mou_Mean[is.na(dt1$mou_Mean)]<-((var1$GreaterThan[1]+var1$LessThan[1])/2)

sum(is.na(dt1$income))
dt1$income[is.na(dt1$income)]<-7

sum(is.na(dt1$rev_Mean))
dt1$rev_Mean[is.na(dt1$rev_Mean)]<- mean(dt1$rev_Mean, na.rm = TRUE)

sum(is.na(dt1$avg6mou))
dt1$avg6mou[is.na(dt1$avg6mou)]<-(var29$GreaterThan[10]+var29$LessThan[10])/2

sum(is.na(dt1$avg6qty))
dt1$avg6qty[is.na(dt1$avg6qty)]<-(var30$GreaterThan[10]+var30$LessThan[10])/2

sum(is.na(dt1$age1))
dt1$age1[is.na(dt1$age1)]<-(var31$GreaterThan[3]+var31$LessThan[3])/2

sum(is.na(dt$totmrc_Mean))
sum(is.na(dt$datovr_Mean))
sum(is.na(dt$adjmou))
sum(is.na(dt$change_mou))
dt1$totmrc_Mean[is.na(dt1$totmrc_Mean)]<- mean(dt1$totmrc_Mean, na.rm = TRUE)
dt1$datovr_Mean[is.na(dt1$datovr_Mean)]<- mean(dt1$datovr_Mean, na.rm = TRUE)
dt1$adjmou[is.na(dt1$adjmou)]<- mean(dt1$adjmou, na.rm = TRUE)
dt1$change_mou[is.na(dt1$change_mou)]<-mean(dt1$change_mou, na.rm = TRUE)
dt1$ovrmou_Mean[is.na(dt1$ovrmou_Mean)]<-mean(dt1$ovrmou_Mean, na.rm = TRUE)

sum(is.na(dt1))

## Data Preparation: Missing value imputation for Categorical Variables
#Imputation logic explained in Profiling Variables xls
sum(is.na(dt1$hnd_webcap))
dt1$hnd_webcap[is.na(dt1$hnd_webcap)]<-"WC"

##Data Preparation: Levels reduction for Categorical Variables##
#Replacing or Keeping levels for a crclscod to a maximum of 3- detailed logic explained in Profiling Variables xls.
levels(dt1$crclscod)
levels(dt1$crclscod)[levels(dt1$crclscod) %in% c("C2" , "C5" , "CY" , "D4" , "D5" , "E2" ,"E4" ,"EA" ,"EC" ,"V1" ,"W" ,"Y" ,"Z4")]<- "Level 1"
levels(dt1$crclscod)[levels(dt1$crclscod) %in% c("A3" ,"TP")]<- "Level 3"
levels(dt1$crclscod)[!levels(dt1$crclscod) %in% c("Level 1", "Level 3")] <- "Level 2"
levels(dt1$crclscod)

## Data Preparation: Dummy variable creation
#Dummy variables for "crclscod"
dt1$crcl_lev1<- ifelse(dt1$crclscod=="Level 1", 1,0)
dt1$crcl_lev2<- ifelse(dt1$crclscod=="Level 2", 1,0)
dt1$crcl_lev3<- ifelse(dt1$crclscod=="Level 3", 1,0)

#Dummy variables for "hnd_webcap"
dt1$hnd_unkw<- ifelse(dt1$hnd_webcap=="UNKW", 1, 0)
dt1$hnd_wc<- ifelse(dt1$hnd_webcap=="WC", 1, 0)
dt1$hnd_wcmb<- ifelse(dt1$hnd_webcap=="WCMB", 1, 0)

# Data Preparation: Bucketing Age 1
dt1$age_bkt[dt1$age1<=35]<- "Adult"
dt1$age_bkt[dt1$age1>35 & dt1$age1<=50]<-"Midlife"
dt1$age_bkt[dt1$age1>50]<-"Senior"

#Dummy variables for "age_bkt"
dt1$Adult<-ifelse(dt1$age_bkt=="Adult", 1, 0)
dt1$Mid<-ifelse(dt1$age_bkt=="Midlife", 1, 0)
dt1$Old<-ifelse(dt1$age_bkt=="Senior", 1, 0)

# Data Preparation: Bucketing Age 1
dt1$age_bkt[dt1$age1<=35]<- "Adult"
dt1$age_bkt[dt1$age1>35 & dt1$age1<=50]<-"Midlife"
dt1$age_bkt[dt1$age1>50]<-"Senior"

#Dummy variables for "age_bkt"
dt1$Adult<-ifelse(dt1$age_bkt=="Adult", 1, 0)
dt1$Mid<-ifelse(dt1$age_bkt=="Midlife", 1, 0)
dt1$Old<-ifelse(dt1$age_bkt=="Senior", 1, 0)


##Data Preparation: Checking outliers
#Check for outliers for mou_mean
summary(dt1$mou_Mean) 
x<-boxplot(dt1$mou_Mean)

max(dt1$mou_Mean)
dt1%>% filter(mou_Mean==12206.75)

# remove outlier value for mou_mean
nrow(dt1)
dt1<-dt1[dt1$mou_Mean!=12206.75, ]

# Outlier check for totcalls- see the percentile distribution for totcalls in object z1.
options(scipen=999)
summary(dt1$totcalls)
boxplot(dt1$totcalls)
max(dt1$totcalls)

# remove outlier value for mou_mean
nrow(dt1)
dt1<-dt1[dt1$totcalls!=98874, ]
dt1<-dt1[dt1$totcalls!=98139,]
dt1<-dt1[dt1$totcalls!=92076,]


# Outlier check for iwylis_vce_Mean- see the percentile distribution for totcalls in object z1.
summary(dt1$iwylis_vce_Mean)
boxplot(dt1$iwylis_vce_Mean)
max(dt1$iwylis_vce_Mean)

dt1%>%filter(iwylis_vce_Mean ==519.3333333)
#Lookin at the other variables' value for this record, 519.3 doesnot feel to be an outlier.

# Outlier check for adjqty
summary(dt1$adjqty)
boxplot(dt1$adjqty)
max(dt1$adjqty)

# Outlier check for rev_Mean
summary(dt1$rev_Mean)
boxplot(dt1$rev_Mean)
min(dt1$rev_Mean)
max(dt1$rev_Mean)

dt1%>%filter(rev_Mean==-6.1675)
dt1%>%filter(rev_Mean==1223.38)
# remove outlier value for rev_mean (Extreme one doesnot seem too far)
nrow(dt1)
dt1<-dt1[dt1$rev_Mean!=-6.1675, ]

# Outlier check forcomp_vce_Mean-- see the percentile distribution for totcalls in object z1.
summary(dt1$comp_vce_Mean)
boxplot(dt1$comp_vce_Mean)
max(dt1$comp_vce_Mean)
dt1%>%filter(comp_vce_Mean>500)

# Outlier check for plcd_vce_Mean
summary(dt1$plcd_vce_Mean)
boxplot(dt1$plcd_vce_Mean)

# Outlier check for avg3mou-- see the percentile distribution for totcalls in object z1.
summary(dt1$avg3mou)
boxplot(dt1$avg3mou)
max(dt1$avg3mou)
dt1%>%filter(avg3mou==7716)

# Outlier check for avgmou
summary(dt1$avgmou)
boxplot(dt1$avgmou)
max(dt1$avgmou)
dt1%>%filter(avgmou==7040.13)

# Outlier check for avg6mou
summary(dt1$avg6mou)
boxplot(dt1$avg6mou)
max(dt1$avg6mou)
dt1%>%filter(avg6mou==7217)

# Outlier check for totrev
summary(dt1$totrev)
boxplot(dt1$totrev)
max(dt1$totrev)
dt1%>%filter(totrev==27321.5)

# Outlier check for totmrc_Mean
summary(dt1$totmrc_Mean)
boxplot(dt1$totmrc_Mean)
max(dt1$totmrc_Mean)
dt1%>%filter(totmrc_Mean<0)
# Replacing the negative values for totmrc_Mean with the closest churn rate decile
dt1$totmrc_Mean[dt1$totmrc_Mean<0]<- 36.595

# Outlier check for datovr_mean
summary(dt1$datovr_Mean)
summary(dt1$adjmou)
boxplot(dt1$adjmou)
dt1%>%filter(adjmou>232855)
##Even though in most of the variables, some high extreme values exist but looking at the values for othe variabes in the same record justify those high values.Hence those high values won't be removed

## Splitting data into train and test samples##
set.seed(200)
index<-sample(nrow(dt1), 0.70 *nrow(dt1), replace=F)
train<-dt1[index,]
test<-dt1[-index,]
# Checking the representativeness of train and test samples with main dataset
table(dt1$churn)/(nrow(dt1))
table(train$churn)/(nrow(train))
table(test$churn)/(nrow(test))


# Model building and running iterations
# Checkng correlation between variables before model building
cor(dt$avgrev, dt$rev_Mean, use = "complete.obs")
cor(dt$totmrc_Mean, dt$rev_Mean, use = "complete.obs")
cor(dt$avgrev, dt$totmrc_Mean, use = "complete.obs")
cor(dt$avgmou, dt$avgqty, use = "complete.obs")
cor(dt$avg3mou, dt$avgmou, use = "complete.obs")
cor(dt$avgmou, dt$mou_Mean, use = "complete.obs")
cor(dt$avg3mou, dt$avg6mou, use = "complete.obs")
cor(dt$avg6qty, dt$totcalls, use = "complete.obs")

# Model Building 
#Iteration 1
myresult1<-glm(data = train, churn ~ mou_Mean+totcalls+ income+iwylis_vce_Mean+adjqty+rev_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avg6mou+avg6qty+age_bkt+crclscod+hnd_webcap+totrev+adjrev+totmrc_Mean+datovr_Mean+adjmou+change_mou+comp_vcecall_perc,family = binomial)
summary(myresult1)

# Iteration 2 (Removing mou_Mean)
myresult2<-glm(data = train, churn ~totcalls+ income+iwylis_vce_Mean+adjqty+rev_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avg6mou+avg6qty+age_bkt+crclscod+hnd_webcap+totrev+adjrev+totmrc_Mean+datovr_Mean+adjmou+change_mou,family = binomial)
summary(myresult2)

# Iteration 3 (Removing totrev)
myresult3<-glm(data = train, churn ~ totcalls+ income+iwylis_vce_Mean+adjqty+rev_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avg6mou+avg6qty+age_bkt+crclscod+hnd_webcap+adjrev+totmrc_Mean+datovr_Mean+adjmou+change_mou,family = binomial)
summary(myresult3)

# Iteration 4 (Removing iwylis_vce_Mean, avg6mou)- Our Final Model##
myresult4<-glm(data = train, churn ~ ovrmou_Mean+totcalls+ income+adjqty+rev_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avg6qty+age_bkt+crclscod+hnd_webcap+adjrev+totmrc_Mean+datovr_Mean+ovrmou_Mean+adjmou+change_mou,family = binomial)
summary(myresult4)

###-----Performance gauging of model through confusin matrix and AUC------------##########################

##Obtain predictions from my final model by using it on train dataset##
pred_test<-predict(myresult4,type = "response", newdata = test)
head(pred_test,20)
##Finding confusion matrix for test dataset.##
##Finding the optimal cutoff proability by using ROC curve##
library(ROCR)
p1<-prediction(pred_test,test$churn)
p2<-performance(p1, "tpr", "fpr")
plot(p2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Labeling high and low churn customers using cut off as 0.28
pred_test_bkt<-ifelse(pred_test>=0.27, "High_Churn", "Low_Churn")
head(pred_test_bkt, 20)
#Confusion matrix
table(pred_test_bkt,test$churn)
(11329+1857)/nrow(test)
##Finding area under the curve##
library(ROCR)
p1<-prediction(pred_test,test$churn)
auc<-performance(p1,"auc")
auc<-unlist(slot(auc, "y.values"))
auc

###----------Drawing insights and presenting recommendations-------------####



#Finding the top 5 drivers of churn
tail(sort(myresult4$coefficients), 5)

#A2a Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour. 
summary(myresult4)$coefficients[c("rev_Mean", "totmrc_Mean", "adjmou"),]

#A2b Whether data usage connectivity issues is leading to churn?
summary(myresult4)$coefficients[c("totcalls","adjqty", "ovrmou_Mean", "comp_vce_Mean","plcd_vce_Mean","rev_Mean"),]

#A3 Finding the impacts of talk time and recharge plans on churn
summary(myresult4)$coefficients[c("avgmou","ovrmou_Mean", "totmrc_Mean", "rev_Mean"),]

#A4 Proactive retention strategies and churn simulator tool

##Converting odds ratio to probabilities
#avg_mou
exp(-1.493721288+0.0007939320*1)/(1+(exp(-1.493721288+0.0007939320*1)))
#ovrmou_mean
exp(-1.493721288+0.0006369651*1)/(1+(exp(-1.493721288+0.0006369651*1)))


###-----Gains Chart----###
library(gains)
gains(test$churn,predict(myresult4,type="response",newdata=test), groups=10)

test$prob<-predict(myresult4,type="response",newdata=test)

quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted<-test[test$prob>=0.2678380&test$prob<=0.7231702,"Customer_ID"]
targeted



