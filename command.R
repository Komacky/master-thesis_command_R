※　このコマンドを使用したことによる過失責任は一切負いませんので，ご了承ください．

##ライブラリの用意###########
library(panelr)
library(plm)
library(foreign)
library(tidyr)
library(dplyr)
library(psych)
library(texreg)
library(lmtest)
library(brms)
library(mitml)
library(bayesplot)
library(blme)
library(labelled)
library(kableExtra)
library(sandwich)
library(tidyverse)
library(magrittr)
library(lmtest)
library(lme4)
library(readr)
library(merDeriv)
library(gtools)
library(LMest)
library(survival)
library(survminer)
library(lubridate)
library(ExPanDaR)
library(lmeresampler)
library(ipw)
library(mice)
library(parameters)
library(polycor)
library(RVAideMemoire)
library(rcompanion)
library(cmprsk)
library(ggeffects)
library(effectsize)
library(xtable)
library(rstan)
library(cmdstanr)
set_cmdstan_path("C:/.cmdstanr/cmdstan-2.27.0")
#devtools::install_github("bbcon/Rxtsum",force=T)
library(Rxtsum)
options(scipen=20)
options(max.print=1000)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(na.action='na.pass')
memory.limit(size = 90000)
set.seed(1234)

####Person-Period形式にデータ変換#########################

	Young<- read_csv("PY100.csv")
	Middle <- read_csv("PM100.csv")

	Young %<>% as.data.frame()
	Middle %<>% as.data.frame()

	data.merge<- rbind(Young,Middle)
	data.merge <- data.merge[order(data.merge$PanelID),]

	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'IQ', 'JQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'HQ', 'IQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'GQ', 'HQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'FQ', 'GQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'EQ', 'FQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'DQ', 'EQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'CQ', 'DQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'BQ', 'CQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'AQ', 'BQ')
	colnames(data.merge)<- str_replace_all(colnames(data.merge), 'ZQ', 'AQ')
	data.merge <- dplyr::rename(data.merge, JQ60_S = JQ58_S)

data.merge.sub.long.re$wave<-   recode(data.merge.sub.long.re$wave, "A"="2007","B"="2008","C"="2009"
,"D"="2010","E"="2011", "F" = "2012","G" = "2013","H"="2014","I"="2015","J"="2016")
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,work=Q03_1)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,income=Q37A)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,marriage=Q50_1)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,intend=Q56_1)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,partner=Q59)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,cohabit=Q13_2)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,marexp=Q10C)
data.merge.sub.long.re<- dplyr::rename(data.merge.sub.long.re,cohabit_n=Q13_10)
data.merge.sub.long.re$LOCK <- NULL
data.merge.sub.long.re %<>% as.data.frame()
data.merge.sub.long.re<- data.merge.sub.long.re[,-c(40:43)]




data.merge.sub.long.re$marexp<- ifelse(!is.na(data.merge.sub.long.re$marexp)
& data.merge.sub.long.re$marexp == 9, NA, data.merge.sub.long.re$marexp)
data.merge.sub.long.re$partner<- ifelse(!is.na(data.merge.sub.long.re$partner)
& data.merge.sub.long.re$partner == 9, NA, data.merge.sub.long.re$partner)
data.merge.sub.long.re$marriage<- ifelse(!is.na(data.merge.sub.long.re$marriage)
 & data.merge.sub.long.re$marriage == 9, NA, data.merge.sub.long.re$marriage)
data.merge.sub.long.re$A_1<- ifelse(!is.na(data.merge.sub.long.re$Q39A_1)
& data.merge.sub.long.re$Q39A_1 == 9, NA, data.merge.sub.long.re$Q39A_1)
data.merge.sub.long.re$B_1<- ifelse(!is.na(data.merge.sub.long.re$Q39B_1)
& data.merge.sub.long.re$Q39B_1 == 9, NA, data.merge.sub.long.re$Q39B_1)
data.merge.sub.long.re$C_1<- ifelse(!is.na(data.merge.sub.long.re$Q39C_1)
& data.merge.sub.long.re$Q39C_1 == 9, NA, data.merge.sub.long.re$Q39C_1)
data.merge.sub.long.re$D_1<- ifelse(!is.na(data.merge.sub.long.re$Q39D_1)
& data.merge.sub.long.re$Q39D_1 == 9, NA, data.merge.sub.long.re$Q39D_1)
data.merge.sub.long.re$E_1<- ifelse(!is.na(data.merge.sub.long.re$Q39E_1)
& data.merge.sub.long.re$Q39E_1 == 9, NA, data.merge.sub.long.re$Q39E_1)
data.merge.sub.long.re$F_1<- ifelse(!is.na(data.merge.sub.long.re$Q39F_1)
& data.merge.sub.long.re$Q39F_1 == 9, NA, data.merge.sub.long.re$Q39F_1)

data.merge.sub.long.re$educ<- ifelse(!is.na(data.merge.sub.long.re$educ)
 & data.merge.sub.long.re$educ == 7, NA, data.merge.sub.long.re$educ)
data.merge.sub.long.re$educ<- ifelse(!is.na(data.merge.sub.long.re$educ)
 & data.merge.sub.long.re$educ == 9, NA, data.merge.sub.long.re$educ)


data.merge.sub.long.re$educ3<- dplyr::recode(data.merge.sub.long.re$educ,"1"="1","2"="1","3"="2","4"="2","5"="3","6"="3"
)


data.merge.sub.long.re$work<- ifelse(!is.na(data.merge.sub.long.re$work)
 & data.merge.sub.long.re$work == 99, NA, data.merge.sub.long.re$work)

data.merge.sub.long.re$work4<- dplyr::recode(data.merge.sub.long.re$work
,"1"="1","2"="1","3"="2","4"="2","5"="2","6"="3"
,"7"="3","8"="2","9"="4","10"="4","11"="4","12"="4"
)

data.merge.sub.long.re$intend2<- ifelse(!is.na(data.merge.sub.long.re$intend)
& data.merge.sub.long.re$intend == 9
| data.merge.sub.long.re$intend == 8
, NA, data.merge.sub.long.re$intend)


#data.merge.sub.long.re$income2<- ifelse(!is.na(data.merge.sub.long.re$income)
& data.merge.sub.long.re$educ == 99, NA, data.merge.sub.long.re$income)

data.merge.sub.long.re$marriage<- ifelse(!is.na(data.merge.sub.long.re$marriage)
& data.merge.sub.long.re$marriage == 9, NA, data.merge.sub.long.re$marriage)


data.merge.sub.long.re$income6<- dplyr::recode(data.merge.sub.long.re$income
,"1"="6","2"="1","3"="1","4"="1","5"="2","6"="2"
,"7"="3","8"="3","9"="4","10"="4","11"="4","12"="4","13"="4","14"="5","99"="5"
)

data.merge.sub.long.re$AGE<- data.merge.sub.long.re$wave-data.merge.sub.long.re$ybirth


data.merge.sub.long.re$partner2<- ifelse(!is.na(data.merge.sub.long.re$partner)
& data.merge.sub.long.re$partner == 8

, NA, data.merge.sub.long.re$partner)


####イベントヒストリー用にデータ整形#########################
data.merge.event<- data.merge.sub.long.re[
data.merge.sub.long.re$partner!=1
& data.merge.sub.long.re$partner!=3
& data.merge.sub.long.re$partner!=8
& data.merge.sub.long.re$marexp==2
& data.merge.sub.long.re$marriage==2 #2007年のみ未婚=1，修正済み
& data.merge.sub.long.re$wave==2007,"PanelID"]


data.merge.event<-na.omit(data.merge.event)


data.merge.event<- data.merge.sub.long.re[data.merge.sub.long.re$PanelID
%in% data.merge.event$PanelID,]


data.merge.event$marexp<- NULL

data.merge.event<- data.merge.event  %>% group_by(PanelID) %>%
filter(event.mar2 == 0 |!duplicated(event.mar2 == 1)|event.mar2 == 2)


tri<- data.merge.event[,-c(3:64)]  %>% group_by(PanelID) %>% filter(event.mar2==1)

data.merge.event$event.mar3<- ifelse(data.merge.event$PanelID
 %in% tri$PanelID& data.merge.event$event.mar2==2,0,data.merge.event$event.mar2)

tri2<- data.merge.event %>% group_by(PanelID)
 %>%  filter(event.mar3==2 & last(event.mar3)==0)

data.merge.event$event.mar4<- ifelse(data.merge.event$PanelID
%in% tri2$PanelID& data.merge.event$event.mar3==2,0,data.merge.event$event.mar3)

data.merge.event<- data.merge.event %>% group_by(PanelID)
%>%  filter(event.mar4 == 0 |!duplicated(event.mar4 == 2)|event.mar4 == 1)

data.merge.event$spell<-  data.merge.event$wave-2008


dur<- data.merge.event[,-c(9:70)]  %>% group_by(PanelID) %>% select(wave,event.mar4,spell) %>%
filter(event.mar4 == 2 |event.mar4==1)

data.merge.event<- dplyr::left_join(data.merge.event , dur[,-c(2,3)],by="PanelID")

data.merge.event$spell.y<- ifelse(is.na(data.merge.event$spell.y)
,8,data.merge.event$spell.y)

data.merge.event<- data.merge.event[data.merge.event$spell.y >= data.merge.event$spell.x,]

############HMMによる推定#################

data.merge.sub.long.re$Y1division<- dplyr::recode(data.merge.sub.long.re$A_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")
data.merge.sub.long.re$Y2workmat<- dplyr::recode(data.merge.sub.long.re$B_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")
data.merge.sub.long.re$Y3indimat<- dplyr::recode(data.merge.sub.long.re$C_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")
data.merge.sub.long.re$Y4marhappy<- dplyr::recode(data.merge.sub.long.re$D_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")
data.merge.sub.long.re$Y5kodomo<- dplyr::recode(data.merge.sub.long.re$E_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")
data.merge.sub.long.re$Y6divorce<- dplyr::recode(data.merge.sub.long.re$F_1,"1"="0"
,  "2"="0","3"="1",  "4"="2","5"="2", "6"="3")


data.merge.sub.long.re<- data.merge.sub.long.re[,-c(43:48)]
data.merge.sub.long.re<- data.merge.sub.long.re[,-c(14:19)]

hmm.set<- data.merge.sub.long.re[,c(1,2,4,44:49)]



hmm.set$Y1division %<>% as.numeric()
hmm.set$Y2workmat %<>% as.numeric()
hmm.set$Y3indimat %<>% as.numeric()
hmm.set$Y4marhappy %<>% as.numeric()
hmm.set$Y5kodomo %<>% as.numeric()
hmm.set$Y6divorce %<>% as.numeric()

test<- hmm.set
test$wave %<>% as.factor()
test$wave %<>% as.numeric()
test <- test[!test$wave==c("2"),]
test <- test[!test$wave==c("4"),]
test <- test[!test$wave==c("6"),]
test <- test[!test$wave==c("8"),]
test <- test[!test$wave==c("10"),]

test$wave<- dplyr::recode(test$wave, "1"="1","3"="2","5"="3","7"="4","9"="5")

test.nonna<- test
test.nonna$wave %<>% as.numeric()

test.nonna<- dplyr::rename(test.nonna,X1AGE=AGE)
test.nonna<- dplyr::rename(test.nonna,X2lagwork4=lagwork4)
test.nonna<- dplyr::rename(test.nonna,X3age_squ=age_squ)
test.nonna<- dplyr::rename(test.nonna,X4age_cub=age_cub)
test.nonna<- dplyr::rename(test.nonna,X5lagincome6=lagincome6)
test.nonna<- dplyr::rename(test.nonna,X6lagmar=lagmar)

test.nonna$X2lagwork4<- tidyr::replace_na(test.nonna$X2lagwork4, 5)
test.nonna$X5lagincome6<- tidyr::replace_na(test.nonna$X5lagincome6, 7)
test.nonna$X6lagmar<- tidyr::replace_na(test.nonna$X6lagmar, 5)


test.nonna$X2lagwork4 %<>% as.factor()
test.nonna$X5lagincome6 %<>% as.factor()
test.nonna$X6lagmar %<>% as.factor()

test.nonna.mat<- model.matrix(as.formula(~PanelID+wave+sex+Y1division+Y2workmat +Y3indimat +Y4marhappy +Y5kodomo +X1AGE+X2lagwork4+X3age_squ+X4age_cub+X5lagincome6+X6lagmar
),data=test.nonna)
test.nonna.mat %<>% as.data.frame()
test.nonna.mat$`(Intercept)`<- NULL


fmBasic.cov<- lmestFormula(data = test.nonna.mat, response = "Y",
LatentInitial = c("X1AGE","X2lagwork42","X2lagwork43","X2lagwork44"
, "X2lagwork45","X3age_squ","X4age_cub","X5lagincome62", "X5lagincome63"
, "X5lagincome64" ,"X5lagincome65", "X5lagincome66","X5lagincome67"
,"X6lagmar2" ,"X6lagmar3","X6lagmar4","X6lagmar5" ),

LatentTransition =c("X1AGE","X2lagwork42","X2lagwork43","X2lagwork44", "X2lagwork45"
,"X3age_squ","X4age_cub","X5lagincome62", "X5lagincome63"
, "X5lagincome64" ,"X5lagincome65", "X5lagincome66","X5lagincome67","X6lagmar2"
,"X6lagmar3","X6lagmar4","X6lagmar5" ))


mod.det <- lmest(responsesFormula = fmBasic$responsesFormula,
index = c("PanelID","wave"),
data = test.nonna[test.nonna$sex==1,-c(9)], k = 1:10,
start = 0, out_se=TRUE,
modBasic = 0,maxit = 10000,
output = T,seed=1234
)


plot(mod2.rand5, what = "transitions")
plot(mod.det.cov, what = "transitions")



boot.mod2.rand5 <- LMest::bootstrap(mod2.rand5
, seed = 1243)

z.boot.mod2.rand5.be <- boot.mod2.rand5$mBe/boot.mod2.rand5$seBe

p.z.boot.mod2.rand5.be <- (1 - pnorm(abs(z.boot.mod2.rand5.be), 0, 1)) * 2;p.z.boot.mod2.rand5.be


se.mod.det.cov<-se(mod.det.cov)

boot.mod.det.cov <- LMest::bootstrap(mod.det.cov, seed = 1243)

z.mod.det.cov.be <- boot.mod.det.cov$mBe/boot.mod.det.cov$seBe

p.mod.be <- (1 - pnorm(abs(z.mod.det.cov.be), 0, 1)) * 2;p.mod.be


######################################################
#############left join to row datatframe###############
##########################################################

test.f<- test.nonna[test.nonna$sex==2,]
dec.f <- lmestDecoding(mod2.rand5)
dec.f<- dec.f$Ug %>% as.data.frame()


dec.f$id <- test.f %>% distinct(PanelID, .keep_all=F)
dec.f<- do.call(data.frame, dec.f)

dec.m <- lmestDecoding(mod.det.cov)
test.m<- test.nonna[test.nonna$sex==1,]
dec.m<- dec.m$Ug %>% as.data.frame()

dec.m$id <- test.m %>% distinct(PanelID, .keep_all=F)
dec.m<- do.call(data.frame, dec.m)

dec.int<- rbind(dec.m,dec.f)

dec.int.long<- long_panel(dec.int
, label_location = "end", begin = 1, end = 5 ,id = "PanelID",check.varying = T)


dec.int.long$PanelID %<>% as.character()
dec.int.long$PanelID %<>% as.numeric()
data.merge.sub.long.re$PanelID %<>% as.numeric()

dec.int.long$wave<- dplyr::recode(dec.int.long$wave,"1"="2007","2"="2009","3"="2011","4"="2013","5"="2015")
dec.int.long$wave %<>% as.numeric()
dec.int.long<- rename(dec.int.long,class=V)
data.merge.sub.long.re<- left_join(data.merge.sub.long.re, dec.int.long, by=c("PanelID","wave"))

##############################################
####固体内偏差の計算#########################
#############################################


##male############

data.merge.event$lagincome5 %<>% as.factor()
data.merge.event$lagwork4 %<>% as.numeric()
data.merge.event$lagwork4 %<>% as.factor()
data.merge.event$lagincome6 %<>% as.numeric()
data.merge.event$lagincome6 %<>% as.factor()
data.merge.event$lagintend3 %<>% as.factor()
data.merge.event$lagintend2 %<>% as.factor()
data.merge.event$lagpartner2 %<>% as.factor()
data.merge.event$lag.int %<>% as.factor()
data.merge.event$event.mar4 %<>% as.factor()
data.merge.event$educ3 %<>% as.factor()
data.merge.event$educ3 %<>% as.numeric()
data.merge.event$lagcohabit %<>% as.factor()

data.merge.event$lagintend2.num<-  data.merge.event$lagintend2
data.merge.event$lagintend2.num %<>% as.character()
data.merge.event$lagintend2.num %<>% as.numeric()



data.merge.event.complete<- data.merge.event[data.merge.event$event.mar4!=2,]

demeanset.M<- data.merge.event[data.merge.event$sex==1 & data.merge.event$wave!=2007,]
demeanset.M %<>% as.data.frame()

demeanset.M.com<- data.merge.event.complete[data.merge.event.complete$sex==1 & data.merge.event.complete$wave!=2007,]
demeanset.M.com %<>% as.data.frame()
demeanset.M.com$wave %<>% as.factor()

demean.M.com<- demean(demeanset.M.com, select = c("lagwork4","lagincome5","lagcohabit","wave"
, "lagintend3" ,"lagpartner2","lag.int","lagintend2_n"
), group = "PanelID")

demean.M.com$PanelID<- demeanset.M.com$PanelID


demean.M.com2<- demean(demean.M.com, select = c("lag.int_2_within*lagintend2_n_within", "lag.int_3_within*lagintend2_n_within"
,"lag.int_4_within*lagintend2_n_within" ,"lag.int_2_within*lagpartner2_2_within"
,"lag.int_3_within*lagpartner2_2_within","lag.int_4_within*lagpartner2_2_within"
,"lag.int_2_within*lagpartner2_3_within","lag.int_3_within*lagpartner2_3_within"
,"lag.int_4_within*lagpartner2_3_within","lagintend2_n_within*lagpartner2_2_within"
,"lagintend2_n_within*lagpartner2_3_within"), group = "PanelID")


demeanset.M.com<- cbind(demeanset.M.com,demean.M.com,demean.M.com2)


##female############

demeanset.F<- data.merge.event[data.merge.event$sex==2 & data.merge.event$wave!=2007,]
demeanset.F %<>% as.data.frame()

demeanset.F.com<- data.merge.event.complete[data.merge.event.complete$sex==2 & data.merge.event.complete$wave!=2007,]
demeanset.F.com %<>% as.data.frame()
demeanset.F.com$wave %<>% as.factor()

#demeanset.M[,-c(3:27)]%>%  group_by(PanelID)   %>% filter(PanelID==17)%>%   print(n=50)

demean.F.com<- demean(demeanset.F.com, select = c("lagwork4","lagincome5","lagcohabit","wave"
, "lagintend3" ,"lagpartner2","lag.int","lagintend2_n"
), group = "PanelID")

demean.F.com$PanelID<- demeanset.F.com$PanelID




demean.F.com2<- demean(demean.F.com, select = c("lag.int_2_within*lagintend2_n_within", "lag.int_3_within*lagintend2_n_within"
,"lag.int_4_within*lagintend2_n_within" ,"lag.int_2_within*lagpartner2_2_within"
,"lag.int_3_within*lagpartner2_2_within","lag.int_4_within*lagpartner2_2_within"
,"lag.int_2_within*lagpartner2_3_within","lag.int_3_within*lagpartner2_3_within"
,"lag.int_4_within*lagpartner2_3_within","lagintend2_n_within*lagpartner2_2_within"
,"lagintend2_n_within*lagpartner2_3_within"), group = "PanelID")


demeanset.F.com<- cbind(demeanset.F.com,demean.F.com,demean.F.com2)

############################################
##############記述統計#####################
####################################

Table = xtabs( ~lagcohabit+event.mar4,
data = cochrand.m)
mcnemar.test(Table, correct=F)
effectsize(mcnemar.test(Table, correct=F))








##############################################
####分析モデルの定式化#########################
#############################################

res.cloglog.m<- glmer(event.mar4~ age2007+age2007_squ+factor(educ3)+factor(cohort)
+wave+lagwork4_2_within+lagwork4_3_within+lagwork4_4_within
+lag.int_2_within_lagintend2_n_within_within
+lag.int_3_within_lagintend2_n_within_within
+lag.int_4_within_lagintend2_n_within_within
+lagincome5_2_within+lagincome5_3_within+lagincome5_4_within+lagincome5_5_within
+lagincome5_2_between+lagincome5_3_between
+lagincome5_4_between+lagincome5_5_between
+lagpartner2_1_between+lagpartner2_2_between
+lagintend2_n_between
+lag.int_2_between+lag.int_3_between+lag.int_4_between
+lagwork4_2_between+lagwork4_3_between+lagwork4_4_between
+lag.int_2_within_lagintend2_n_within_between
+lag.int_3_within_lagintend2_n_within_between
+lag.int_4_within_lagintend2_n_within_between


+(1|PanelID)
, data=demeanset.M.com,
control = glmerControl(optimizer="bobyqa" ,check.nobs.vs.nRE = "ignore",optCtrl=list(maxfun=1e6))
,family=binomial(link = "cloglog")
,na.action = "na.omit",nAGQ=0, verbose=TRUE)





performance::check_collinearity(res.cloglog.m)
summary(res.cloglog.m)


res.cloglog.f<- glmer(event.mar4~ age2007+age2007_squ+factor(educ3)+factor(cohort)
+wave+lagwork4_2_within+lagwork4_3_within+lagwork4_4_within
+lag.int_2_within_lagintend2_n_within_within
+lag.int_3_within_lagintend2_n_within_within
+lag.int_4_within_lagintend2_n_within_within
+lagincome5_2_within+lagincome5_3_within+lagincome5_4_within+lagincome5_5_within
+lagincome5_2_between+lagincome5_3_between
+lagincome5_4_between+lagincome5_5_between
+lagpartner2_1_between+lagpartner2_2_between
+lagintend2_n_between
+lag.int_2_between+lag.int_3_between+lag.int_4_between
+lagwork4_2_between+lagwork4_3_between+lagwork4_4_between
+lag.int_2_within_lagintend2_n_within_between
+lag.int_3_within_lagintend2_n_within_between
+lag.int_4_within_lagintend2_n_within_between


+(1|PanelID)
, data=demeanset.F.com,
control = glmerControl(optimizer="bobyqa" ,check.nobs.vs.nRE = "ignore",optCtrl=list(maxfun=1e6))
,family=binomial(link = "cloglog")
,na.action = "na.omit",nAGQ=0, verbose=TRUE)






performance::check_collinearity(res.cloglog.f)
summary(res.cloglog.f)



ipwd<- data.merge.sub.long.re[c("PanelID","wave","age2007" , "attri.int","educ3","cohort","sex","lagincome6","lagwork4","lagmar","lag.attri.int")]
ipwd<- ipwd[ipwd$wave!=2007,] %<>% as.data.frame()


#####ipw ########


ipwd.na<- ipwd[
is.na(ipwd$lagmar)
& is.na(ipwd$lagwork4)
& is.na(ipwd$lagincome6)
,]
ipwd.na<- rownames(ipwd.na)
ipwd<- ipwd[rownames(ipwd)
%nin% ipwd.na,]

ipwd$lagmar2<- dplyr::recode(ipwd$lagmar,"1"="1",  "2"="2","3"="3",  "4"="3")
ipwd$lagmar2 %<>% as.factor()
ipwd$lagincome5_na %<>% as.factor()
ipwd$lagwork4 %<>% as.factor()
ipwd$educ3 %<>% as.factor()
ipwd$lagcity %<>% as.factor()
ipwd$lagmar <- NULL
ipwd$sex %<>% as.factor()
ipwd$cohort %<>% as.factor()

ipwd$attri.int %<>% as.character()
ipwd$attri.int %<>% as.numeric()


imp <- amelia(ipwd[,-c(7)], m = 20,seed=1234,cs="PanelID",ts="wave"
,noms = c("educ3","lagincome5_na","lagmar2","attri.int","lagwork4"
,"sex","cohort","lagcity"))

compare.density(imp, var="lagincome5_na")
plot(imp, which.vars=c(1:5))
disperse(imp, dims = 1, m = 20)
disperse(imp, dims = 2, m = 20)


model_denom  <- zelig(attri.int ~ educ3 + lagwork4+lagincome5_na+lagmar2+lagcity + sex+cohort+age2007
, model = "logit.gee",
id = "PanelID", data = imp,
corstr = "unstructured")


summary(model_denom)
summary(model_num)

propensity_denom <- predict(model_denom, type = "response")

propensity_denom.int<- do.call(cbind.data.frame, propensity_denom)
propensity_denom.int.m<-apply(propensity_denom.int, 1, mean)


model_num  <- zelig(attri.int ~lagwork4+lagincome5_na+lagmar2+lagcity
, model = "logit.gee",
id = "PanelID", data = imp,
corstr = "unstructured")

propensity_num <- predict(model_num , type = "response")




propensity_num.int<- do.call(cbind.data.frame, propensity_num)
propensity_num.int.m<-apply(propensity_num.int, 1, mean)


sw <-  ipwd %>% mutate(propensity_num_outcome = ifelse(ipwd$attri.int == 1, propensity_num.int.m, 1 - propensity_num.int.m),
propensity_denom_outcome = ifelse(ipwd$attri.int == 1, propensity_denom.int.m, 1 - propensity_denom.int.m)) %>%
mutate(weights_no_time = propensity_num_outcome / propensity_denom_outcome)

sw <- sw %>%   group_by(PanelID) %>%
mutate(ipw = cumprod(weights_no_time)) %>%
ungroup()

