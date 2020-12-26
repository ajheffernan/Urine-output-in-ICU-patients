#Load Libraries
library(DataExplorer)
library(pROC)
library(caret)
library(pdp)
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(tidyverse)
library(Amelia)
library(partykit)
library(tableone)
library(boot)
library(OIsurv)
library(ggfortify)
library(e1071)
library(DMwR)
library(VIM)
library(ICEbox)
library(vcd)
library(naniar)

library(ModelMetrics)

#Data Cleaning
ANZICS=read.csv("/APD2006to2016.csv")
dim(ANZICS)
#baseline number patients 1371060
ANZICS24=ANZICS
icusource=c(1, 2, 3, 4, 5,9) #exclude admission from another ICU; include transfers assume R) censor
treatlimits=c(1, 2, 3, 99, NA) #active treatment or unknown; exclude organ donation
firstadmit=c(0,1)
#include elective admissions
ANZICS24 = dplyr::filter(ANZICS24, (ANZICS24$chr_ren==0 & (ANZICS24$icu_srce %in% icusource) & (ANZICS24$treat_lmt %in% treatlimits) & (ANZICS24$admepisode %in% firstadmit) & (ANZICS24$age>=16)))

ANZICSren = dplyr::filter(ANZICS, (ANZICS$chr_ren==0))
dim(ANZICS)-dim(ANZICSren) #44980
ANZICSicusrce = dplyr::filter(ANZICS, (ANZICS$icu_srce %in% icusource))
dim(ANZICS)-dim(ANZICSicusrce) #11445
ANZICStxlim = dplyr::filter(ANZICS, ((ANZICS$treat_lmt %in% treatlimits)))
dim(ANZICS)-dim(ANZICStxlim) #1132
ANZICSreadmis = dplyr::filter(ANZICS, ((ANZICS$admepisode %in% firstadmit)))
dim(ANZICS)-dim(ANZICSreadmis) #66640 
ANZICSage = dplyr::filter(ANZICS, ((ANZICS$age>=16)))
dim(ANZICS)-dim(ANZICSage) #25341

#Important missing values
ANZICS24$weight[ANZICS24$weight < 20] <- NA
ANZICSwt = dplyr::filter(ANZICS24, (is.na(ANZICS24$weight)))
dim(ANZICSwt) #930491
ANZICS24$urineop[ANZICS24$urineop < 0] <- NA
ANZICSuo = dplyr::filter(ANZICS24, (is.na(ANZICS24$urineop)))
dim(ANZICSuo) #118843
ANZICSdied = dplyr::filter(ANZICS24, (is.na(ANZICS24$died_hosp)))
dim(ANZICSdied) #5751

ANZICS24Missing=subset(ANZICS24, select=c(hospitalclassificationid,
                                          publicprivate,
                                          icuadmityyyy,
                                          hosp_hrs,
                                          ap3diag,
                                          died_hosp,
                                          died_icu,
                                          iddm,
                                          preg_stat,
                                          resparrest,
                                          indigenous,
                                          thrombpro,
                                          sex,
                                          elect,
                                          chr_resp,
                                          chr_cvs,
                                          chr_liv,
                                          lymphoma, 
                                          metast, 
                                          leukaem,
                                          immundis,
                                          immunsup,
                                          immunrx,
                                          cirrhos,
                                          weight, 
                                          aids, 
                                          hepfail, 
                                          age, 
                                          ventilated, 
                                          intubated, 
                                          gcsverb,
                                          gcsmotor,
                                          gcseye,
                                          cardarrest,
                                          urineop,
                                          temp_anz,
                                          map_anz,
                                          hr_anz,
                                          rr_anz,
                                          fio2_anz,
                                          pao2_anz,
                                          paco2_anz,
                                          ph_anz,
                                          hco3_ap2,
                                          na_anz,
                                          k_ap2,
                                          creat_anz,
                                          hct_anz,
                                          wcc_anz,
                                          albumin_anz,
                                          urea_anz,
                                          bili_anz,
                                          glucose_anz,
                                          anzrodriskofdeath))

#Alter variables
ANZICS24UOWT=subset(ANZICS24, !is.na(ANZICS24$urineop)&!is.na(ANZICS24$weight)&!is.na(ANZICS24$died_hosp))
dim(ANZICS24UOWT) #277949
ANZICS24UOWT$uo_wt_hr=(ANZICS24UOWT$urineop/ANZICS24UOWT$weight)/24

#Change variable types for important columns
ANZICS24UOWT$sex=ifelse(ANZICS24UOWT$sex == 'M', 0, 1)
ANZICS24UOWT$sex=factor(ANZICS24UOWT$sex)
cols=c("hospitalclassificationid", "publicprivate", "died_icu", "iddm", "preg_stat", "resparrest", "indigenous", "thrombpro", "sex", "died_hosp", "chr_resp", "chr_cvs", "chr_liv", "chr_ren", "immundis",
       "immunrx", "aids", "hepfail", "lymphoma", "metast", "leukaem", "immunsup", "cirrhos", "iddm", "cardarrest", "resparrest",
       "intubated", "ventilated", "gcsverb", "gcsmotor", "gcseye", "ap3diag")
ANZICS24UOWT %<>% mutate_at(cols, factor)

#Only include relevant columns
ANZICS24FilterNA=subset(ANZICS24UOWT, select=c(hospitalclassificationid,
                                               publicprivate,
                                               icuadmityyyy,
                                               hosp_hrs,
                                               icu_hrs,
                                               hosp_outcm,
                                               ap3diag,
                                               died_hosp,
                                               died_icu,
                                               iddm,
                                               preg_stat,
                                               resparrest,
                                               indigenous,
                                               thrombpro,
                                               sex,
                                               elect,
                                               chr_resp,
                                               chr_cvs,
                                               chr_liv,
                                               lymphoma, 
                                               metast, 
                                               leukaem,
                                               immundis,
                                               immunsup,
                                               immunrx,
                                               cirrhos,
                                               weight, 
                                               aids, 
                                               hepfail, 
                                               age, 
                                               ventilated, 
                                               intubated, 
                                               gcsverb,
                                               gcsmotor,
                                               gcseye,
                                               cardarrest,
                                               uo_wt_hr,
                                               temp_anz,
                                               map_anz,
                                               hr_anz,
                                               rr_anz,
                                               fio2_anz,
                                               pao2_anz,
                                               paco2_anz,
                                               ph_anz,
                                               hco3_ap2,
                                               na_anz,
                                               k_ap2,
                                               creat_anz,
                                               hct_anz,
                                               wcc_anz,
                                               albumin_anz,
                                               urea_anz,
                                               bili_anz,
                                               glucose_anz,
                                               anzrodriskofdeath))

#adjust diagnosis code
ANZICS24FilterNA$ap3diag=recode(ANZICS24FilterNA$ap3diag,
                                '501'=17, '502'=17, '503'=17, '504'=17,
                                '601'=1, '602'=1, '603'=1, '604'=1, '605'=1, '1601'=1, '1602'=1,'1603'=1,'1604'=1,'1605'=1,
                                '201'=2, '202'=2, '203'=2, '204'=2, '205'=2, '206'=2, '207'=2, '208'=2, '209'=2, '210'=2, '211'=2, '212'=2, '213'=2,
                                '0'=0, 
                                '1401'=3,'1402'=3,'1403'=3,'1404'=3,'1405'=3,'1406'=3,'1407'=3,'1408'=3,'1409'=3,'1410'=3,'1411'=3,'1412'=3,'1413'=3,
                                '101'=4,'102'=4,'103'=4,'104'=4,'105'=4,'106'=4,'107'=4,'108'=4,'109'=4,'110'=4,'111'=4,
                                '301'=5,'302'=5,'303'=5,'304'=5,'305'=5,'306'=5,'307'=5,'308'=5,'309'=5,'310'=5,'311'=5,'312'=5,'313'=5,
                                '401'=6,'402'=6,'403'=6,'404'=6,'405'=6,'406'=6,'407'=6,'408'=6,'409'=6,'410'=6,
                                '701'=7,'2201'=7,'702'=7,'703'=7,'704'=7,
                                '801'=8,'2101'=8,'802'=8,
                                '901'=9,'902'=9,'903'=9,
                                '1101'=10,'1102'=10,
                                '1202'=11,'1203'=11,'1204'=11,'1205'=11,'1206'=11,'1207'=11,'1208'=11,'1209'=11,'1210'=11,'1211'=11,'1212'=11,'1213'=11,
                                '1301'=12,'1302'=12,'1303'=12,'1304'=12,
                                '1501'=13,'1502'=13,'1503'=13,'1504'=13,'1505'=13,'1506'=13,
                                '1701'=14,'1702'=14,'1703'=14,'1704'=14,'1705'=14,
                                '1801'=15,'1802'=15,'1803'=15,
                                '1902'=16,'1903'=16,'1904'=16)
#Missing data analysis
plot_missing(ANZICS24Missing)
plot_missing(ANZICS24FilterNA)
matrixplotmissing=matrixplot(ANZICS24Missing, labels=TRUE, cex.axis=0.5) #1500x750 

ggplot(ANZICS24Missing,
       aes(x = icuadmityyyy,
           y = weight)) +
  geom_miss_point()

ANZICS24Missing %>%
  bind_shadow() %>%
  ggplot(aes(x = icuadmityyyy,
             fill = weight)) + 
  geom_density(alpha = 0.5)

#missing data associations 
ANZICS24Missingdata=subset(ANZICS24Missing, select=c(weight, icuadmityyyy, ap3diag, urineop))
ANZICS24Missingdata$icuadmityyyy=as.factor(ANZICS24Missingdata$icuadmityyyy)
ANZICS24Missingdata$ap3diag=as.factor(ANZICS24Missingdata$ap3diag)

ANZICS24Missingwtyr=subset(ANZICS24Missingdata, select=c(weight, icuadmityyyy))
misswtyr2=gg_miss_fct(x = ANZICS24Missingwtyr, fct = icuadmityyyy) + labs(title = "Missing weight values and Year of admission")

ANZICS24Missingdata=subset(ANZICS24Missing, select=c(weight, ph_anz, anzrodriskofdeath, urineop))
ANZICS24Missingph=subset(ANZICS24Missingdata, select=c(ph_anz, anzrodriskofdeath))
ggplot(ANZICS24Missingph, 
       aes(x = anzrodriskofdeath, 
           y = ph_anz)) + 
  geom_miss_point()

ANZICS24Missingwtdiag=subset(ANZICS24Missingdata, select=c(weight, ap3diag))
ANZICS24Missingwtdiag$ap3diag=recode(ANZICS24Missingwtdiag$ap3diag,
                                     '501'=17, '502'=17, '503'=17, '504'=17,
                                     '601'=1, '602'=1, '603'=1, '604'=1, '605'=1, '1601'=1, '1602'=1,'1603'=1,'1604'=1,'1605'=1,
                                     '201'=2, '202'=2, '203'=2, '204'=2, '205'=2, '206'=2, '207'=2, '208'=2, '209'=2, '210'=2, '211'=2, '212'=2, '213'=2,
                                     '0'=0, 
                                     '1401'=3,'1402'=3,'1403'=3,'1404'=3,'1405'=3,'1406'=3,'1407'=3,'1408'=3,'1409'=3,'1410'=3,'1411'=3,'1412'=3,'1413'=3,
                                     '101'=4,'102'=4,'103'=4,'104'=4,'105'=4,'106'=4,'107'=4,'108'=4,'109'=4,'110'=4,'111'=4,
                                     '301'=5,'302'=5,'303'=5,'304'=5,'305'=5,'306'=5,'307'=5,'308'=5,'309'=5,'310'=5,'311'=5,'312'=5,'313'=5,
                                     '401'=6,'402'=6,'403'=6,'404'=6,'405'=6,'406'=6,'407'=6,'408'=6,'409'=6,'410'=6,
                                     '701'=7,'2201'=7,'702'=7,'703'=7,'704'=7,
                                     '801'=8,'2101'=8,'802'=8,
                                     '901'=9,'902'=9,'903'=9,
                                     '1101'=10,'1102'=10,
                                     '1202'=11,'1203'=11,'1204'=11,'1205'=11,'1206'=11,'1207'=11,'1208'=11,'1209'=11,'1210'=11,'1211'=11,'1212'=11,'1213'=11,
                                     '1301'=12,'1302'=12,'1303'=12,'1304'=12,
                                     '1501'=13,'1502'=13,'1503'=13,'1504'=13,'1505'=13,'1506'=13,
                                     '1701'=14,'1702'=14,'1703'=14,'1704'=14,'1705'=14,
                                     '1801'=15,'1802'=15,'1803'=15,
                                     '1902'=16,'1903'=16,'1904'=16)
#0 = Unknown diagnosis
#1 = Trauma
#2 = Respiratory Medicine
#3 = Gastrointestinal Surgery
#4 = Cardiology Medicine
#5 = Gastrointestinal Medicine
#6 = Neurological Medicine
#7 = Metabolic
#8 = Haematology
#9 = Genitourinary/Renal Medicine
#10 = Musculoskeletal Medicine
#11 = Cardiology Surgery
#12 = Respiratory Surgery
#13 = Neurology Surgery
#14 = Genitourinary/Renal Surgery
#15 = Obstetrics/Gynaecology Surgery
#16 = Musculoskeletal Surgery
#17 = Sepsis

misswtyr3=gg_miss_fct(x = ANZICS24Missingwtdiag, fct = ap3diag) + labs(title = "Missing weight values and Admission Diagnosis")
plot(misswtyr3)

#Dummy variables
ANZICS24FilterNA$ap3diag=as.factor(ANZICS24FilterNA$ap3diag)
dmy <- dummyVars(" ~ ap3diag+gcsmotor+gcseye+gcsverb", data = ANZICS24FilterNA)
dmyvar <- data.frame(predict(dmy, newdata = ANZICS24FilterNA))
col=colnames(dmyvar)
dmyvar[col] <- lapply(dmyvar[col], factor)
#too few ap3_subcodes to make meaningful analysis
ANZICS24UOWTdv=cbind(ANZICS24FilterNA, dmyvar)
ANZICS24UOWTdmy=subset(ANZICS24UOWTdv, select=-c(gcsverb, gcsmotor, gcseye, ap3diag))

#Correlations
#Numeric variables
ANZICSNumeric=select_if(ANZICS24UOWTdmy, is.numeric)
ANZICSNumeric=ANZICSNumeric[complete.cases(ANZICSNumeric),]
cor(ANZICSNumeric)
#Urea and Creat correlation 0.66
cor.test(ANZICSNumeric$creat_anz, ANZICSNumeric$urea_anz) #p-value <0.0001
#remove urea given the hypothesis focusing on Creatinine and UO per definitions.
#fio2 and Pao2 0.59 correlation. Remove fio2
#albumin and creat correlated

#Categorical variable association
# Initialize empty matrix to store coefficients
ANZICSCat=select_if(ANZICS24UOWTdmy, is.factor)
ANZICSCat2=subset(ANZICSCat, select=-c(hospitalclassificationid, publicprivate, died_icu, iddm, preg_stat, resparrest, indigenous, thrombpro))
ANZICSCat2=ANZICSCat2[complete.cases(ANZICSCat2),]

empty_m <- matrix(ncol = length(ANZICSCat2),
                  nrow = length(ANZICSCat2),
                  dimnames = list(names(ANZICSCat2), 
                                  names(ANZICSCat2)))
# Function that accepts matrix for coefficients and data and returns a correlation matrix
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- with(df, cv.test(df[[r]], df[[c]]))
    }
  }
  return(m)
}
cor_matrix <- calculate_cramer(empty_m, ANZICSCat2)
round(cor_matrix, digits=3)

#ventilated and intubated 0.94 association; remove ventilated
#chr_liv and cirrhos correlation 0.96; remove cirrhos
#>20% data missing for preg_stat and iddm
ANZICS24FilterNA
ANZICS24UOwt2=subset(ANZICS24UOWTdmy, select=-c(fio2_anz, urea_anz, immunsup, ventilated, cirrhos, preg_stat, iddm))
ANZICS24UOwt2a=subset(ANZICS24FilterNA, select=-c(preg_stat, iddm))
############################################################################################
#ANALYSIS#
############################################################################################
levels(ANZICS24UOwt2$died_hosp)<-c("survived", "died")
levels(ANZICS24UOwt2$died_icu)<-c("survived", "died")

#Remove unnecessary variables for prediction

ANZICS24UOwt4=subset(ANZICS24UOwt2, select=-c(anzrodriskofdeath, hospitalclassificationid, publicprivate, icuadmityyyy, hosp_hrs, died_icu,icu_hrs,hosp_outcm))
dim(ANZICS24UOwt4)#277949
ANZICS24UOwt4=ANZICS24UOwt4[complete.cases(ANZICS24UOwt4),]
dim(ANZICS24UOwt4)#161940

#Raw analysis
ANZICS24UOwt5=ANZICS24UOwt2a[complete.cases(ANZICS24UOwt2a),]
ANZICS24UOwt5$uo_wt_hr <- cut(ANZICS24UOwt5$uo_wt_hr, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 2, Inf))
ANZICS24UOwt5$uo_wt_hr=as.factor(ANZICS24UOwt5$uo_wt_hr)
ANZICS24UOwt5$died_hosp=as.factor(ANZICS24UOwt5$died_hosp)
ANZICS24UOwt5$died_icu=as.factor(ANZICS24UOwt5$died_icu)
ANZICS24UOwt5$hosp_outcm=as.factor(ANZICS24UOwt5$hosp_outcm)
ANZICS24UOwt5$icuadmityyyy=as.factor(ANZICS24UOwt5$icuadmityyyy)
ANZICS24UOwt5$elect=as.factor(ANZICS24UOwt5$elect)
ANZICS24UOwt5$publicprivate=as.factor(ANZICS24UOwt5$publicprivate)

vars=colnames(ANZICS24UOwt5)
factorvars=ANZICS24UOwt5 %>%  select_if(is.factor)
head(factorvars)
factorvarname=colnames(factorvars)
#determine non-normality
qqnorm(ANZICS24UOwt5$weight)
ks.test(ANZICS24UOwt5$weight, 'pnorm', alternative='two.sided') #p <0.001
qqnorm(ANZICS24UOwt5$age)
ks.test(ANZICS24UOwt5$age, 'pnorm', alternative='two.sided') #p <0.001
qqnorm(ANZICS24UOwt5$temp_anz)
ks.test(ANZICS24UOwt5$temp_anz, 'pnorm', alternative='two.sided') #p <0.001
qqnorm(ANZICS24UOwt5$map_anz)
qqnorm(ANZICS24UOwt5$hr_anz)
qqnorm(ANZICS24UOwt5$pao2_anz)
qqnorm(ANZICS24UOwt5$paco2_anz)
qqnorm(ANZICS24UOwt5$ph_anz)
qqnorm(ANZICS24UOwt5$hco3_ap2)
qqnorm(ANZICS24UOwt5$na_anz)
qqnorm(ANZICS24UOwt5$k_ap2)
qqnorm(ANZICS24UOwt5$creat_anz)
qqnorm(ANZICS24UOwt5$hct_anz)
qqnorm(ANZICS24UOwt5$wcc_anz)
qqnorm(ANZICS24UOwt5$albumin_anz)
qqnorm(ANZICS24UOwt5$bili_anz)
qqnorm(ANZICS24UOwt5$glucose_anz)
qqnorm(ANZICS24UOwt5$uo_wt_hr)
qqnorm(ANZICS24UOwt5$anzrodriskofdeath)

nonnormalvar=c("weight", "age", "temp_anz", "map_anz", "pao2_anz", "paco2_anz", "ph_anz", "na_anz", "k_ap2", "creat_anz","wcc_anz", "bili_anz", "glucose_anz", "uo_wt_hr", "icu_hrs", "hosp_hrs")

tableOne3 <- CreateTableOne(vars = vars, strata = "uo_wt_hr", data = ANZICS24UOwt5, factorVars = factorvarname)
summary(tableOne3)
tableOne3basic=print(tableOne3, nonnormal = nonnormalvar)
write.csv(tableOne3basic, file="C:/LAPK/Urine_Nation/tableone_anz2.csv")

tableOneall <- CreateTableOne(vars = vars, data = ANZICS24UOwt5, factorVars = factorvarname)
summary(tableOne3)
tableOneall=print(tableOneall, nonnormal = nonnormalvar)
write.csv(tableOneall, file="C:/LAPK/Urine_Nation/tableone_nostrata.csv")

#Plot proportions UO by segment
DiedHospProp=table(ANZICS24UOwt5$uo_wt_hr, ANZICS24UOwt5$died_hosp)
DiedHospProp2=DiedHospProp/rowSums(DiedHospProp)
DiedHospProp2=as.data.frame(DiedHospProp2)
DiedHospProp2=subset(DiedHospProp2, Var2==1)
DiedHospProp2$Freq=as.numeric(DiedHospProp2$Freq)
DiedHospProp2$Var1=as.factor(DiedHospProp2$Var1)

ggplot(data=DiedHospProp2, aes(x=Var1, y=Freq))+
  geom_bar(stat="identity") +
  scale_fill_grey() +
  labs(x = "Urine Output (mL/kg/h)", y="Proportion Died", title="Died in hospital")+
  scale_x_discrete(breaks=levels(DiedHospProp2$Var1),
                   labels=c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "1-2", ">2"))

# Use 95% confidence intervals instead of SEM
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

#ROC Curves for diagnosis
rocdata=subset(ANZICS24UOwt4, ANZICS24UOwt4$intubated=='0')
rocCurve_Reg_UO <- roc(response=ANZICS24UOwt4$died_hosp,
                       predictor=ANZICS24UOwt4$uo_wt_hr,
                       smooth=FALSE,
                       auc=TRUE, 
                       ci=TRUE, of ="auc")
plot(rocCurve_Reg_UO, print.thres="best", print.auc=TRUE)
rocCurve_Reg_UO

rocdata=subset(ANZICS24UOwt4, ANZICS24UOwt4$ap3diag.11=='1')
rocCurve_Reg_UO <- roc(response=rocdata$died_hosp,
                       predictor=rocdata$uo_wt_hr,
                       smooth=FALSE,
                       auc=TRUE, 
                       ci=TRUE, of ="auc")
plot(rocCurve_Reg_UO, print.thres="best", print.auc=TRUE)
rocCurve_Reg_UO

#Separate data
#Create Train and Test sets
set.seed(3033)
intrain <- createDataPartition(y = ANZICS24UOwt4$died_hosp, p= 0.8, list = FALSE)
ANZICS_Train <- ANZICS24UOwt4[intrain,]
ANZICS_Test <- ANZICS24UOwt4[-intrain,]
dim(ANZICS_Test) #32388 patients
dim(ANZICS_Train) #129552 patients

#XGBoost
trctrl=trainControl(method="repeatedcv", number=5, repeats=3, search="random", sampling="smote", summaryFunction = twoClassSummary, classProbs=TRUE)
XGBoost_death_search_smote=train(died_hosp~., data=ANZICS_Test, method="xgbTree", trControl=trctrl, tuneLength=750, metric="ROC", importance="permutation")
Hosp_Death_XGB_smote=predict(XGBoost_death_search_smote, newdata=ANZICS_Test)
XGB1_ConfMatrix_DiedHosp_smote=confusionMatrix(data=Hosp_Death_XGB_smote, reference=ANZICS_Test$died_hosp, positive="died")
XGB1_ConfMatrix_DiedHosp_smote
#Save model to working directory
saveRDS(XGBoost_death_search_smote)
#Name file XGBoost_ICU_ANZICS_DeathPred
#Load model 
load("XGBoost_ICU_ANZICS_DeathPred.rda") 

trctrl=trainControl(method="repeatedcv", number=5, repeats=3, search="random", summaryFunction = twoClassSummary, classProbs=TRUE)
XGBoost_death_search=train(died_hosp~., data=ANZICS_Test, method="xgbTree", trControl=trctrl, tuneLength=250, metric="ROC", importance="permutation")
Hosp_Death_XGB=predict(XGBoost_death_search, newdata=ANZICS_Test)
XGB1_ConfMatrix_DiedHosp=confusionMatrix(data=Hosp_Death_XGB, reference=ANZICS_Test$died_hosp, positive="died")
XGB1_ConfMatrix_DiedHosp

variableimportant=varImp(XGBoost_death_search_smote)
variableimportant2=as.character(variableimportant)
write.csv(variableimportant2, file="C://LAPK/Urine_Nation/importantvari.csv")

RF_ANZICS_predict_prob_roc=predict(XGBoost_death_search_smote, newdata=ANZICS_Test, type=c("prob"))
rocCurve_Reg_UO_SMOTE <- roc(response=ANZICS_Test$died_hosp,
                             predictor=RF_ANZICS_predict_prob_roc$survived,
                             smooth=FALSE,
                             auc=TRUE, 
                             ci=TRUE, of ="auc")
plot(rocCurve_Reg_UO_SMOTE, print.thres="best", print.auc=TRUE)

rocthresh=ci.thresholds(response=RF_ANZICS_predict_prob_roc,
                        predictor=ANZICS_Test$urineop)

#ROC by diagnosis code adjusted
aucvalues=list()
diagdata=subset(ANZICS24UOwt4, ANZICS24UOwt4$ap3diag.11==1)
RF_ANZICS_predict_prob_roc_diag=predict(XGBoost_death_search_smote, newdata=diagdata)
rocCurve_UO <- roc(response=RF_ANZICS_predict_prob_roc_diag,
                   predictor=diagdata$uo_wt_hr,
                   smooth=FALSE,
                   auc=TRUE, 
                   ci=TRUE, of="auc")
plot(rocCurve_UO, print.thres="best", print.auc=TRUE)

#PDP
pdpurineop <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr"), prob=TRUE, which.class="died", grid.resolution=204)
plotPartial(pdpurineop, ylab="Probability Died", xlab="Urine Output (mL/kg/h)")
ggplot(data=pdpurineop, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

pdpcreatuo <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "creat_anz"), prob=TRUE, which.class="died", grid.resolution=204)
plotPartial(pdpcreatuo, cex.lab=1.5, xlab="Urine Output (mL/kg/h)", ylab="Creatinine (micromol/L)", xlim=c(0, 5), ylim=c(0, 1000))

ggplot(data=pdpcreatuo, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 10))+
  labs(x="Urine Output (mL/kg/h)", y="Creatinine (micromol/L)")

#Plot urine output Partial Dependence plot with bootstrapped 95% CI
urinecutoffs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2)
ice_xgb=ice(object=XGBoost_death_search_smote,ANZICS_Test[1:100,], predictor = "uo_wt_hr",predictfcn = function(object, newdata){
  predict(object, newdata, type = "prob")[,2]
})
icecurves=ice_xgb$ice_curves
icecurves2=as.matrix(icecurves, ncol=ncol(ice_xgb$ice_curves), nrow=nrow(ice_xgb$ice_curves))
i=1
upper=c()
mid=c()
lower=c()
for (i in 1:ncol(ice_xgb$ice_curves)){
  upper[i]=quantile(icecurves2[,i], probs=c(0.95))
  lower[i]=quantile(icecurves2[,i], probs=c(0.05))
  mid[i]=quantile(icecurves2[,i], probs=c(0.5))
}
par(mar=(c(5.1, 5.1, 5.1, 2.1)))
plot(ice_xgb$gridpts, upper, type="l", ylab="Probability Died", xlab="Urine Output (mL/kg/h)", ylim=c(0, 1),xlim=c(0, 2), cex.axis=2, cex.lab=2, xaxt="n")
polygon(c(ice_xgb$gridpts, rev(ice_xgb$gridpts)), c(lower, rev(upper)), col="grey75", border=FALSE)
lines(ice_xgb$gridpts, mid, lwd=4)
lines(ice_xgb$gridpts, upper, col="red", lty=2, lwd=4)
lines(ice_xgb$gridpts, lower, col="red", lty=2, lwd=4)
axis(side=1,
     at=c(0, 0.25,0.5, 0.75, 1, 1.25,1.5, 1.75, 2),
     labels=c("0","0.25", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2"),
     cex.axis=1.5)
saveRDS(ice_xgb, "icexgb.rds")

#Plot mortality mean and 95% CI margins for diagnosis

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

datatester=subset(ANZICS_Test, ANZICS_Test$ap3diag.12==1)
Hosp_Death_XGB_smote=predict(XGBoost_death_search_smote, newdata=datatester, type="prob")
Hosp_Death_Pred=cbind(Hosp_Death_XGB_smote, datatester)
Hosp_Death_Pred$uo_wt_hr <- cut(Hosp_Death_Pred$uo_wt_hr, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 2, Inf))
Hosp_Death_Pred$uo_wt_hr=as.factor(Hosp_Death_Pred$uo_wt_hr)
tgc <- summarySE(Hosp_Death_Pred, measurevar="died", groupvars=c("uo_wt_hr"))
tgc2 <- tgc[1:8,]

ggplot(tgc2, aes(x=uo_wt_hr, y=died)) + 
  geom_errorbar(aes(ymin=died-ci, ymax=died+ci), width=.1) +
  geom_line() +
  geom_point() +
  theme(text=element_text(size=10))+
  ylab("Adjusted Probability Died")+
  scale_x_discrete(name="Urine Output (mL/kg/hr)",labels=c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "1-2", ">2"))



#PDP by diagnosis
pdp.diag17 <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "ap3diag.17"), prob=TRUE, which.class="died", grid.resolution=306)
sepsispdp=subset(pdp.diag17, pdp.diag17$ap3diag.17==1)
sepsispdp=subset(sepsispdp, sepsispdp$uo_wt_hr<5)
sepsispdp$Sepsis=c("Sepsis")
names(sepsispdp)[4] <- "Diagnosis"
sepsispdp=sepsispdp[,-c(2)]
ggplot(data=sepsispdp, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

pdp.diag11 <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "ap3diag.11"), prob=TRUE, which.class="died", grid.resolution=306)
cardoppdp=subset(pdp.diag11, pdp.diag11$ap3diag.11==1)
cardoppdp=subset(cardoppdp, cardoppdp$uo_wt_hr<5)
cardoppdp$Cardiology=c("Cardiology Operative")
names(cardoppdp)[4] <- "Diagnosis"
cardoppdp=cardoppdp[,-c(2)]
ggplot(data=cardoppdp, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

pdp.diag9 <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "ap3diag.9"), prob=TRUE, which.class="died", grid.resolution=306)
renalmedpdp=subset(pdp.diag9, pdp.diag9$ap3diag.9==1)
renalmedpdp=subset(renalmedpdp, renalmedpdp$uo_wt_hr<5)
renalmedpdp$Renal=c("Genitourinary Non-Operative")
names(renalmedpdp)[4] <- "Diagnosis"
renalmedpdp=renalmedpdp[,-c(2)]
ggplot(data=renalmedpdp, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

pdp.diag5 <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "ap3diag.5"), prob=TRUE, which.class="died", grid.resolution=306)
gastromedpdp=subset(pdp.diag5, pdp.diag5$ap3diag.5==1)
gastromedpdp=subset(gastromedpdp, gastromedpdp$uo_wt_hr<5)
gastromedpdp$Gastro=c("Gastrointestinal Non-Operative")
names(gastromedpdp)[4] <- "Diagnosis"
gastromedpdp=gastromedpdp[,-c(2)]
ggplot(data=gastromedpdp, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

pdp.diag13 <- partial(XGBoost_death_search_smote, pred.var = c("uo_wt_hr", "ap3diag.13"), prob=TRUE, which.class="died", grid.resolution=306)
neurooppdp=subset(pdp.diag13, pdp.diag13$ap3diag.13==1)
neurooppdp=subset(neurooppdp, neurooppdp$uo_wt_hr<5)
neurooppdp$Neuroop=c("Neurology Operative")
names(neurooppdp)[4] <- "Diagnosis"
neurooppdp=neurooppdp[,-c(2)]
ggplot(data=neurooppdp, aes(x=uo_wt_hr, y=yhat))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")

PDPDataDiag=rbind(gastromedpdp, renalmedpdp, cardoppdp, sepsispdp)

ggplot(data=PDPDataDiag, aes(x=uo_wt_hr, y=yhat, col=Diagnosis))+
  geom_line()+
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x="Urine Output (mL/kg/h)", y="Probability of Death")
