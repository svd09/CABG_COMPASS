#################################################################
##                COMPASS-CABG:Cohort Selection                ##
#################################################################

# libraries:

library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate")

# first get all the patients that had CABG during the period 2010-01-01 to 2019-12-31.

df = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_compass.sas7bdat')

df$SURGDATE = as_date(df$SURGDATE)

df = df[df$SURGDATE < '2019-01-01', ]

summary(df$SURGDATE)



# now to start first with the exclusion criteria.
# calculate REACH bleeding risk score.

glimpse(df)

# use ICDcodes for the bleeding risk score calculation; need - CHF/PAD/HLP.
# antiplatelet agents and oral anticoagulants can be obtained from the outpat drug files.

pad9 = read_excel('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\pad_codes.xlsx', sheet = 1)

pad10 =  read_excel('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\pad_codes.xlsx', sheet = 2)

# now to get a single list of codes.

pad_codes = c(pad9$icd9,pad10$icd10codes)

# now to identify using codes.

df$peri_art_d = with(df, ifelse((ICD902 %in% pad_codes|ICD903 %in% pad_codes|ICD904 %in% pad_codes|ICD905 %in% pad_codes),
                                1, 0))

df %>% count(peri_art_d) # this contains information regarding PAD.

# now to look at diagnosis of CHF.

chf = c('428.0','I50.2')

df$chf = with(df, ifelse((ICD902 %in% chf|ICD903 %in% chf|ICD904 %in% chf|ICD905 %in% chf), 1, 0))

df %>% count(chf)

hlp = c('272.4','E78.5')

df$hlp = with(df, ifelse((ICD902 %in% hlp|ICD903 %in% hlp|ICD904 %in% hlp|ICD905 %in% hlp), 1, 0))

df %>% count(hlp)

df %>% count(HTN) ; # change NA to 1 --- give missing % here 11/30,000

df$HTN[is.na(df$HTN)]<- 1

# NYHA class 

df %>% count(FCC)

# we need to get the LVEF for all the patients using the TIU data.
# let us first identify those patients that fulfil the inclusion criteria:

# we need to remove those patients that may need anticoagulationi due to mechanical valves/ we do not know that #
# so we will remove those patients that had concomitant valve replacement as CPT02/CPT03.


v = c('33364','33427','33426','33405','33420','33411','33425','33426','33430','33460','33463','33464','33465')

df$valve = with(df, ifelse((CPT02 %in% v| CPT03 %in% v), 1, 0))

df %>% count(valve)


# 334 patients had concomitant valve procedures as CPT02/CPT03 and hence removed.

df2 = df %>% filter(valve == 0)

# remove stemi and nstemi first.
# remove stemi/nstemi patients.
# am going to remove those that are EF < 30% and also those that have NYHA class III/IV symptoms.

stemi = c('410.00',"410.10","410.20","410.30","410.40","410.40","410.50","410.60","410.70","410.80","410.90",
          "I21.3")


nstemi = c("I21.4","410.70")

ami = c(stemi, nstemi)

df2$AMI = with(df2, ifelse((ICD901 %in% ami|ICD902 %in% ami), 1, 0))

df2 %>% count(AMI)

# 1725 have STEMI/NSTEMI -- removed these.

df3 = df2 %>% filter(AMI == 0)

# now to remove those with EF < 30% 

df3 %>% count(LVCGRADE) # missing , so get better values from the TIU data.

summary(df3$SURGDATE)

lv = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\lvef_tiu.sas7bdat')

# also get the crosswalk 

cw = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat')

# now to look at the lvef and am going to limit the lvef value to within 3o days of surgery date.

glimpse(lv)

lv2 = lv %>% dplyr::select(PatientSID,  ValueDateTime, High_Value, Low_Value)

# now going to get the data for only my patients using the crosswalk.

lv2$mine = with(lv2, ifelse(PatientSID %in% cw$PatientSID, 1, 0))

lv3 = lv2[lv2$mine == 1, ]

lv4 = left_join(lv3, cw, by = "PatientSID")

d_s = df3 %>% dplyr::select(scrssn, SURGDATE)

names(lv4) = tolower(names(lv4))

lv5 = left_join(lv4, d_s, by = "scrssn")

# need to limit to nonmissing surgdate 

lv6 = lv5[!is.na(lv5$SURGDATE), ]

lv6$valuedatetime = as_date(lv6$valuedatetime)

summary(lv6$valuedatetime)

lv6 = lv6[!is.na(lv6$valuedatetime), ]

lv6$days = (lv6$SURGDATE %--% lv6$valuedatetime)/ddays(1)

describe(lv6$days)

# limit to within 3 months of the surgdate.

lv6$keep = with(lv6, ifelse(days < 0 & days > -90, 1, 0))

lv6 %>% count(keep)

length(unique(lv6$scrssn))

lv7 = lv6 %>% filter(keep == 1)

lv8 = lv7 %>% arrange(scrssn, -days)

lv9 = lv8[!duplicated(lv8$scrssn), ]

summary(lv9)

lv9$low = with(lv9, ifelse(low_value < 30, 1, 0))

lv9 %>% count(low)

# 2036 have LVEF < 30% 
# now to get the data into main dataset df3

lv10 = lv9 %>% dplyr::select(scrssn, low_value, low)

df4 = left_join(df3, lv10 ,by = "scrssn" )

df4 %>% count(low)



df5 = df4[!is.na(df4$low), ]

# remove those with missing LVEF and then keep to only those with LVEF > 30%
# then will need to keep for NYHA class I/II

df6 = df5 %>% filter(low == 0)


# calculate the eGFR and then limit to above 15.

describe(df6$CR) 

quantile(df6$CR, 0.1, na.rm = T)
quantile(df6$CR, 0.99, na.rm = T)

df6$CR[df6$CR < 0.79]<- 0.79
df6$CR[df6$CR > 6.08]<- 6.08

names(df6) = tolower(names(df6))

# race 

df6$race.mod = with(df6, ifelse(race  %in% c("9"), "black",
                                  ifelse(race %in% c("B"), "white", "others")))

df6 %>% count(race.mod)


# BMI calculation

summary(df6$htin)

summary(df6$wtlbs)

# limit to 99th and 1st percentile to clean the data.
# height 

quantiles = function(x){
  a = quantile(x,0.99,na.rm = T)
  b = quantile(x, 0.01, na.rm = T)
  c = c(a,b)
  c
}

quantiles(df6$htin)

df6$htin2 = with(df6, ifelse(htin < 62, 62,
                               ifelse(htin > 76, 76, htin)))

summary(df6$htin2)

describe(df6$wtlbs)

quantiles(df6$wtlbs)

df6$wtlbs2 = with(df6, ifelse(wtlbs < 126.86, 126.86,
                                ifelse(wtlbs > 317, 317, wtlbs)))

df6$htin2[is.na(df6$htin2)]<- 69.39

df6$wtlbs2[is.na(df6$wtlbs2)]<- 215.6




df6$bmi = (df6$wtlbs2/(df6$htin2)^2)*703

describe(df6$bmi)



#- going to convert the BMI into groups 
#- going to get eGFR and convert eGFR also into groups.
#- sex female = 1
#- race white = 1

df6$race_n <- with(df6, ifelse(race.mod == 'white', 1, 0
))


df6 %>% count(race_n)

gfr <- function(age, scr,sex, race){
  male <- age^(-0.203)*scr^(-1.154)*175
  female <- age^(-0.203)*scr^(-1.154)*175*0.742
  
  a <- ifelse(sex == 1, female , male)
  b <- ifelse(race == 1, a, a*1.212)
  return(b)
}


df6$egfr <- with(df6, gfr(age = age, scr = cr, sex = sex, race = race_n))

describe(df6$egfr)

quantiles(df6$egfr)

df7 = df6[df6$egfr > 15, ] # limited to only eGFR > 15.

# write the dataset as further ...

write_csv(df7, 
  'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_further.csv' )

# 2020-11-06

# going to get the dataset again and go futher for cohort selection.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_further.csv')

# remove those with severe liver disease.
# am going to use ICD codes for liver disease.

liv = c("456.0","456.1","456.2","572.2","572.3","572.4",
        "572.5","572.6","572.7","572.8", "I85.0","I85.9","I86.4",'I98.2',
        "K70.4","K71.1","K72.1","K72.9","K76.5","K76.6","K76.7")

df$liver_d = with(df, ifelse((icd901 %in% liv|icd902 %in% liv|icd903 %in% liv|
                               icd904 %in% liv|icd905 %in% liv), 1, 0))


df %>% count(liver_d)

df2  = df[df$liver_d == 0, ]


# also get patients that are on DAPT prior to surgery.

# now need to obtain the information regarding patients that we on DAPT 
# and OAC prior to surgery.
# other criteria is stroke within 1 month prior to surgery
# we can assume that given the need for CPB, the surgery would not be 
# performed within 1 month of hemorragic stroke
# we need to identify those patients that at higher risk of bleeding.
# we can do that using codes for bleeding risk in the past 1 year prior to
# surgery.
# we need to remove patients that have chronic AF.

af = c("427.31","427.32","I48.20")

df2$chronicaf = with(df2, ifelse((icd903 %in% af|icd904 %in% af|icd905 %in% af|
        icd906 %in% af|icd907 %in% af), 1, 0))

df2 %>% count(chronicaf)

# remove those patients that have chronic AF

df3 = df2[df2$chronicaf == 0, ]

# DAPT prior to surgery.
# identify those patients that had DAPT fill 6 months prior to surgery date.

# get clopidogrel fill 

c = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\clopidogrelfill_c.sas7bdat')

# get the list of patientsid of my patients.


cw = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat')

cw$mine = with(cw, ifelse(ScrSSN %in% df3$scrssn, 1, 0))

cw %>% count(mine)

cw2 = cw %>% filter(mine == 1)

mypat = cw2$PatientSID

c$keep = with(c, ifelse(PatientSID %in% mypat, 1, 0))

c %>% count(keep)

c2 = c %>% filter(keep == 1)

names(c2) = tolower(names(c2))

# now that we have outpat refills for clopidogrel we need to keep only those that are 
# 6 months before surgdate.

cd = df3 %>% dplyr::select(scrssn, surgdate)

names(cw2) = tolower(names(cw2))

cd2 = left_join(cd, cw2, by = "scrssn")

c3 = left_join(c2, cd2, by = "patientsid")

c4 = c3 %>% dplyr::select(scrssn, patientsid, filldatetime, rxstatus, surgdate)

c4$surgdate = as_date(c4$surgdate)

c4$filldatetime = as_date(c4$filldatetime)

# now to keep prescription within 1 month prior to surgery.

c4$days = (c4$surgdate %--% c4$filldatetime)/ddays(1)

# 0 = active / ACTIVE = ACTIVE 

summary(c4$days)

c5 = c4 %>% filter(days < 0 & days > -30)

glimpse(c5)

c6 = c5 %>% filter(rxstatus == "0"| rxstatus == "ACTIVE")

glimpse(c6)


clopidogrel = c6$scrssn

# now to flag those that have active clopidogrel therapy within 1 month of surgery.

df3$clopidogrel = with(df3, ifelse(scrssn %in% clopidogrel, 1, 0))

df3 %>% count(clopidogrel)

# now to do the same for prasugrel and ticagrelor and then warfarin.
############### PRASUGREL 



p = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\prasugrelfill_c.sas7bdat')

# limit to active medication refill now 

p2 = p %>% filter(RxStatus == "0"|RxStatus == "ACTIVE")


cw = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat')

cw$mine = with(cw, ifelse(ScrSSN %in% df3$scrssn, 1, 0))

cw %>% count(mine)

cw2 = cw %>% filter(mine == 1)

mypat = cw2$PatientSID

p2$keep = with(p2, ifelse(PatientSID %in% mypat, 1, 0))

p2 %>% count(keep)

# no one on prasugrel before surgery.


############### TICAGRELOR


t = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\ticagrelorfill_c.sas7bdat')

t2 = t %>% filter(RxStatus == "0"|RxStatus == "ACTIVE")

t2$keep = with(t2, ifelse(PatientSID %in% mypat, 1, 0))

t2 %>% count(keep)

t3 = t2 %>% filter(keep == 1)


cd = df3 %>% dplyr::select(scrssn, surgdate)

names(cw2) = tolower(names(cw2))

cd2 = left_join(cd, cw2, by = "scrssn")

names(t3) = tolower(names(t3))

t4 = left_join(t3, cd2, by = "patientsid")

t5 = t4 %>% dplyr::select(scrssn, patientsid, filldatetime, rxstatus, surgdate)

t5$surgdate = as_date(t5$surgdate)

t5$filldatetime = as_date(t5$filldatetime)

# now to keep prescription within 1 month prior to surgery.

t5$days = (t5$surgdate %--% t5$filldatetime)/ddays(1)

t6 = t5 %>% filter(days < 0 & days > -30)

glimpse(t6)


tica = t6$scrssn 

df3$ticagrelor = with(df3, ifelse(scrssn %in% tica, 1, 0))

df3 %>% count(ticagrelor)


# now dapt == clopidogrel or ticagrelor 


df3$dapt = with(df3, ifelse((clopidogrel == 1|ticagrelor == 1), 1, 0))

df3 %>% count(dapt)

df4 = df3[df3$dapt == 0, ]



# to limit to those on Coumadin.

w = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\warfarinfill_c.sas7bdat')

w2 = w %>% filter(RxStatus == "0"|RxStatus == "ACTIVE")


w2$keep = with(w2, ifelse(PatientSID %in% mypat, 1, 0))

w2 %>% count(keep)

w3 = w2 %>% filter(keep == 1)


cd = df3 %>% dplyr::select(scrssn, surgdate)

names(cw2) = tolower(names(cw2))

cd2 = left_join(cd, cw2, by = "scrssn")

names(w3) = tolower(names(w3))

w4 = left_join(w3, cd2, by = "patientsid")

w5 = w4 %>% dplyr::select(scrssn, patientsid, filldatetime, rxstatus, surgdate)

w5$surgdate = as_date(w5$surgdate)

w5$filldatetime = as_date(w5$filldatetime)


w5$days = (w5$surgdate %--% w5$filldatetime)/ddays(1)

w6 = w5 %>% filter(days < 0 & days > -30)

glimpse(w6)


coum = w6$scrssn

df4$coumadin = with(df4, ifelse(scrssn %in% coum, 1, 0))

df4 %>% count(coumadin)


# remove coumadim now.


df5 = df4[df4$coumadin == 0, ]



write_csv(df5,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_further2.csv'     )



# high risk of bleeding defined as those with history for admission with ICD codes
# of GI bleeding or ICH within 1 year prior to surgery.

# start with GIB.

gib9 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\gib_icd9.sas7bdat')

# limit to my patients and then limit to within 1 year prior to CABG.

gib10 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\gib_icd10.sas7bdat')

names(gib9); names(gib10)

gib9 = gib9 %>% rename(code = ICD9SID); gib10 = gib10 %>% rename(code = ICD10SID)

gib = rbind(gib9, gib10)


# now to limit to my patients only 

gib$keep = with(gib, ifelse(PatientSID %in% mypat, 1, 0))

gib2 = gib %>% filter(keep == 1)

# now to get the surgery date and limit to 1 year prior to surgery.



cd = df3 %>% dplyr::select(scrssn, surgdate)

names(cw2) = tolower(names(cw2))

cd2 = left_join(cd, cw2, by = "scrssn")

names(gib2) = tolower(names(gib2))

gib3 = left_join(gib2, cd2, by = "patientsid")

gib3$admitdatetime = as_date(gib3$admitdatetime) ; gib3$surgdate = as_date(gib3$surgdate)

gib3$days = (gib3$surgdate %--% gib3$admitdatetime)/ddays(1)

gib4 = gib3 %>% filter(days < 0)

gib4 = gib4$scrssn

df5$gib = with(df5, ifelse(scrssn %in% gib, 1, 0))

df5 %>% count(gib) # no one had significant admission with GIB in the past.

###### ICH


ich9 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\ich_icd9.sas7bdat')

# limit to my patients and then limit to within 1 year prior to CABG.

ich10 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\ich_icd10.sas7bdat')

names(gib9); names(gib10)

ich9 = ich9 %>% rename(code = ICD9SID); ich10 = ich10 %>% rename(code = ICD10SID)

ich = rbind(ich9, ich10)


# now to limit to my patients only 

ich$keep = with(ich, ifelse(PatientSID %in% mypat, 1, 0))

ich2 = ich %>% filter(keep == 1)

# now to get the surgery date and limit to 1 year prior to surgery.



cd = df4 %>% dplyr::select(scrssn, surgdate)

names(cw2) = tolower(names(cw2))

cd2 = left_join(cd, cw2, by = "scrssn")

names(ich2) = tolower(names(ich2))

ich3 = left_join(ich2, cd2, by = "patientsid")


ich3$admitdatetime = as_date(ich3$admitdatetime) ; ich3$surgdate = as_date(ich3$surgdate)

ich3$days = (ich3$surgdate %--% ich3$admitdatetime)/ddays(1)

ich4 = ich3 %>% filter(days < 0)

ich = ich4$scrssn

df5$ich = with(df5, ifelse(scrssn %in% ich, 1, 0))

df5 %>% count(ich)

# remove these patients with significant bleeding risk.

df6 = df5[df5$ich == 0, ]

write_csv(df6, 
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_further3.csv'    )


# now with all the exclusion criteria listed, cabg_further3 is the dataset.
# now to work on the inclusion criteria from this and this would be the COMPASS eligible cohort.
# rest would be the COMPASS ineligible cohort.



