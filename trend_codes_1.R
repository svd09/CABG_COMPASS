# going to get the new figure for the COMPASS eligible for each year.

library(pacman)
p_load("tidyverse","Hmisc","rms","survival",
       "haven","readxl","naniar","lubridate","DescTools")


df = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_compass.sas7bdat')

df$SURGDATE = as_date(df$SURGDATE)

summary(df$SURGDATE)

df = df[df$SURGDATE < '2019-01-01', ]

summary(df$SURGDATE)

glimpse(df)

length(unique(df$scrssn))

df = df %>% arrange(scrssn, SURGDATE)

df2 = df 

# final data-set here.

fd = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_allevents.csv')

sc = fd$scrssn

df2 = df2[!duplicated(df2$scrssn), ]

df2$elig = with(df2, ifelse(scrssn %in% sc, 1, 0)); df2 %>% count(elig)


# look at changes in inclusion criteria per year of study.

df2$year = year(df2$SURGDATE)

# overall prop.
prop = df2 %>% group_by(year) %>% summarise(mean = mean(elig))

linear = lm(mean ~ year, data = prop)

linear

# age 
age = df2 %>% group_by(year) %>% summarise(age = mean(AGE))

df2$young = with(df2, ifelse(AGE < 65, 1, 0))

prop_young = df2 %>% group_by(year) %>% summarise(young = mean(young))

prop_young

y = as.matrix(prop_young)

CochranArmitageTest(y)

# 
# year young
# <dbl> <dbl>
#   1  2010 0.600
# 2  2011 0.597
# 3  2012 0.527
# 4  2013 0.475
# 5  2014 0.412
# 6  2015 0.359
# 7  2016 0.327
# 8  2017 0.316
# 9  2018 0.310

# PAD

df2 %>% count(PVD)

df2$PVD[is.na(df2$PVD)]<- 0

prop_pad = df2 %>% group_by(year) %>% summarise(pad = mean(PVD))

prop_pad

# # A tibble: 9 x 2
# year   pad
# <dbl> <dbl>
#   1  2010 0.215
# 2  2011 0.233
# 3  2012 0.226
# 4  2013 0.214
# 5  2014 0.211
# 6  2015 0.167
# 7  2016 0.164
# 8  2017 0.148
# 9  2018 0.145

# DM

df2 %>% count(DIABETES)

df2$dm = with(df2, ifelse(DIABETES != 0, 1, 0))

df2 %>% count(dm)

DM = df2 %>% group_by(year) %>% summarise(dm = mean(dm))

DM

dm = as.matrix(DM)

DM$year = factor(DM$year, ordered = T)

CochranArmitageTest(dm, alternative = "two.sided")

# # A tibble: 9 x 2
# year    dm
# <dbl> <dbl>
#   1  2010 0.434
# 2  2011 0.436
# 3  2012 0.420
# 4  2013 0.459
# 5  2014 0.468
# 6  2015 0.461
# 7  2016 0.466
# 8  2017 0.486
# 9  2018 0.474

df2 %>% count(CURRSMOK)
df2$CURRSMOK[is.na(df2$CURRSMOK)]<- 0

smoking = df2 %>% group_by(year) %>% summarise(smoking = mean(CURRSMOK))

smoking

# > smoking
# # A tibble: 9 x 2
# year smoking
# <dbl>   <dbl>
#   1  2010   0.284
# 2  2011   0.272
# 3  2012   0.268
# 4  2013   0.246
# 5  2014   0.249
# 6  2015   0.228
# 7  2016   0.232
# 8  2017   0.220
# 9  2018   0.225



df2 %>% count(CVD)

df2$CVD[df2$CVD == 2]<- 1


cvd = df2 %>% group_by(year) %>% summarise(cvd = mean(CVD))

cvd

# > cvd
# # A tibble: 9 x 2
# year   cvd
# <dbl> <dbl>
#   1  2010 0.226
# 2  2011 0.210
# 3  2012 0.230
# 4  2013 0.233
# 5  2014 0.261
# 6  2015 0.258
# 7  2016 0.246
# 8  2017 0.251
# 9  2018 0.232

# heart failure

names(df2) = tolower(names(df2))

hf = c("428.9","I50.9")

df2$hf = with(df2, ifelse((icd901 %in% hf | icd902 %in% hf | icd903 %in% hf | 
                             icd904 %in% hf | icd905 %in% hf |
                             icd906 %in% hf | icd907 %in% hf | icd908 %in% hf | icd909 %in% hf | icd910 %in% hf)
                          , 1, 0)) 


df2 %>% count(hf)

df2 %>% count(curdiur)

df2$HF = with(df2, ifelse((hf == 1|curdiur == 1), 1, 0))

df2 %>% count(HF)

heart_f = df2 %>% group_by(year) %>% summarise(heart_f = mean(HF))

heart_f

# # A tibble: 9 x 2
# year heart_f
# <dbl>   <dbl>
#   1  2010   0.323
# 2  2011   0.312
# 3  2012   0.323
# 4  2013   0.313
# 5  2014   0.311
# 6  2015   0.310
# 7  2016   0.326
# 8  2017   0.325
# 9  2018   0.306

# CKD

names(df2) = tolower(names(df2))

# calculate the eGFR and then limit to above 15.

describe(df2$cr) 

quantile(df2$cr, 0.1, na.rm = T)
quantile(df2$cr, 0.99, na.rm = T)

df2$cr[df2$cr < 0.79]<- 0.79
df2$cr[df2$cr > 6.08]<- 6.08



# race 

df2$race.mod = with(df2, ifelse(race  %in% c("9"), "black",
                                ifelse(race %in% c("B"), "white", "others")))

df2 %>% count(race.mod)


# BMI calculation

summary(df2$htin)

summary(df2$wtlbs)

# limit to 99th and 1st percentile to clean the data.
# height 

quantiles = function(x){
  a = quantile(x,0.99,na.rm = T)
  b = quantile(x, 0.01, na.rm = T)
  c = c(a,b)
  c
}

quantiles(df2$htin)

df2$htin2 = with(df2, ifelse(htin < 62, 62,
                             ifelse(htin > 76, 76, htin)))

summary(df2$htin2)

describe(df2$wtlbs)

quantiles(df2$wtlbs)

df2$wtlbs2 = with(df2, ifelse(wtlbs < 126.86, 126.86,
                              ifelse(wtlbs > 317, 317, wtlbs)))

df2$wtlbs2[is.na(df2$wtlbs2)]<- 215.6

df2$htin2[is.na(df2$htin2)]<- 69.32


df2$bmi = (df2$wtlbs2/(df2$htin2)^2)*703

describe(df2$bmi)



#- going to convert the BMI into groups 
#- going to get eGFR and convert eGFR also into groups.
#- sex female = 1
#- race white = 1

df2$race_n <- with(df2, ifelse(race.mod == 'white', 1, 0
))


df2 %>% count(race_n)

gfr <- function(age, scr,sex, race){
  male <- age^(-0.203)*scr^(-1.154)*175
  female <- age^(-0.203)*scr^(-1.154)*175*0.742
  
  a <- ifelse(sex == 1, female , male)
  b <- ifelse(race == 1, a, a*1.212)
  return(b)
}


df2$egfr <- with(df2, gfr(age = age, scr = cr, sex = sex, race = race_n))



# CKD

df2$egfr

df2$ckd = with(df2, ifelse(egfr < 60, 1, 0))

df2 %>% count(ckd)

df2$ckd

df2$ckd[is.na(df2$ckd)]<- 0

ckd_prop = df2 %>% group_by(year) %>% summarise(ckd_res = mean(ckd))

ckd_prop

# > ckd_prop
# # A tibble: 9 x 2
# year ckd_res
# <dbl>   <dbl>
#   1  2010   0.204
# 2  2011   0.213
# 3  2012   0.216
# 4  2013   0.217
# 5  2014   0.225
# 6  2015   0.224
# 7  2016   0.232
# 8  2017   0.244
# 9  2018   0.259

ckd = as.matrix(ckd_prop)

CochranArmitageTest(ckd, alternative = "decreasing")