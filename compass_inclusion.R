
#################################################################
##                      COMPASS Inclusion                      ##
#################################################################


# libraries:

library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "tableone")

df = read_csv(  'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\cabg_further3.csv' )



# now to keep only first / primary CABG

df = df %>% arrange(scrssn, surgdate)

# now all pts > 65 years 

df2 = df[!duplicated(df$scrssn), ]

df_old = df2 %>% filter(age >= 65)

scrssn_old = df_old$scrssn

df_young = df2[!(df2$scrssn %in% scrssn_old), ]

# all old will be included in the COMPASS cohort.
# for young patients i.e. < 65 years,
# PAD/CVD

df_young %>% count(pvd) ; df_young %>% count(cvd)


df_young$c1 = with(df_young, ifelse((pvd == 1|cvd == 1), 1, 0))

df_young %>% count(c1)

df_young$low_egfr = with(df_young, ifelse(egfr < 60, 1, 0))

df_young %>% count(low_egfr)

# heart failure 

hf = c("428.9","I50.9")

df_young$hf = with(df_young, ifelse((icd901 %in% hf|icd902 %in% hf|icd903 %in% hf|icd904 %in%hf|
            icd905 %in% hf|icd906 %in% hf|icd907 %in% hf|icd908 %in% hf|icd909 %in% hf), 1, 0))

df_young %>% count(hf)

df_young$hf = with(df_young, ifelse((hf ==1|curdiur == 1),1, 0))

# now to get the inclusion as 2 or more of the above.

df_young %>% count(currsmok)

df_young$dm = with(df_young, ifelse(diabetes %in% c(1,2), 1, 0))

df_young %>% count(dm)

# current smoker.

df_young %>% count(csmok)

df_young$active_smoke = with(df_young, ifelse(csmok == 1, 1, 0))

df_young$add = df_young$dm + df_young$hf + df_young$active_smoke + df_young$low_egfr

df_young$compass_e = with(df_young, ifelse((c1 == 1|add > 1), 1, 0))

df_young %>% count(compass_e)

df_young$compass_e[is.na(df_young$compass_e)]<- 0

df_young %>% count(compass_e)


# save this dataset and also the old one.

write_csv( df_old, 
  'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_old.csv')


write_csv( df_young, 
           'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_young.csv')


# now to join the young and old data to combine as CABG-COMPASS eligible cohort in the VA.
# 2010 - 2019.

# now am going to combine both the datasets. I will need to make the columns same prior to rbind.

df_old = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_old.csv')


df_young = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_young.csv')



# now to ensure that we have these columns in df_old

df_old$c1 = NA
df_old$low_egfr = NA
df_old$hf = NA
df_old$dm = NA
df_old$compass_e = 1
df_old$active_smoke = NA

df_total = rbind(df_young, df_old)

glimpse(df_total)

df_total %>% count(compass_e)

# now am going to save this dataset and then we need to go further to understand the end-points.
# save

write_csv(df_total,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_total.csv')

# distribution of inclusion criteria factors in young patients.


vars = c("dm","hf","active_smoke","low_egfr","pvd","cvd")

table = CreateCatTable(vars = vars, data = df_young[df_young$compass_e == 1, ])

table

# 
# Overall     
# n                    4861        
# dm = 1 (%)           2924 (60.2) 
# hf = 1 (%)           2087 (42.9) 
# active_smoke = 1 (%) 2230 (45.9) 
# low_egfr = 1 (%)     1110 (22.8) 
# pvd = 1 (%)          1700 (35.0) 
# cvd = 1 (%)          1814 (37.3) 


