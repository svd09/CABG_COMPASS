
#################################################################
##                         Limb events                         ##
#################################################################

# this script will give the limb events for our cohort.
# am going to do the limb events stratified by prior PAD.


# critical limb ishemia



library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "haven", "etm")

# get the dataset.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte.csv')

# get the crosswalk and then the inpatient admission dataset


c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

# keep for my patients.

d = df %>% dplyr::select(scrssn, surgdate)

names(c) = tolower(names(c))

c2 = left_join(d, c, by = "scrssn")

psid = c2$patientsid

# now psid contains the patientsid for all my patients.
# to get the inpat files.

ip = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_admissions_compass.sas7bdat')

# limit to my patients

ip$keep = with(ip, ifelse(PatientSID %in% psid, 1, 0))

ip %>% count(keep)

ip2 = ip[ip$keep == 1, ]

cli9 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\crit_li_icd9sid.sas7bdat')

cli9sid = cli9$ICD9SID

cli10 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\crit_li_icd10sid.sas7bdat')

cli10sid = cli10$ICD10SID


ip2$cli9 = with(ip2, ifelse(ICD9SID %in% cli9sid, 1, 0))

ip2$cli10 = with(ip2, ifelse(ICD10SID %in% cli10sid, 1, 0))

ip2 %>% count(cli9) ; ip2 %>% count(cli10)


# looking at vascular procedures during the follow-up.


ip = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_proc_compass.sas7bdat')

vasc =  read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vasc_bypass_cptsid.sas7bdat')

ip$keep = with(ip, ifelse(PatientSID %in% psid, 1, 0)) ; ip2 = ip[ip$keep == 1, ]

vasc_cptsid = vasc$CPTSID

ip2$vasc = with(ip2, ifelse(CPTSID %in% vasc_cptsid, 1, 0)) ; ip2 %>% count(vasc)

# amputations procedures inpatient.

amp =  read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\amputation_cptsid.sas7bdat')

amp_cptsid = amp$CPTSID

ip2$amp = with(ip2, ifelse(CPTSID %in% amp_cptsid, 1, 0)) ; ip2 %>% count(amp)

# embolectomy/endarterctomy.

# get the cptsid codes here.

emb = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\embolectomy_cptsid.sas7bdat')

emb_cptsid = emb$CPTSID

ip2$emb = with(ip2, ifelse(CPTSID %in% emb_cptsid, 1, 0))

ip2 %>% count(emb)

# now to code for vascular event if emb|amp|vasc == 1

glimpse(ip2)

ip2$vasc_event = with(ip2, ifelse((emb == 1|vasc == 1|amp == 1), 1, 0))

ip2 %>% count(vasc_event)

ip3 = ip2 %>% dplyr::select(PatientSID, vasc_event, AdmitDateTime)

ip4 = ip3 %>% rename(vasc_event_date = AdmitDateTime)

ip4$vasc_event_date = as_date(ip4$vasc_event_date)

ip5 = ip4[ip4$vasc_event_date < '2020-05-01', ]

ip6 = ip5[ip5$vasc_event == 1, ]

names(ip6) = tolower(names(ip6))

ip7 = left_join(ip6, c2, by = "patientsid")

ip8 = ip7 %>% arrange(scrssn, vasc_event_date)

ip8$vasc_days = (ip8$surgdate %--% ip8$vasc_event_date)/ddays(1)

describe(ip8$vasc_days)

ip9 = ip8[ip8$vasc_days > 0, ]

ip10 = ip9 %>% dplyr::select(scrssn, vasc_event, vasc_days)

ip11 = ip10[!duplicated(ip10$scrssn), ]

df2 = left_join(df, ip11 , by = "scrssn")

# now df2 contains the information for vascular events - amputation/bypass/embolectomy.

df2$vasc_event[is.na(df2$vasc_event)]<- 0

# composite for CIF 

df2$comp_vasc_event = with(df2, ifelse(vasc_event == 1, 1, 
                                       ifelse(died == 1, 2, 0)))

df2 %>% count(comp_vasc_event)

df2$comp_vasc_days = with(df2, ifelse(vasc_event == 1, vasc_days, fupdays))

df2$comp_vasc_years = (df2$comp_vasc_days + 1)/365.24

# now to calculate CIF for vascular events.

vasc_cif = survfit(Surv(comp_vasc_years, comp_vasc_event, type = "mstate") ~ 1,
                   data = df2)

summary(vasc_cif, times = c(0,1,3,5))

# Call: survfit(formula = Surv(comp_vasc_years, comp_vasc_event, type = "mstate") ~ 
#                 1, data = df2)
# 
# time n.risk n.event P((s0))    P(1)   P(2)
# 0  15586       0   1.000 0.00000 0.0000
# 1  14808     733   0.953 0.00752 0.0396
# 3  11285     912   0.889 0.01721 0.0937
# 5   7502     852   0.811 0.02570 0.1631

# now to save this dataset and that contains all end-points apart from CV hospitalization...

write_csv(df2,
    'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc.csv')

# the last event rate that I need is MI. MI is important, so I will do both inpatient and outpatient MI.

# inpatient admissions 

ip = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_admissions_compass.sas7bdat')

mi9 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\icd9code_mi.sas7bdat')

mi10 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\icd10code_mi.sas7bdat')

ip$mi9 = with(ip, ifelse(ICD9SID %in% mi9$ICD9SID, 1, 0))

ip %>% count(mi9)

ip$mi10 = with(ip, ifelse(ICD10SID %in% mi10$ICD10SID, 1, 0))

ip %>% count(mi10)

ip$mi = with(ip, ifelse((mi9 == 1|mi10 == 1), 1, 0))

ip %>% count(mi)

ip2 = ip[ip$mi == 1, ]

ip2$keep = with(ip2, ifelse(PatientSID %in% psid, 1, 0))

ip2 %>% count(keep)

ip3 = ip2[ip2$keep == 1, ]

ip4 = ip3 %>% dplyr::select(PatientSID, AdmitDateTime, mi)

names(ip4) = tolower(names(ip4))

ip5 = left_join(ip4, c2, by = "patientsid")

ip6 = ip5 %>% rename(mi_date = admitdatetime) ; ip6$mi_date = as_date(ip6$mi_date)

ip6$mi_days = (ip6$surgdate %--% ip6$mi_date)/ddays(1)

ip7 = ip6[ip6$mi_days > 0, ]

ip8 = ip7 %>% arrange(scrssn, mi_days)

ip9 = ip8[!duplicated(ip8$scrssn), ]

ip10 = ip9 %>% dplyr::select(scrssn, mi_days, mi) %>% rename(mi_event = mi)

ip10

df2 = left_join(df, ip10, by = "scrssn")

df2$mi_event[is.na(df2$mi_event)]<- 0

df2$comp_mi_event = with(df2, ifelse(mi_event == 1, 1, 
                                     ifelse(died == 1, 2, 0)))

df2 %>% count(mi_event)

df2$comp_mi_days = with(df2, ifelse(mi_event == 1, mi_days, fupdays))

df2$comp_mi_years = (1 + df2$comp_mi_days)/365.24

# get the MI CIF.

mi_est = survfit(Surv(comp_mi_years, comp_mi_event, type = "mstate") ~ 1,
                 data = df2)

summary(mi_est, times = c(0,1,3,5))


# Call: survfit(formula = Surv(comp_mi_years, comp_mi_event, type = "mstate") ~ 
#                 1, data = df2)
# 
# time n.risk n.event P((s0))   P(1)   P(2)
# 0  15586       0   1.000 0.0000 0.0000
# 1  14581     961   0.938 0.0232 0.0385
# 3  11057     982   0.870 0.0410 0.0893
# 5   7326     914   0.786 0.0597 0.1542

# save this dataset with all end-points/ CV hospitalization is left...

write_csv(df2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc_mi.csv')