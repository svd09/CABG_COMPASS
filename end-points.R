##################################################################
##                          End-points                          ##
##################################################################

# from the data will identify end-points at 1, 3, and 5 years.
# look at the following end-points.

# all-cause Mortality
# Stroke
# PCI/Redo-CABG
# 
# Bleeding needing admission #
# 
# Acute limb ischemia
# Chronic limb ischemia
# Critical limb ischemia = ALI + CLI + need for vascular surgery
# Amputation

# get informtion on vital status.


library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "haven")

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds.csv')

# crosswalk 


c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

# now to get the vital status also 

vs = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vitals_compass.sas7bdat")

# get the vs into df

names(vs) = tolower(names(vs))


glimpse(vs)


vs2 = vs %>% arrange(vs, desc(act_last_dt))

summary(vs2$act_last_dt)


vs3 = vs2[!duplicated(vs2$scrssn), ]

df2 = left_join(df, vs3, by = "scrssn")

df2$fupdays = (df2$surgdate %--% df2$act_last_dt)/ddays(1)

summary(df2$fupdays)

df2$fupyears = (df2$fupdays + 1)/365.24

df2$died = with(df2, ifelse(living_ind == 0, 1, 0))

df2 %>% count(living_ind)

df2 %>% count(died)

#df2 now contains information regarding mortality and vital status.
# now to look at all-cause mortality.

s = survfit(Surv(fupyears, died) ~ 1, data = df2)

plot(s, conf.int = T)

# look at 1, 3, 5 year all-cause mortality.

summary(s, times = c(0,1,3,5))
# 
# > summary(s, times = c(0,1,3,5))
# Call: survfit(formula = Surv(fupyears, died) ~ 1, data = df2)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 0  15586       0    1.000 0.00000        1.000        1.000
# 1  14920     621    0.960 0.00157        0.957        0.963
# 3  11477     800    0.904 0.00243        0.899        0.909
# 5   7702     786    0.832 0.00334        0.826        0.839

# now to look at PCI done during follow-up.

pci = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\pci_compass.sas7bdat")

glimpse(pci)

names(pci ) = tolower(names(pci))

d = df %>% dplyr::select(scrssn, surgdate)

names(c) = tolower(names(c))

c2 = left_join(d, c, by = "scrssn")

psid = c2$patientsid

pci$mine = with(pci, ifelse(patientsid %in% psid, 1, 0))

pci %>% count(mine)

pci2 = pci %>% filter(mine == 1)

glimpse(pci2)

pci3 = pci2 %>% arrange(scrssn, proceduredatetime)

pci3$pci = 1

pci4 = pci3[!duplicated(pci3$scrssn), ]

d2 = left_join(pci4,d, by = "scrssn")

glimpse(d2)

d2$pci[is.na(d2$pci)]<- 0

d2$proceduredatetime = as_date(d2$proceduredatetime)

d2 = d2[d2$proceduredatetime < '2020-05-01', ]

d2$pci_days = (d2$surgdate %--% d2$proceduredatetime)/ddays(1)

describe(d2$pci_days)

d3 = d2[d2$pci_days > 0, ]


d_pci = d3 %>% dplyr::select(scrssn, pci, proceduredatetime, pci_days)

length(unique(d_pci$scrssn))

d_pci2 = d_pci %>% arrange(scrssn, proceduredatetime)


d_pci3 = d_pci2[!duplicated(d_pci2$scrssn), ]

glimpse(d_pci3)

df3 = left_join(df2, d_pci3, by = "scrssn")

df3 %>% count(pci.y)

glimpse(df3)

df3 = df3 %>% rename(pci_event = pci.y)

df3 %>% count(pci_event)

df3$pci_event[is.na(df3$pci_event)]<- 0

df3$comp_pci_event = with(df3, ifelse(pci_event == 1, 1,
                                      ifelse(died == 1, 2, 0)))

df3 %>% count(comp_pci_event)


df3$comp_pci_days = with(df3, ifelse(pci_event == 1, 
                        pci_days, fupdays))

summary(df3$comp_pci_days)

df3$comp_pci_years = (df3$comp_pci_days + 1)/365.24

# pci event 

pci_event = survfit(Surv(comp_pci_years, comp_pci_event, type = "mstate") ~ 1,
                    data = df3)

summary(pci_event, times = c(0,1,3,5))

# Call: survfit(formula = Surv(comp_pci_years, comp_pci_event, type = "mstate") ~ 
#                 1, data = df3)
# 
# time n.risk n.event P((s0))   P(1)   P(2)
# 0  15586       0   1.000 0.0000 0.0000
# 1  14531    1011   0.935 0.0265 0.0384
# 3  10931    1056   0.861 0.0473 0.0914
# 5   7178     930   0.776 0.0644 0.1593

# heart failure readmission.
# will need to identify readmissions due to HF and only keep those
# that are from my cohort.
# df3 is going to be used as that is now the latest dataset.

# am going to save df3 now.

write_csv(df3,
      'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci.csv')    

# HF readmissions.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci.csv')

# now have to get the inpatient admissions and then 

# get the data on inpatient admissions.

ipadmit = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_admissions_compass.sas7bdat')



c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

# now to get only my patients.

c$mine = with(c, ifelse(ScrSSN %in% df$scrssn, 1, 0))

c %>% count(mine)

c2 = c %>% dplyr::filter(mine == 1)

psid = c2$PatientSID

# now to look at only my patients using the patientsid.
# psid is already coded from earlier.

glimpse(ipadmit)

ipadmit$mine = with(ipadmit, ifelse(PatientSID %in% psid, 1, 0))

ipadmit %>% count(mine)

ip2 = ipadmit %>% filter(mine == 1)

# now to limit to beyond surgery date and then save that.
# can use that then to identify both hf and cv hospitalizations too.
# first to limit to after surgery.

d = df %>% dplyr::select(scrssn, surgdate)

names(c2) = tolower(names(c2))

d2 = left_join(d, c2, by = "scrssn")

glimpse(d2)

glimpse(ip2)

d3 = d2 %>% dplyr::select(-mine)

ip3 = ip2 %>% dplyr::select(-mine)

# now to joing both.

names(ip3) = tolower(names(ip3))

d4 = left_join(d3, ip3, by = "patientsid")

glimpse(d4)

d4$admitdatetime = as_date(d4$admitdatetime)

d4 = d4[d4$admitdatetime < '2020-05-01', ]

d4$hf_days = (d4$surgdate %--% d4$admitdatetime)/ddays(1)

describe(d4$hf_days)

d5 = d4[d4$hf_days > 0, ]

glimpse(d5)

# now d5 is the data for admissions after surgery for all patients.
# that had readmissions.

length(unique(d5$scrssn)) # 9970 patients from 15586 patients.

write_csv(d5,
	'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpatients_compass.csv')

# now to get the hf sid for icd9 and icd10.


hf_icd9 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\hf_icd9sid.sas7bdat')


hf_icd10 = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\hf_icd10sid.sas7bdat')

glimpse(hf_icd9)

d5$hficd9 = with(d5, ifelse(icd9sid %in% hf_icd9$ICD9SID, 1, 0))

d5$hficd10 = with(d5, ifelse(icd10sid %in% hf_icd10$ICD10SID, 1, 0))

d5 %>% count(hficd9)

d5 %>% count(hficd10)

#  now to only keep those that have hf readmissions.

d6 = d5 %>% filter(hficd9 == 1 | hficd10 == 1)

glimpse(d6)

length(unique(d6$scrssn))

d6$hf_readmit = 1

# now to calculate the days from surgery to admission.

d6$admitdatetime = as_date(d6$admitdatetime)

d6 = d6[d6$admitdatetime < '2020-05-01', ]

d6$hf_readmit_days = (d6$surgdate %--% d6$admitdatetime)/ddays(1)

describe(d6$hf_readmit_days)

d7 = d6 %>% arrange(scrssn, hf_readmit_days)

d8 = d7[!duplicated(d7$scrssn), ]

glimpse(d8)

d8_keep = d8 %>% dplyr::select(scrssn, hf_readmit, hf_readmit_days)

df2 = left_join(df, d8_keep, by = "scrssn")

glimpse(df2)

df2$hf_readmit[is.na(df2$hf_readmit)]<- 0

df2 %>% count(hf_readmit)

df2$hf_readmit_comp = with(df2, ifelse(hf_readmit == 1, 1,
				ifelse(died == 1, 2, 0)))

df2 %>% count(hf_readmit_comp)


df2$hf_days_comp = with(df2, ifelse(hf_readmit == 1, hf_readmit_days, fupdays))

describe(df2$hf_days_comp)

df2$hf_years_comp = (df2$hf_days_comp + 1)/365.24

hf_est = survfit(Surv(hf_years_comp, hf_readmit_comp, type = "mstate") ~ 1,
                 data = df2)

summary(hf_est, times = c(0,1,3,5))

# Call: survfit(formula = Surv(hf_years_comp, hf_readmit_comp, type = "mstate") ~ 
#                 1, data = df2)
# 
# time n.risk n.event P((s0))   P(1)   P(2)
# 0  15586       0   1.000 0.0000 0.0000
# 1  14782     760   0.951 0.0097 0.0391
# 3  11268     921   0.887 0.0214 0.0920
# 5   7471     933   0.802 0.0405 0.1580

glimpse(df2)

write_csv(df2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf.csv' )

# CV hospitalization/ stroke
# stroke is going to be modeled as both outpat and inpat stroke.
# am not sure how to do the PAD data and if that is needed.
# also to present results according to pre-specified sub-groups.
# CKD/PAD/Age/

# STROKE:- both inpat and outpat.
# outpat stroke was identified for postop AF paper.

# inpat stroke first.

# get the new dataset, crosswalk, inpat files and then use patientsid.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf.csv')


ipadmit = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_admissions_compass.sas7bdat')



c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

# now to get only my patients.

c$mine = with(c, ifelse(ScrSSN %in% df$scrssn, 1, 0))

c %>% count(mine)

c2 = c %>% dplyr::filter(mine == 1)

psid = c2$PatientSID; length(psid) # 46961

# now to further limit the inpat admissions to those from the psid.

ipadmit$keep = with(ipadmit, ifelse(PatientSID %in% psid, 1, 0))

ipadmit %>% count(keep); ipadmit2 = ipadmit[ipadmit$keep == 1, ]

# now to get the stroke icd9 and icd10 sid.


st9 = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_icd9sid.sas7bdat")


st10 = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_icd10sid.sas7bdat")

# now to limit to those admission for stroke as primary diagnosis.

glimpse(ipadmit2)

ipadmit2$st9 = with(ipadmit2, ifelse(ICD9SID %in% st9$ICD9SID,1, 0))


ipadmit2$st10 = with(ipadmit2, ifelse(ICD10SID %in% st10$ICD10SID,1, 0))

ipadmit2 %>% count(st9); ipadmit2 %>% count(st10)

# keep only those with admission for stroke.

ipadmit_st9 = ipadmit2[ipadmit2$st9 == 1, ] ; ipadmit_st10 = ipadmit2[ipadmit2$st10 , ]

glimpse(ipadmit_st9);glimpse(ipadmit_st10)

ipadmit_st9$ip_stroke = 1 ; ipadmit_st10$ip_stroke = 1

ipadmit_st9 = ipadmit_st9[,c("PatientSID","AdmitDateTime",'ip_stroke')]


ipadmit_st10 = ipadmit_st10[,c("PatientSID","AdmitDateTime",'ip_stroke')]

ip_stroke = rbind(ipadmit_st9, ipadmit_st10) ; glimpse(ip_stroke)

# now to get the scrssn and then list as first stroke.
# before that will also get the outpat stroke list.

# 

op_st9 = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\outpat_stroke_icd9_compass.sas7bdat")


op_st10 = 
  read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\outpat_stroke_icd10_compass.sas7bdat")

# now to limit to only my patients.

op_st9$keep = with(op_st9, ifelse(PatientSID %in% psid, 1, 0))

op_st9 %>% count(keep)

op_st10$keep = with(op_st10, ifelse(PatientSID %in% psid, 1, 0))

op_st10 %>% count(keep)

op_st9$op_stroke = 1

op_st10$op_stroke = 1

op_st9 = op_st9[op_st9$keep == 1, ]

op_st10 = op_st10[op_st10$keep == 1, ]

op_st9 = op_st9 %>% dplyr::select(PatientSID, op_stroke, VisitDateTime)

op_st10 = op_st10 %>% dplyr::select(PatientSID, op_stroke, VisitDateTime)

op_stroke = rbind(op_st9, op_st10)

# now to combine both outpat and inpat stroke together.

ip_stroke = ip_stroke %>% rename(date = AdmitDateTime, stroke_event = ip_stroke)

op_stroke = op_stroke %>% rename(date = VisitDateTime, stroke_event = op_stroke)

stroke = rbind(ip_stroke,op_stroke )






stroke$date = as_date(stroke$date)

stroke = stroke %>% arrange(PatientSID, date)

stroke2 = stroke[!duplicated(stroke$PatientSID), ]

names(stroke2) = tolower(names(stroke2))

# now to get the scrssn for these patients 

d = df %>% dplyr::select(scrssn, surgdate)

glimpse(c2)

names(c2) = tolower(names(c2))

d2 = left_join(d, c2, by = "scrssn")

stroke3 = left_join(stroke2, d2, by = "patientsid")

# now to calculate the days between surgery and stroke.

stroke3$date = as_date(stroke3$date)

stroke3 = stroke3[stroke3$date < '2020-05-01', ]

stroke3$stroke_days = (stroke3$surgdate %--% stroke3$date)/ddays(1)

describe(stroke3$stroke_days)

# keep only those that have stroke after surgery.

stroke4 = stroke3[stroke3$stroke_days > 0, ]

stroke4 = stroke4 %>% arrange(scrssn, days)

stroke5 = stroke4[!duplicated(stroke4$scrssn), ]

stroke6 = stroke5 %>% dplyr::select(scrssn, stroke_days, stroke_event)

df2 = left_join(df, stroke6, by = "scrssn")

df2 %>% count(stroke_event)


df2$stroke_event[is.na(df2$stroke_event)]<- 0

# now to create the time for the composite event outcomes.

df2$comp_stroke = with(df2, ifelse(stroke_event == 1, 1,
                                   ifelse(died == 1, 2, 0)))


df2 %>% count(comp_stroke)

df2$stroke_days_comp = with(df2, ifelse(stroke_event == 1, stroke_days,
                                        fupdays))


df2$stroke_years_comp = (df2$stroke_days_comp + 1)/365.24

stroke_est = survfit(Surv(stroke_years_comp, comp_stroke, type = "mstate") ~ 1, 
                     data = df2 )

summary(stroke_est, times = c(0,1,3,5))


# Call: survfit(formula = Surv(stroke_years_comp, comp_stroke, type = "mstate") ~ 
#                 1, data = df2)
# 
# time n.risk n.event P((s0))   P(1)   P(2)
# 0  15586       0   1.000 0.0000 0.0000
# 1  14670     871   0.944 0.0168 0.0392
# 3  11090    1072   0.869 0.0394 0.0915
# 5   7276    1072   0.772 0.0724 0.1553

write_csv(df2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_stroke.csv')


# now to look at VTE for all patients.
# am going to start with a clean slate here again.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_stroke.csv')



ipadmit = read_sas('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\inpat_admissions_compass.sas7bdat')


vte9 = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vte_icd9sid.sas7bdat")

vte10 = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vte_icd10sid.sas7bdat")




c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

# now to get only my patients.

c$mine = with(c, ifelse(ScrSSN %in% df$scrssn, 1, 0))

c %>% count(mine)

c2 = c %>% dplyr::filter(mine == 1)

psid = c2$PatientSID; length(psid) # 46961

glimpse(ipadmit)

ipadmit$AdmitDateTime = as_date(ipadmit$AdmitDateTime)

ip = ipadmit[ipadmit$AdmitDateTime < '2020-05-01', ]

# limit to my patients

ip$keep = with(ip,  ifelse(PatientSID %in% psid, 1, 0))

ip2 = ip[ip$keep == 1, ]

ip2$vte9 = with(ip2, ifelse(ICD9SID %in% vte9$ICD9SID,1, 0))

ip2$vte10 = with(ip2, ifelse(ICD10SID %in% vte10$ICD10SID,1, 0))

ip2 %>% count(vte9); ip2 %>% count(vte10)

ip2$vte_inpat = with(ip2, ifelse((vte9 == 1|vte10 == 1), 1, 0))

ip2 %>% count(vte_inpat)


vte_ip = ip2[ip2$vte_inpat == 1, ]

vte_ip2 = vte_ip %>% dplyr::select(PatientSID, vte_inpat, AdmitDateTime)

vte_ip3 = left_join(vte_ip2, c2, by = "PatientSID")

vte_ip3 = vte_ip3 %>% arrange(ScrSSN, AdmitDateTime)

vte_ip4 = vte_ip3[!duplicated(vte_ip3$ScrSSN), ]

d = df %>% dplyr::select(scrssn, surgdate)

names(vte_ip4) = tolower(names(vte_ip4))

vte_ip5 = left_join(vte_ip4, d, by = "scrssn")

vte_ip5$vte_days = (vte_ip5$surgdate %--% vte_ip5$admitdatetime)/ddays(1)

describe(vte_ip5$vte_days)

vte_ip6 = vte_ip5[vte_ip5$vte_days > 0, ]

vte_ip7 = vte_ip6 %>% dplyr::select(scrssn, vte_days, vte_inpat)

df2 = left_join(df, vte_ip7, by = "scrssn")

# create composite outcome for CIF

df2$vte_inpat[is.na(df2$vte_inpat)]<- 0

df2$vte_comp_event = with(df2, ifelse(vte_inpat == 1, 1,
                                ifelse( died == 1, 2, 0)))

df2 %>% count(vte_comp_event)

df2$vte_days_comp = with(df2, ifelse(vte_inpat == 1, vte_days, fupdays))

df2$vte_years_comp = (df2$vte_days_comp + 1)/365.24


vte_est = survfit(Surv(vte_years_comp, vte_comp_event, type = "mstate") ~ 1, 
                     data = df2 )

summary(vte_est, times = c(0,1,3,5))



write_csv(df2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte.csv' )



# now from the df that I have, need to also get the CI for each event using ETM package
# ETM package is also good for CIF curves.