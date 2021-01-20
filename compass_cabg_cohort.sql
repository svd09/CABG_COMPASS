/* This is the SQL code for COMPASS-CABG paper.
Aim: 
1. Get Compass-like CABG cohort operated within VA in 10 years.
2. See differences in baseline between COMPASS study pop and our pop.
3. Seperate patients in to OPCAB and ONCAB.
4. Use the other COMPASS paper to calculate baseline H and 1 year event free outcome.
5. Look at survival @ 5 years for our data.
?6.See how we can plan for using the other paper to obtain predicted estimates for our cohort */

use ORD_Deo_202008039D

/* identify patients that had CABG from 2010 - 2020 in the VA database */

select *
into #cabg_compass
from Src.VASQIP_nso_cardiac
	where CPT01 in ('33521','33522','33523','33517','33518','33519',
'33510','33511','33512','33513','33514','33515','33516','33533','33534','33535','33536') 
and SURGDATE > '2010-01-01 00:00:00'

select *
into Dflt.cabg_compass
from #cabg_compass

/* look at the lvef text file */

select top 10*
from Src.VINCI_TIU_NLP_LVEF

-- am going to save the whole lvef into my folder.

select *
into Dflt.lvef_tiu
from Src.VINCI_TIU_NLP_LVEF

select a.PatientSID, a.ScrSSN
into Dflt.compass_crosswalk
from Src.CohortCrosswalk a
inner join #cabg_compass b
on a.ScrSSN = b.scrssn

-- 2020-11-06

/* 

1. we need to get outpat refills for our cohort to identify those patients 
that we on DAPT or OAC prior to surgery 
2. we need to identify those patients that are at high risk for bleeding. */

use ORD_Deo_202008039D

select top 10*
from Src.RxOut_RxOutpatFill


select *
into #aspirinfill#
from Src.RxOut_RxOutpatFill
where DrugNameWithoutDose like '%aspirin%' and FillDateTime > '2009-01-01 00:00:00'

select *
into #clopidogrelfill
from Src.RxOut_RxOutpatFill
where DrugNameWithoutDose like '%clopidogrel%' and FillDateTime > '2009-01-01 00:00:00'

select *
into #prasugrelfill
from Src.RxOut_RxOutpatFill
where DrugNameWithoutDose like '%prasugrel%' and FillDateTime > '2009-01-01 00:00:00'

select *
into #ticagrelorfill
from Src.RxOut_RxOutpatFill
where DrugNameWithoutDose like '%ticagrelor%' and FillDateTime > '2009-01-01 00:00:00'

select *
into Dflt.prasugrelfill_c
from #prasugrelfill

select *
into Dflt.clopidogrelfill_c
from #clopidogrelfill

select *
into Dflt.ticagrelorfill_c
from #ticagrelorfill

select a.LocalDrugSID, a.LocalDrugNameWithDose
into Dflt.warfarin_sid
from CDWWork.Dim.LocalDrug a
where LocalDrugNameWithDose like '%warfarin%'

select a.FillDateTime, a.LocalDrugSID,a.PatientSID, a.RxStatus
into #warfarinfill_c 
from Src.RxOut_RxOutpatFill a
inner join Dflt.warfarin_sid b
on a.LocalDrugSID = b.LocalDrugSID

select *
into Dflt.warfarinfill_c
from #warfarinfill_c


-- now to define high bleeding risk.
-- define as patients admitted with a primary diagnosis if either GI bleed/ICH within 1 year prior to surgery.

select top 10*
from Src.Inpat_InpatDischargeDiagnosis

-- define the icd9 and icd10 codes for ICH first as they will be less.

select a.ICD9Code, a.ICD9SID
into Dflt.ich_9sid
from CDWWork.Dim.ICD9 a
where ICD9Code like ('%430%') or 
	ICD9Code like ('%431%') or 
	ICD9Code in ('432.1','432.9')

	select a.ICD10Code, a.ICD10SID
into Dflt.ich_10sid
from CDWWork.Dim.ICD10 a
where ICD10Code like ('%I61%') or
	ICD10Code like ('%I62%') or 
	ICD10Code in ('I62.1','I62.9')

select *
into Dflt.ich_sid
from Dflt.ich_10sid
	union  
select *
from Dflt.ich_9sid

select top 10*
from Dflt.ich_sid

-- GI bleed SID


select a.ICD9Code, a.ICD9SID
into Dflt.gib_9sid
from CDWWork.Dim.ICD9 a
where ICD9Code in ('531.0','531.2','531.6','532.0','532.2','532.4','532.6',
'533.0','533.2','533.4','533.6','534.0','534.2','534.4','534.6','578.0','578.1','578.9',
'569.3','786.3')


select a.ICD10Code, a.ICD10SID
into Dflt.gib_10sid
from CDWWork.Dim.ICD10 a
where ICD10Code in ('K92.0','I85.0','I98.20','I98.3','K22.10','K22.12','K22.14','K22.16',
'K25.0','K25.2','K25.4','K25.6','K26.0','K26.2','K26.4','K26.6','K27.0','K27.2','K27.4','K27.6',
'K28.0','K28.2','K28.4','K28.6','K29.0','K63.80','K31.80','K55.20','K62.25','K92.2')

select top 10*
from Src.Inpat_InpatDischargeDiagnosis

-- ich or gib using icd9 codes 

select a.PatientSID, a.AdmitDateTime, a.ICD9SID
into Dflt.gib_icd9
from Src.Inpat_InpatDischargeDiagnosis a
inner join Dflt.gib_9sid b
on a.ICD9SID = b.ICD9SID 

select a.PatientSID, a.AdmitDateTime, a.ICD9SID
into Dflt.ich_icd9
from Src.Inpat_InpatDischargeDiagnosis a
inner join Dflt.ich_9sid b
on a.ICD9SID = b.ICD9SID 


select a.PatientSID, a.AdmitDateTime, a.ICD10SID
into Dflt.gib_icd10
from Src.Inpat_InpatDischargeDiagnosis a
inner join Dflt.gib_10sid b
on a.ICD10SID = b.ICD10SID 

select a.PatientSID, a.AdmitDateTime, a.ICD10SID
into Dflt.ich_icd10
from Src.Inpat_InpatDischargeDiagnosis a
inner join Dflt.ich_10sid b
on a.ICD10SID = b.ICD10SID 

-- get the vital status as Dflt

select top 10*
from Src.VitalStatus_Mini

select a.SCRSSN, a.ACT_LAST_DT, a.LIVING_IND
into Dflt.vitalstatus_compass
from Src.VitalStatus_Mini a

