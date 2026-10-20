require(dplyr)
require(icd.data)
# Step 1: Indications on dispensations
dtaIndication <- dtaMedADHD %>% select(id,iid,atc,refICD10, refICPC2, packages_date, Substance, i) %>%
  #Transform missing to NA
  mutate(refICD10=case_when(refICD10==''~NA, .default=refICD10), refICPC2=case_when(refICPC2==''~NA,.default=refICPC2)) %>%
  # Put indication in one column
  mutate(indication=coalesce(refICD10,refICPC2), system=case_when(is.na(refICD10)~'ICPC', .default='ICD')) %>% distinct()

# Remove some indications that are process codes
dtaIndication <- dtaIndication %>% mutate(indication=case_when(
  grepl('^-*\\d+$', indication)~NA,
  grepl('^-F',indication)~NA,
  indication==''~NA,
  .default=indication
))

# Indicate system and merge to one indicator
dtaIndication <- dtaIndication %>% mutate(indicationSource=case_when(!is.na(indication)~'LMR',.default=NA))

# 2. Take the indication from an individuals other dispensation fi they are all ADHD
dtaIndication <- dtaIndication %>% arrange(iid, packages_date)
dtaIndication <- dtaIndication %>% mutate(indication=case_when(is.na(indication) & sum(indication %in% c('P81','F90')) == n()~'F90', .default=indication), .by=iid)
dtaIndication <- dtaIndication %>% mutate(indicationSource=case_when(!is.na(indication) & is.na(indicationSource)~'LMR dispensations', .default=indicationSource),
                                          system=case_when(indicationSource=='LMR dispensations'~'ICD', .default=system))
dtaIndicationMissing <- dtaIndication %>% filter(is.na(indication))
dtaIndication <- dtaIndication %>% filter(!is.na(indication))

# 3. Source indications from NPR if not available on dispensation
# Remove missing
dtaNPR <- dbGetQuery(dbCon, 'SELECT distinct iid, date_in_diagnosis, diagnosis_primary FROM npr') %>%
  inner_join(dtaIndicationMissing %>% select(iid) %>% distinct(), join_by(iid))
dtaNPR <- dtaIndicationMissing %>% left_join(dtaNPR, join_by(iid), relationship='many-to-many')
# Filter out events occuring more than 30 days prior
dtaNPR <- dtaNPR %>% mutate(dateRel=packages_date - date_in) %>% filter(dateRel >= -30 & dateRel <= 0)
# Create a frequency table
tbNPRIndications <- dtaNPR %>% mutate(diagnosis=substr(diagnosis,1,3)) %>% select(iid,dateRel, diagnosis) %>% distinct() %>%
  summarise(n=n(), .by=c(dateRel,diagnosis))
tbNPRIndications <- tbNPRIndications %>% arrange(desc(dateRel), desc(n)) %>% rqcr::indexObservations(dateReel) %>% rqcr::percent(dateRel, .colCount='n')
tbNPRIndications <- merge(tbNPRIndications, icd10cm2016[,c('code','short_desc')], by.x='diagnosis', by.y='code',all.x=T)

# Collect individuals with (1) ADHD (2) narcolepsy (3) eating disorders (4) MS
for (diag in c('F90','G474','F50','G36')) {
  message(diag)
  tmpNPR <- dtaNPR %>% group_by(id) %>% mutate(indication=case_when(startsWith(as.character(diagnosis), diag)~diag, .default=NA)) %>%
    filter(startsWith(indication, diag)) %>% ungroup() %>% select(!c(date_in,diagnosis,diagnosis_primary,dateRel)) %>% distinct()
  if (rqcr::countUnique(tmpNPR, id) != nrow(tmpNPR)) stop('Not one indication per dispensation')
  dtaIndication <- dtaIndication %>% bind_rows(tmpNPR %>% mutate(system='ICD', indicationSource='NPR'))
  # Remove indicated dispensations
  dtaNPR <- dtaNPR %>% filter(!id %in% tmpNPR$id)
  dtaIndicationMissing <- dtaIndicationMissing %>% filter(!id %in% tmpNPR$id)
}

# 4. Source indications from KUHR if still not available
dtaPC <- dbGetQuery(dbCon, "SELECT distinct iid,date,diagnosis,diagnosis_type FROM kuhr") %>%
  inner_join(dtaIndicationMissing %>% select(iid) %>% distinct(), join_by(iid))
dtaPC <- dtaIndicationMissing %>% left_join(dtaPC, join_by(iid), relationship='many-to-many')
dtaPC <- dtaPC %>% mutate(dateREl=packages_date - date) %>% filter(dateRel >= -30 & dateRel <= 0)
# Create a frequency table
tbPCIndications <- dtaPC %>% mutate(diagnosis=substr(diagnosis,1,3)) %>% select(iid,dateRel,diagnosis) %>% distinct() %>%
  summarise(n=n(), .by=c(dateRel, diagnosis))
tbPCIndications <- tbPCIndications %>% arrange(desc(dateRel), desc(n)) %>% rqcr::indexObservations(dateRel) %>% rqcr::percent(dateRel, .colCount='n')
# Collect individuals with (1) ADHD (2) Narcolepsy, (3) eating disorders, (4) MS
# ICD system
for (diag in c('F90','G474', 'F50','G36')) {
  message(diag)
  tmpPC <- dtaPC %>% group_by(id) %>% mutate(indication=case_when(startsWith(as.character(diagnosis), diag) & diagnosis_type=='ICD-10'~diag,.default=NA)) %>%
    filter(startsWith(indication, diag)) %>% ungroup() %>% select(!c(date_in, diagnosis,dateRel)) %>% distinct()
  if (rqcr::countUnique(tmpPC, id) != nrow(tmpPC)) stop('Not one indication per dispensation')
  dtaIndication <- dtaIndication %>% bind_rows(tmpPC %>% mutate(system='ICD', indicationSource='KUHR'))
  # Remove indicated dispensations
  dtaPC <- dtaPC %>% filter(!id %in% tmpPC$id)
  dtaIndicationMissing <- dtaIndicationMissing %>% filter(!id %in% tmpPC$id)
}

# ICPC system (only ADHD and eating disorders)
for (diag in c('P81','P86')) {
  message(idag)
  tmpPC <- dtaPC %>% group_by(id) %>% mutate(indication=case_when(startswith(as.character(diagnosis), diag) & diagnosis_type!='ICD-10'), .default=NA) %>%
    filter(startswith(indication, diag)) %>% ungroup() %>% mutate(diagnosis_type='ICPC-2') %>% select(!c(date,diagnosis,dateRel)) %>% distinct()
  if (rqcr::countUnique(tmpPC, id) != nrow(tmpPC)) stop('Not one indication per dispensation')
  dtaIndication <- dtaPC %>% filter(!id %in% tmpPC$id)
  dtaIndicationMissing <- dtaIndicationMissing %>% filter(!id %in% tmpPC$id)
}




