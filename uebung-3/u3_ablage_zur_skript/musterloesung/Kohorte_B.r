# R Skript Kohorte B

# männliche Covid-Patienten im Alter 30 - 60

install.packages('fhircrackr')

library(fhircrackr)
library(tidyverse)
library(ggplot2)

search_request <- paste0(
  'https://mii-agiop-cord.life.uni-leipzig.de/fhir/',
  'Condition?',
  'code=J12.8%20U07.1%21,U08.9%20U09.9%21',
  #',M30.3', #choose, if you want to search for kawasaki
  '&_include=Condition:subject'
)

# define design
design <- list(
  Conditions = list(
    resource = "//Condition",
    cols = list(
      condition_id = "id",
      code = "code/coding/code",
      display = "code/coding/display",
      system = "code/coding/system",
      patient_id = "subject/reference",
      encounter_id = "encounter/reference",
      recorded_date = "recordedDate"
    ),
    style = list(
      sep="|",
      brackets = c("[", "]"),
      rm_empty_cols = FALSE
    )
  ),
  Patients = list(
    resource = "//Patient",
    cols = list(
      patient_id = "identifier/value",
      name_use = "name/use",
      name_family = "name/family",
      name_given = "name/given",
      gender = "gender",
      birthdate = "birthDate"
    ),
    style = list(
      sep="|",
      brackets = c("[", "]"),
      rm_empty_cols = FALSE
    )
  ) 
)

# download fhir bundles
bundles <- fhir_search(request = search_request, max_bundles = 50)

# crack fhir bundles
dfs <- fhir_crack(bundles, design)

# save raw patients dataframe
covid_patients_raw <- dfs$Patients

# unnest raw patients dataframe columns name/use, name/family
covid_patients_tmp <- fhir_melt(covid_patients_raw,
                                columns = c('name_use','name_family'),
                                brackets = c('[',']'), sep = '|', all_columns = TRUE,)

# remove brackets from cells
covid_patients_tmp <- fhir_rm_indices(covid_patients_tmp, brackets = c("[", "]") )

# filter patients by name_use = official
covid_patients_tmp <- covid_patients_tmp[covid_patients_tmp$name_use == 'official',]

# filter patients by gender = male
covid_patients_tmp <- covid_patients_tmp[covid_patients_tmp$gender == 'male',]

# calculate age for each row
covid_patients_tmp$age <- round( as.double( as.Date( Sys.time() ) - as.Date( covid_patients_tmp$birthdate ) ) / 365.25, 0 )

# split into specific age groups
x <- c(1,29,60,999)
covid_patients_tmp$age_group <- cut(covid_patients_tmp$age,x,breaks= c(0,29,60,999), labels = c("[1,29]","[30,60]","[61,999]"))

# filter patients by age >= 30 & < 60
covid_patients_tmp <- covid_patients_tmp[covid_patients_tmp$age >= 30 & covid_patients_tmp$age <= 60,]

# remove duplicate patients
covid_patients <- covid_patients_tmp[!duplicated(covid_patients_tmp$patient_id),]

# reorder columns
covid_patients <- covid_patients[,c('patient_id','name_family','name_given','gender','age','age_group')]

count(covid_patients)

# save raw conditions dataframe
covid_conditions_raw <- dfs$Conditions

# unnest raw conditions dataframe columns code/coding/code, code/coding/display, code/coding/system
covid_conditions_tmp <- fhir_melt(covid_conditions_raw,
                                  columns = c('code','display','system'),
                                  brackets = c('[',']'), sep = '|', all_columns = TRUE,)
covid_conditions_tmp <- fhir_melt(covid_conditions_tmp,
                                  columns = c('code','display','system'),
                                  brackets = c('[',']'), sep = '|', all_columns = TRUE,)

# remove brackets from cells
covid_conditions_tmp <- fhir_rm_indices(covid_conditions_tmp, brackets = c("[", "]") )

# filter conditions by system = icd-10-gm
covid_conditions_tmp <- covid_conditions_tmp[covid_conditions_tmp$system == 'http://fhir.de/CodeSystem/dimdi/icd-10-gm',]

# remove duplicate patients
covid_conditions <- covid_conditions_tmp[!duplicated(covid_conditions_tmp$patient_id),]

# remove Patient/ from subject/reference and Encounter from encounter/reference
covid_conditions$patient_id <- sub("Patient/", "", covid_conditions[,5])
covid_conditions$encounter_id <- sub("Encounter/", "", covid_conditions[,6])

# separate patient_id into Airolo, Bapu, Cynthia institution_id
covid_conditions$institution_id <- unlist(strsplit(covid_conditions$patient_id,'-P-'))[ c(TRUE,FALSE) ]

# split code column in pri and sec code
covid_conditions$pri_code <- ifelse(nchar(covid_conditions$code)>6,sapply(strsplit(covid_conditions$code,' '), function(x) x[1]),covid_conditions$code)
covid_conditions$sec_code <- ifelse(nchar(covid_conditions$code)>6,sapply(strsplit(covid_conditions$code,' '), function(x) x[2]),'-')

# if necessary or wanted, filter by Airolo, Bapu, Cynthia, default: all data
#covid_conditions <- covid_conditions[grep('Airolo', covid_conditions$institution_id ),]
#covid_conditions <- covid_conditions[grep('Bapu', covid_conditions$institution_id ),]
#covid_conditions <- covid_conditions[grep('Cynthia', covid_conditions$institution_id ),]

# merge patients and conditions dataframes
df_merged <- merge(covid_patients, covid_conditions, by = "patient_id")

# create summarized data frame
df_final <- as.data.frame(df_merged%>%group_by(Einrichtungsindikator=df_merged$institution_id,AngabeDiagn1=df_merged$pri_code,AngabeDiagn2=df_merged$sec_code,AngabeGeschlecht=df_merged$gender,AngabeAlter=df_merged$age_group)%>%summarise(count=n()))
names(df_final)[names(df_final)== "count"] <- "Anzahl"

# display the final output
df_final

# optional, save to file
###############################################
#write.csv(df_merged,file= "df_merged.csv")
#write.csv(df_final,file= "df_final.csv")
###############################################