#This to wrangle GOSH data from various large lists into a single data table

#get a list of all the PICU ward stays
source("PICU_list_create.r") #define extractor fuction
all_PICU_patients <- PICU_list_create("11868/ward_stays.csv")

source("create_patient_time_ID.r")
all_PICU_patients <- create_patient_time_ID(all_PICU_patients)

write.csv(all_PICU_patients, "all_PICU_patients.csv", row.names=TRUE)