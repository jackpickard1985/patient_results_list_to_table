# takes an input and converts the PR-0000000000001-type IDs into patient_time_IDs
# removes PR-
# splits admissions as 0.01, 0.02 etc for repeat admissions
# adds the arrival time hour
# addes columns for all variables

create_patient_time_ID <- function(patient_list) {

    counter <- 0

    patient_time_ID <- matrix(NA, nrow(patient_list), 1)
    
    for (y in 1:nrow(patient_time_ID)) {
    
        patient_time_ID[y,1] <- substr(patient_list[y,1], 4, 11)
    
    }
    
    patient_time_ID[1, 1] <- paste(patient_time_ID[1, 1], ".01", sep="")
    
    for (y in 2:nrow(patient_time_ID)) {
    
        if (substr(patient_list[y,1], 4, 11) == substr(patient_list[y-1,1], 4, 11)) {
        
            counter <- counter + 1}
        
        else {
        
            counter <- 0
        
        }
        
        if (counter < 10) {patient_time_ID[y, 1] <- paste(patient_time_ID[y, 1], ".0", counter+1, sep="")}
        else {patient_time_ID[y, 1] <- paste(patient_time_ID[y, 1], ".", counter+1, sep="")}
    
    }
    
    for (y in 1:nrow(patient_time_ID)) {

        patient_time_ID[y,1] <- paste(patient_time_ID[y, 1], substr(patient_list[y,2], 1, 16), sep=" ")
    
    }
    
    patient_list <- cbind(patient_time_ID, patient_list)
    
    variable_key <- read.delim("variable_key.csv", header=TRUE, sep=",", quote="", stringsAsFactors = FALSE)
    
    for (i in 1:nrow(variable_key)){
        new_column <- matrix(NA, nrow(patient_list), 1)
        colnames(new_column)[1] <- variable_key[i, "Variable"]
        patient_list <- cbind(patient_list, new_column)
    }

return (patient_list)

}
 
 
# converts lists of results into table as per instructions in variable_key.csv

variable_key <- read.delim("variable_key.csv", header=TRUE, sep=",", quote="", stringsAsFactors = FALSE)
file_locs <- unique(variable_key[,2], incomparables = FALSE, fromLast = FALSE, nmax = NA)

all_data <- read.delim("all_PICU_patients.csv", header=TRUE, sep=",", quote="", stringsAsFactors = FALSE)
all_data <- subset(all_data, select = -1)

for (loc in 1:length(file_locs)) {
   some_data <- read.delim(file_locs[loc], header=TRUE, sep=",", quote="", stringsAsFactors = FALSE) #cycles through all relevant files loading one at a time
   
   for (y in 1:nrow(some_data)) {
        for (x in 7:(ncol(some_data)-1)) {
            for (entry in 1:nrow(variable_key)) {
           
                #locate entry of interest
                if (some_data[y, x] == variable_key[entry, "Code"]) {
                    
                    #extract the figure (last column always)
                    
                    this_value <- some_data[y, ncol(some_data)]
                    this_variable <- variable_key[entry, "Variable"]
                    
                    #insert in correct location or create location in all_data (must be after admission and before discharge)
                    
                    entry_entered = FALSE
                    
                    for (n in 1:nrow(all_PICU_patients)) {
                        
                        if (all_PICU_patients["project_id"] == some_data["project_id"]) { #if right patients
                            if (substr(all_PICU_patients["patient_time_id"],13,28) == substr(some_data["start_datetime"]),1,16) #if right time
                                all_PICU_patients[n, this_variable] <- this_value #enter value in results table
                                entry_entered <- TRUE
                            }
                        }
                        
                    }
                    
                    if (entry_entered = FALSE) {
                        #check entry is within the time limits of that patient's admission
                        #copy the most recent row from all_PICU_patients to the next hour, insert entry there
                    }
                    
                }
            }
       }
   }
   
}    
    
 
# generate a list of all the patients who have had a PICU stay
# accesses 11846/ward_stays.csv for this
# note this effectively excludes covid times when the PICU patients went to Dolphin

PICU_list_create <- function(ward_stay_file) {

#load data
ward_stay_data <- read.delim(ward_stay_file, header=TRUE, sep=",", quote="", stringsAsFactors = FALSE)

#PICU_patients <- matrix(c(0, 0, 0), nrow=1, ncol=3, byrow = TRUE)
PICU_patients <- c()

#search through data for presence of PICU in the ward_code column
for (i in 1:nrow(ward_stay_data)) {
    if (ward_stay_data[i, "ward_code"] == "PICU") {
        PICU_patients <- rbind(PICU_patients, ward_stay_data[i, 2:4]) #formerly ward_stay_data[i, "project_id"]
    }
}

PICU_patients <- PICU_patients[order(PICU_patients[,"project_id"], decreasing = FALSE),]

return (PICU_patients)

}
 
#This to wrangle GOSH data from various large lists into a single data table

#get a list of all the PICU ward stays
source("PICU_list_create.r") #define extractor fuction
all_PICU_patients <- PICU_list_create("11868/ward_stays.csv")

source("create_patient_time_ID.r")
all_PICU_patients <- create_patient_time_ID(all_PICU_patients)

write.csv(all_PICU_patients, "all_PICU_patients.csv", row.names=TRUE) 
Variable,File,Code
Weight,11871/flowsheet_rows.csv,82447
HR,11871/flowsheet_rows.csv,67261
RR,11871/flowsheet_rows.csv,67466
MAP,11871/flowsheet_rows.csv,61924
SATs,11871/flowsheet_rows.csv,40721
Temperature,11871/flowsheet_rows.csv,74513
Elective/non-elective,11871/flowsheet_rows.csv,66304
Blood gas pH,11870/lab_components.csv,"pH POC"
Blood gas CO2,11870/lab_components.csv,"paCO2 POC"
Blood gas O2,11870/lab_components.csv,"paP2 POC"
Blood gas lactate,11870/lab_components.csv,"Lactate POC"
Blood gas sodium,11870/lab_components.csv,"Sodium POC"
Blood gas potassuim,11870/lab_components.csv,"Potassium POC"
Blood gas calcium,11870/lab_components.csv,"Calcium POC"
Boood gas chloride,11870/lab_components.csv,"Chloride POC"
Blood gas glucose,11870/lab_components.csv,"glucose POC"
