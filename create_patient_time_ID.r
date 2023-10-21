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
