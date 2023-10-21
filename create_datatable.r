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
    
