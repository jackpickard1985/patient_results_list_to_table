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