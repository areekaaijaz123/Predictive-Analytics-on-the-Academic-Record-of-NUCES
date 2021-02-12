library(dplyr)
library(ggplot2)

setwd("E:\\FYP\\FYP-1 (Part 2)\\Implementation")

# Read data
semester_data <- read.csv("E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Cleaned_Semester_Data.csv", 
                          stringsAsFactors = FALSE, 
                          na.strings=c("","NA"))

student_data <- read.csv("E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Cleaned_Student_Data.csv", 
                         na.strings=c("","NA"))


# Data Cleaning in Students Data File

student_data$SCHOOL <- toupper(student_data$SCHOOL)
student_data$COLLEGE <- toupper(student_data$COLLEGE)

student_data$SCHOOL[student_data$SCHOOL == "-"] <- NA
student_data$COLLEGE[student_data$COLLEGE == "-"] <- NA

student_data$SCHOOL[student_data$SCHOOL %in% c("NOT KNOWN", "NOT GIVEN", "OTHERS")] <- NA

student_data$SCHOOL[nchar(student_data$SCHOOL) < 5] <- NA
student_data$COLLEGE[nchar(student_data$COLLEGE) < 5] <- NA

student_data$SCHOOL <- as.character(student_data$SCHOOL)
student_data$COLLEGE <- as.character(student_data$COLLEGE)

student_data[is.na(student_data[,13]), 13] <- round(mean(student_data[,13], na.rm = TRUE), digits = 2)
student_data[is.na(student_data[,16]), 16] <- round(mean(student_data[,16], na.rm = TRUE), digits = 2)

# Data Cleaning in Semester Data File

semester_data <- semester_data %>% select(-18:-32)

for(i in c(2, 4, 6, 8, 10, 12, 14, 16)){
  semester_data[is.na(semester_data[,i]), i] <- round(mean(semester_data[,i] , na.rm = TRUE), digits = 2)
}

for (i in 1:nrow(semester_data)){
  
  if(is.na(semester_data$SEM_1_CGPA[i]))
    semester_data$SEM_1_CGPA[i] = round(semester_data$SEM_1_SGPA[i], 
                                        digits = 2)
  
  if(is.na(semester_data$SEM_2_CGPA[i]))
    semester_data$SEM_2_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i])/2, 
                                        digits = 2)
  
  if(is.na(semester_data$SEM_3_CGPA[i]))
    semester_data$SEM_3_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i])/3, 
                                        digits = 2)
  
  if(is.na(semester_data$SEM_4_CGPA[i]))
    semester_data$SEM_4_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i] +
                                           semester_data$SEM_4_SGPA[i])/4,
                                        digits = 2)
  
  if(is.na(semester_data$SEM_5_CGPA[i]))
    semester_data$SEM_5_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i] +
                                           semester_data$SEM_4_SGPA[i] +
                                           semester_data$SEM_5_SGPA[i])/5,
                                        digits = 2)  
  
  if(is.na(semester_data$SEM_6_CGPA[i]))
    semester_data$SEM_6_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i] +
                                           semester_data$SEM_4_SGPA[i] +
                                           semester_data$SEM_5_SGPA[i] +
                                           semester_data$SEM_6_SGPA[i])/6,
                                        digits = 2)  
  
  if(is.na(semester_data$SEM_7_CGPA[i]))
    semester_data$SEM_7_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i] +
                                           semester_data$SEM_4_SGPA[i] +
                                           semester_data$SEM_5_SGPA[i] +
                                           semester_data$SEM_6_SGPA[i] +
                                           semester_data$SEM_7_SGPA[i])/7,
                                        digits = 2)  
  
  if(is.na(semester_data$SEM_8_CGPA[i]))
    semester_data$SEM_8_CGPA[i] = round((semester_data$SEM_1_SGPA[i] + 
                                           semester_data$SEM_2_SGPA[i] +
                                           semester_data$SEM_3_SGPA[i] +
                                           semester_data$SEM_4_SGPA[i] +
                                           semester_data$SEM_5_SGPA[i] +
                                           semester_data$SEM_6_SGPA[i] +
                                           semester_data$SEM_7_SGPA[i] +
                                           semester_data$SEM_8_SGPA[i])/8,
                                        digits = 2)  
  
}

# Merging Semester data & student data

data <- merge(semester_data, student_data, by = "STUDENT_ID")

# Data Cleaning on complete data set

data <- data %>% filter(!grepl("AI", data$PROG_CODE))
data <- data %>% filter(!grepl("SE", data$PROG_CODE))
data <- data %>% filter(!grepl("DS", data$PROG_CODE))
data <- data %>% filter(!grepl("CSDF", data$PROG_CODE))

for (i in 1:nrow(data)){
 if (is.na(data$CGPA[i])) data$CGPA[i] <- data$SEM_8_CGPA[i] 
}

remove_row <- function(X, Cols) { select_ <- complete.cases(X[, Cols]); return(X[select_, ])}

data <- remove_row(data, c("SCHOOL", "COLLEGE"))

write.csv(data, "E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Dataset.csv", row.names = FALSE)
