library(dplyr)
library(ggplot2)

setwd("E:\\FYP\\FYP-1 (Part 2)\\Implementation")

# Read data
semester_data <- read.csv("..\\..\\Datasets\\Cleaned_Semester_Data.csv", 
                          stringsAsFactors = FALSE, 
                          na.strings=c("","NA"))

student_data <- read.csv("..\\..\\Datasets\\Cleaned_Student_Data.csv", 
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

# Merging Semester data & student data

training_data <- merge(semester_data, student_data, by = "STUDENT_ID")

training_data <- training_data %>% filter(GENDER != "-")
training_data$WARNING[is.na(training_data$WARNING)] <- 0

training_data <- training_data %>% filter(!grepl("AI", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("SE", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("DS", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("CSDF", training_data$PROG_CODE))

training_data$SECONDARY <- as.character(training_data$SECONDARY)
training_data$HIGHER_SECONDARY <- as.character(training_data$HIGHER_SECONDARY)

training_data[is.na(training_data[, 42]), 42] <- "OTHER"
training_data[is.na(training_data[, 45]), 45] <- "OTHER"

training_data$SECONDARY <- as.factor(training_data$SECONDARY)
training_data$HIGHER_SECONDARY <- as.factor(training_data$HIGHER_SECONDARY)

testing_data <- training_data %>% filter(STATUS == "Current") %>% filter(WARNING == 0)
warning_train_data <- training_data %>% filter(WARNING > 0) %>% filter(STATUS == "Completed")
warning_test_data <- training_data %>% filter(WARNING > 0) %>% filter(STATUS == "Current")
training_data <- training_data %>% filter(STATUS == "Completed") %>% filter(WARNING == 0)

for(i in c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)){
  for (j in 1:nrow(training_data)){
    if (training_data[j, 32] >= i/2 && is.na(training_data[j, i])){
      training_data[j, i] = round(mean(training_data[, i], na.rm = TRUE), digits = 2)
    }
  }
}

training_data[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)][is.na(training_data[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)])] <- 0

for (i in 1:nrow(training_data)){
  
  if(is.na(training_data$SEM_1_CGPA[i]) && training_data[i, 32] >= 1)
    training_data$SEM_1_CGPA[i] = round(training_data$SEM_1_SGPA[i], 
                                        digits = 2)
  
  if(is.na(training_data$SEM_2_CGPA[i]) && training_data[i, 32] >= 2)
    training_data$SEM_2_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i])/2, 
                                        digits = 2)
  
  if(is.na(training_data$SEM_3_CGPA[i]) && training_data[i, 32] >= 3)
    training_data$SEM_3_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i])/3, 
                                        digits = 2)
  
  if(is.na(training_data$SEM_4_CGPA[i]) && training_data[i, 32] >= 4)
    training_data$SEM_4_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i])/4,
                                        digits = 2)
  
  if(is.na(training_data$SEM_5_CGPA[i]) && training_data[i, 32] >= 5)
    training_data$SEM_5_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i] +
                                           training_data$SEM_5_SGPA[i])/5,
                                        digits = 2)  
  
  if(is.na(training_data$SEM_6_CGPA[i]) && training_data[i, 32] >= 6)
    training_data$SEM_6_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i] +
                                           training_data$SEM_5_SGPA[i] +
                                           training_data$SEM_6_SGPA[i])/6,
                                        digits = 2)  
  
  if(is.na(training_data$SEM_7_CGPA[i]) && training_data[i, 32] >= 7)
    training_data$SEM_7_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i] +
                                           training_data$SEM_5_SGPA[i] +
                                           training_data$SEM_6_SGPA[i] +
                                           training_data$SEM_7_SGPA[i])/7,
                                        digits = 2)  
  
  if(is.na(training_data$SEM_8_CGPA[i]) && training_data[i, 32] >= 8)
    training_data$SEM_8_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i] +
                                           training_data$SEM_5_SGPA[i] +
                                           training_data$SEM_6_SGPA[i] +
                                           training_data$SEM_7_SGPA[i] +
                                           training_data$SEM_8_SGPA[i])/8,
                                        digits = 2)  
  
  if(is.na(training_data$SEM_9_CGPA[i]) && training_data[i, 32] >= 9)
    training_data$SEM_9_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                           training_data$SEM_2_SGPA[i] +
                                           training_data$SEM_3_SGPA[i] +
                                           training_data$SEM_4_SGPA[i] +
                                           training_data$SEM_5_SGPA[i] +
                                           training_data$SEM_6_SGPA[i] +
                                           training_data$SEM_7_SGPA[i] +
                                           training_data$SEM_8_SGPA[i] +
                                           training_data$SEM_9_SGPA[i])/9,
                                        digits = 2)  
  
  if(is.na(training_data$SEM_10_CGPA[i]) && training_data[i, 32] >= 10)
    training_data$SEM_10_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i])/10,
                                         digits = 2) 
  
  if(is.na(training_data$SEM_11_CGPA[i]) && training_data[i, 32] >= 11)
    training_data$SEM_11_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i] +
                                            training_data$SEM_11_SGPA[i])/11,
                                         digits = 2) 
  
  if(is.na(training_data$SEM_12_CGPA[i]) && training_data[i, 32] >= 12)
    training_data$SEM_12_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i] +
                                            training_data$SEM_11_SGPA[i] +
                                            training_data$SEM_12_SGPA[i])/12,
                                         digits = 2) 
  
  if(is.na(training_data$SEM_13_CGPA[i]) && training_data[i, 32] >= 13)
    training_data$SEM_11_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i] +
                                            training_data$SEM_11_SGPA[i] +
                                            training_data$SEM_12_SGPA[i] +
                                            training_data$SEM_13_SGPA[i])/13,
                                         digits = 2) 
  
  if(is.na(training_data$SEM_14_CGPA[i]) && training_data[i, 32] >= 14)
    training_data$SEM_14_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i] +
                                            training_data$SEM_11_SGPA[i] +
                                            training_data$SEM_12_SGPA[i] +
                                            training_data$SEM_13_SGPA[i] +
                                            training_data$SEM_14_SGPA[i])/14,
                                         digits = 2) 
  
  if(is.na(training_data$SEM_15_CGPA[i]) && training_data[i, 32] >= 15)
    training_data$SEM_15_CGPA[i] = round((training_data$SEM_1_SGPA[i] + 
                                            training_data$SEM_2_SGPA[i] +
                                            training_data$SEM_3_SGPA[i] +
                                            training_data$SEM_4_SGPA[i] +
                                            training_data$SEM_5_SGPA[i] +
                                            training_data$SEM_6_SGPA[i] +
                                            training_data$SEM_7_SGPA[i] +
                                            training_data$SEM_8_SGPA[i] +
                                            training_data$SEM_9_SGPA[i] +
                                            training_data$SEM_10_SGPA[i] +
                                            training_data$SEM_11_SGPA[i] +
                                            training_data$SEM_12_SGPA[i] +
                                            training_data$SEM_13_SGPA[i] +
                                            training_data$SEM_14_SGPA[i] +
                                            training_data$SEM_15_SGPA[i])/15,
                                         digits = 2)
  
  
}

training_data <- training_data %>% filter(TOTAL_SEM > 7 & TOTAL_SEM < 16)

List <- c("SEM_1_SGPA", "SEM_2_SGPA", "SEM_3_SGPA", "SEM_4_SGPA", "SEM_5_SGPA", "SEM_6_SGPA", "SEM_7_SGPA", 
          "SEM_8_SGPA", "SEM_9_SGPA", "SEM_10_SGPA", "SEM_11_SGPA", "SEM_12_SGPA", "SEM_13_SGPA", "SEM_14_SGPA", 
          "SEM_15_SGPA", "WARNING", "STATUS")

training_data <- training_data[, !(names(training_data) %in% List)]

training_data[c(10, 11, 12, 13, 14, 15, 16)][is.na(training_data[c(10, 11, 12, 13, 14, 15, 16)])] <- 0


for(i in c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)){
  for (j in 1:nrow(warning_train_data)){
    if (warning_train_data[j, 32] >= i/2 && is.na(warning_train_data[j, i])){
      warning_train_data[j, i] = round(mean(warning_train_data[, i], na.rm = TRUE), digits = 2)
    }
  }
}

warning_train_data[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)][is.na(warning_train_data[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)])] <- 0

for (i in 1:nrow(warning_train_data)){
  
  if(is.na(warning_train_data$SEM_1_CGPA[i]) && warning_train_data[i, 32] >= 1)
    warning_train_data$SEM_1_CGPA[i] = round(warning_train_data$SEM_1_SGPA[i], 
                                             digits = 2)
  
  if(is.na(warning_train_data$SEM_2_CGPA[i]) && warning_train_data[i, 32] >= 2)
    warning_train_data$SEM_2_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i])/2, 
                                             digits = 2)
  
  if(is.na(warning_train_data$SEM_3_CGPA[i]) && warning_train_data[i, 32] >= 3)
    warning_train_data$SEM_3_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i])/3, 
                                             digits = 2)
  
  if(is.na(warning_train_data$SEM_4_CGPA[i]) && warning_train_data[i, 32] >= 4)
    warning_train_data$SEM_4_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i])/4,
                                             digits = 2)
  
  if(is.na(warning_train_data$SEM_5_CGPA[i]) && warning_train_data[i, 32] >= 5)
    warning_train_data$SEM_5_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i] +
                                                warning_train_data$SEM_5_SGPA[i])/5,
                                             digits = 2)  
  
  if(is.na(warning_train_data$SEM_6_CGPA[i]) && warning_train_data[i, 32] >= 6)
    warning_train_data$SEM_6_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i] +
                                                warning_train_data$SEM_5_SGPA[i] +
                                                warning_train_data$SEM_6_SGPA[i])/6,
                                             digits = 2)  
  
  if(is.na(warning_train_data$SEM_7_CGPA[i]) && warning_train_data[i, 32] >= 7)
    warning_train_data$SEM_7_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i] +
                                                warning_train_data$SEM_5_SGPA[i] +
                                                warning_train_data$SEM_6_SGPA[i] +
                                                warning_train_data$SEM_7_SGPA[i])/7,
                                             digits = 2)  
  
  if(is.na(warning_train_data$SEM_8_CGPA[i]) && warning_train_data[i, 32] >= 8)
    warning_train_data$SEM_8_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i] +
                                                warning_train_data$SEM_5_SGPA[i] +
                                                warning_train_data$SEM_6_SGPA[i] +
                                                warning_train_data$SEM_7_SGPA[i] +
                                                warning_train_data$SEM_8_SGPA[i])/8,
                                             digits = 2)  
  
  if(is.na(warning_train_data$SEM_9_CGPA[i]) && warning_train_data[i, 32] >= 9)
    warning_train_data$SEM_9_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                warning_train_data$SEM_2_SGPA[i] +
                                                warning_train_data$SEM_3_SGPA[i] +
                                                warning_train_data$SEM_4_SGPA[i] +
                                                warning_train_data$SEM_5_SGPA[i] +
                                                warning_train_data$SEM_6_SGPA[i] +
                                                warning_train_data$SEM_7_SGPA[i] +
                                                warning_train_data$SEM_8_SGPA[i] +
                                                warning_train_data$SEM_9_SGPA[i])/9,
                                             digits = 2)  
  
  if(is.na(warning_train_data$SEM_10_CGPA[i]) && warning_train_data[i, 32] >= 10)
    warning_train_data$SEM_10_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i])/10,
                                              digits = 2) 
  
  if(is.na(warning_train_data$SEM_11_CGPA[i]) && warning_train_data[i, 32] >= 11)
    warning_train_data$SEM_11_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i] +
                                                 warning_train_data$SEM_11_SGPA[i])/11,
                                              digits = 2) 
  
  if(is.na(warning_train_data$SEM_12_CGPA[i]) && warning_train_data[i, 32] >= 12)
    warning_train_data$SEM_12_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i] +
                                                 warning_train_data$SEM_11_SGPA[i] +
                                                 warning_train_data$SEM_12_SGPA[i])/12,
                                              digits = 2) 
  
  if(is.na(warning_train_data$SEM_13_CGPA[i]) && warning_train_data[i, 32] >= 13)
    warning_train_data$SEM_11_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i] +
                                                 warning_train_data$SEM_11_SGPA[i] +
                                                 warning_train_data$SEM_12_SGPA[i] +
                                                 warning_train_data$SEM_13_SGPA[i])/13,
                                              digits = 2) 
  
  if(is.na(warning_train_data$SEM_14_CGPA[i]) && warning_train_data[i, 32] >= 14)
    warning_train_data$SEM_14_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i] +
                                                 warning_train_data$SEM_11_SGPA[i] +
                                                 warning_train_data$SEM_12_SGPA[i] +
                                                 warning_train_data$SEM_13_SGPA[i] +
                                                 warning_train_data$SEM_14_SGPA[i])/14,
                                              digits = 2) 
  
  if(is.na(warning_train_data$SEM_15_CGPA[i]) && warning_train_data[i, 32] >= 15)
    warning_train_data$SEM_15_CGPA[i] = round((warning_train_data$SEM_1_SGPA[i] + 
                                                 warning_train_data$SEM_2_SGPA[i] +
                                                 warning_train_data$SEM_3_SGPA[i] +
                                                 warning_train_data$SEM_4_SGPA[i] +
                                                 warning_train_data$SEM_5_SGPA[i] +
                                                 warning_train_data$SEM_6_SGPA[i] +
                                                 warning_train_data$SEM_7_SGPA[i] +
                                                 warning_train_data$SEM_8_SGPA[i] +
                                                 warning_train_data$SEM_9_SGPA[i] +
                                                 warning_train_data$SEM_10_SGPA[i] +
                                                 warning_train_data$SEM_11_SGPA[i] +
                                                 warning_train_data$SEM_12_SGPA[i] +
                                                 warning_train_data$SEM_13_SGPA[i] +
                                                 warning_train_data$SEM_14_SGPA[i] +
                                                 warning_train_data$SEM_15_SGPA[i])/15,
                                              digits = 2)
  
}

warning_train_data <- warning_train_data %>% filter(TOTAL_SEM > 7 & TOTAL_SEM < 16)

warning_train_data <- warning_train_data[, !(names(warning_train_data) %in% List)]

warning_train_data[c(10, 11, 12, 13, 14, 15, 16)][is.na(warning_train_data[c(10, 11, 12, 13, 14, 15, 16)])] <- 0

testing_data <- testing_data %>% filter(TOTAL_SEM < 15)

for (i in 1:nrow(testing_data)){
  j <- testing_data$TOTAL_SEM[i] * 2
  if(is.na(testing_data[i, j]) & !is.na(testing_data[i, j+1])){
    testing_data$TOTAL_SEM[i] <- testing_data$TOTAL_SEM[i] - 1
    testing_data[i, j+1] <- NA
  }
}

testing_data <- testing_data[, !(names(testing_data) %in% List)]

warning_test_data <- warning_test_data %>% filter(TOTAL_SEM < 15)

for (i in 1:nrow(warning_test_data)){
  j <- warning_test_data$TOTAL_SEM[i] * 2
  if(is.na(warning_test_data[i, j]) & !is.na(warning_test_data[i, j+1])){
    warning_test_data$TOTAL_SEM[i] <- warning_test_data$TOTAL_SEM[i] - 1
    warning_test_data[i, j+1] <- NA
  }
}

warning_test_data <- warning_test_data[, !(names(warning_test_data) %in% List)]

training_data <- training_data %>% filter(!grepl("AI", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("SE", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("DS", training_data$PROG_CODE))
training_data <- training_data %>% filter(!grepl("CSDF", training_data$PROG_CODE))

testing_data <- testing_data %>% filter(!grepl("AI", testing_data$PROG_CODE))
testing_data <- testing_data %>% filter(!grepl("SE", testing_data$PROG_CODE))
testing_data <- testing_data %>% filter(!grepl("DS", testing_data$PROG_CODE))
testing_data <- testing_data %>% filter(!grepl("CSDF", testing_data$PROG_CODE))

warning_train_data <- warning_train_data %>% filter(!grepl("AI", warning_train_data$PROG_CODE))
warning_train_data <- warning_train_data %>% filter(!grepl("SE", warning_train_data$PROG_CODE))
warning_train_data <- warning_train_data %>% filter(!grepl("DS", warning_train_data$PROG_CODE))
warning_train_data <- warning_train_data %>% filter(!grepl("CSDF", warning_train_data$PROG_CODE))

warning_test_data <- warning_test_data %>% filter(!grepl("AI", warning_test_data$PROG_CODE))
warning_test_data <- warning_test_data %>% filter(!grepl("SE", warning_test_data$PROG_CODE))
warning_test_data <- warning_test_data %>% filter(!grepl("DS", warning_test_data$PROG_CODE))
warning_test_data <- warning_test_data %>% filter(!grepl("CSDF", warning_test_data$PROG_CODE))

write.csv(training_data, "..\\..\\Datasets\\Training_Data.csv", row.names = FALSE)
write.csv(testing_data, "..\\..\\Datasets\\Testing_Data.csv", row.names = FALSE)
write.csv(warning_train_data, "..\\..\\Datasets\\Warning_Train_Data.csv", row.names = FALSE)
write.csv(warning_test_data, "..\\..\\Datasets\\Warning_Test_Data.csv", row.names = FALSE)
