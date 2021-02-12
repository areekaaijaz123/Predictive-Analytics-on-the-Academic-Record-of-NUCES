setwd("E:\\FYP\\FYP-1 (Part 2)\\Implementation")
source("functions.R")

student_data <- read.csv("E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Student_Data.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

student_data <- student_data[, c("STUDENT_ID", "GENDER", "BATCH", "CAMPUS", "PROG_CODE", "CGPA", 
                                 "FIRST_SEM", "LAST_SEM", "STATUS", "CITY", "SSC_BOARD", "SSC_OBTAINED", 
                                 "HSSC_BOARD", "HSSC_OBTAINED", "OLEVEL_BOARD", "OLEVEL_OBTAINED",
                                 "ALEVEL_BOARD", "ALEVEL_OBTAINED")]


student_data$GENDER <- as.factor(student_data$GENDER)
student_data$BATCH <- as.factor(student_data$BATCH)
student_data$CAMPUS <- as.factor(student_data$CAMPUS)
student_data$PROG_CODE <- as.factor(student_data$PROG_CODE)
student_data$FIRST_SEM <- as.factor(student_data$FIRST_SEM)
student_data$LAST_SEM <- as.factor(student_data$LAST_SEM)
student_data$STATUS <- as.factor(student_data$STATUS)
student_data$CITY <- as.factor(student_data$CITY)

student_data$CGPA <- suppressWarnings(as.numeric(student_data$CGPA))

student_data <- student_data %>% distinct()
student_data <- student_data[!duplicated(student_data[, "STUDENT_ID"]), ]

SSC_OLEVEL <- vector(mode = "character", length = nrow(student_data))
School <- vector(mode = "character", length = nrow(student_data))
Sec_Grade <- vector(mode = "numeric", length = nrow(student_data))

HSSC_ALEVEL <- vector(mode = "character", length = nrow(student_data))
College <- vector(mode = "character", length = nrow(student_data))
High_Sec_Grade <- vector(mode = "numeric", length = nrow(student_data))

student_data$SECONDARY <- SSC_OLEVEL
student_data$SCHOOL <- School
student_data$SEC_GRADE <- Sec_Grade

student_data$HIGHER_SECONDARY <- HSSC_ALEVEL
student_data$COLLEGE <- College
student_data$HIG_SEC_GRADE <- High_Sec_Grade

for (i in 1:nrow(student_data)){
  if (!is.na(student_data[i, 12])){
    student_data[i, 19] <- "SSC"
    student_data[i, 20] <- student_data[i, 11]
    student_data[i ,21] <- student_data[i, 12]
  }
  else if (!is.na(student_data[i, 16])) {
    student_data[i, 19] <- "OLEVEL"
    student_data[i, 20] <- student_data[i, 15]
    student_data[i ,21] <- student_data[i, 16]    
  }
  else {
    student_data[i, 19] <- NA
    student_data[i, 20] <- NA
    student_data[i ,21] <- NA     
  }
  
  
  if (!is.na(student_data[i, 14])){
    student_data[i, 22] <- "HSSC"
    student_data[i, 23] <- student_data[i, 13]
    student_data[i ,24] <- student_data[i, 14]
  }
  else if (!is.na(student_data[i, 18])) {
    student_data[i, 22] <- "ALEVEL"
    student_data[i, 23] <- student_data[i, 17]
    student_data[i ,24] <- student_data[i, 18]    
  }
  else {
    student_data[i, 22] <- NA
    student_data[i, 23] <- NA
    student_data[i ,24] <- NA     
  }
}

student_data <- student_data[ , c("STUDENT_ID", "GENDER", "BATCH", "CAMPUS", "PROG_CODE", "CGPA", "FIRST_SEM", 
                                  "LAST_SEM", "STATUS", "CITY", "SECONDARY", "SCHOOL", "SEC_GRADE", "HIGHER_SECONDARY",
                                  "COLLEGE", "HIG_SEC_GRADE")]


write.csv(student_data, "E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Cleaned_Student_Data.csv", row.names = FALSE)
