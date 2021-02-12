library(dplyr)
library(ggplot2)

setwd("E:\\FYP\\FYP-1 (Part 2)\\Implementation")
source("functions.R")

semester_data <- read.csv("E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Semester_Data.csv", stringsAsFactors = FALSE)

colnames(semester_data) <- c("Semester", "Student_Id", "SGPA", "CGPA", "CoreCount", "ElectiveCount")


split <- function(x, Tail = TRUE){ temp <- unlist(strsplit(x, " ")); if (Tail) tail(temp,1) else head(temp,1)}

semester_data$Year <- sapply(semester_data$Semester, FUN = split)
semester_data$Session <- sapply(semester_data$Semester, FUN = split, Tail = FALSE)

semester_data <- semester_data[, c("Student_Id", "Semester", "Session", "Year", "SGPA", "CGPA", 
                                   "CoreCount", "ElectiveCount")]

sem_seq <- c("Spring", "Summer", "Fall")
semester_data <- semester_data[order(factor(semester_data$Session, levels = sem_seq)), ]

semester_data <- semester_data %>% arrange(semester_data$Year)

semester_data <- subset(semester_data, select = c("Student_Id", "Semester", "SGPA", "CGPA"))

unique_semester <- unique(semester_data$Semester)
unique_ids <- unique(semester_data$Student_Id)

semester_details <- c("SEM_1_SGPA", "SEM_1_CGPA", "SEM_2_SGPA", "SEM_2_CGPA", "SEM_3_SGPA", "SEM_3_CGPA", 
                      "SEM_4_SGPA", "SEM_4_CGPA", "SEM_5_SGPA", "SEM_5_CGPA", "SEM_6_SGPA", "SEM_6_CGPA",
                      "SEM_7_SGPA", "SEM_7_CGPA", "SEM_8_SGPA", "SEM_8_CGPA", "SEM_9_SGPA", "SEM_9_CGPA", 
                      "SEM_10_SGPA", "SEM_10_CGPA", "SEM_11_SGPA", "SEM_11_CGPA", "SEM_12_SGPA", "SEM_12_CGPA",
                      "SEM_13_SGPA", "SEM_13_CGPA", "SEM_14_SGPA", "SEM_14_CGPA", "SEM_15_SGPA", "SEM_15_CGPA")

data <- data.frame(matrix(ncol = length(semester_details), nrow = length(unique_ids)))
colnames(data) <- semester_details

data[semester_details] <- sapply(data[semester_details], as.character)
  
data = cbind(STUDENT_ID = unique_ids, data)

Total_Sem <- vector(mode = "integer", length = nrow(data))

data$TOTAL_SEM <- Total_Sem

for (i in 1:nrow(data)){
  z <- data[i,1] == semester_data[ ,1]
  row_ <- which(z)
  data[i, 32] <- length(z[z==TRUE])
  for (j in 1:15){
    data[i, j*2] <- semester_data[row_[j], 3]
    data[i, (j*2)+1] <- semester_data[row_[j], 4]
  }
}

data[, 2:31] <- suppressWarnings(sapply(data[, 2:31], as.numeric))

write.csv(data, "E:\\FYP\\FYP-1 (Part 2)\\Datasets\\Cleaned_Semester_Data.csv", row.names = FALSE)