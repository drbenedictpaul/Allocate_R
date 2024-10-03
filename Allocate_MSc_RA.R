# Load necessary libraries
library(googlesheets4)
library(dplyr)

# Authenticate with Google Sheets
# gs4_auth()

# Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1GP1l8CDSL7_DV8UStRuxSmVxYHjYaHTkW54GiMtbS9A/edit?usp=sharing"

# Read the duplicated "Supervisor_Allotment" sheet
form_responses <- read_sheet(sheet_url, sheet = "Supervisor_Allotment")
research_supervisor_area <- read_sheet(sheet_url, sheet = "Research_Supervisor_Area")

# Initialize columns for allotment
form_responses$Allotted_Research_Supervisor <- NA

# Set the maximum number of students each supervisor can take
max_students_per_supervisor <- 5
max_students_sudha_warrier <- 3

# Initialize a count of students allocated to each supervisor
supervisor_count <- data.frame(
  Supervisor = research_supervisor_area$Research_Supervisor,
  Count = 0,
  Max_Students = ifelse(research_supervisor_area$Research_Supervisor == "Sudha_Warrier", 
                        max_students_sudha_warrier, max_students_per_supervisor)
)

# Function to clean and format supervisor names, ensuring "Kalaivani_D" is formatted as "Ms. Kalaivani D"
format_supervisor_name <- function(supervisor_name) {
  # Remove underscores and format specific names as requested
  supervisor_name <- gsub("_", " ", supervisor_name)
  if (supervisor_name == "Benedict Paul") {
    return("Dr. Benedict Paul C")
  } else if (supervisor_name == "Mary Elizabeth") {
    return("Dr. Mary Elizabeth Gnanambal")
  } else if (supervisor_name == "Charles EJW") {
    return("Dr. Charles Emmanuel Jebaraj W")
  } else if (supervisor_name == "Kalaivani D") {
    return("Ms. Kalaivani D")  # Correct rendering for Kalaivani_D
  } else {
    return(paste0("Dr. ", supervisor_name))
  }
}

# Function to allocate supervisors based on preferences and available slots
allocate_supervisor <- function(student_row, areas, preferences, supervisor_count) {
  sorted_preferences <- order(preferences)  # Sort preferences from 1st to 9th
  
  # Iterate through each sorted preference to allocate the supervisor
  for (index in sorted_preferences) {
    area_of_interest <- areas[index]
    
    # Find the supervisor based on the area of research
    supervisor_row <- research_supervisor_area %>%
      filter(grepl(area_of_interest, Area_1) | grepl(area_of_interest, Area_2))
    
    if (nrow(supervisor_row) > 0) {
      supervisor_name <- supervisor_row$Research_Supervisor[1]
      
      # Check if the supervisor has available slots
      supervisor_index <- which(supervisor_count$Supervisor == supervisor_name)
      if (supervisor_count$Count[supervisor_index] < supervisor_count$Max_Students[supervisor_index]) {
        # Allocate the student to this supervisor
        supervisor_count$Count[supervisor_index] <- supervisor_count$Count[supervisor_index] + 1
        
        # Format the supervisor name
        supervisor_name <- format_supervisor_name(supervisor_name)
        
        return(list(Supervisor = supervisor_name, supervisor_count = supervisor_count))
      }
    }
  }
  
  # If no supervisor is available, return NA
  return(list(Supervisor = NA, supervisor_count = supervisor_count))
}

# First, allocate students without arrears
students_without_arrears <- form_responses %>%
  filter(is.na(RA) | RA == 0) %>%  # Select students with no arrears
  arrange(desc(CGPA))

# Process students without arrears first
for (i in 1:nrow(students_without_arrears)) {
  student_name <- students_without_arrears$Name[i]
  student_cgpa <- students_without_arrears$CGPA[i]
  
  # Extract the student's areas of interest and preferences
  areas_of_interest <- c(students_without_arrears$AR1[i], students_without_arrears$AR2[i], students_without_arrears$AR3[i], 
                         students_without_arrears$AR4[i], students_without_arrears$AR5[i], students_without_arrears$AR6[i], 
                         students_without_arrears$AR7[i], students_without_arrears$AR8[i], students_without_arrears$AR9[i])
  
  preferences <- c(students_without_arrears$AR1_Preference[i], students_without_arrears$AR2_Preference[i], students_without_arrears$AR3_Preference[i], 
                   students_without_arrears$AR4_Preference[i], students_without_arrears$AR5_Preference[i], students_without_arrears$AR6_Preference[i], 
                   students_without_arrears$AR7_Preference[i], students_without_arrears$AR8_Preference[i], students_without_arrears$AR9_Preference[i])
  
  # Allocate the supervisor based on sorted preferences
  result <- allocate_supervisor(students_without_arrears[i, ], areas_of_interest, preferences, supervisor_count)
  form_responses$Allotted_Research_Supervisor[which(form_responses$Name == student_name)] <- result$Supervisor
  supervisor_count <- result$supervisor_count
}

# Now allocate students with arrears, prioritizing by fewer arrears first
students_with_arrears <- form_responses %>%
  filter(RA > 0) %>%
  arrange(RA, desc(CGPA))  # First by number of arrears, then by CGPA

# Process students with arrears
for (i in 1:nrow(students_with_arrears)) {
  student_name <- students_with_arrears$Name[i]
  student_cgpa <- students_with_arrears$CGPA[i]
  
  # Extract the student's areas of interest and preferences
  areas_of_interest <- c(students_with_arrears$AR1[i], students_with_arrears$AR2[i], students_with_arrears$AR3[i], 
                         students_with_arrears$AR4[i], students_with_arrears$AR5[i], students_with_arrears$AR6[i], 
                         students_with_arrears$AR7[i], students_with_arrears$AR8[i], students_with_arrears$AR9[i])
  
  preferences <- c(students_with_arrears$AR1_Preference[i], students_with_arrears$AR2_Preference[i], students_with_arrears$AR3_Preference[i], 
                   students_with_arrears$AR4_Preference[i], students_with_arrears$AR5_Preference[i], students_with_arrears$AR6_Preference[i], 
                   students_with_arrears$AR7_Preference[i], students_with_arrears$AR8_Preference[i], students_with_arrears$AR9_Preference[i])
  
  # Allocate the supervisor based on sorted preferences
  result <- allocate_supervisor(students_with_arrears[i, ], areas_of_interest, preferences, supervisor_count)
  form_responses$Allotted_Research_Supervisor[which(form_responses$Name == student_name)] <- result$Supervisor
  supervisor_count <- result$supervisor_count
}

# Write the updated form_responses back to Google Sheets
sheet_write(form_responses, ss = sheet_url, sheet = "Supervisor_Allotment")
