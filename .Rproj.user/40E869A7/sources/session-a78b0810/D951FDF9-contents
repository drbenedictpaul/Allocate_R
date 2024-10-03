# Load necessary libraries
library(googlesheets4)
library(dplyr)

# # Authenticate with Google Sheets
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

# Function to clean and format supervisor names
format_supervisor_name <- function(supervisor_name) {
  # Remove underscores and format specific names as requested
  supervisor_name <- gsub("_", " ", supervisor_name)
  if (supervisor_name == "Benedict Paul") {
    return("Dr. Benedict Paul C")
  } else if (supervisor_name == "Mary Elizabeth") {
    return("Dr. Mary Elizabeth Gnanambal")
  } else if (supervisor_name == "Charles EJW") {
    return("Dr. Charles Emmanuel Jebaraj W")
  } else if (supervisor_name == "Kalaivani") {
    return("Ms. Kalaivani")
  } else {
    return(paste0("Dr. ", supervisor_name))
  }
}

# Sort the students by CGPA in descending order
form_responses <- form_responses %>%
  arrange(desc(CGPA))

# Function to allocate supervisors based on preferences and available slots
allocate_supervisor <- function(student_row, areas, preferences, supervisor_count) {
  # Sort areas and preferences by student's stated preference (from 1st to 9th)
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

# Iterate over each student and assign supervisors based on preferences and availability
for (i in 1:nrow(form_responses)) {
  student_name <- form_responses$Name[i]
  student_cgpa <- form_responses$CGPA[i]
  
  # Extract the student's areas of interest and preferences
  areas_of_interest <- c(form_responses$AR1[i], form_responses$AR2[i], form_responses$AR3[i], 
                         form_responses$AR4[i], form_responses$AR5[i], form_responses$AR6[i], 
                         form_responses$AR7[i], form_responses$AR8[i], form_responses$AR9[i])
  
  preferences <- c(form_responses$AR1_Preference[i], form_responses$AR2_Preference[i], form_responses$AR3_Preference[i], 
                   form_responses$AR4_Preference[i], form_responses$AR5_Preference[i], form_responses$AR6_Preference[i], 
                   form_responses$AR7_Preference[i], form_responses$AR8_Preference[i], form_responses$AR9_Preference[i])
  
  # Allocate the supervisor based on sorted preferences
  result <- allocate_supervisor(form_responses[i, ], areas_of_interest, preferences, supervisor_count)
  form_responses$Allotted_Research_Supervisor[i] <- result$Supervisor
  supervisor_count <- result$supervisor_count
}

# Write the updated form_responses back to Google Sheets
sheet_write(form_responses, ss = sheet_url, sheet = "Supervisor_Allotment")
