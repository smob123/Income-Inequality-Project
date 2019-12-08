# Sultan Banabila

# This script is responsible for translating the column data in the data.csv
# file to make it more readable, and useful

# import the fille
incomes <- read.csv("../data/data.csv", stringsAsFactors = FALSE)

# translates the sex column's data to male, and female
translate.sex <- function() {
  # translate male data
  male.data <- incomes[incomes$sex == 1, ]
  
  male.data$sex <- "M"
  
  incomes[incomes$sex == 1, ] <- male.data
  
  # translate female data
  female.data <- incomes[incomes$sex == 2, ]
  
  female.data$sex <- "F"
  
  incomes[incomes$sex == 2, ] <- female.data
  
  return(incomes)
}

# translates ethinicty data
translate.ethnicity <- function() {
  # list of the available ethinicities
  e1 <- "European"
  e2 <- "Maori"
  e3 <- "Pacific People"
  e4 <- "Asian"
  e5 <- "Middle Eastern/Latin American/African"
  e6 <- "Other Ethnicity"
  e9 <- "Residual Categories"
  
  ethnicity.translation <- c(e1, e2, e3, e4, e5, e6, e9)
  
  # list of individual ethnicities as they are stored in the raw file
  ethnicities <- c(1, 2, 3, 4, 5, 6, 9)
  
  len <- length(ethnicities)
  
  
  for (i in 1:len) {
    # get the ethnicity data at the current index
    e <- incomes[incomes$ethnicity == ethnicities[i],]
    # change the ethnicity column to the currosponding translation
    e$ethnicity <- ethnicity.translation[i]
    # update the original set
    incomes[incomes$ethnicity == ethnicities[i],] <- e
  }
  
  # translate other ethnicities that are not mentioned above
  other <- incomes[!incomes$ethnicity %in% ethnicity.translation, ]
  
  other$ethnicity <- "Other"
  incomes[!incomes$ethnicity %in% ethnicity.translation, ] <- other
  
  return(incomes)
}

# translate the qualification column
translate.qualification <- function() {
  # available qualifications
  q1 <- "None"
  q2 <- "School"
  q3 <- "Vocational/Trade"
  q4 <- "Bachelore or Higher"
  q5 <- "Other"
  
  qualifications <- c(q1, q2, q3, q4, q5)
  
  # translate the data
  for (i in 1:5) {
    q <- incomes[incomes$qualification == i, ]
    q$qualification <- qualifications[i]
    incomes[incomes$qualification == i, ] <- q
  }
  
  return(incomes)
}

translate.occupation <- function() {
  o1 <- "Managers"
  o2 <- "Professionals"
  o3 <- "Technicians and Trade Workers"
  o4 <- "Community and Personal Service Worker"
  o5 <- "Clerical and Administrative Workers"
  o6 <- "Sales Workers"
  o7 <- "Machinery Operators and Drivers"
  o8 <- "Labourers"
  o9 <- "Residual Categories"
  o10 <- "No Occupation"
  
  occupations <- c(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10)
  
  for (i in 1:10) {
    o <- incomes[incomes$occupation == i, ]
    o$occupation <- occupations[i]
    incomes[incomes$occupation == i, ] <- o
  }
  
  return(incomes)
}

translate.region <- function() {
  colnames(incomes)[2] <- "region"
  
  r1 <- "Northland"
  r2 <- "Auckland"
  r3 <- "Waikato"
  r4 <- "Bay of Plenty"
  r5 <- "Gisborne / Hawke's Bay"
  r6 <- "Taranaki"
  r7 <- "Manawatu-Wanganui"
  r8 <- "Wellington"
  r9 <- "Nelson/Tasman/Marlborough/West Coast"
  r10 <- "Canterbury"
  r11 <- "Otago"
  r12 <- "Southland"
  
  regions <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
  
  for (i in 1:10) {
    r <- incomes[incomes$region == i, ]
    r$region <- regions[i]
    incomes[incomes$region == i, ] <- r
  }
  
  other <- incomes[!incomes$region %in% regions, ]
  
  other$region <- "Other"
  incomes[!incomes$region %in% regions, ] <- other
  
  return(incomes)
}

incomes <- translate.sex()
incomes <- translate.ethnicity()
incomes <- translate.qualification()
incomes <- translate.occupation()
incomes <- translate.region()

write.csv(incomes, "../data/translated_data.csv")
