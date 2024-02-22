# Package ----

library(haven)

# Functions ----

fun.agreement_rate <- function(a, 
                               b, 
                               c, 
                               d) {
  agreement_rate <- (b + d) / (a + b + c + d)
  return(agreement_rate)
}

fun.cohens_kappa <- function(a, 
                             b, 
                             c, 
                             d) {
  p_A <- (b + d) / (a + b + c + d)
  p_E <- ((a + b) / (a + b + c + d)) * ((b + c) / (a + b + c + d)) + ((c + d) / (a + b + c + d)) * ((a + d) / (a + b + c + d)) 
  cohens_kappa <- (p_A - p_E) / (1 - p_E)
  return(cohens_kappa)
}

# Data ----



# Table 1 ----

table_1 <- as.data.frame(matrix(nrow = 28,
                                ncol = 7))
colnames(table_1) <- c("ICD10",
                       "events_participants",
                       "TMS",
                       "DHD",
                       "intersect_1",
                       "intersect_2",
                       "intersect_3")
table_1$ICD10 <- rep(selected_ICD10, 2)
table_1[1:14, "events_participants"] <- "events"
table_1[15:28, "events_participants"] <- "participants"

# TMS

for(i in 1:nrow(table_1)) {
  if(table_1[i, "events_participants"] == "events"){
    table_1[i, "TMS"] <- nrow(subset(student_data_ICD10, ICD10_student == table_1[i, "ICD10"]))
  }
  if(table_1[i, "events_participants"] == "participants"){
    table_1[i, "TMS"] <- length(unique(subset(student_data_ICD10, ICD10_student == table_1[i, "ICD10"])$RINPERSOON))
  }
}

# DHD 

for(i in 1:nrow(table_1)) {
  if(table_1[i, "events_participants"] == "events"){
    table_1[i, "DHD"] <- nrow(subset(DHD_TMS_ICD10, ICD10_DHD == table_1[i, "ICD10"]))
  }
  if(table_1[i, "events_participants"] == "participants"){
    table_1[i, "DHD"] <- length(unique(subset(DHD_TMS_ICD10, ICD10_DHD == table_1[i, "ICD10"])$RINPERSOON))
  }
}

# intersect_1: intersection matching on pseudonym and exact date 

match_exact_date_ICD10 <- merge(student_data_ICD10,
                                DHD_TMS_ICD10[, c("RINPERSOON",
                                                  "admission_date",
                                                  "ICD10_DHD")],
                                by = c("RINPERSOON",
                                       "admission_date"))

for(i in 1:nrow(table_1)) {
  if(table_1[i, "events_participants"] == "events"){
    table_1[i, "intersect_1"] <- nrow(subset(match_exact_date_ICD10, (ICD10_student == table_1[i, "ICD10"])
                                                                     &
                                                                     (ICD10_DHD == table_1[i, "ICD10"])))
  }
  if(table_1[i, "events_participants"] == "participants"){
    table_1[i, "intersect_1"] <- length(unique(subset(match_exact_date_ICD10, (ICD10_student == table_1[i, "ICD10"])
                                                                              &
                                                                              (ICD10_DHD == table_1[i, "ICD10"]))$RINPERSOON))
  }
}

# intersect_2: intersection matching on pseudonym and date +/- 30 days 

student_data_ICD10$admission_date_minus_30_days <- as.Date(strptime(student_data_ICD10$admission_date, "%Y%m%d")) - 30
student_data_ICD10$admission_date_minus_30_days <- as.integer(format(student_data_ICD10$admission_date_minus_30_days, "%Y%m%d"))
student_data_ICD10$admission_date_plus_30_days <- as.Date(strptime(student_data_ICD10$admission_date, "%Y%m%d")) + 30
student_data_ICD10$admission_date_plus_30_days <- as.integer(format(student_data_ICD10$admission_date_plus_30_days, "%Y%m%d"))

match_date_plus_minus_30_days_ICD10 <- DHD_TMS_ICD10
match_date_plus_minus_30_days_ICD10$ICD10_student <- NA
for (i in 1:nrow(match_date_plus_minus_30_days_ICD10)) {
  for (j in 1:nrow(student_data_ICD10)) {
    if((match_date_plus_minus_30_days_ICD10[i, "RINPERSOON"] == student_data_ICD10[j, "RINPERSOON"])
       &
       (student_data_ICD10[j, "admission_date_minus_30_days"] <= match_date_plus_minus_30_days_ICD10[i, "admission_date"])
       &
       (match_date_plus_minus_30_days_ICD10[i, "admission_date"] <= student_data_ICD10[j, "admission_date_plus_30_days"])) {
      match_date_plus_minus_30_days_ICD10[i, "ICD10_student"] <- student_data_ICD10[j, "ICD10_student"]
    }
  }
}

for(i in 1:nrow(table_1)) {
  if(table_1[i, "events_participants"] == "events"){
    table_1[i, "intersect_2"] <- nrow(subset(match_date_plus_minus_30_days_ICD10, (ICD10_student == table_1[i, "ICD10"])
                                                                                  &
                                                                                  (ICD10_DHD == table_1[i, "ICD10"])))
  }
  if(table_1[i, "events_participants"] == "participants"){
    table_1[i, "intersect_2"] <- length(unique(subset(match_date_plus_minus_30_days_ICD10, (ICD10_student == table_1[i, "ICD10"])
                                                                                           &
                                                                                           (ICD10_DHD == table_1[i, "ICD10"]))$RINPERSOON))
  }
}

# intersect_3: intersection matching on pseudonym 

for(i in 1:nrow(table_1)) {
  if(table_1[i, "events_participants"] == "participants"){
    table_1[i, "intersect_3"] <- sum(unique(subset(student_data_ICD10,
                                                   ICD10_student == table_1[i, "ICD10"])$RINPERSOON) %in% unique(subset(DHD_TMS_ICD10,
                                                                                                                        ICD10_DHD == table_1[i, "ICD10"])$RINPERSOON))
  }
}

# Table

table_1

# Table 2 ----

table_2 <- as.data.frame(matrix(nrow = 6,
                                ncol = 7))
colnames(table_2) <- c("CHD_CVA_TIA",
                       "events_participants",
                       "TMS",
                       "DHD",
                       "intersect_1",
                       "intersect_2",
                       "intersect_3")
table_2$CHD_CVA_TIA <- rep(c("CHD",
                             "CVA",
                             "TIA"), 
                           2)
table_2[1:3, "events_participants"] <- "events"
table_2[4:6, "events_participants"] <- "participants"

# TMS

for(i in 1:nrow(table_2)){
  if(table_2[i, "events_participants"] == "events"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "TMS"] <- sum(specialist_data$CHD_specialist, na.rm = TRUE)
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "TMS"] <- sum(specialist_data$CVA_specialist, na.rm = TRUE)
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "TMS"] <- sum(specialist_data$TIA_specialist, na.rm = TRUE)
    }
  }
  if(table_2[i, "events_participants"] == "participants"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "TMS"] <- length(unique(subset(specialist_data, CHD_specialist == 1)$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "TMS"] <- length(unique(subset(specialist_data, CVA_specialist == 1)$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "TMS"] <- length(unique(subset(specialist_data, TIA_specialist == 1)$RINPERSOON))
    }
  }
}

# DHD

for(i in 1:nrow(table_2)){
  if(table_2[i, "events_participants"] == "events"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "DHD"] <- sum(DHD_TMS_CHD_CVA_TIA$CHD_DHD, na.rm = TRUE)
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "DHD"] <- sum(DHD_TMS_CHD_CVA_TIA$CVA_DHD, na.rm = TRUE)
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "DHD"] <- sum(DHD_TMS_CHD_CVA_TIA$TIA_DHD, na.rm = TRUE)
    }
  }
  if(table_2[i, "events_participants"] == "participants"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "DHD"] <- length(unique(subset(DHD_TMS_CHD_CVA_TIA, CHD_DHD == 1)$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "DHD"] <- length(unique(subset(DHD_TMS_CHD_CVA_TIA, CVA_DHD == 1)$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "DHD"] <- length(unique(subset(DHD_TMS_CHD_CVA_TIA, TIA_DHD == 1)$RINPERSOON))
    }
  }
}

# intersect_1: intersection matching on pseudonym and exact date 

match_exact_date_CHD_CVA_TIA <- merge(specialist_data,
                                      DHD_TMS_CHD_CVA_TIA,
                                      by = c("RINPERSOON",
                                             "admission_date"))

for(i in 1:nrow(table_2)){
  if(table_2[i, "events_participants"] == "events"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "intersect_1"] <- nrow(subset(match_exact_date_CHD_CVA_TIA, (CHD_specialist == 1)
                                                                             &
                                                                             (CHD_DHD == 1)))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "intersect_1"] <- nrow(subset(match_exact_date_CHD_CVA_TIA, (CVA_specialist == 1)
                                                                             &
                                                                             (CVA_DHD == 1)))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "intersect_1"] <- nrow(subset(match_exact_date_CHD_CVA_TIA, (TIA_specialist == 1)
                                                                             &
                                                                             (TIA_DHD == 1)))
    }
  }
  if(table_2[i, "events_participants"] == "participants"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "intersect_1"] <- length(unique(subset(match_exact_date_CHD_CVA_TIA, (CHD_specialist == 1)
                                                                                      &
                                                                                      (CHD_DHD == 1))$RINPERSOON))
      
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "intersect_1"] <- length(unique(subset(match_exact_date_CHD_CVA_TIA, (CVA_specialist == 1)
                                                                                      &
                                                                                      (CVA_DHD == 1))$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "intersect_1"] <- length(unique(subset(match_exact_date_CHD_CVA_TIA, (TIA_specialist == 1)
                                                                                      &
                                                                                      (TIA_DHD == 1))$RINPERSOON))
    }
  }
}

# intersect_2: intersection matching on pseudonym and date +/- 30 days 

specialist_data$admission_date_minus_30_days <- as.Date(strptime(specialist_data$admission_date, "%Y%m%d")) - 30
specialist_data$admission_date_minus_30_days <- as.integer(format(specialist_data$admission_date_minus_30_days, "%Y%m%d"))
specialist_data$admission_date_plus_30_days <- as.Date(strptime(specialist_data$admission_date, "%Y%m%d")) + 30
specialist_data$admission_date_plus_30_days <- as.integer(format(specialist_data$admission_date_plus_30_days, "%Y%m%d"))

match_date_plus_minus_30_days_CHD_CVA_TIA <- DHD_TMS_CHD_CVA_TIA
match_date_plus_minus_30_days_CHD_CVA_TIA$CHD_specialist <- NA
match_date_plus_minus_30_days_CHD_CVA_TIA$CVA_specialist <- NA
match_date_plus_minus_30_days_CHD_CVA_TIA$TIA_specialist <- NA
for (i in 1:nrow(match_date_plus_minus_30_days_CHD_CVA_TIA)) {
  for (j in 1:nrow(specialist_data)) {
    if((match_date_plus_minus_30_days_CHD_CVA_TIA[i, "RINPERSOON"] == specialist_data[j, "RINPERSOON"])
       &
       (specialist_data[j, "admission_date_minus_30_days"] <= match_date_plus_minus_30_days_CHD_CVA_TIA[i, "admission_date"])
       &
       (match_date_plus_minus_30_days_CHD_CVA_TIA[i, "admission_date"] <= specialist_data[j, "admission_date_plus_30_days"])) {
      match_date_plus_minus_30_days_CHD_CVA_TIA[i, "CHD_specialist"] <- specialist_data[j, "CHD_specialist"]
      match_date_plus_minus_30_days_CHD_CVA_TIA[i, "CVA_specialist"] <- specialist_data[j, "CVA_specialist"]
      match_date_plus_minus_30_days_CHD_CVA_TIA[i, "TIA_specialist"] <- specialist_data[j, "TIA_specialist"]
    }
  }
}

for(i in 1:nrow(table_2)){
  if(table_2[i, "events_participants"] == "events"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "intersect_2"] <- nrow(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (CHD_specialist == 1)
                                                                                          &
                                                                                          (CHD_DHD == 1)))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "intersect_2"] <- nrow(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (CVA_specialist == 1)
                                                                                          &
                                                                                          (CVA_DHD == 1)))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "intersect_2"] <- nrow(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (TIA_specialist == 1)
                                                                                          &
                                                                                          (TIA_DHD == 1)))
    }
  }
  if(table_2[i, "events_participants"] == "participants"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "intersect_2"] <- length(unique(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (CHD_specialist == 1)
                                                                                                   &
                                                                                                   (CHD_DHD == 1))$RINPERSOON))
      
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "intersect_2"] <- length(unique(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (CVA_specialist == 1)
                                                                                                   &
                                                                                                   (CVA_DHD == 1))$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "intersect_2"] <- length(unique(subset(match_date_plus_minus_30_days_CHD_CVA_TIA, (TIA_specialist == 1)
                                                                                                   &
                                                                                                   (TIA_DHD == 1))$RINPERSOON))
    }
  }
}

# intersect_3: intersection matching on pseudonym 

for(i in 1:nrow(table_2)){
  if(table_2[i, "events_participants"] == "participants"){
    if(table_2[i, "CHD_CVA_TIA"] == "CHD"){
      table_2[i, "intersect_3"] <- sum(unique(subset(specialist_data,
                                                     CHD_specialist == 1)$RINPERSOON) %in% unique(subset(DHD_TMS_CHD_CVA_TIA,
                                                                                                         CHD_DHD == 1)$RINPERSOON))
      
    }
    if(table_2[i, "CHD_CVA_TIA"] == "CVA"){
      table_2[i, "intersect_3"] <- sum(unique(subset(specialist_data,
                                                     CVA_specialist == 1)$RINPERSOON) %in% unique(subset(DHD_TMS_CHD_CVA_TIA,
                                                                                                         CVA_DHD == 1)$RINPERSOON))
    }
    if(table_2[i, "CHD_CVA_TIA"] == "TIA"){
      table_2[i, "intersect_3"] <- sum(unique(subset(specialist_data,
                                                     TIA_specialist == 1)$RINPERSOON) %in% unique(subset(DHD_TMS_CHD_CVA_TIA,
                                                                                                         TIA_DHD == 1)$RINPERSOON))
    }
  }
}

# Table

table_2

# Table 3 ----

table_3 <- as.data.frame(matrix(nrow = 28,
                                ncol = 8))
colnames(table_3) <- c("ICD10",
                       "events_participants",
                       "agreement_rate_1",
                       "agreement_rate_2",
                       "agreement_rate_3",
                       "cohens_kappa_1",
                       "cohens_kappa_2",
                       "cohens_kappa_3")
table_3$ICD10 <- rep(selected_ICD10, 2)
table_3[1:14, "events_participants"] <- "events"
table_3[15:28, "events_participants"] <- "participants"

for(i in 1:nrow(table_3)){
  if(table_3[i, "events_participants"] == "events"){
    table_3[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                             subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         b = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         c = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                             subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         d = 8789 - 
                                                             length(union(unique(subset(student_data_ICD10, ICD10_student == table_3[i, "ICD10"])$RINPERSOON), 
                                                                          unique(subset(DHD_TMS_ICD10, ICD10_DHD == table_3[i, "ICD10"])$RINPERSOON))))
    
    table_3[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                             subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         b = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         c = subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                                             subset(table_1, (events_participants == "events")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         d = 8789 - 
                                                             length(union(unique(subset(student_data_ICD10, ICD10_student == table_3[i, "ICD10"])$RINPERSOON), 
                                                                          unique(subset(DHD_TMS_ICD10, ICD10_DHD == table_3[i, "ICD10"])$RINPERSOON))))
    
    table_3[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                         subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     b = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     c = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                         subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     d = 8789 - 
                                                         length(union(unique(subset(student_data_ICD10, ICD10_student == table_3[i, "ICD10"])$RINPERSOON), 
                                                                      unique(subset(DHD_TMS_ICD10, ICD10_DHD == table_3[i, "ICD10"])$RINPERSOON))))
    
    table_3[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                         subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     b = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     c = subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                         subset(table_1, (events_participants == "events")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     d = 8789 - 
                                                         length(union(unique(subset(student_data_ICD10, ICD10_student == table_3[i, "ICD10"])$RINPERSOON), 
                                                                      unique(subset(DHD_TMS_ICD10, ICD10_DHD == table_3[i, "ICD10"])$RINPERSOON))))
  }
  if(table_3[i, "events_participants"] == "participants"){
    table_3[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         b = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         c = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                         d = 8789 -
                                                             ((subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_1) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_1) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_1)))
    
    table_3[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         b = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         c = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                         d = 8789 -
                                                             ((subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_2) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_2) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_2)))
    
    table_3[i, "agreement_rate_3"] <- fun.agreement_rate(a = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                         b = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                         c = subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                             subset(table_1, (events_participants == "participants")
                                                                             &
                                                                             (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                         d = 8789 -
                                                             ((subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_3) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                                &
                                                                                (ICD10 == table_3[i, "ICD10"]))$intersect_3) +
                                                              (subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                               subset(table_1, (events_participants == "participants")
                                                                               &
                                                                               (ICD10 == table_3[i, "ICD10"]))$intersect_3)))
    
    table_3[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     b = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     c = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_1,
                                                     d = 8789 -
                                                         ((subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_1) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_1) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_1)))
    
    table_3[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     b = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     c = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_2,
                                                     d = 8789 -
                                                         ((subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_2) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_2) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_2)))
    
    table_3[i, "cohens_kappa_3"] <- fun.cohens_kappa(a = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                     b = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                     c = subset(table_1, (events_participants == "participants")
                                                                         &
                                                                        (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                         subset(table_1, (events_participants == "participants")
                                                                         &
                                                                         (ICD10 == table_3[i, "ICD10"]))$intersect_3,
                                                     d = 8789 -
                                                         ((subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$TMS -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_3) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                            &
                                                                            (ICD10 == table_3[i, "ICD10"]))$intersect_3) +
                                                          (subset(table_1, (events_participants == "participants")
                                                                            &
                                                                            (ICD10 == table_3[i, "ICD10"]))$DHD -
                                                           subset(table_1, (events_participants == "participants")
                                                                           &
                                                                           (ICD10 == table_3[i, "ICD10"]))$intersect_3)))
  }
}

# Table

table_3

# Table 4 ----

table_4 <- as.data.frame(matrix(nrow = 6,
                                ncol = 8))
colnames(table_4) <- c("CHD_CVA_TIA",
                       "events_participants",
                       "agreement_rate_1",
                       "agreement_rate_2",
                       "agreement_rate_3",
                       "cohens_kappa_1",
                       "cohens_kappa_2",
                       "cohens_kappa_3")
table_4$CHD_CVA_TIA <- rep(c("CHD",
                             "CVA",
                             "TIA"), 
                           2)
table_4[1:3, "events_participants"] <- "events"
table_4[4:6, "events_participants"] <- "participants"

for(i in 1:nrow(table_4)){
  if(table_4[i, "events_participants"] == "events"){
    if(table_4[i, "CHD_CVA_TIA"] == "CHD"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           d = 8789 - 
                                                               length(union(unique(subset(specialist_data, CHD_specialist == 1)$RINPERSOON), 
                                                                            unique(subset(DHD_TMS_CHD_CVA_TIA, CHD_DHD == 1)$RINPERSOON))))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           d = 8789 - 
                                                               length(union(unique(subset(specialist_data, CHD_specialist == 1)$RINPERSOON), 
                                                                            unique(subset(DHD_TMS_CHD_CVA_TIA, CHD_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, CHD_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, CHD_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, CHD_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, CHD_DHD == 1)$RINPERSOON))))
    }
    if(table_4[i, "CHD_CVA_TIA"] == "CVA"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           d = 8789 - 
                                                               length(union(unique(subset(specialist_data, CVA_specialist == 1)$RINPERSOON), 
                                                                            unique(subset(DHD_TMS_CHD_CVA_TIA, CVA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           d = 8789 - 
                                                               length(union(unique(subset(specialist_data, CVA_specialist == 1)$RINPERSOON), 
                                                                            unique(subset(DHD_TMS_CHD_CVA_TIA, CVA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, CVA_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, CVA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, CVA_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, CVA_DHD == 1)$RINPERSOON))))
    }
    if(table_4[i, "CHD_CVA_TIA"] == "TIA"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           d = 8789 - 
                                                               length(union(unique(subset(specialist_data, TIA_specialist == 1)$RINPERSOON), 
                                                                            unique(subset(DHD_TMS_CHD_CVA_TIA, TIA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$TMS -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$DHD -
                                                               subset(table_2, (events_participants == "events")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           d = 8789 - length(union(unique(subset(specialist_data, TIA_specialist == 1)$RINPERSOON), 
                                                                                   unique(subset(DHD_TMS_CHD_CVA_TIA, TIA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, TIA_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, TIA_DHD == 1)$RINPERSOON))))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$TMS -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$DHD -
                                                           subset(table_2, (events_participants == "events")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       d = 8789 - 
                                                           length(union(unique(subset(specialist_data, TIA_specialist == 1)$RINPERSOON), 
                                                                        unique(subset(DHD_TMS_CHD_CVA_TIA, TIA_DHD == 1)$RINPERSOON))))
    }
  }
  if(table_4[i, "events_participants"] == "participants"){
    if(table_4[i, "CHD_CVA_TIA"] == "CHD"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                                &
                                                                               (CHD_CVA_TIA == "CHD"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_1)))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                (CHD_CVA_TIA == "CHD"))$intersect_2)))
      
      table_4[i, "agreement_rate_3"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                (CHD_CVA_TIA == "CHD"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CHD"))$intersect_3)))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_1,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_1)))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_2,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_2)))
      
      table_4[i, "cohens_kappa_3"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CHD"))$intersect_3,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CHD"))$intersect_3)))
    }
    if(table_4[i, "CHD_CVA_TIA"] == "CVA"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                                &
                                                                                (CHD_CVA_TIA == "CVA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_1)))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_2)))
      
      table_4[i, "agreement_rate_3"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "CVA"))$intersect_3)))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_1,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_1)))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_2,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_2)))
      
      table_4[i, "cohens_kappa_3"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "CVA"))$intersect_3,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "CVA"))$intersect_3)))
    }
    if(table_4[i, "CHD_CVA_TIA"] == "TIA"){
      table_4[i, "agreement_rate_1"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_1) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_1)))
      
      table_4[i, "agreement_rate_2"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                                &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_2) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_2)))
      
      table_4[i, "agreement_rate_3"] <- fun.agreement_rate(a = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$TMS -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                           b = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                           c = subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$DHD -
                                                               subset(table_2, (events_participants == "participants")
                                                                               &
                                                                               (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                           d = 8789 -
                                                               ((subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$TMS -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_3) +
                                                                (subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$DHD -
                                                                 subset(table_2, (events_participants == "participants")
                                                                                 &
                                                                                 (CHD_CVA_TIA == "TIA"))$intersect_3)))
      
      table_4[i, "cohens_kappa_1"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                          (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                          (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_1,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_1) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_1)))
      
      table_4[i, "cohens_kappa_2"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_2,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_2) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_2)))
      
      table_4[i, "cohens_kappa_3"] <- fun.cohens_kappa(a = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$TMS -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                       b = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                       c = subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$DHD -
                                                           subset(table_2, (events_participants == "participants")
                                                                           &
                                                                           (CHD_CVA_TIA == "TIA"))$intersect_3,
                                                       d = 8789 -
                                                           ((subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$TMS -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_3) +
                                                            (subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$DHD -
                                                             subset(table_2, (events_participants == "participants")
                                                                             &
                                                                             (CHD_CVA_TIA == "TIA"))$intersect_3)))
    }
  }
}

# Table

table_4

# Session info ----

sessionInfo()
