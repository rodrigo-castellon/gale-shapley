gen_random_preferences = function(n=3, num_samples=3) {
  samples = list(c())
  for (j in 1:num_samples) {
    preferences = sample.int(n)
    samples[[j]] = preferences
  }
  data_frame = data.frame(samples[1:length(samples)])
  colnames(data_frame) = 1:length(colnames(data_frame))
  return(data_frame)
}

random_pref = function(n=3, num_samples=3) {
  samples = c()
  for (i in 1:num_samples) {
    preferences = sample.int(n)
    samples = c(samples, preferences)
  }
  new_matrix = matrix(samples, nrow=num_samples, ncol=n)
  return(new_matrix)
}

find_best_applicant = function(preferences, app_list) {
  best_preference = 999
  best_applicant = 0
  if (length(app_list) == 1) {
    return(app_list[[1]])
  } else {
    for (i in 1:length(app_list)) {
      preference = which(preferences == app_list[i])
      if (preference < best_preference) {
        best_preference = preference
        best_applicant = app_list[i]
      }
    }
    return(best_applicant)
  }
}

find_rejected_applicants = function(preferences, app_list) {
  best_preference = 999
  best_applicant = 0
  if (length(app_list) == 1) {
    return(NA)
  } else {
    for (i in 1:length(app_list)) {
      preference = which(preferences == app_list[i])
      if (preference < best_preference) {
        best_preference = preference
        best_applicant = app_list[i]
      }
    }
    return(app_list[-which(app_list == best_applicant)])
  }
}

ban_student = function(all_student_preferences, student_id) {
  number_of_students = ncol(all_student_preferences)
  
  # get the specific student's preferences as a vector
  student_preferences = all_student_preferences[,student_id]
  
  # move the first preference to the last, modified_student_preferences is this new student-specific preference ordering
  modified_student_preferences = c(student_preferences[2:length(student_preferences)], student_preferences[1])
  
  # now reconstruct the entire all_student_preferences vector by replacing it with the modified student-specific preferences
  all_student_preferences[,student_id] = modified_student_preferences
  return(all_student_preferences)
}
