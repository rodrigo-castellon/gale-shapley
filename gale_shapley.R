set.seed(3283270)

# Change this here to the path to helper.R
source(".../gale_shapley/helper.R")

num_students = 3
num_schools = 3

students = random_pref(num_schools, num_students)
schools =  random_pref(num_students, num_schools)

gale_shapley = function(st_preferences, sch_preferences) {
  round = 1
  
  # the purpose of the 'done' vector is to document if all schools are matched with a student or not
  # 'done' is initialized as c(0) just to allow (mean(done) != 1) to run
  # In each round, 'done' is initialized as an empty vector
  # When going through every school, either FALSE or TRUE is appended to 'done', depending on whether or not they have at least one applicant
  # If every school has received an applicant, the vector would be [1] TRUE TRUE ...
  # This is why mean(done) is useful; it evaluates to 1 if all schools have at least one applicant (matched)
  done = c(0)
  while (mean(done) != 1) {
    done = c()
    application_list = st_preferences[1,] # vector of schools each student applied to
    # for example: [1] 1 3 3 2
    # this means that student 1 applied to school 1, student 2 applied to school 3, student 3 applied to school 3, etc.
    
    # initialize a best_applicants vector which keeps track of the current round's matched students/schools in case it is the last round
    best_applicants = c()
    
    for (j in 1:num_schools) {
      # for each school
      if (j %in% application_list == FALSE) {
        # in this case, no one applied to school j, so we must continue with the next school in the list
        done = c(done, FALSE)
        next
      } else {
        # the 'else' part of this does the following three things:
        # 1. gather all applicants as a vector (applicant_list)
        # 2. Determine which applicant to waitlist and which to reject (if any)
        # 3. For each rejected applicant, change their preferences (in st_preferences) such that school j now ranks last
        
        done = c(done, TRUE)
        # school is in application list
        applicant_list = which(application_list == j) # vector of students who applied to this school
        # so if application_list is [1] 4 2 3 1
        # then applicant_list for school 4 would be [1] 1

        # determine the best applicant
        best_applicant = find_best_applicant(sch_preferences[,j], applicant_list)
        
        # save the best applicant just in case this is the last round necessary
        best_applicants = c(best_applicants, best_applicant)
        
        # now get all of the rejected applicants
        rejected_applicants = find_rejected_applicants(sch_preferences[,j], applicant_list)
        
        # now for each of the rejected applicants (if any, as evidenced by the is.na(...)), ban them from ever applying to this school again
        # rejected_applicants is a vector containing the ID of all students who got rejected, indices don't matter, so for example:
        # [1] 1 4
        # this means that this school j rejected students 1 and 4
        if (is.na(rejected_applicants) == FALSE) {
          # now that we rejected at least one student, we need to go into st_preferences and change their preferences
          for (index in 1:length(rejected_applicants)) {
            st_preferences = ban_student(st_preferences, rejected_applicants[index])
          }
        }
      }
    }
    round = round + 1
  }
  return(best_applicants)
}

system.time({
  pairing = gale_shapley(students, schools)
  # pairing is a num_schools-length vector where each element represents the student ID which is paired to the school (ID given by index in vector)
  for (i in 1:length(pairing)) {
    print(sprintf('School %s is paired to student %s', i, pairing[i]))
  }
})
