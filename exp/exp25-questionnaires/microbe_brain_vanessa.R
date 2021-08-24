library(tidyverse)
library(lubridate)

# Part 1: Screening Survey ------------------------------------------------

# Description:
# Two surveys, one in English and one in French, are imported as csv files 
# from the Limesurvey website.
# The order of the questions on the surveys differed slightly. 
# To correct this, the variable names are temporarily assigned [1-82] on the English survey.
# Next, the question order of the French survey is rearranged to mirror the English.
# Then, the FR variables are set temporarily to [1-82] also.
# The FR and EN surveys are bound together and the 52 variable names are replaced.
# The original survey questions have been imported into the tibble as rows of the csv file, 
# allowing the user to check question pair alignment and change names if needed.
# Next is to translate, set variable types, trim responses, fix BMI and NAs.


# Import EN and FR responses for screening participants w/ full questions selected from Limesurvey.
# Remove col names and adjust NAs.
en_screen_raw <- read_csv("en_screen.fullq.csv", col_names = FALSE, na = c("", "NA"))
fr_screen_raw <- read_csv("fr_screen.fullq.csv", col_names = FALSE, na = c("", "NA"))

# Select only relevant questions and temporarily set their col names to [1-82]. 
en_screen <- select(en_screen_raw, 1:82) %>% as_tibble() %>% set_names(., 1:82)

# Rearrange the FR order of columns to match those in EN, select cols, and name them [1-82] also.
fr_screen <- select(fr_screen_raw, 
             1:15, 
             19, 
             16:18, 
             20:42, 
             45:46, 
             43:44, 
             48, 
             47, 
             49:53, 
             63,
             54:62, 
             64:82) %>% as_tibble() %>% set_names(., 1:82)

# Bind EN and FR responses together, and remove additional unwanted columns. 
survey_screen <- rbind(en_screen,fr_screen)%>% select(., 1, 6, 17:18, 20:66, 82)


# Rename the 52 variables selected from above: cols = [1,6,17,18,20:66,82]. 
survey_screen <- set_names(survey_screen,
            "sampleID",
            "participant",
            "allergies",
            "diseases",
            "weight_kg",
            "unit_wt",
            "BMI1",
            "height_m",
            "unit_ht",
            "bmi",
            "BMI3",
            "obesity",
            "BMI4",
            "hard_exercise",
            "hard_exercise_comments",
            "easy_exercise",
            "easy_exercise_comments",
            "min_exercise",
            "min_exercise_comments",
            "heart_treatment",
            "high_bp",
            "dizzy_exercise",
            "fast_heartbeat",
            "shortness_breath",
            "fainting",
            "concussion",
            "concussion_comments",
            "pain_mobility",
            "pain_mobility_comments",
            "fam_heart_prob",
            "fam_heart_prob_comments",
            "stroke_head_inj",
            "handedness",
            "smoker",
            "drugs",
            "PET",
            "meds",
            "meds_comments",
            "sad",
            "uninterested",
            "depressed",
            "sleeping",
            "tired",
            "eating",
            "low_esteem",
            "concentrating",
            "slowness",
            "suicidal",
            "diff_level",
            "alcohol_per_wk",
            "alcohol_comments",
            "other_comments")

# Check that the FR and EN questions line up and column names are correct.

# Convert weights that are in pounds -> kg and height in inches -> meters.
survey_screen$weight_kg[survey_screen$weight_kg == "180"] <- "82"
survey_screen$weight_kg[survey_screen$weight_kg == "245"] <- "111"
survey_screen$weight_kg[survey_screen$weight_kg == "212"] <- "96"
survey_screen$weight_kg[survey_screen$weight_kg == "202"] <- "92"

survey_screen$height_m[survey_screen$height_m == "67"] <- "1.70"
survey_screen$height_m[survey_screen$height_m == "72"] <- "1.83"
survey_screen$height_m[survey_screen$height_m == "70"] <- "1.78"
survey_screen$height_m[survey_screen$height_m == "68"] <- "1.73"

# Remove the columns specifying the units since they have been added to col names already. 
survey_screen <- select(survey_screen,-unit_wt,-BMI1,-unit_ht,-BMI3,-BMI4)

# Pull out and create a separate tibble for the survey_screen questions in English. 
survey_screen_questions <- slice(survey_screen, 1) %>% pivot_longer(.,everything(),values_to = "screen")

# To compute BMI, first convert wt. and ht. to numeric. Then compute BMI.
survey_screen$weight_kg <- as.numeric(survey_screen$weight_kg)
survey_screen$height_m <- as.numeric(survey_screen$height_m)
survey_screen$bmi <- survey_screen$weight_kg/(survey_screen$height_m)^2

# Remove rows EN and FR questions, and the row that is incomplete. 
# Remove col1 and arrange by participant for the next step.
survey_screen <- slice(survey_screen, 2:9,11,12) %>% arrange(.,participant) %>% select(.,-1)


# Replace the removed col1 with the correct sample IDs by creating a new tibble 
# with the correct IDs, and then bind that as col 1 onto the survey_screen.
id <- tibble(sampleID = c("EN152001A", "EN152002A","EN152003A","EN152004A","EN152005A","EN152005A","EN152006A","EN152006A","FR152001A","FR152002A"))
as_tibble(id)
survey_screen <- cbind(id,survey_screen)


# Translation
survey_screen[survey_screen == "No"] <- "no"
survey_screen[survey_screen == "Non"] <- "no"
survey_screen[survey_screen == "Yes"] <- "yes"
survey_screen[survey_screen == "Oui"] <- "yes"
survey_screen[survey_screen == "N/A"] <- NA
survey_screen[survey_screen == "Not applicable"] <- NA

survey_screen[survey_screen == "Droitier"] <- "Right-handed"
survey_screen[survey_screen == "high blood pressure medication (medicated for 10y)"] <- "high bp med"
survey_screen[survey_screen == "high blood pressure medication."] <- "high bp med"

# Questions about alcohol consumption per week.
survey_screen[survey_screen == "Never"] <- 0
survey_screen[survey_screen == "Monthly or less"] <- 1
survey_screen[survey_screen == "2 à 4 fois par mois"] <- 2
survey_screen[survey_screen == "2 to 4 times a month"] <- 2
survey_screen[survey_screen == "2 to 3 times a week"] <- 3
survey_screen[survey_screen == "1 ou 2 verres"] <- 2
survey_screen[survey_screen == "1 or 2"] <- 2
survey_screen[survey_screen == "3 or 4"] <- 4


# Note: hard_exercise comments and concussion_comments don't make a lot of sense.
# Note: There are 2 participant 5's and 2 participant 6's. 


# Part 2: Inclusion Survey ------------------------------------------------

# Import EN and FR inclusion surveys w/ the full questions and remove col names.
en_inclusion_raw <- read_csv("en_inclusion.fullq.csv", col_names = FALSE, na = c("", "NA"))
fr_inclusion_raw <- read_csv("fr_inclusion.fullq.csv", col_names = FALSE, na = c("", "NA"))

# Add extra column to EN because "pain" question is missing. 
en_inclusion_match <- select(en_inclusion_raw, 1:116) %>% 
  add_column(., x = NA)

# Recombine English columns and name columns simply [1:133] for now.
en_inclusion <- cbind(en_inclusion_match, en_inclusion_raw[117:132]) %>% as_tibble() %>% set_names(., 1:133)

# Reassign FR columns 1:133 to match EN.
fr_inclusion <- select(fr_inclusion_raw, 1:133) %>% 
  as_tibble() %>% 
  set_names(., 1:133)

# Bind EN and FR together. Remove unwanted variables. That leaves 125 vars left.
survey_inclusion <- rbind(en_inclusion,fr_inclusion) %>% select(.,1,6,10,12:133)

# Rename cols = [1,6,10,12:133].
survey_inclusion <- set_names(survey_inclusion,
                              "sampleID",
                              "participant",
                              "birthday",
                              "edu",
                              "edu_comments", # Education
                              "income", # end
                              "white", # Race/ethnicity
                              "s_asian",
                              "chinese",
                              "filipino",
                              "black",
                              "latin",
                              "arab",
                              "se_asian",
                              "w_asian",
                              "korean",
                              "japanese",
                              "race", # end
                              "meds", # General Health Assessment
                              "meds_comments",
                              "ill",
                              "ill_comments",
                              "ill_dr",
                              "fast_heartbeat",
                              "leg_cramp",
                              "leg_cramp_comments", # end
                              "fam_heart_prob", # Family Medical History
                              "fam_heart_prob_comments",
                              "allergies",
                              "arthritis",
                              "pulmonary",
                              "cf",
                              "coronary",
                              "depression",
                              "mental",
                              "heart_attack",
                              "heart_murmur",
                              "high_bp",
                              "lung",
                              "implants",
                              "ibs",
                              "nutrition",
                              "neurological",
                              "seizures",
                              "cholesterol",
                              "vertigo",
                              "stroke",
                              "anxiety",
                              "health",
                              "health_comments", # end
                              "uninterested", # Mental Health Assessment
                              "depressed",
                              "sleeping",
                              "tired",
                              "eating",
                              "low_esteem",
                              "concentrating",
                              "slowness",
                              "suicidal",
                              "diff_level", # end
                              "love_food.ef", # AEBQ
                              "no_wo_trying.ff",
                              "enjoy_eating.ef",
                              "anticipate_meals.ef",
                              "annoyed_eating.eoe",
                              "belly_rumbling.hz",
                              "refuse_new_food.ff",
                              "worried_eating.eoe",
                              "irritated_if_skip.hz",
                              "upset_eating.eoe",
                              "leave_food.sr",
                              "enjoy_new_food.ffrev",
                              "stimulated_hunger.fr",
                              "finish_quickly.serev",
                              "worried_starve.eue",
                              "anxious_eating.eoe",
                              "would_eat_frequently.fr",
                              "angry_starve.eue",
                              "interested_new_food.ffrev",
                              "upset_starve.eue",
                              "angry_eating.eoe",
                              "think_food.fr",
                              "full_before_finished.sr",
                              "enjoy_variety.ffrev",
                              "last_finished.sez",
                              "slow_down.sez",
                              "annoyed_starve.eue",
                              "eat_immediately.hz",
                              "slow_eater.sez",
                              "snack_hinders.sr",
                              "easily_full.sr",
                              "often_hungry.hz",
                              "smell_want_eat.fr",
                              "light_headed_delay.hz",
                              "anxious_starve.eue", # end
                              "bedtime", # Pitt Sleep Quality Index
                              "time_fall_asleep_min",
                              "wakeup",
                              "asleep_hrs",
                              "in_bed_hrs",
                              "insomnia",
                              "interrupted_sleep",
                              "bathroom",
                              "cant_breathe",
                              "cough_snore",
                              "cold",
                              "hot",
                              "bad_dreams",
                              "pain",
                              "other_bad_sleep",
                              "sleep_meds",
                              "daytime_problems",
                              "other_sleep_prob",
                              "affects_motivation",
                              "sleep_qual_overall", # end
                              "solve_prob_try_hard", # General Self-Efficacy Scale
                              "overcome_opposition",
                              "easy_meet_goals",
                              "confident_unexpected",
                              "resourcefulness_unplanned",
                              "solve_nec_effort",
                              "stay_calm_cope",
                              "find_several_solutions",
                              "think_of_solution",
                              "handle_whatever") #end

# Survey_inclusion questions pulled out into a separate tibble.
survey_inclusion_questions <- slice(survey_inclusion,1)
survey_inclusion_questions <- pivot_longer(survey_inclusion_questions, everything(),values_to = "inclusion")

# Remove rows w/ FR and EN questions. Remove col1 so it can be replaced in the next step. Arrange by participant.
survey_inclusion <- slice(survey_inclusion, 2:7,9,10) %>% arrange(.,participant) %>% select(.,-1)

# Replace values under col1 "sampleID" with correct IDs.
p <- tibble(sampleID = c("EN152001B", "EN152002B","EN152003B","EN152004B","EN152005B","EN152006B","FR152001B","FR152002B"))
as_tibble(p)
survey_inclusion <- cbind(p,survey_inclusion)

# [4:6] Education level/income?
survey_inclusion[survey_inclusion == "Doctorate"] <- "dr"
survey_inclusion[survey_inclusion == "Masters"] <- "ma"
survey_inclusion[survey_inclusion == "Bachelors"] <- "ba"
survey_inclusion[survey_inclusion == "Baccalauréat"] <- "ba"
survey_inclusion[survey_inclusion == "College"] <- "college"
survey_inclusion[survey_inclusion == "Formation professionnelle ou pratique"] <- "vocational"
survey_inclusion[survey_inclusion == "Refuse to answer"] <- NA
survey_inclusion[survey_inclusion == "45,000$ to 59,999$"] <- "45-60"
survey_inclusion[survey_inclusion == "15,000$ to 29,999$"] <- "15-30"
survey_inclusion[survey_inclusion == "75,000$ or more"] <- "75 <"
survey_inclusion[survey_inclusion == "75 000$ ou plus"] <- "75 <"

# [7:18] Race/ethnicity
survey_inclusion[survey_inclusion == "No"] <- "no"
survey_inclusion[survey_inclusion == "Non"] <- "no"
survey_inclusion[survey_inclusion == "Oui"] <- "yes"
survey_inclusion[survey_inclusion == "Yes"] <- "yes"
survey_inclusion[survey_inclusion == "N/A"] <- NA

# [19: 26] General Health Assessment
survey_inclusion[survey_inclusion == "rhûme"] <- "cold"
survey_inclusion[survey_inclusion == "Insomnie"] <- "insomnia"

# [27: 49] Family Medical History + [50] comments -  Y and FM

# [51:60] Mental Health Assessment
survey_inclusion[survey_inclusion == "Not at all"] <- "no"
survey_inclusion[survey_inclusion == "Jamais"] <- "no"
survey_inclusion[survey_inclusion == "Several days"] <- "several_days"
survey_inclusion[survey_inclusion == "Plusieurs jours"] <- "several_days"
survey_inclusion[survey_inclusion == "Not applicable"] <- NA
survey_inclusion[survey_inclusion == "Ne s'applique pas"] <- NA
survey_inclusion[survey_inclusion == "Not difficult at all"] <- "no"
survey_inclusion[survey_inclusion == "Pas du tout difficile(s)"] <- "no"


# [61:95] Adult Eating Behaviors Questionnaire (AEBQ)
survey_inclusion[survey_inclusion == "Strongly disagree"] <- 1
survey_inclusion[survey_inclusion == "Disagree"] <- 2
survey_inclusion[survey_inclusion == "Neither agree or disagree"] <- 3
survey_inclusion[survey_inclusion == "Agree"] <- 4
survey_inclusion[survey_inclusion == "Strongly agree"] <- 5
survey_inclusion[survey_inclusion == "Pas du tout d'accord"] <- 1
survey_inclusion[survey_inclusion == "Pas d'accord"] <- 2
survey_inclusion[survey_inclusion == "Neutre"] <- 3
survey_inclusion[survey_inclusion == "D'accord"] <- 4
survey_inclusion[survey_inclusion == "Tout à fait d'accord"] <- 5


# [bedtime:sleep_qual_overall] [96:115] Pitt Sleep Quality Index 

# bedtime:in_bed_hrs [96:100] How long to fall asleep? etc.
survey_inclusion[survey_inclusion == "9 pm"] <- "21:00"
survey_inclusion[survey_inclusion == "30 minutes"] <- "30" 
survey_inclusion[survey_inclusion == "between 6 and 7"] <- "6.5"
survey_inclusion[survey_inclusion == "07:00"] <- "7:00" 
survey_inclusion[survey_inclusion == "5 30 am"] <- "5:30"
survey_inclusion[survey_inclusion == "5:30 am"] <- "5:30"
survey_inclusion[survey_inclusion == "vers 10:30 pm"] <- "22:30"

# insomnia:daytime_problems [101:112] How often trouble sleeping bc ___?
survey_inclusion[survey_inclusion == "Not during the past month"] <- 0
survey_inclusion[survey_inclusion == "Less than once a week"] <- 1
survey_inclusion[survey_inclusion == "Once or twice a week"] <- 2
survey_inclusion[survey_inclusion == "Three or more times a week"] <- 3
survey_inclusion[survey_inclusion == "Pas au cours du dernier mois"] <- 0
survey_inclusion[survey_inclusion == "Moins d'une fois par semaine"] <- 1
survey_inclusion[survey_inclusion == "Une ou deux fois par semaine"] <- 2
survey_inclusion[survey_inclusion == "Trois ou quatre fois par semaine"] <- 3

# other_sleep_prob [113] ? 

# affects_motivation [114] How much of a problem to keep enthusiasm to get things done?
survey_inclusion[survey_inclusion == "Not a problem at all"] <- 0
survey_inclusion[survey_inclusion == "Pas du tout un problème"] <- 0
survey_inclusion[survey_inclusion == "Only a small problem"] <- 1
survey_inclusion[survey_inclusion == "Seulement un tout petit problème"] <- 1

# sleep_qual_overall [115] How would you rate overall sleep quality?  
survey_inclusion[survey_inclusion == "Very good"] <- 0
survey_inclusion[survey_inclusion == "Fairly good"] <- 1
survey_inclusion[survey_inclusion == "Assez bonne"] <- 1
survey_inclusion[survey_inclusion == "Fairly bad"] <- 2


# [116:125] General Self-Efficacy Scale 
survey_inclusion[survey_inclusion == "Not at all true"] <- 1
survey_inclusion[survey_inclusion == "Hardly true"] <- 2
survey_inclusion[survey_inclusion == "Moderately true"] <- 3
survey_inclusion[survey_inclusion == "Plutôt vrai"] <- 3
survey_inclusion[survey_inclusion == "Exactly true"] <- 4
survey_inclusion[survey_inclusion == "Tout à fait vrai"] <- 4


# Convert birthdays into dates. 
survey_inclusion <- separate(survey_inclusion, birthday, 
                             sep = " ", 
                             into = c("birthday","delete")) %>% survey_inclusion[-4]
survey_inclusion$birthday <- as_date(survey_inclusion$birthday)



# Part 3: End Survey ------------------------------------------------------

# Import EN and FR End Survey w/ full questions and remove col names.
en_end_raw <- read_csv("en_end.fullq.csv", col_names = FALSE, na = c("", "NA"))
fr_end_raw <- read_csv("fr_end.fullq.csv", col_names = FALSE, na = c("", "NA"))

# Add extra column to EN because "pain" question is missing. 
en_end_match <- select(en_end_raw, 1:72) %>% 
  add_column(., x = NA)

# Recombine English columns and rename simply ["1:89"], for now.
en_end <- cbind(en_end_match, en_end_raw[73:88]) %>% as_tibble() %>% set_names(., 1:89)

# Rename FR columns ["1:89"] to match EN.
fr_end <- select(fr_end_raw, 1:89) %>% 
  as_tibble() %>% 
  set_names(., 1:89)

# Bind EN and FR together, and remove unwanted columns. Produces 82 variables.
survey_end <- rbind(en_end,fr_end) %>% select(.,1,6,10:89)

# Rename columns = [1,6,10:89].
survey_end <- set_names(survey_end,
                     "sampleID",
                     "participant",
                     "ill",
                     "ill_comments",
                     "meds",
                     "meds_comments",
                     "ill_dr",
                     "uninterested",
                     "depressed",
                     "sleeping",
                     "tired",
                     "eating",
                     "low_esteem",
                     "concentrating",
                     "slowness",
                     "suicidal",
                     "diff_level",
                     "love_food.ef",
                     "no_wo_trying.ff",
                     "enjoy_eating.ef",
                     "anticipate_meals.ef",
                     "annoyed_eating.eoe",
                     "belly_rumbling.hz",
                     "refuse_new_food.ff",
                     "worried_eating.eoe",
                     "irritated_if_skip.hz",
                     "upset_eating.eoe",
                     "leave_food.sr",
                     "enjoy_new_food.ffrev",
                     "stimulated_hunger.fr",
                     "finish_quickly.serev",
                     "worried_starve.eue",
                     "anxious_eating.eoe",
                     "would_eat_frequently.fr",
                     "angry_starve.eue",
                     "interested_new_food.ffrev",
                     "upset_starve.eue",
                     "angry_eating.eoe",
                     "think_food.fr",
                     "full_before_finished.sr",
                     "enjoy_variety.ffrev",
                     "last_finished.sez",
                     "slow_down.sez",
                     "annoyed_starve.eue",
                     "eat_immediately.hz",
                     "slow_eater.sez",
                     "snack_hinders.sr",
                     "easily_full.sr",
                     "often_hungry.hz",
                     "smell_want_eat.fr",
                     "light_headed_delay.hz",
                     "anxious_starve.eue",
                     "bedtime",
                     "time_fall_asleep_min",
                     "wakeup",
                     "asleep_hrs",
                     "in_bed_hrs",
                     "insomnia",
                     "interrupted_sleep",
                     "bathroom",
                     "cant_breathe",
                     "cough_snore",
                     "cold",
                     "hot",
                     "bad_dreams",
                     "pain",
                     "other_bad_sleep",
                     "sleep_meds",
                     "daytime_problems",
                     "other_sleep_prob",
                     "affects_motivation",
                     "sleep_qual_overall",
                     "solve_prob_try_hard",
                     "overcome_opposition",
                     "easy_meet_goals",
                     "confident_unexpected",
                     "resourcefulness_unplanned",
                     "solve_nec_effort",
                     "stay_calm_cope",
                     "find_several_solutions",
                     "think_of_solution",
                     "handle_whatever")

# Pull out and isolate End Survey Questions for reference.
survey_end_questions <- slice(survey_end,1) %>% pivot_longer(.,everything(),values_to = "end")

# Remove row w/ EN and FR questions. Remove col1 so it can be replaced in the next step. 
# Arrange by participant.
survey_end <- slice(survey_end, -1,-6) %>% arrange(.,participant) %>% select(.,-1)

# Replace values under col1 "sampleID" with correct IDs.
k <- tibble(sampleID = c("EN152001A", "EN152002A","EN152003A","EN152005A","FR152001A","FR152002A"))
as_tibble(k)
survey_end <- cbind(k,survey_end)


# [3:7] Ill, meds, doctor visit
survey_end[survey_end == "Non"] <- "no"
survey_end[survey_end == "Oui"] <- "yes"
survey_end[survey_end == "Yes"] <- "yes"
survey_end[survey_end == "No"] <- "no"
survey_end[survey_end == "N/A"] <- NA

# [8:16] Mental Health Assessment
survey_end[survey_end == "Not at all"] <- "no"
survey_end[survey_end == "Jamais"] <- "no"
survey_end[survey_end == "Several days"] <- "several_days"
survey_end[survey_end == "Plusieurs jours"] <- "several_days"
survey_end[survey_end == "Not applicable"] <- NA
survey_end[survey_end == "Ne s'applique pas"] <- NA
survey_end[survey_end == "Not difficult at all"] <- "no"
survey_end[survey_end == "Pas du tout difficile(s)"] <- "no"

# [18:52] AEBQ
survey_end[survey_end == "Strongly disagree"] <- 1
survey_end[survey_end == "Pas du tout d'accord"] <- 1
survey_end[survey_end == "Disagree"] <- 2
survey_end[survey_end == "Pas d'accord"] <- 2
survey_end[survey_end == "Neither agree or disagree"] <- 3
survey_end[survey_end == "Neutre"] <- 3
survey_end[survey_end == "Agree"] <- 4
survey_end[survey_end == "D'accord"] <- 4
survey_end[survey_end == "Strongly agree"] <- 5
survey_end[survey_end == "Tout à fait d'accord"] <- 5

# Pitt Sleep Quality Index
# bedtime:in_bed_hrs [53:57] How long to fall asleep? etc.
survey_end[survey_end == "11:00 PM"] <- "23:00"
survey_end[survey_end == "8:00 AM"] <- "8:00"
survey_end[survey_end == "5:55"] <- "6"
survey_end[survey_end == "7:30"] <- "7.5"
survey_end$wakeup[survey_end$wakeup == "7"] <- "7:00"

# insomnia:daytime_problems [58:69] How often trouble sleeping bc ___?
survey_end[survey_end == "Not during the past month"] <- 0
survey_end[survey_end == "Less than once a week"] <- 1
survey_end[survey_end == "Once or twice a week"] <- 2
survey_end[survey_end == "Three or more times a week"] <- 3
survey_end[survey_end == "Pas au cours du dernier mois"] <- 0
survey_end[survey_end == "Moins d'une fois par semaine"] <- 1
survey_end[survey_end == "Une ou deux fois par semaine"] <- 2
survey_end[survey_end == "Trois ou quatre fois par semaine"] <- 3

# other_sleep_prob [70] ? 

# affects_motivation [71] How much of a problem to keep enthusiasm to get things done?
survey_end[survey_end == "Not a problem at all"] <- 0
survey_end[survey_end == "Pas du tout un problème"] <- 0
survey_end[survey_end == "Only a small problem"] <- 1
survey_end[survey_end == "Seulement un tout petit problème"] <- 1

# sleep_qual_overall [72] How would you rate overall sleep quality? 
survey_end[survey_end == "Very good"] <- 0
survey_end[survey_end == "Fairly good"] <- 1
survey_end[survey_end == "Assez bonne"] <- 1
survey_end[survey_end == "Fairly bad"] <- 2

# [73:82] General Self-Efficacy Scale 
survey_end[survey_end == "Not at all true"] <- 1
survey_end[survey_end == "Hardly true"] <- 2
survey_end[survey_end == "Moderately true"] <- 3
survey_end[survey_end == "Plutôt vrai"] <- 3
survey_end[survey_end == "Exactly true"] <- 4
survey_end[survey_end == "Tout à fait vrai"] <- 4


# Part 4: Merge Surveys ---------------------------------------------------
# Check if screen survey has anything valuable. Bind them together. Then create a summary table.

surveys_merged <- full_join(survey_inclusion,survey_end) %>% arrange(.,participant)

# Merge survey questions into 1 tibble by question abbreviation (col name).
survey_q_merged <- full_join(survey_screen_questions,survey_inclusion_questions)
survey_q_merged <- full_join(survey_q_merged,survey_end_questions)


# Part 5: Scores Adult Eating Behaviors ----------------------------------------------------------

# Answer Key for AEB, strongly disagree = 1, disagree = 2, neutral = 3, agree = 4, strongly agree = 5

# 8 Categories
# 1. Enjoy food (3 questions)(ef)
# 2. Emotional over-eating (5 questions)(eoe)
# 3. Emotional under-eating (5 questions)(eue)
# 4. Food fussiness (5 questions)(ff) and (ffrev)
# 5. Food responsiveness (4 questions)(fr)
# 6. Hunger (5 questions)(hz)
# 7. Slowness in eating (4 questions)(sez) and (serev)
# 8. Satiety responsiveness (4 questions)(sr)

# Reverse the scoring for the following 4 questions.
surveys_merged$enjoy_new_food.ffrev[surveys_merged$enjoy_new_food.ffrev == "1"] <- 5
surveys_merged$enjoy_new_food.ffrev[surveys_merged$enjoy_new_food.ffrev == "2"] <- 4
surveys_merged$enjoy_new_food.ffrev[surveys_merged$enjoy_new_food.ffrev == "3"] <- 3
surveys_merged$enjoy_new_food.ffrev[surveys_merged$enjoy_new_food.ffrev == "4"] <- 2
surveys_merged$enjoy_new_food.ffrev[surveys_merged$enjoy_new_food.ffrev == "5"] <- 1

surveys_merged$enjoy_variety.ffrev[surveys_merged$enjoy_variety.ffrev == "1"] <- 5
surveys_merged$enjoy_variety.ffrev[surveys_merged$enjoy_variety.ffrev == "2"] <- 4
surveys_merged$enjoy_variety.ffrev[surveys_merged$enjoy_variety.ffrev == "3"] <- 3
surveys_merged$enjoy_variety.ffrev[surveys_merged$enjoy_variety.ffrev == "4"] <- 2
surveys_merged$enjoy_variety.ffrev[surveys_merged$enjoy_variety.ffrev == "5"] <- 1

surveys_merged$interested_new_food.ffrev[surveys_merged$interested_new_food.ffrev == "1"] <- 5
surveys_merged$interested_new_food.ffrev[surveys_merged$interested_new_food.ffrev == "2"] <- 4
surveys_merged$interested_new_food.ffrev[surveys_merged$interested_new_food.ffrev == "3"] <- 3
surveys_merged$interested_new_food.ffrev[surveys_merged$interested_new_food.ffrev == "4"] <- 2
surveys_merged$interested_new_food.ffrev[surveys_merged$interested_new_food.ffrev == "5"] <- 1

surveys_merged$finish_quickly.serev[surveys_merged$finish_quickly.serev == "1"] <- 5
surveys_merged$finish_quickly.serev[surveys_merged$finish_quickly.serev == "2"] <- 4
surveys_merged$finish_quickly.serev[surveys_merged$finish_quickly.serev == "3"] <- 3
surveys_merged$finish_quickly.serev[surveys_merged$finish_quickly.serev == "4"] <- 2
surveys_merged$finish_quickly.serev[surveys_merged$finish_quickly.serev == "5"] <- 1

# Add new columns to existing surveys_merged for means of 8 AEBQ sections

surveys_merged[,61:95] <- lapply(surveys_merged[,61:95],as.numeric)
ef <- select(surveys_merged,
               ends_with("ef"))
eoe <- select(surveys_merged,
              ends_with("eoe"))
eue <- select(surveys_merged,
               ends_with("eue"))
ff <- select(surveys_merged,
               ends_with("ff"),
               ends_with("ffrev"))
fr <- select(surveys_merged,
               ends_with("fr"))
sez <- select(surveys_merged,
               ends_with("sez"),
               ends_with("serev"))
sr <- select(surveys_merged,
               ends_with("sr"))
hz <- select(surveys_merged,
               ends_with("hz"))
surveys_merged %>% mutate(mean_enjoy_food = rowMeans(ef),
                          mean_emo_over_eat = rowMeans(eoe),
                          mean_emo_under_eat = rowMeans(eue),
                          mean_food_fussiness = rowMeans(ff),
                          mean_food_response = rowMeans(fr),
                          mean_hunger = rowMeans(hz),
                          mean_slow_eat = rowMeans(sez),
                          mean_satiety_response = rowMeans(sr)
                          ) -> surveys_merged


# Part 6: Scores Pittsburgh Sleep Quality Index ---------------------------

# Score key: 0<=21: lowest is no problems, 21 is severe problems.
# change back the NAs
# Since 'time_fall_asleep_min' and 'pain' have NAs, change to 0 for scoring.
# replace_na(surveys_merged, list(
#   time_fall_asleep_min = 0,
#   pain = 0)
#   ) -> surveys_merged

surveys_merged[,97] %>% as.numeric() -> surveys_merged[,97]
surveys_merged[,99:112] <- lapply(surveys_merged[,99:112],as.numeric)
surveys_merged[,114:125] <- lapply(surveys_merged[,114:125],as.numeric)

# Component 1 = AS IS -> sleep_qual_overall [115]
surveys_merged %>% mutate(comp1 = sleep_qual_overall) -> surveys_merged


# Component 2 = CONVERT, THEN -> re-score (insomnia + time_fall_asleep)
# AS IS -> insomnia [101] How often trouble sleeping bc can't sleep w/in 30 min.?
# Scoring for: time_fall_asleep_min [97]
# 0 = 1-15
# 1 = 16-30
# 2 = 31-60
# 3 = 61+
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "2"] <- 0
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "5"] <- 0
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "5"] <- 0
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "10"] <- 0
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "15"] <- 0
surveys_merged$time_fall_asleep_min[surveys_merged$time_fall_asleep_min == "30"] <- 1

comp2 <- select(surveys_merged,time_fall_asleep_min,insomnia)
surveys_merged %>% mutate(comp2 = rowSums(comp2, na.rm = TRUE)) -> surveys_merged

# Re-score sum to comp2
surveys_merged$comp2[surveys_merged$comp2 == "2"] <- 1
surveys_merged$comp2[surveys_merged$comp2 == "3"] <- 2
surveys_merged$comp2[surveys_merged$comp2 == "4"] <- 2


#surveys_merged$comp2 <- ifelse(is.na(surveys_merged$time_fall_asleep_min), "2+", surveys_merged$comp2)

# Component 3 = CONVERT TO SCORE, THEN AS IS -> asleep_hrs [99]
surveys_merged %>% mutate(comp3 = asleep_hrs) -> surveys_merged
# 0 = >7 hrs.
# 1 = 6-7
# 2 = 5<6
# 3 = <5
surveys_merged$comp3[surveys_merged$comp3 == "8.5"] <- 0
surveys_merged$comp3[surveys_merged$comp3 == "8"] <- 0
surveys_merged$comp3[surveys_merged$comp3 == "7.5"] <- 0
surveys_merged$comp3[surveys_merged$comp3 == "7"] <- 1
surveys_merged$comp3[surveys_merged$comp3 == "6.5"] <- 1
surveys_merged$comp3[surveys_merged$comp3 == "6"] <- 1
surveys_merged$comp3[surveys_merged$comp3 == "5.5"] <- 2


# Component 4 = (asleep_hrs[99]/in_bed_hrs). THEN RE-SCORE.
surveys_merged %>% mutate(comp4 = asleep_hrs/in_bed_hrs) -> surveys_merged

surveys_merged$comp4[surveys_merged$comp4 == ".84"] <- 0
inbed <- surveys_merged[96:100] %>% as.numeric()


bed_calc <- tibble(x=1:14, y=c(7,7.5,7.5,6.5,9,9,8.5,7.5,7.5,8.5,7,7.75,6.5,7.5))

# 
# 1 7
# 2 7.5
# 3: 7.5<-6.00
# 4 6.5
# 5 9
# 6 9
# 7: 8.5<-7.75
# 8 7.5
# 9 7.5
# 10: 8.5<-8.00
# 11: 7.0<-8.00
# 12: 7.75<-9.00
# 13: 6.5<-7.00
# 14: 7.5<-7.00

# Re-Score:
# 0 = 0.84+
# 1 = 0.75-0.84
# 2 = 0.65-0.74
# 3 = <0.65



# Component 5 = Add up [interrupted_sleep:other_bad_sleep] [102:110], THEN CONVERT.
comp5 <- select(surveys_merged,interrupted_sleep:other_bad_sleep)
comp5_sums <- rowSums(comp5, na.rm = TRUE)
surveys_merged %>% mutate(comp5 = comp5_sums) -> surveys_merged
# Scoring:
# 0 = 0
# 1 = 1-9
# 2 = 10-18
# 3 = 19-27
surveys_merged$comp5[surveys_merged$comp5 %in% 1:9] <- 1
surveys_merged$comp5[surveys_merged$comp5 %in% 10:18] <- 2
surveys_merged$comp5[surveys_merged$comp5 %in% 19:27] <- 3

# surveys_merged$comp5[surveys_merged$comp5 >=1 & surveys_merged$comp5 <=9 ] <- 1



# Component 6 = AS IS -> sleep_meds [111]
surveys_merged %>% mutate(comp6 = sleep_meds) -> surveys_merged


# Component 7 = mean(daytime_problems [112] + affects_motivation [114])
comp7 <- select(surveys_merged,daytime_problems,affects_motivation)
surveys_merged %>% mutate(comp7 = rowMeans(comp7)
                          )-> surveys_merged

# Global PSQI Score = Add up the 7 component scores together
PSQI <- select(surveys_merged,comp1:comp7)
surveys_merged %>% mutate(sleep_qual_score = rowSums(PSQI)) -> surveys_merged



# Part 7: Scores Stress -----------------------------------
# Generalized Self-efficacy Scale (10 items; Schwarzer & Jerusalem, 1995)
# Scoring: Summation of all 10 questions 10<=40

GSES <- select(surveys_merged,solve_prob_try_hard:handle_whatever)
surveys_merged %>% mutate(self_efficacy_score = rowSums(GSES)) -> surveys_merged

# 
# 
# 
# 
# 
# 

# Appendix: Brittany's Notes ----------------------------------------------
# 
# 
### experimenting
# AEBQ <- select(surveys_merged,
#                ends_with("ef"),
#                ends_with("eoe"),
#                ends_with("eue"),
#                ends_with("ff"),
#                ends_with("ffrev"),
#                ends_with("fr"),
#                ends_with("sez"),
#                ends_with("sr"),
#                ends_with("srrev"),
#                ends_with("hz"))
# 
# AEBQ <- lapply(AEBQ,as.numeric) %>% as_tibble() %>% cbind(surveys_merged[1])
# eating <- lapply(AEBQ[,61:95],as.numeric) %>% as_tibble %>% cbind(surveys_merged[1:2],.)
# 
# ### 
# eating <- lapply(surveys_merged[,61:95],as.numeric) %>% as_tibble %>% cbind(surveys_merged[1:2],.)
# eatingnew <- gather(eating,
#                     'love_food.ef',
#                     'no_wo_trying.ff',
#                     'enjoy_eating.ef',
#                     'anticipate_meals.ef',
#                     'annoyed_eating.eoe',
#                     'belly_rumbling.hz',
#                     'refuse_new_food.ff',
#                     'worried_eating.eoe',
#                     'irritated_if_skip.hz',
#                     'upset_eating.eoe',
#                     'leave_food.sr',
#                     'enjoy_new_food.ffrev',
#                     'stimulated_hunger.fr',
#                     'finish_quickly.serev',
#                     'worried_starve.eue',
#                     'anxious_eating.eoe',
#                     'would_eat_frequently.fr',
#                     'angry_starve.eue',
#                     'interested_new_food.ffrev',
#                     'upset_starve.eue',
#                     'angry_eating.eoe',
#                     'think_food.fr',
#                     'full_before_finished.sr',
#                     'enjoy_variety.ffrev',
#                     'last_finished.sez',
#                     'slow_down.sez',
#                     'annoyed_starve.eue',
#                     'eat_immediately.hz',
#                     'slow_eater.sez',
#                     'snack_hinders.sr',
#                     'easily_full.sr',
#                     'often_hungry.hz',
#                     'smell_want_eat.fr',
#                     'light_headed_delay.hz',
#                     'anxious_starve.eue',
#                     key = "question", value = "answer",
#                     na.rm = FALSE, convert = FALSE, factor_key = FALSE)
# 
# ?rowMeans()
# 
# ?endsWith()
# ?where()
# ?rowmeans()
# r <- AEBQ %>% select(ends_with("eoe"),sampleID) %>% rowwise(sampleID)
# 
# 
# ?summarise()
# eoe <- mean(as.numeric(select(surveys_merged[1,], ends_with("eoe"))))
# eue <- mean(as.numeric(select(surveys_merged[1,], ends_with("eue"))))
# ff <- mean(as.numeric(select(surveys_merged[1,], ends_with("ff"))))#
# fr <- mean(as.numeric(select(surveys_merged[1,], ends_with("fr"))))
# sez <- mean(as.numeric(select(surveys_merged[1,], ends_with("se"))))
# sr <- mean(as.numeric(select(surveys_merged[1,], ends_with("sr"))))#
# hz <- mean(as.numeric(select(surveys_merged[1,], ends_with("h"))))
# 
# h
# summarise_if(surveys_merged)
# 
# 
# print(count(surveys_merged))
# 
# for (row in surveys_merged) {

# 
# 
# # str function is for returning structure.
# str(survey_inclusion)  or  str(survey_inclusion[,61:95])
# 
# # package: plyr.
# 
# 
# lapply returns list.
# survey_inclusion[,61:95] <- lapply(survey_inclusion[,61:95],as.numeric)
# 
# # notes for B.
# lapply(list, function(x){
#   as.numeric()})
# lapply(survey_inclusion,function(x){
#   blob <- as.numeric()
#   as.numeric()
#   return(list = blob1)
# })
# 
# apply(survey_inclusion,1,as.numeric)
# # 1 = rows, 2 = cols

# Compile scores
# Stress scale
# names in the Ethics
# ranges: age, BMI before/after, participants with these characteristics, 
# we saw these effects.. 

# Training 3x over 8 weeks, biking, calories burned, maxO2, etc., measure of compliance
# Lax training can account for decreased effects, possibly. 
# onterotype microgut comp. defines those clusters. classify ppl by 3/4 types of gut microbes
# continuum of classes
# dcs