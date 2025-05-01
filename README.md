# Inequalties.education
Looking at the factors of what effects peoples beliefs of Inequalties in the classroom
# Load necessary packages
library(dplyr)
library(psych)

# Load data
GSS <- read.csv("GSS2022.csv")

################################################################################
# PHASE 1: VARIABLE EXAMINATION AND RECODING
################################################################################

# DEPENDENT VARIABLE: Belief that racial inequality is due to lack of education
table(GSS$racdif3)
GSS <- GSS %>%
  mutate(
    yes_race_ed = ifelse(racdif3 == 1, 1, 0),
    no_race_ed  = ifelse(racdif3 == 2, 1, 0)
  )

# INCOME AT AGE 16
table(GSS$incom16)
GSS <- GSS %>%
  mutate(
    far_below_average = ifelse(incom16 == 1, 1, 0),
    below_average     = ifelse(incom16 == 2, 1, 0),
    average           = ifelse(incom16 == 3, 1, 0),
    above_average     = ifelse(incom16 == 4, 1, 0),
    far_above_average = ifelse(incom16 == 5, 1, 0)
  )

# GENDER
table(GSS$sex)
GSS <- GSS %>%
  mutate(
    man   = ifelse(sex == 1, 1, 0),
    woman = ifelse(sex == 2, 1, 0)
  )

# REGION OF UPBRINGING
table(GSS$reg16)
GSS <- GSS %>%
  mutate(
    foreign              = ifelse(reg16 == 1, 1, 0),
    New_England          = ifelse(reg16 == 2, 1, 0),
    middle_atlantic      = ifelse(reg16 == 3, 1, 0),
    East_north_central   = ifelse(reg16 == 4, 1, 0),
    west_north_central   = ifelse(reg16 == 5, 1, 0),
    south_atlantic       = ifelse(reg16 == 6, 1, 0),
    east_south_atlantic  = ifelse(reg16 == 7, 1, 0),
    west_south_central   = ifelse(reg16 == 8, 1, 0),
    mountain             = ifelse(reg16 == 9, 1, 0),
    pacific              = ifelse(reg16 == 10, 1, 0)
  )

# RACE
table(GSS$race)
GSS <- GSS %>%
  mutate(
    White = ifelse(race == 1, 1, 0),
    Black = ifelse(race == 2, 1, 0),
    other = ifelse(race == 3, 1, 0)
  )

# EDUCATION LEVEL
table(GSS$educ)
GSS <- GSS %>%
  mutate(
    No_formal_schooling = ifelse(educ == 1, 1, 0),
    first_grade = ifelse(educ == 2, 1, 0),
    second_grade = ifelse(educ == 3, 1, 0),
    third_grade = ifelse(educ == 4, 1, 0),
    fourth_grade = ifelse(educ == 5, 1, 0),
    fifth_grade = ifelse(educ == 6, 1, 0),
    sixth_grade = ifelse(educ == 7, 1, 0),
    seventh_grade = ifelse(educ == 8, 1, 0),
    eighth_grade = ifelse(educ == 9, 1, 0),
    ninth_grade = ifelse(educ == 10, 1, 0),
    high_school = ifelse(educ == 11, 1, 0),
    college_degree = ifelse(educ == 12, 1, 0)
  )

# AGE GROUPINGS
table(GSS$age)
GSS <- GSS %>%
  mutate(
    teen_thru_thirty = ifelse(age >= 18 & age <= 30, 1, 0),
    thirtyone_thru_fifty = ifelse(age >= 31 & age <= 50, 1, 0),
    fiftyone_thru_seventy = ifelse(age >= 51 & age <= 70, 1, 0),
    seventyone_plus = ifelse(age >= 71, 1, 0)
  )
table(GSS$age, GSS$teen_thru_thirty)
table(GSS$age, GSS$thirtyone_thru_fifty)
table(GSS$age, GSS$fiftyone_thru_seventy)
table(GSS$age, GSS$seventyone_plus)

# NEIGHBORHOOD DIVERSITY
table(GSS$raclive)
GSS <- GSS %>%
  mutate(
    YES = ifelse(raclive == 1, 1, 0),
    NO  = ifelse(raclive == 2, 1, 0)
  )

# HEALTH
table(GSS$health)
GSS <- GSS %>%
  mutate(
    Excellent = ifelse(health == 1, 1, 0),
    Good      = ifelse(health == 2, 1, 0),
    Fair      = ifelse(health == 3, 1, 0),
    Poor      = ifelse(health == 4, 1, 0)
  )

################################################################################
# PHASE 2: CREATE ANALYSIS DATASET
################################################################################

my_varlist <- c(
  "yes_race_ed", "below_average", "average", "above_average", "far_above_average",
  "incom16", "racdif3", "reg16", "educ", "age", "teen_thru_thirty", "YES", "NO",
  "fiftyone_thru_seventy", "seventyone_plus", "first_grade", "second_grade",
  "third_grade", "fourth_grade", "fifth_grade", "sixth_grade", "seventh_grade",
  "eighth_grade", "ninth_grade", "thirtyone_thru_fifty", "White", "Black",
  "high_school", "college_degree", "race", "sex", "man", "woman", "foreign",
  "New_England", "middle_atlantic", "East_north_central", "west_north_central",
  "south_atlantic", "east_south_atlantic", "west_south_central", "mountain",
  "pacific", "other", "health", "Excellent", "Good", "Fair", "Poor" , "raclive"
)

my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

describe(my_dataset)

################################################################################
# PHASE 3: DESCRIPTIVE STATISTICS
################################################################################

table(my_dataset$racdif3)
table(my_dataset$incom16)
table(GSS$racdif3, GSS$below_average)
table(GSS$racdif3, GSS$average)
table(GSS$racdif3, GSS$above_average)
table(GSS$racdif3, GSS$far_above_average)

################################################################################
# PHASE 4: CONTINGENCY TABLES + CHI-SQUARE TESTS
################################################################################

table(my_dataset$incom16, my_dataset$racdif3)
chisq.test(table(my_dataset$incom16, my_dataset$racdif3))
cor(my_dataset$racdif3, my_dataset$incom16)

################################################################################
# PHASE 5: LOGISTIC REGRESSION
################################################################################

# m1: demographics
model1 <- glm(yes_race_ed ~ age + White + man, data = my_dataset, family = binomial)
summary(model1)

# m2: experiences at 16
model2 <- glm(yes_race_ed ~ average + above_average + far_above_average, data = my_dataset, family = binomial)
summary(model2)

# m3: social context
model3 <- glm(yes_race_ed ~ YES + educ + health, data = my_dataset, family = binomial)
summary(model3)

# m4: same as model3 – consider modifying or removing
model4 <- glm(yes_race_ed ~ high_school + Good + fiftyone_thru_seventy, data = my_dataset, family = binomial)
summary(model4)




