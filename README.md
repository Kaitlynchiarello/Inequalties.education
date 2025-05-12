data and variables 
3.0: Data and Methods
3.1: Data
This analysis uses data from the 2022 General Social Survey (GSS), a nationally representative survey of adults living in the United States. The GSS targets non-institutionalized adults aged 18 and older, using a multistage probability sampling method to ensure the representativeness of the sample. The survey is administered in person or online by the National Opinion Research Center (NORC) and collects data on a wide array of topics including social attitudes, demographic characteristics, and behaviors. While the full 2022 GSS includes responses from 4,194 individuals, our analytical sample consists of 1129. The discrepancy in sample size results from missing data on key variables of interest used in our models, as well as our exclusion criteria for certain subpopulations 
3.2: Variables Used
This section describes the dependent and independent variables used in our regression model. Table 1 summarizes descriptive statistics for each.
Dependent Variable:
Differences due to lack of Education. The question is on the average African Americans have worse job, income, and housing than white people. Do you think these differences are because more African Americans don’t have the chance for education? This is the dependent variable in our regression table (Table 3). It’s measured by yes racism is in education and no racism is not in education. People who answered this survey question either said yes, no or, I don’t know.  The dependent variable in our analysis is whether respondents believe racial disparities in job, income, and housing outcomes are due to African Americans not having access to equal educational opportunities. Responses were coded into a binary outcome where agreement indicates acknowledgment of racism in education and disagreement indicates rejection of that belief. Responses marked “I don’t know” were excluded from the final analysis to ensure interpretive clarity. In the final sample, a majority of respondents (X%) agreed that differences are due to unequal educational opportunity, while a minority (Y%) did not—a meaningful distribution for assessing patterns in public belief.
Key independent variables were income at 16 categorized under demographic from model A. Income at 16 is Socioeconomic status at age 16. Participants were asked compared with other families in the U.S., was your family income when you were 16 years old: far below average, below average, average, above average, or far above average?" The recoding was far below average, Below Average, Average, Above Average, and far above average. 
Region of upbringing is another key independent variable under the variable exposure from model A. It sought out cultural and structural regional differences in the U.S. Respondents were asked in what region of the U.S. did you live most of the time until age 16? Which was recoded with the variables foreign, New England, middle Atlantic, east north central, west north central, south atlantic, east south central, west north central, south Atlantic, east south Atlantic, west south central, mountain, and pacific. The exposure of a region of upbringing captures early geographic exposure, which may shape attitudes through local norms and structural inequalities. This regional spread allows for a meaningful exploration of cultural influences tied to geography.
Another key variable I used was Gender which was categorized under demographic from Model A. Gender is a socially constructed identity that may influence perspectives on inequality. The operalization was male and female. Gender is a demographic variable that may influence sensitivity to issues of inequality. Contact theory and education experience were influences that changed how each gender viewed the dependent variable beliefs of inequalities in education.
Another key concept that was used in my data was academic performance. The concept was personal attainment of education, and if their academic performance influenced their belief of inequalities in the classroom. The operlization is based on the highest year of school completed. The variables were in three main categories. Low (less than high school), Medium (high school diploma to some college), and High (college degree and above). This spread allows us to assess whether higher levels of education correlate with greater recognition of racism in education. Independent factors that influenced their beliefs of inequalities in education was contact theory.





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




