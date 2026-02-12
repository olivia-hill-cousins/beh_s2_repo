wd <- "Library/CloudStorage/OneDrive-UniversityofExeter/PhD/Beh. Study (Replication)/beh_s2_repo/data cleaning and analysis"
set.platform <- function(subdir = "") {
  base_wd <- if (Sys.info()[["sysname"]] == "Darwin") {
    file.path("~/",wd)
  } else if (Sys.info()[["sysname"]] == "Windows") {
    file.path("C:/Users/~/",wd)
  } else {
    stop("Unknown operating system, set your working directory manually.")
  }
  # allow empty subdir for top level, or paste others
  full_wd <- if (subdir == "") base_wd else file.path(base_wd, subdir)
  setwd(full_wd)
}
set.platform()
### set seed for reproducibility
set.seed(123)

### Load in some packages
# package names 
packages <- c("dplyr","tidyr")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
library(dplyr)
library(tidy)

## Read in Data

set.platform("data_raw")
# Where key for yes=F
dataF <- read.csv("data_exp_249979-v6_task-nzt8.csv", header=TRUE, na.strings="")
# Where key for yes=J
dataJ <- read.csv("data_exp_249979-v6_task-hrcr.csv", header=TRUE, na.strings = "")
# demographics
demo <- read.csv("data_exp_249979-v6_questionnaire-qemh.csv", header=TRUE, na.strings="")

## Join Key-Segregated Data

# combine data
df <- rbind(dataF, dataJ)

# keep only data rows with moral judgements, remove irrelevant rows (e.g., instructions, etc)
df <- df %>% 
  filter(Response.Type == "response")

# select data we need 
df <- df %>% 
  dplyr::select(Spreadsheet..TVideo_1, Participant.Private.ID, Task.Name, Trial.Number, 
                Spreadsheet..Dilemma, Spreadsheet..Context, Spreadsheet..DilemmaByContext, 
                # Spreadsheet..Prohibited.act, Spreadsheet..PDE, Spreadsheet..PDE.noBA, Spreadsheet..PDE.GE.BE, 
                # Spreadsheet..PDE.SE_notME, Spreadsheet..MG.Permissible, Spreadsheet..Agent, 
                Absolute.Reaction.Time, Response, randomiser.zmvq)

# rename columns to make them easier to work with
df <- df %>%
  rename(TVideo_1 = Spreadsheet..TVideo_1,
         # PBH = Spreadsheet..Prohibited.act,
         # PDE = Spreadsheet..PDE,
         # BA = Spreadsheet..PDE.noBA,
         # MC = Spreadsheet..PDE.GE.BE,
         # intent = Spreadsheet..PDE.SE_notME,
         # MG = Spreadsheet..MG.Permissible,
         RT = Absolute.Reaction.Time,
         ID = Participant.Private.ID,
         MJ = Response,
         #Agent = Spreadsheet..Agent,
         Dilemma = Spreadsheet..DilemmaByContext,
         dilemmaAll = Spreadsheet..Dilemma,
         Context = Spreadsheet..Context,
         keys = randomiser.zmvq)

## Add variable values back in 
stimuli_full_spreadsheet <- read.csv("stimuli_full_spreadsheet.csv")

# rename variables to be consistent with main data
stimuli_full_spreadsheet <- stimuli_full_spreadsheet %>%
  rename(PBH = PBH,
         PDE = PDE,
         Agent = Agent,
         BA = PDE.noBA,
         MC = PDE.GE.BE,
         intent = PDE.SE_notME,
         MG = MG.Permissible)

# Select only the columns you want to merge 
design_vars <- stimuli_full_spreadsheet %>% 
  dplyr::select(TVideo_1, PBH, PDE, Agent, BA, MC, intent, MG) 

# Merge ONLY these columns into data 
df <- df %>% left_join(design_vars, by = "TVideo_1")


# filter demo to only include rows from the Gorilla task where generic demographics were collected
demo <- demo %>% 
  filter(Task.Name == "Generic Demographics")

# 1. Pivot longer for .text columns (get ID, question_col, and text_response)
text_cols <- grep("\\.text$", names(demo), value = TRUE)

text_long <- demo %>%
  dplyr::select(Participant.Private.ID, all_of(text_cols)) %>%
  pivot_longer(cols = -Participant.Private.ID, names_to = "text_col", values_to = "text_response") %>%
  filter(!is.na(text_response) & text_response != "")

# 2. Get cultural columns starting with the prefix but excluding .text or .quantised
cultural_cols <- grep("^questionnaire\\.qemh\\.cultural\\.", names(demo), value = TRUE)
cultural_cols <- cultural_cols[!str_detect(cultural_cols, "\\.text$|\\.quantised$")]

# 3. Pivot longer on cultural columns for all participants
cultural_long <- demo %>%
  dplyr::select(Participant.Private.ID, all_of(cultural_cols)) %>%
  pivot_longer(cols = -Participant.Private.ID, names_to = "cultural_col", values_to = "cultural_value") %>%
  filter(!is.na(cultural_value) & cultural_value != "")

# 4. Combine by participant ID
# First filter cultural_long to only those participants who have text responses
combined <- cultural_long %>%
  semi_join(text_long, by = "Participant.Private.ID") %>%
  left_join(text_long, by = "Participant.Private.ID")




# keep only data rows in demographics we need not all the other stuff gorilla gives you
demo <- demo %>% 
  filter(Question.Key == "Gender"| Question.Key == "Age" | Question.Key == "cultural-arab"| Question.Key == "cultural-arab-text"|  Question.Key == "cultural-asian"| Question.Key == "cultural-asian-text"| Question.Key == "cultural-black"| Question.Key == "cultural-black-text"|  Question.Key == "cultural-multiple"| Question.Key == "cultural-multiple-text"| Question.Key == "cultural-white"| Question.Key == "cultural-white-text"| Question.Key == "cultural-prefNo"| Question.Key == "cultural-prefNo-text"|  Question.Key == "cultural-another"| Question.Key == "cultural-another-text")

# separate the age and gender into two separate variables - wide form
demowide<-spread(data=demo, key=Question.Key, value=MJ)

# now add the demographics to the data
df <- merge(df,demowide, 
            by="Participant.Private.ID")

# load in demo from prolific
prolific_demo <- read.csv("~.csv")

# filter prolific_demo by column: Status so removes rows w. "RETURNED" or "TIMED-OUT" 
prolific_demo <- prolific_demo %>% 
  filter(Status != "RETURNED" & Status != "TIMED-OUT")

# next, we create a shared column between the two datasets
# prolific incl. a column called Participant.id which corresponds to the ID column called Participant.Public.ID in demo data
# therefore, we will rename the Participant.Public.ID column in demo to Participant.id
demo <- demo %>%
  rename(Participant.id = "Participant.Public.ID")

# now we can add the nationality data from prolific_demo to demo using the Participant.id column to ensure these are added to the correct Ps
demo <- demo %>%
  left_join(prolific_demo %>% dplyr::select(Participant.id, Nationality), by = "Participant.id")


### Cultural demographic data from Gorilla
# pivot longer for .text columns (get ID, question_col, and text_response)
text_cols <- grep("\\.text$", names(demo), value = TRUE)

text_long <- demo %>%
  dplyr::select(Participant.Private.ID, all_of(text_cols)) %>%
  pivot_longer(cols = -Participant.Private.ID, names_to = "text_col", values_to = "text_response") %>%
  filter(!is.na(text_response) & text_response != "")

# get cultural columns starting with the prefix but excluding .text or .quantised
cultural_cols <- grep("^questionnaire\\.qemh\\.cultural\\.", names(demo), value = TRUE)
cultural_cols <- cultural_cols[!str_detect(cultural_cols, "\\.text$|\\.quantised$")]

# for ps that put other / multiple, we might need to manually add these into the correct column
demo <- demo %>% 
  mutate(
    questionnaire.qemh.cultural.white = case_when(
      Participant.Private.ID == "14606099" ~ "British or English or Scottish or Welsh or Northern Irish",
      Participant.Private.ID == "14606101" ~ "Dutch",
      Participant.Private.ID == "14606130" ~ "Portuguese",
      Participant.Private.ID == "14606511" ~ "Italian",
      .........
      TRUE ~ questionnaire.qemh.cultural.white
    ),
    questionnaire.qemh.cultural.multiple = case_when(
      Participant.Private.ID == "14606099"  ~ NA_character_,
      Participant.Private.ID == "14606626" ~ "Latino",
      Participant.Private.ID == "14609697" ~ "Mexican-Japanese",
      TRUE ~ questionnaire.qemh.cultural.multiple
    ),
    questionnaire.qemh.cultural.arab = case_when(
      ...
      .......
      TRUE ~ questionnaire.qemh.cultural.arab
    ),
    questionnaire.qemh.cultural.asian = case_when(
      ...
      TRUE ~ questionnaire.qemh.cultural.asian
    ),
    questionnaire.qemh.cultural.another = case_when(
      ...
      TRUE ~ questionnaire.qemh.cultural.another
    ),
  )


# remove values not needed after manual check
#convert all columns to character class
demo <- demo %>%
  mutate(across(everything(), as.character))
# Identify columns to modify: all except those ending with .text or .quantised
cols_to_modify <- names(demo)[!str_detect(names(demo), "\\.text$|\\.quantised$")]

demo <- demo %>%
  mutate(across(all_of(cols_to_modify),
                ~ if_else(. == "In another way (specify, if you wish).", NA_character_, .)))

# rename the column headers to something shorter and more intuitive 
demo <- demo %>% 
  rename(
    ID = "Participant.Private.ID",
    Gender = "questionnaire.qemh.Gender",
    Gender.quant = "questionnaire.qemh.Gender.quantised",
    Age = "questionnaire.qemh.Age",
    cultural.arab = "questionnaire.qemh.cultural.arab",
    cultural.asian = "questionnaire.qemh.cultural.asian",
    cultural.black = "questionnaire.qemh.cultural.black",
    cultural.multiple = "questionnaire.qemh.cultural.multiple",
    cultural.white = "questionnaire.qemh.cultural.white",
    cultural.prefNo = "questionnaire.qemh.cultural.prefNo",
    cultural.another = "questionnaire.qemh.cultural.another",
    Nationality = "Nationality"
  )

# select only columns we renamed above
demo <- demo %>% 
  dplyr::select(ID, Gender, Gender.quant, Age, Nationality, cultural.arab, cultural.asian, cultural.black,
                cultural.multiple, cultural.white, cultural.prefNo, cultural.another)

demo <- demo %>%
  mutate(cultural.another = if_else(cultural.another == "In another way (specify, if you wish)", 
                                    NA_character_, cultural.another)) %>% 
  mutate(cultural.multiple = if_else(cultural.multiple == "Any other mixed or multiple ethnic backgrounds (specify, if you wish).",
                                     NA_character_, cultural.multiple))



cultural_cols <- c("cultural.arab", "cultural.asian", "cultural.black", 
                   "cultural.multiple", "cultural.white", "cultural.prefNo", 
                   "cultural.another")

# Assign primary cultural category as first non-empty for each participant
demo <- demo %>%
  mutate(
    primary_cultural = case_when(
      !is.na(cultural.white) & cultural.white != "" ~ "White",
      !is.na(cultural.black) & cultural.black != "" ~ "Black",
      !is.na(cultural.asian) & cultural.asian != "" ~ "Asian",
      !is.na(cultural.arab) & cultural.arab != "" ~ "Arab",
      !is.na(cultural.multiple) & cultural.multiple != "" ~ "Multiple",
      !is.na(cultural.prefNo) & cultural.prefNo != "" ~ "No preference",
      !is.na(cultural.another) & cultural.another != "" ~ "Other (specified)",
      TRUE ~ NA_character_
    ),
    multiple_cultural = rowSums(across(all_of(cultural_cols), ~ !is.na(.) & . != "")) > 1
  )

demo <- demo %>%
  rowwise() %>%
  mutate(
    identified_prefs = list(
      c_across(all_of(cultural_cols)) %>%
        keep(~ !is.na(.) && . != "")  # Keep only non-NA non-empty values
    ),
    prefs_string = ifelse(length(identified_prefs) == 0, 
                          NA_character_, 
                          paste(identified_prefs, collapse = "; "))
  ) %>%
  ungroup()

# remove "DATA_EXPIRED" value from Nationality
demo <- demo %>%
  mutate(
    Nationality = if_else(Nationality == "DATA_EXPIRED", NA_character_, Nationality)
  )


# separate the age and gender into two separate variables - wide form
dfDemowide<-spread(data=demo, key=Question.Key, value=Response)

# finally, we add the demographics to the behavioural data
df <- merge(df,dfDemowide, 
            by="ID")

write.csv()