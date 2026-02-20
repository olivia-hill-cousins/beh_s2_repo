wd <- "Library/CloudStorage/OneDrive-UniversityofExeter/PhD/Beh. Study (Replication)/beh_s2_repo/data cleaning and analysis"
set.platform <- function(subdir = "") {
  base_wd <- if (Sys.info()[["sysname"]] == "Darwin") {
    file.path("~/",wd)
  } else if (Sys.info()[["sysname"]] == "Windows") {
    file.path("C:/Users/~/",wd)
  } else {
    stop("Unknown operating system, set your working directory manually.")
  }
  # allow empty subdir for to level, or paste others
  full_wd <- if (subdir == "") base_wd else file.path(base_wd, subdir)
  setwd(full_wd)
}
set.platform()
### set seed for reproducibility
set.seed(123)

### Load in some packages
# package names 
packages <- c("dplyr","tidyverse","stringr","stringdist")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
library(dplyr)
library(tidyverse)
library(stringr)
library(stringdist)

## Read in Data

set.platform("data_raw")
# Where key for yes=F
dataF <- read.csv("data_exp_249979-v6_task-nzt8.csv", header=TRUE, na.strings="")
# Where key for yes=J
dataJ <- read.csv("data_exp_249979-v6_task-hrcr.csv", header=TRUE, na.strings = "")
## mistake in coding meant that for dataJ, the keys were incorrectly coded as J=No, F=Yes, so we need to recode these to match dataF where J=Yes, F=No
dataJ <- dataJ %>%
  mutate(Response = case_when(
    Response == "Yes" ~ "No",
    Response == "No" ~ "Yes",
    TRUE ~ Response  # keep other responses unchanged
  ))
# demographics
demo <- read.csv("data_exp_249979-v6_questionnaire-qemh.csv", header=TRUE, na.strings="")
gorilla_demo <- demo
set.platform()
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
         # Intent = Spreadsheet..PDE.SE_notME,
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
set.platform("resources")
stimuli_full_spreadsheet <- read.csv("stimuli_full_spreadsheet.csv")
set.platform()
# rename variables to be consistent with main data
stimuli_full_spreadsheet <- stimuli_full_spreadsheet %>%
  rename(PBH = PBH,
         PDE = PDE,
         Agent = Agent,
         BA = PDE.noBA,
         MC = PDE.GE.BE,
         Intent = PDE.SE_notME,
         MG = MG.Permissible)

# Select only the columns you want to merge 
design_vars <- stimuli_full_spreadsheet %>% 
  dplyr::select(TVideo_1, PBH, PDE, Agent, BA, MC, Intent, MG) 

# Merge ONLY these columns into data 
df <- df %>% left_join(design_vars, by = "TVideo_1")

full_df <- df

# filter demo to only include rows from the Gorilla task where generic demographics were collected
demo <- demo %>% 
  filter(Task.Name == "Generic Demographics")

# load in demo from prolific
# the first 3 Ps were run with only the nationality filter on Prolific set to US. To ensure a US homogeneous sample was more closely adhered to, 
# the remaining Ps were run with the country of residence set to US as well. 
# It is important to note that the first 3 Ps that completed the study when only the nationality filter was present, also all had US as their
# country of residence. This means that the sample itself was invariable across these two groups. 
prolific_demo1 <- read.csv("data_raw/prolific_demographic_export_6989fafedcfe50faf774801e.csv")

# filter out Ps that returned their submission
prolific_demo1 <- prolific_demo1 %>% 
  filter(Status != "RETURNED")

# save df w. all Ps so have value for Ps that dropped out 
prolific_demo1_full <- prolific_demo1

# filter Ps who timed out
prolific_demo1 <- prolific_demo1 %>% 
  filter(Status != "TIMED-OUT")

# show that Ps in prolific_demo1 have Nationality == US and Country.of.Residence == US
prolific_demo1$Nationality
prolific_demo1$Country.of.residence

## second data set
prolific_demo2 <- read.csv("data_raw/prolific_demographic_export_698deddc023416730af28b7e.csv")

# filter Ps that returned their submission
prolific_demo2 <- prolific_demo2 %>% 
  filter(Status != "RETURNED")

# save df w. all Ps so have value for Ps that dropped out
prolific_demo2_full <- prolific_demo2

# filter Ps who timed out
prolific_demo2 <- prolific_demo2 %>% 
  filter(Status != "TIMED-OUT")

# show that Ps in prolific_demo2 have Nationality == US and Country.of.Residence == US
table(prolific_demo2$Nationality, prolific_demo2$Country.of.residence)

# write csv with prolific_demo1_full and prolific_demo2_full combined
prolific_demo_full <- rbind(prolific_demo1_full, prolific_demo2_full)

write.csv(prolific_demo_full, "data_raw/beh_s2_prolific_demographic_full.csv", row.names = FALSE)

# write csv with prolific_demo1 and prolific_demo2 combined (so only includes Ps who completed the study and didn't time out)
prolific_demo <- rbind(prolific_demo1, prolific_demo2)

write.csv(prolific_demo, "data_raw/beh_s2_prolific_demographic.csv", row.names = FALSE)

# next, we create a shared column between the two datasets
# prolific incl. a column called Participant.id which corresponds to the ID column called Participant.Public.ID in demo data
# therefore, we will rename the Participant.Public.ID column in demo to Participant.id
demo <- demo %>%
  rename(Participant.id = "Participant.Public.ID")

# now we can add the nationality data from prolific_demo to demo using the Participant.id column to ensure these are added to the correct Ps
demo <- demo %>%
  left_join(prolific_demo %>% dplyr::select(Participant.id, Nationality, Country.of.residence), by = "Participant.id")

demo <- demo %>% 
  mutate(
    questionnaire.qemh.cultural.white = case_when(
      Participant.Private.ID == "15132689" ~ "brazilian, italian white",
      Participant.Private.ID == "15133547" ~ "white american",
      Participant.Private.ID == "15132952" ~ "white american",
      Participant.Private.ID == "15133528" ~ "white",
      Participant.Private.ID == "15133529" ~ "white",
      Participant.Private.ID == "15132676" ~ "white",
      TRUE ~ questionnaire.qemh.cultural.white
    ),
    questionnaire.qemh.cultural.multiple = case_when(
      Participant.Private.ID == "15133638"  ~ "black, asian, native american",
      TRUE ~ questionnaire.qemh.cultural.multiple
    ),
    questionnaire.qemh.cultural.arab = case_when(
      Participant.Private.ID == "15132357" ~ "arab",
      TRUE ~ questionnaire.qemh.cultural.arab
    ),
    questionnaire.qemh.cultural.arab.text = case_when(
      Participant.Private.ID == "15133547"  ~ NA_character_,
      Participant.Private.ID == "15132332"  ~ NA_character_,
      TRUE ~ questionnaire.qemh.cultural.arab.text
    )
  )
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


# Add a prefix column to text_long
text_long2 <- text_long %>%
  mutate(cultural_col = str_replace(text_col, "\\.text$", ""))

# Join on BOTH ID and matching cultural column
combined <- cultural_long %>%
  inner_join(
    text_long2,
    by = c("Participant.Private.ID", "cultural_col")
  )


# 5. The result 'combined' has columns:
# ID, cultural_col, cultural_value, text_col, text_response
# For each participant with a text response, you can see their cultural values and what they typed in text columns


# create a function that updates combined so we can check what's left after each change we make below
update_culture <- function(demo, combined, keywords, max_dist = 2) {
  
  # 1. Create fuzzy-match flag for ANY keyword
  combined <- combined %>%
    mutate(
      flag = sapply(
        str_to_lower(str_trim(text_response)),
        function(x) any(stringdist(x, keywords) <= max_dist)
      )
    )
  
  # 2. Extract updates ONLY where the .text column matches the cultural column
  updates_df <- combined %>%
    filter(
      flag,
      str_replace(text_col, "\\.text$", "") == cultural_col
    ) %>%
    select(Participant.Private.ID, cultural_col, text_response)
  
  # 3. Apply updates to demo
  for (i in seq_len(nrow(updates_df))) {
    pid <- updates_df$Participant.Private.ID[i]
    col <- updates_df$cultural_col[i]
    val <- updates_df$text_response[i]
    
    demo[demo$Participant.Private.ID == pid, col] <- val
  }
  
  # 4. Remove resolved rows + drop flag column
  combined <- combined %>%
    filter(!flag) %>%
    select(-flag)
  
  list(demo = demo, combined = combined)
}


# ps w. multiple specifications 
# find ps this applies to
multi_text <- text_long %>%
  group_by(Participant.Private.ID) %>%
  summarise(
    n_text = n(),
    combined_text = paste(text_response, collapse = ", ")
  ) %>%
  filter(n_text > 1)

# combine their specified values & put under "multiple"
for (i in seq_len(nrow(multi_text))) {
  
  pid <- multi_text$Participant.Private.ID[i]
  val <- multi_text$combined_text[i]
  
  # 1. Write combined text into the multiple column
  demo[demo$Participant.Private.ID == pid,
       "questionnaire.qemh.cultural.multiple"] <- val
  
  # 2. Remove values from ALL other cultural columns
  other_cols <- setdiff(cultural_cols, "questionnaire.qemh.cultural.multiple")
  
  demo[demo$Participant.Private.ID == pid, other_cols] <- NA
}

# update combined
combined <- combined %>%
  filter(!Participant.Private.ID %in% multi_text$Participant.Private.ID)

# american
result <- update_culture(demo, combined, "american")

demo <- result$demo
combined <- result$combined
combined 

# let's do the rest at once now we've checked it's working
result <- update_culture(demo, combined, c("german", "white", "hispanic", "black", "african american", "black african american", 
                                           "black african american", "french", "italian", "korean", "caucasian", "native american/caucasian", "mixed"))

demo <- result$demo
combined <- result$combined


# remove values not needed after manual check
#convert all columns to character class
demo <- demo %>%
  mutate(across(everything(), as.character))


# Identify columns to modify: all except those ending with .text or .quantised
cols_to_modify <- names(demo)[!str_detect(names(demo), "\\.text|\\.quantised")]


demo <- demo %>%
  mutate(across(
    all_of(cols_to_modify),
    ~ if_else(
      str_detect(str_squish(.), "^In another way \\(specify, if you wish\\)"),
      NA_character_,
      .
    )
  ))


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
    Nationality = "Nationality",
    Country_of_residence = "Country.of.residence"
  )

# select only columns we renamed above
demo <- demo %>% 
  dplyr::select(ID, Gender, Gender.quant, Age, Nationality, Country_of_residence, cultural.arab, cultural.asian, cultural.black,
                cultural.multiple, cultural.white, cultural.prefNo, cultural.another)

# demo <- demo %>%
#   mutate(cultural.another = if_else(cultural.another == "In another way (specify, if you wish)", 
#                                     NA_character_, cultural.another)) %>% 
#   mutate(cultural.multiple = if_else(cultural.multiple == "Any other mixed or multiple ethnic backgrounds (specify, if you wish).",
#                                      NA_character_, cultural.multiple))

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



# finally, we add the demographics to the behavioural data
df <- merge(df,demo, 
            by="ID")

# write clean data to data_clean folder
## demo (so we have a wide format for reporting stats later)
## first create a df for this purpose so we can convert everything to character
save_df <- demo
#convert all columns to character class
save_df <- save_df %>%
  mutate(across(everything(), as.character))
write.csv(save_df, "data_clean/beh_s2_cleanedDemo.csv", row.names = FALSE)
## first create a df for this purpose so we can convert everything to character
save_df <- df
#convert all columns to character class
save_df <- save_df %>%
  mutate(across(everything(), as.character))
write.csv(save_df, "data_clean/beh_s2_cleanedData.csv", row.names = FALSE)

# omission scenarios
totalOmissionAdditional_scenarios <- c("Unprohibited_BetterAlternative_Equipment_Intended_DisproportionateCost_Crane", 
                                       "Unprohibited_BetterAlternative_Equipment_Intended_DisproportionateCost_Trolley",
                                       "Unprohibited_BetterAlternative_Equipment_Intended_DisproportionateCost_Truck",
                                       "Unprohibited_BetterAlternative_Equipment_Intended_DisproportionateCost_Mine",
                                       "Unprohibted_BetterAlternative_DisproportionateCost_Crane",
                                       "Unprohibted_BetterAlternative_DisproportionateCost_Mine",
                                       "Unprohibted_BetterAlternative_DisproportionateCost_Truck",
                                       "Unprohibted_BetterAlternative_DisproportionateCost_Trolley")

# now remove the dilemmas in the list from df.test 
reduced_df <- df %>%
  filter(!Dilemma %in% totalOmissionAdditional_scenarios)
save_df <- reduced_df
save_df <- save_df %>%
  mutate(across(everything(), as.character))
write.csv(save_df, "data_clean/beh_s2_reducedCleanedData.csv", row.names = FALSE)

