wd <- "~"
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

### set seed for reproducibility
set.seed(123)

### Load in some packages
# package names 
packages <- c("lme4")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
library(lme4)


### Demographics
# calculating mean age
dfDemowide$Age <- as.numeric(dfDemowide$Age)
mean_age <- mean(dfDemowide$Age)
mean_age

# calculating standard deviation of age
sd(dfDemowide$Age)

# find the age of the youngest participant
youngest_age <- min(dfDemowide$Age)
youngest_age

# find the age of the oldest participant
oldest_age <- max(dfDemowide$Age)
oldest_age

# change Other (please specify) to what was specified in next question
dfDemowide$Gender <- ifelse(dfDemowide$Gender == "Other (please specify)", "Non Binary", dfDemowide$Gender)

# get count of number of participants of each gender
gender_count <- table(dfDemowide$Gender)
gender_count

woman_count <- gender_count["Woman"]
woman_count

man_count <- gender_count["Man"]
man_count

nonbinary_count <- gender_count["Non-binary"]
nonbinary_count
prefNoGender_count <- gender_count["Prefer Not To Say"]
prefNoGender_count
anotherWayGender_count <- gender_count["In another way (specify, if you wish)"] 
anotherWayGender_count

print(gender_count)

# cultural demographics

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

# Summary table excluding NAs in primary_cultural
summary_table <- demo %>%
  filter(!is.na(primary_cultural)) %>%   # Exclude NAs here
  count(primary_cultural, multiple_cultural) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(n, " (", percent, "%)"))

print("Summary of primary cultural group and multiple identity flag excluding NAs:")
print(summary_table)


cultural_cols <- c("cultural.arab", "cultural.asian", "cultural.black", 
                   "cultural.multiple", "cultural.white", "cultural.prefNo", 
                   "cultural.another")

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

# View results: 
cultural_count <- demo %>%
  dplyr::select(ID, primary_cultural, prefs_string) %>%
  dplyr::arrange(primary_cultural,prefs_string)
cultural_count

### Prep variables for analysis
# make sure everything is factor
df$Task.Name <- as.factor(df$Task.Name)
df$Agent <- as.factor(df$Agent)
df$Context <- as.factor(df$Context)
df$PDE <- as.factor(df$PDE)
df$BA <- as.factor(df$BA)
df$MC <- as.factor(df$MC)
df$Intent <- as.factor(df$Intent)
df$PBH <- as.factor(df$PBH)
df$MG <- as.factor(df$MG)
df$RT <- abs(as.numeric(df$RT))

## Contrasts Coding
#set deviation contrasts for ease of interpretation -.5 vs .5
c<-contr.treatment(2)
my.coding<-matrix(rep(1/2, 2), ncol=1)
my.simple<-c-my.coding
my.simple
### Set the Contrast Coding Per Variable
contrasts(df$PDE)<-my.simple
contrasts(df$PDE)
contrasts(df$BA)<-my.simple
contrasts(df$BA)
contrasts(df$MC)<-my.simple
contrasts(df$MC)
contrasts(df$Intent)<-my.simple
contrasts(df$Intent)
contrasts(df$PBH)<-my.simple
contrasts(df$PBH)
contrasts(df$MG)<-my.simple
contrasts(df$MG)
contrasts(df$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
contrasts(df$Agent)

df <- df %>%
  mutate(MJ = recode(MJ, "Yes" = "1",
                     "No" = "0"))

df$MJ <- as.factor(df$MJ)

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PDE*PBH*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))

summary(hVai)

#OR coefficients 
OR_hVai <- exp(fixef(hVai))
# confidence intervals for log-odds and OR
ci_hVai <- confint(hVai, method = "Wald")
ci_hVai.or <- exp(ci_hVai)
ses_hVai <- sqrt(diag(vcov(hVai)))


### Estimated Marginal Means

# by Agent
emmeans_hVai <- emmeans(hVai, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai

CIhVai <- confint(emmeans_hVai)
CIhVai

# by PBH
emmeans_PBHvN <- emmeans(hVai, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_PBHvN

CIPBHvN <- confint(emmeans_PBHvN)
CIPBHvN

# by PDE
emmeans_PDEvN <- emmeans(hVai, pairwise ~ Agent*PBH|PDE, cov.reduce = range)
emmeans_PDEvN

CIhVai <- confint(emmeans_PDEvN)
CIhVai


### Simple Contrasts

# by Agent
contrasts_hVai <- contrast(emmeans_hVai, interaction = "pairwise", by=c("Agent"))
contrasts_hVai

# by PBH
contrasts_PBHvN <- contrast(emmeans_PBHvN, interaction = "pairwise", by=c("PBH"))
contrasts_PBHvN

# by PDE
contrasts_PDEvN <- contrast(emmeans_PDEvN, interaction = "pairwise", by=c("PDE"))
contrasts_PDEvN

### Plots
plot(allEffects(hVai))
set_theme(base = theme_classic())
plot_model(hVai, terms = "PBH", type = "pred")
plot_model(hVai, terms = "PDE", type = "pred")
plot_model(hVai, terms = "Agent", type = "pred")
plot_model(hVai, type = "int")



### Estimated Marginal Means
emmeans_hVai <- emmeans(hVaiRT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)

CIhVaiRT <- confint(emmeans_hVai)
CIhVaiRT


### Simple Contrasts
contrasts_hVai <- contrast(emmeans_hVai, interaction = "pairwise", by=c("Agent"))
contrasts_hVai


## Mixed Effects Linear Regression (Three-Way) w. RT as DV
hVaiRT <- glmer(RT ~ PBH*PDE*Agent +  (1+PDE*PBH*Agent|ID) + (1+Agent|Dilemma),  data=df)

summary(hVaiRT)

confint(hVaiRT)

### Convert Log Odds to Odds Ratios so they are easier to interpret
OR_hVai <- exp(coef(summary(hVai))[, "Estimate"])

lower <- exp(confint(hVai)[,1])
upper <- exp(confint(hVai)[,2])


### TOST
TOaST <- equivalence_test(hVai, rule = "classic")
TOaST
plot(TOaST)

# set bounds to OR of 1.68 (log(1.68)=0.52) 
TOaST <- equivalence_test(hVai, rule = "classic", range = c(-0.52,0.52))
TOaST
plot(TOaST)

# Item-Level Analyses
df$MG_pred <- ifelse(df$PBH == "Yes" & df$PDE == "No", 0, 1)
df$match_MG <- ifelse(df$MJ == df$MG, 1, 0)


full_item.level <- glmer(MJ ~ MG  + (1|ID) + (1|Dilemma),
               data = df, family = binomial)
summary(full_item.level)


# Dilemma
full_Dilemma.level <- glmer(MJ ~ MG  +  (1|Dilemma),
               data = df, family = binomial)
summary(full_Dilemma.level)
ranef(full_Dilemma.level)$Dilemma


# id
full_ID.level <- glmer(MJ ~ MG  +  (1|ID),
               data = df, family = binomial)
summary(full_ID.level)


#### From Original Model
Dilemma_effects <- ranef(hVai)$Dilemma
Dilemma_effects

id_effects <- ranef(hVai)$ID
id_effects


# extract fixed effect estimates
fixef_intercept <- fixef(hVai)[1]
fixef_slope <- fixef(hVai)[2]

# Base fixed effect line
fixed_line <- fixef_intercept + fixef_slope





# STEP 6: Model Comparisons 

# Agent only
Agent <- glmer(MJ ~ Agent +  (1+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
               control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(Agent)
# PBH only
PBH <- glmer(MJ ~ PBH +  (1+PBH|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(PBH)
# PDE only
PDE <- glmer(MJ ~ PDE +  (1+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(PDE)

### two-way

# PBH+Agent only
PBH_Agent <- glmer(MJ ~ PBH*Agent +  (1+PBH*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(PBH_Agent)
# PDE+Agent only
PDE_Agent <- glmer(MJ ~ PDE*Agent +  (1+PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(PDE_Agent)

# PBH+PDE only
PBH_PDE <- glmer(MJ ~ PBH*PDE +  (1+PBH*PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(PBH_PDE)

### model comparison analysis
compare_performance(Agent, PBH, PDE, PBH_Agent, PDE_Agent, PBH_PDE, hVai)


# STEP 7: Intent
Intent <- glmer(MJ ~ Intent +  (1+Intent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(Intent)

hVai.Intent <- glmer(MJ ~ Intent*Agent +  (1+Intent+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
summary(hVai.Intent)

TOaST.i <- equivalence_test(hVai.Intent, rule = "classic", range = c(-0.52,0.52))
TOaST.i
plot(TOaST.i)

