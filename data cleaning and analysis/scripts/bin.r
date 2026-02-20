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
packages <- c("lme4","lmerTest","performance","easystats","dplyr","emmeans","effects","tidyr","kableExtra", "sjPlot","qqplotr", "DHARMa")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
library(lme4)
library(lmerTest)
library(performance)
library(easystats)
library(dplyr)
library(emmeans)
library(effects)
library(tidyr)
library(kableExtra)
library(sjPlot)
library(qqplotr)
library(DHARMa)

### Read in Cleaned Data
demo <- read.csv("data_clean/beh_s2_cleanedDemo.csv")
df <- read.csv("data_clean/beh_s2_cleanedData.csv")
reduced_df <- read.csv("data_clean/beh_s2_reducedCleanedData.csv")



### Demographics
# calculating mean age
demo$Age <- as.numeric(demo$Age)
mean_age <- mean(demo$Age)
mean_age

# calculating standard deviation of age
sd(demo$Age)

# find the age of the youngest participant
youngest_age <- min(demo$Age)
youngest_age

# find the age of the oldest participant
oldest_age <- max(demo$Age)
oldest_age

# change Other (please specify) to what was specified in next question
demo$Gender <- ifelse(demo$Gender == "Other (please specify)", "Non Binary", demo$Gender)

# get count of number of participants of each gender
gender_count <- table(demo$Gender)
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


# Summary table excluding NAs in primary_cultural
summary_table <- demo %>%
  filter(!is.na(primary_cultural)) %>%   # Exclude NAs here
  count(primary_cultural, multiple_cultural) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(n, " (", percent, "%)"))

print("Summary of primary cultural group and multiple identity flag excluding NAs:")
print(summary_table)



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
contrasts(df$PDE)<- my.simple
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

df$MJ <- as.numeric(as.character(df$MJ))


#df$MJ <- as.factor(df$MJ)

set.platform("outputs")
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "fullRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## slope for agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentdilRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## Drop PDE interaction for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEintRE_dropAgentdilRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## Drop PDE interaction for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAllintRE_dropAgentdilRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEintRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## Drop PBH * Agent interaction slope for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropintRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop PDE slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEslope_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)
# w.o agent dilemma slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEslope_dropAgentDil_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop Agent ID slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentIDRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop Agent ID & dilemma slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentIDdilRE_hVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop PDE and Agent ID slope
# hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
#               control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
# saveRDS(hVai, "dropPDEAgentIDRE_hVai_glmModel.rds")
# isSingular(hVai)
# summary(hVai)


### fitting the maximal model possible for full data provided 
hVai <- readRDS("dropAgentIDdilRE_hVai_glmModel.rds")
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
contrasts_hVai <- contrast(emmeans_hVai, interaction = "pairwise")
contrasts_hVai

# # by PBH
# contrasts_PBHvN <- contrast(emmeans_PBHvN, interaction = "pairwise", by=c("PBH"))
# contrasts_PBHvN
# 
# # by PDE
# contrasts_PDEvN <- contrast(emmeans_PDEvN, interaction = "pairwise", by=c("PDE"))
# contrasts_PDEvN

### Plots
plot(allEffects(hVai))
set_theme(base = theme_classic())
plot_model(hVai, terms = "PBH", type = "pred")
plot_model(hVai, terms = "PDE", type = "pred")
plot_model(hVai, terms = "Agent", type = "pred")
plot_model(hVai, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST <- equivalence_test(hVai, rule = "classic", range = c(-0.94,0.94))

toast <- as.data.frame(TOaST)

toast <- toast %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast
plot(TOaST)

# use default bounds
TOaST <- equivalence_test(hVai, rule = "classic")

toast <- as.data.frame(TOaST)

toast <- toast %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast
plot(TOaST)


## Model Comparisons
set.platform("outputs")
# Agent only
Agent.noSlopes <- glmer(MJ ~ Agent +  (1|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                        control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent.noSlopes, "Agent.noSlopes.rds")

Agent.idSlope <-  glmer(MJ ~ Agent +  (1+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                        control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent.idSlope, "Agent.idSlope.rds")

Agent.DilemmaSlope <-  glmer(MJ ~ Agent +  (1|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent.DilemmaSlope, "Agent.DilemmaSlope.rds")

Agent <- glmer(MJ ~ Agent +  (1+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
               control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent, "Agent.rds")
# PBH only
PBH <- glmer(MJ ~ PBH +  (1+PBH|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH, "PBH.rds")
# PDE only
PDE <- glmer(MJ ~ PDE +  (1+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PDE, "PDE.rds")

### two-way
# PBH+Agent only
PBH_Agent <- glmer(MJ ~ PBH*Agent +  (1+PBH*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH_Agent, "PBH_Agent.rds")
# PDE+Agent only
PDE_Agent <- glmer(MJ ~ PDE*Agent +  (1+PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))

saveRDS(PDE_Agent, "PDE_Agent.rds")
# PBH+PDE only
PBH_PDE <- glmer(MJ ~ PBH*PDE +  (1+PBH*PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH_PDE, "PBH_PDE.rds")

### model comparison analysis
model_comp <- compare_performance(Agent, PBH, PDE, PBH_Agent, PDE_Agent, PBH_PDE, hVai)
saveRDS(model_comp, "model_comp.rds")
model_comp

## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots <- plot(check_model(hVai, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots


p <- testDispersion(hVai,plot=F) 
p

## Reduced df
### Prep variables for analysis
# make sure everything is factor
reduced_df$Task.Name <- as.factor(reduced_df$Task.Name)
reduced_df$Agent <- as.factor(reduced_df$Agent)
reduced_df$Context <- as.factor(reduced_df$Context)
reduced_df$PDE <- as.factor(reduced_df$PDE)
reduced_df$BA <- as.factor(reduced_df$BA)
reduced_df$MC <- as.factor(reduced_df$MC)
reduced_df$Intent <- as.factor(reduced_df$Intent)
reduced_df$PBH <- as.factor(reduced_df$PBH)
reduced_df$MG <- as.factor(reduced_df$MG)
reduced_df$RT <- abs(as.numeric(reduced_df$RT))

## Contrasts Coding
#set deviation contrasts for ease of interpretation -.5 vs .5
c<-contr.treatment(2)
my.coding<-matrix(rep(1/2, 2), ncol=1)
my.simple<-c-my.coding
my.simple
### Set the Contrast Coding Per Variable
contrasts(reduced_df$PDE)<- my.simple
contrasts(reduced_df$PDE)
contrasts(reduced_df$BA)<-my.simple
contrasts(reduced_df$BA)
contrasts(reduced_df$MC)<-my.simple
contrasts(reduced_df$MC)
contrasts(reduced_df$Intent)<-my.simple
contrasts(reduced_df$Intent)
contrasts(reduced_df$PBH)<-my.simple
contrasts(reduced_df$PBH)
contrasts(reduced_df$MG)<-my.simple
contrasts(reduced_df$MG)
contrasts(reduced_df$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
contrasts(reduced_df$Agent)

reduced_df <- reduced_df %>%
  mutate(MJ = recode(MJ, "Yes" = "1",
                     "No" = "0"))

reduced_df$MJ <- as.numeric(as.character(reduced_df$MJ))

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "fullRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## slope for agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentdilRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## Drop PDE interaction for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEintRE_dropAgentdilRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## Drop PDE interaction for ID RF
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAllintRE_dropAgentdilRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAllintRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop PDE slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEslope_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)
# w.o agent dilemma slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropPDEslope_dropAgentDil_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop Agent ID slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentIDRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## drop Agent ID & dilemma slope
hVai <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai, "dropAgentIDdilRE_reducedhVai_glmModel.rds")
isSingular(hVai)
summary(hVai)

## now with reduced data, check emmeans, contrast, plots, tost and model checks 
hVai <- readRDS("outputs/dropAgentIDdilRE_reducedhVai_glmModel.rds")

emmeans_reducedhVai <- emmeans(hVai, pairwise ~ PDE*Agent|PBH, cov.reduce = range)
emmeans_reducedhVai

CI_reducedhVai <- confint(emmeans_reducedhVai)

# emmeans_reducedPDEPBH <- emmeans(hVai, pairwise ~ PBH|PDE, cov.reduce = range)
# emmeans_reducedPDEPBH
# CI_PDEPBH <- confint(emmeans_reducedPDEPBH)
# CI_PDEPBH
# 
# emmeans_reducedPDEAgent <- emmeans(hVai, pairwise ~ Agent|PDE, cov.reduce = range)
# emmeans_reducedPDEAgent
# CI_PDEAgent <- confint(emmeans_reducedPDEAgent)
# CI_PDEAgent
# 
# emmeans_reducedPBHAgent <- emmeans(hVai, pairwise ~ Agent|PBH, cov.reduce = range)
# emmeans_reducedPBHAgent
# CI_PBHAgent <- confint(emmeans_reducedPBHAgent)
# CI_PBHAgent

# contrasts
contrasts_reducedhVai <- contrast(emmeans_reducedhVai, interaction = "pairwise")
contrasts_reducedhVai
# contrasts_reducedPDEPBH <- contrast(emmeans_reducedPDEPBH, interaction = "pairwise")
# contrasts_reducedPDEPBH
# 
# contrasts_reducedPDEAgent <- contrast(emmeans_reducedPDEAgent, interaction = "pairwise")
# contrasts_reducedPDEAgent
# 
# contrasts_reducedPBHAgent <- contrast(emmeans_reducedPBHAgent, interaction = "pairwise")
# contrasts_reducedPBHAgent

# TOST
TOaST_reducedhVai <- equivalence_test(hVai, rule = "classic", range = c(-0.94,0.94))
TOaST_reducedhVai
plot(TOaST_reducedhVai)
# model checks
diagnostic_plots_reducedhVai <- plot(check_model(hVai),type="discrete_both")
diagnostic_plots_reducedhVai

# Model comparisons 
set.platform("outputs")
# Agent only
Agent_reduced_noSlopes <- glmer(MJ ~ Agent +  (1|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent_reduced_noSlopes, "Agent_reduced_noSlopes.rds")

Agent_reduced_idSlope <-  glmer(MJ ~ Agent +  (1+Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent_reduced_idSlope, "Agent_reduced_idSlope.rds")

Agent_reduced_DilemmaSlope <-  glmer(MJ ~ Agent +  (1|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent_reduced_DilemmaSlope, "Agent_reduced_DilemmaSlope.rds")

Agent_reduced <- glmer(MJ ~ Agent +  (1+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                       control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(Agent_reduced, "Agent_reduced.rds")
# PBH only
PBH_reduced <- glmer(MJ ~ PBH +  (1+PBH|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH_reduced, "PBH_reduced.rds")
# PDE only
PDE_reduced <- glmer(MJ ~ PDE +  (1+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PDE_reduced, "PDE_reduced.rds")

### two-way
# PBH+Agent only
PBH_Agent_reduced <- glmer(MJ ~ PBH*Agent +  (1+PBH*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH_Agent_reduced, "PBH_Agent_reduced.rds")
# PDE+Agent only
PDE_Agent <- glmer(MJ ~ PDE*Agent +  (1+PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))

saveRDS(PDE_Agent_reduced, "PDE_Agent_reduced.rds")
# PBH+PDE only
PBH_PDE_reduced <- glmer(MJ ~ PBH*PDE +  (1+PBH*PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(PBH_PDE_reduced, "PBH_PDE_reduced.rds")

### model comparison analysis
model_comp_reduced <- compare_performance(Agent_reduced, PBH_reduced, PDE_reduced, PBH_Agent_reduced, PDE_Agent_reduced, PBH_PDE_reduced, hVai)
saveRDS(model_comp_reduced, "model_comp_reduced.rds")
model_comp_reduced


## Reaction time modelling 
## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  df_temp <- df %>% mutate(logRT = log(RT + c))
  ggplot(df_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
    #georgia_theme
})



wrap <- wrap_plots(plots, nrow = 3)
wrap

## log-transform RT with selected constant
df$logRT <- log(df$RT + 1)
hVaiRT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, control=lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
# only converges if we drop the Agent slope for dilemma. Nothing significant though. 
saveRDS(hVaiRT, "hVaiRT.rds")
isSingular(hVaiRT)
summary(hVaiRT)


ci_hVaiRT <- confint(hVaiRT, method = "Wald")
#OR coefficients 
OR_hVaiRT <- exp(fixef(hVaiRT))
ses_hVaiRT <- exp(sqrt(diag(vcov(hVaiRT)))) 

############################################################
# 1. PREDICTED-EFFECT PLOTS
############################################################
#model_data <- insight::get_data(hVaiRT)
# names(model_data)
#plot_model(hVaiRT,pred.type="fe",type="emm",terms=c("PBH"),colors=colours)

make_pred_plot <- function(term) {
  plot_model(
    hVaiRT,
    terms = term,
    #colors = colours,
    type="eff",
    transformed = c("exp")
  ) + #georgia_theme +
    labs(title = NULL)
}

p_PBH   <- make_pred_plot("PBH")
p_PDE   <- make_pred_plot("PDE")
p_Agent <- make_pred_plot("Agent")
p_PBH
p_PDE
p_Agent



# interaction plots
p_int_list <- plot_model(hVaiRT, type = "int",
                         transform = c("exp"))
p_int_list


### Simple effects analysis for RT
# by Agent
emmeans_hVaiRT <- emmeans(hVaiRT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVaiRT

CIhVaiRT <- confint(emmeans_hVaiRT)


# # by PBH
# emmeans_PBHvRT <- emmeans(hVaiRT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
# emmeans_PBHvRT
# 
# CIPBHvN <- confint(emmeans_PBHvRT)
# 
# 
# # by PDE
# emmeans_PDEvRT <- emmeans(hVaiRT, pairwise ~ Agent*PBH|PDE, cov.reduce = range)
# emmeans_PDEvRT
# 
# CIhVaiRT <- confint(emmeans_PDEvRT)
# 
# 
# ## agent x PDE
# emmeans_hVaiRT_PDE <- emmeans(hVaiRT, pairwise ~ PDE|Agent, cov.reduce = range)
# emmeans_hVaiRT_PDE
# 
# CIhVaiRT_PDE <- confint(emmeans_hVaiRT_PDE)

## contrasts
# by Agent
contrasts_hVaiRT <- contrast(emmeans_hVaiRT, interaction = "pairwise", by=c("Agent"))
contrasts_hVaiRT

# # by PBH
# contrasts_PBHvNRT <- contrast(emmeans_PBHvN, interaction = "pairwise", by=c("PBH"))
# contrasts_PBHvNRT
# 
# # by PDE
# contrasts_PDEvN <- contrast(emmeans_PDEvN, interaction = "pairwise", by=c("PDE"))
# contrasts_PDEvN


## TOST for RT
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVaiRT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound


TOaST_RT <- equivalence_test(hVaiRT, rule = "classic", range = c(-bound,bound))
TOaST_RT
plot(TOaST_RT)

## Model checks
diagnostic_plots_RT <- plot(check_model(hVaiRT),type="all")
diagnostic_plots_RT


## test main binary model with logRT added as random slope for ID
hVai_logRTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE+logRT|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_logRTRE, "hVai_logRTRE.rds")
isSingular(hVai_logRTRE)
summary(hVai_logRTRE)

diagnostic_plots_logRTRE <- plot(check_model(hVai_logRTRE),type="discrete_both")
diagnostic_plots_logRTRE


## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  reduced_df_temp <- reduced_df %>% mutate(logRT = log(RT + c))
  ggplot(reduced_df_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})

## log-transform RT with selected constant
reduced_df$logRT <- log(reduced_df$RT + 1)
hVaiRT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, control=lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
# only converges if we drop the Agent slope for dilemma. Nothing significant though. 
saveRDS(hVaiRT, "reducedhVaiRT.rds")
isSingular(hVaiRT)
summary(hVaiRT)


ci_hVaiRT <- confint(hVaiRT, method = "Wald")
#OR coefficients 
OR_hVaiRT <- exp(fixef(hVaiRT))
ses_hVaiRT <- exp(sqrt(diag(vcov(hVaiRT)))) 

############################################################
# 1. PREDICTED-EFFECT PLOTS
############################################################
#model_data <- insight::get_data(hVaiRT)
# names(model_data)
#plot_model(hVaiRT,pred.type="fe",type="emm",terms=c("PBH"),colors=colours)

make_pred_plot <- function(term) {
  plot_model(
    hVaiRT,
    terms = term,
    #colors = colours,
    type="eff",
    transformed = c("exp")
  ) + #georgia_theme +
    labs(title = NULL)
}

p_PBH   <- make_pred_plot("PBH")
p_PDE   <- make_pred_plot("PDE")
p_Agent <- make_pred_plot("Agent")
p_PBH
p_PDE
p_Agent



# interaction plots
p_int_list <- plot_model(hVaiRT, type = "int",
                         transform = c("exp"))
p_int_list

### Simple effects analysis for RT
# by Agent
emmeans_hVaiRT <- emmeans(hVaiRT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVaiRT

CIhVaiRT <- confint(emmeans_hVaiRT)


# by PBH
emmeans_PBHvRT <- emmeans(hVaiRT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_PBHvRT

CIPBHvN <- confint(emmeans_PBHvRT)


# by PDE
emmeans_PDEvRT <- emmeans(hVaiRT, pairwise ~ Agent*PBH|PDE, cov.reduce = range)
emmeans_PDEvRT

CIhVaiRT <- confint(emmeans_PDEvRT)


## agent x PDE
emmeans_hVaiRT_PDE <- emmeans(hVaiRT, pairwise ~ PDE|Agent, cov.reduce = range)
emmeans_hVaiRT_PDE

CIhVaiRT_PDE <- confint(emmeans_hVaiRT_PDE)

## contrasts
# by Agent
contrasts_hVaiRT <- contrast(emmeans_hVaiRT, interaction = "pairwise", by=c("Agent"))
contrasts_hVaiRT

# by PBH
contrasts_PBHvN <- contrast(emmeans_PBHvN, interaction = "pairwise", by=c("PBH"))
contrasts_PBHvN

# by PDE
contrasts_PDEvN <- contrast(emmeans_PDEvN, interaction = "pairwise", by=c("PDE"))
contrasts_PDEvN


## TOST for RT
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVaiRT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound


TOaST <- equivalence_test(hVaiRT, rule = "classic", range = c(-bound,bound))
TOaST

plot(TOaST)


## test main binary model with logRT added as random slope for ID
hVai_logRTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE+logRT|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_logRTRE, "reducedhVai_logRTRE.rds")
isSingular(hVai_logRTRE)
summary(hVai_logRTRE)

diagnostic_plots <- plot(check_model(hVai_logRTRE),type="discrete_both")
diagnostic_plots


## Intent
hVai_intent <- glmer(MJ ~ Intent*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "hVai_intent.rds")
isSingular(hVai_intent)
summary(hVai_intent)


ci_hVai_intent <- confint(hVai_intent, method = "Wald") 
OR_hVai_intent <- exp(fixef(hVai_intent))
ses_hVai_intent <- exp(sqrt(diag(vcov(hVai_intent))))

emmeans_hVai_intent <- emmeans(hVai_intent, pairwise ~ Intent|Agent, cov.reduce = range)
emmeans_hVai_intent

CIhVai_intent <- confint(emmeans_hVai_intent)
contrasts_hVai_intent <- contrast(emmeans_hVai_intent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_intent
plot_model(hVai_intent, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# TOST
TOaST_intent <- equivalence_test(hVai_intent, rule = "classic", range = c(-0.94,0.94))
TOaST_intent
plot(TOaST_intent)

# Model checks
diagnostic_plots_hVai_intent <- plot(check_model(hVai_intent),type="discrete_both")
diagnostic_plots_hVai_intent 

# intent effect alone (no agent)
intent <- glmer(MJ ~ Intent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
               control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(intent, "intent.rds")
isSingular(intent)
summary(intent)

ci_intent <- confint(intent, method = "Wald")
OR_intent <- exp(fixef(intent))
ses_intent <- exp(sqrt(diag(vcov(intent))))

# emmeans & contrasts
emmeans_intent <- emmeans(intent, pairwise ~ Intent, cov.reduce = range)
emmeans_intent
CI_intent <- confint(emmeans_intent)
contrasts_intent <- contrast(emmeans_intent, interaction = "pairwise")
contrasts_intent

# plots
plot_model(intent, type = "eff", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# tost
TOaST_intent <- equivalence_test(intent, rule = "classic", range = c(-0.94,0.94))
TOaST_intent
plot(TOaST_intent)

# Model checks
diagnostic_plots_intent <- plot(check_model(intent),type="discrete_both")
diagnostic_plots_intent

## Intent with reduced df
hVai_intent <- glmer(MJ ~ Intent*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "reducedhVai_intent.rds")
isSingular(hVai_intent)
summary(hVai_intent)


ci_hVai_intent <- confint(hVai_intent, method = "Wald") 
OR_hVai_intent <- exp(fixef(hVai_intent))
ses_hVai_intent <- exp(sqrt(diag(vcov(hVai_intent))))

emmeans_hVai_intent <- emmeans(hVai_intent, pairwise ~ Intent|Agent, cov.reduce = range)
emmeans_hVai_intent

CIhVai_intent <- confint(emmeans_hVai_intent)
contrasts_hVai_intent <- contrast(emmeans_hVai_intent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_intent
plot_model(hVai_intent, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# TOST
TOaST_reducedintent <- equivalence_test(hVai_intent, rule = "classic", range = c(-0.94,0.94))
TOaST_reducedintent
plot(TOaST_reducedintent)

# Model checks
diagnostic_plots_reducedhVai_intent <- plot(check_model(hVai_intent),type="discrete_both")


# intent effect alone (no agent)
intent <- glmer(MJ ~ Intent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(intent, "reducedintent.rds")
isSingular(intent)
summary(intent)

ci_intent <- confint(intent, method = "Wald")
OR_intent <- exp(fixef(intent))
ses_intent <- exp(sqrt(diag(vcov(intent))))

# emmeans & contrasts
emmeans_intent <- emmeans(intent, pairwise ~ Intent, cov.reduce = range)
emmeans_intent
CI_intent <- confint(emmeans_intent)
contrasts_intent <- contrast(emmeans_intent, interaction = "pairwise")
contrasts_intent

# plots
plot_model(intent, type = "eff", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# tost
TOaST_intent <- equivalence_test(intent, rule = "classic", range = c(-0.94,0.94))
TOaST_intent
plot(TOaST_intent)

# Model checks
diagnostic_plots_reduced_intent <- plot(check_model(intent),type="discrete_both")




## Excluding participants without matches for costless rescue dilemmas (human-only)

# create a variable that calculates matches between MG and MJ
df <- df %>%
  mutate(MG_match = ifelse(MG == "Yes" & MJ == 1 | 
                             MG == "No"  & MJ == 0, 1, 0))

# count matches per participant for human-only costless rescue scenarios
costless_check_all <- df %>% 
  filter(Agent %in% c("Human")) %>% 
  filter(Dilemma %in% c("costless rescue", "costless rescueCrane", "costless rescueMine", "costless rescue Truck")) %>% 
  group_by(ID) %>%
  summarise(matches = sum(MG_match),
            n_costless_seen = n(), 
            n_costless_correct = sum(MG_match == 1),
            passed_costless_all = (n_costless_seen == n_costless_correct),
            percent_costless = ((n_costless_correct / n_costless_seen) *100),
            .groups = "drop")

costless_check_all

# print rows where passed_costless_all is FALSE
costless_check_all %>%
  filter(passed_costless_all == FALSE)


## at least half were incorrect. Due to the vast number, we remove if more than 1 incorrect (rather than any)
df_match <- df %>%
  filter(ID %in% costless_check_all$ID[costless_check_all$percent_costless != 0]) # keep if 50-100% correct on costless rescue dilemmas
# check how many Ps were removed
length(unique(df$ID)) - length(unique(df_match$ID))
# fit model again with reduced df
hVai_match <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match, "hVai_match.rds")
isSingular(hVai_match)
summary(hVai_match)


# emmeans & contrasts
emmeans_PDEPBH_match <- emmeans(hVai_match, pairwise ~ PBH|PDE, cov.reduce = range)
emmeans_PDEPBH_match
emmeans_PDEAgent_match <- emmeans(hVai_match, pairwise ~ Agent|PDE, cov.reduce = range)
emmeans_PDEAgent_match
emmeans_PBHAgent_match <- emmeans(hVai_match, pairwise ~ Agent|PBH, cov.reduce = range)
emmeans_PBHAgent_match
CI_PDEPBH_match <- confint(emmeans_PDEPBH_match)
CI_PDEPBH_match
CI_PDEAgent_match <- confint(emmeans_PDEAgent_match)
CI_PDEAgent_match
CI_PBHAgent_match <- confint(emmeans_PBHAgent_match)
CI_PBHAgent_match
contrasts_PDEPBH_match <- contrast(emmeans_PDEPBH_match, interaction = "pairwise")
contrasts_PDEPBH_match
contrasts_PDEAgent_match <- contrast(emmeans_PDEAgent_match, interaction = "pairwise")
contrasts_PDEAgent_match
contrasts_PBHAgent_match <- contrast(emmeans_PBHAgent_match, interaction = "pairwise")
contrasts_PBHAgent_match

# plots
plot_model(hVai_match, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# TOST
TOaST_match <- equivalence_test(hVai_match, rule = "classic", range = c(-0.94,0.94))
TOaST_match
plot(TOaST_match)

# Model checks
diagnostic_plots_match <- plot(check_model(hVai_match),type="discrete_both")
diagnostic_plots_match

## Excluding participants without matches for costless rescue dilemmas (human-only)

# create a variable that calculates matches between MG and MJ
reduced_df <- reduced_df %>%
  mutate(MG_match = ifelse(MG == "Yes" & MJ == 1 | 
                             MG == "No"  & MJ == 0, 1, 0))

# count matches per participant for human-only costless rescue scenarios
costless_check_all <- reduced_df %>% 
  filter(Agent %in% c("Human")) %>% 
  filter(Dilemma %in% c("costless rescue", "costless rescueCrane", "costless rescueMine", "costless rescue Truck")) %>% 
  group_by(ID) %>%
  summarise(matches = sum(MG_match),
            n_costless_seen = n(), 
            n_costless_correct = sum(MG_match == 1),
            passed_costless_all = (n_costless_seen == n_costless_correct),
            percent_costless = ((n_costless_correct / n_costless_seen) *100),
            .groups = "drop")

costless_check_all

# print rows where passed_costless_all is FALSE
costless_check_all %>%
  filter(passed_costless_all == FALSE)


## at least half were incorrect. Due to the vast number, we remove if more than 1 incorrect (rather than any)
reduced_df_match <- reduced_df %>%
  filter(ID %in% costless_check_all$ID[costless_check_all$percent_costless != 0]) # keep if 50-100% correct on costless rescue dilemmas
# check how many Ps were removed
length(unique(reduced_df$ID)) - length(unique(reduced_df_match$ID))
# fit model again with reduced reduced_df
hVai_match <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                    control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match, "reducedhVai_match.rds")
isSingular(hVai_match)
summary(hVai_match)


# emmeans & contrasts
emmeans_PDEPBH_match <- emmeans(hVai_match, pairwise ~ PBH|PDE, cov.reduce = range)
emmeans_PDEPBH_match
emmeans_PDEAgent_match <- emmeans(hVai_match, pairwise ~ Agent|PDE, cov.reduce = range)
emmeans_PDEAgent_match
emmeans_PBHAgent_match <- emmeans(hVai_match, pairwise ~ Agent|PBH, cov.reduce = range)
emmeans_PBHAgent_match
CI_PDEPBH_match <- confint(emmeans_PDEPBH_match)
CI_PDEPBH_match
CI_PDEAgent_match <- confint(emmeans_PDEAgent_match)
CI_PDEAgent_match
CI_PBHAgent_match <- confint(emmeans_PBHAgent_match)
CI_PBHAgent_match
contrasts_PDEPBH_match <- contrast(emmeans_PDEPBH_match, interaction = "pairwise")
contrasts_PDEPBH_match
contrasts_PDEAgent_match <- contrast(emmeans_PDEAgent_match, interaction = "pairwise")
contrasts_PDEAgent_match
contrasts_PBHAgent_match <- contrast(emmeans_PBHAgent_match, interaction = "pairwise")
contrasts_PBHAgent_match

# plots
plot_model(hVai_match, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme

# TOST
TOaST_reduced_match <- equivalence_test(hVai_match, rule = "classic", range = c(-0.94,0.94))
TOaST_reduced_match
plot(TOaST_reduced_match)

# Model checks
diagnostic_plots_reduced_match <- plot(check_model(hVai_match),type="discrete_both")
diagnostic_plots_reduced_match 


## best model was PDExPBH only, so let's do EMMEANS, Contrasts, plots, TOST and model checks for that
# first with regular df
hVai_PDEPBH <- glmer(MJ ~ PBH*PDE +  (1+PBH*PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_PDEPBH, "hVai_PDEPBH.rds")
isSingular(hVai_PDEPBH)
summary(hVai_PDEPBH)

# emmeans
emmeans_PDEPBH <- emmeans(hVai_PDEPBH, pairwise ~ PBH|PDE, cov.reduce = range)
emmeans_PDEPBH
CI_PDEPBH <- confint(emmeans_PDEPBH)
CI_PDEPBH
# contrasts
contrasts_PDEPBH <- contrast(emmeans_PDEPBH, interaction = "pairwise")
contrasts_PDEPBH

# plots
plot_model(hVai_PDEPBH, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme
# TOST (using 0.94 as bounds)
TOaST_PDEPBH <- equivalence_test(hVai_PDEPBH, rule = "classic", range = c(-0.94,0.94))
TOaST_PDEPBH
plot(TOaST_PDEPBH)
# model checks
diagnostic_plots_PDEPBH <- plot(check_model(hVai_PDEPBH),type="discrete_both")
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for PDE x PBH Model
</div>
')
diagnostic_plots_PDEPBH
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# next with reduced_df
hVai_PDEPBH <- glmer(MJ ~ PBH*PDE +  (1+PBH*PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_PDEPBH, "reducedhVai_PDEPBH.rds")
isSingular(hVai_PDEPBH)
summary(hVai_PDEPBH)

# emmeans
emmeans_PDEPBH <- emmeans(hVai_PDEPBH, pairwise ~ PBH|PDE, cov.reduce = range)
emmeans_PDEPBH
CI_PDEPBH <- confint(emmeans_PDEPBH)
CI_PDEPBH
# contrasts
contrasts_PDEPBH <- contrast(emmeans_PDEPBH, interaction = "pairwise")
contrasts_PDEPBH

# plots
plot_model(hVai_PDEPBH, type = "int", transform = c("exp")) + labs(title = NULL) #+ georgia_theme
# TOST (using 0.94 as bounds)
TOaST_reduced_PDEPBH <- equivalence_test(hVai_PDEPBH, rule = "classic", range = c(-0.94,0.94))
TOaST_reduced_PDEPBH
plot(TOaST_reduced_PDEPBH)
# model checks
diagnostic_plots_reduced_PDEPBH <- plot(check_model(hVai_PDEPBH),type="discrete_both")
diagnostic_plots_reduced_PDEPBH




## write function arguments for reduced_df
# prep names etc. 
model_comp <- readRDS("outputs/model_comp_reduced.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
run_full_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_reducedhVai_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Reduced Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Reduced Model",
  emmeans_list = list(
    agent = emmeans_reducedhVai
  ),
  contrasts_list = list(
    agent = contrasts_reducedhVai
  ),
  tost_obj = TOaST_reduced,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_reduced,
  prefix = "Study2_reduced"
)

## full model w. RT modelling

run_full_analysis(
  model = readRDS("outputs/hVaiRT.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Full Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Full Model",
  emmeans_list = list(
    agent = emmeans_hVaiRT
  ),
  contrasts_list = list(
    agent = contrasts_hVaiRT
  ),
  tost_obj = TOaST_hVaiRT,
  model_comp = NULL,
  diagnostic_plots = diagnostic_plots_hVaiRT,
  prefix = "Study2_RT"
)

## logRT as RE
# prep names etc. 
run_full_analysis(
  model = readRDS("outputs/hVai_logRTRE.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Full Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Full Model",
  emmeans_list = list(
    agent = emmeans_logRTRE
  ),
  contrasts_list = list(
    agent = contrasts_logRTRE
  ),
  tost_obj = TOaST_logRTRE,
  model_comp = NULL,
  diagnostic_plots = diagnostic_plots_logRTRE,
  prefix = "Study2_logRTRE"
)


run_full_analysis(
  model = readRDS("outputs/hVaiRT.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Full Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Full Model",
  emmeans_list = list(
    agent = emmeans_hVaiRT
  ),
  contrasts_list = list(
    agent = contrasts_hVaiRT
  ),
  tost_obj = TOaST_hVaiRT,
  model_comp = NULL,
  diagnostic_plots = diagnostic_plots_hVaiRT,
  prefix = "Study2_RT"
)

## logRT as RE
# prep names etc. 
run_full_analysis(
  model = readRDS("outputs/hVai_logRTRE.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Full Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Full Model",
  emmeans_list = list(
    agent = emmeans_logRTRE
  ),
  contrasts_list = list(
    agent = contrasts_logRTRE
  ),
  tost_obj = TOaST_logRTRE,
  model_comp = NULL,
  diagnostic_plots = diagnostic_plots_logRTRE,
  prefix = "Study2_logRTRE"
)
































### main analysis w. full data 
# regression table for OR stats
tab_model(hVai,
          show.se = TRUE,
          string.se = c("SE"),
          string.est = c("OR"),
          dv.labels = c("Moral Judgement"),
          linebreak = TRUE,
          show.aic = TRUE,
          show.dev = TRUE,
          show.loglik = TRUE,
          show.reflvl = TRUE,
          show.re.var = TRUE,
          show.ngroups = TRUE,
          p.style = "stars",
          emph.p=FALSE,
          pred.labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
          title = "Regression Table for Overall Model",
          CSS = list(
            css.table = 'border-collapse: collapse;',
            css.thead = 'border-top: 1px solid black !important;',
            '.col2' = 'border-bottom: 1px solid black !important;',  # Line above Random Effects
            'td' = 'border: none !important;',
            css.footer = 'border: none !important;'
          )
)






# Regression table for log-odds stats
tab_model(hVai,
          show.se = TRUE,
          string.se = c("SE"),
          dv.labels = c("Moral Judgement"),
          transform = NULL,
          linebreak = TRUE,
          show.aic = TRUE,
          show.dev = TRUE,
          show.loglik = TRUE,
          show.reflvl = TRUE,
          show.re.var = TRUE,
          show.ngroups = TRUE,
          p.style = "stars",
          emph.p=FALSE,
          pred.labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
          title = "Regression Table for Overall Model",
          CSS = list(
            css.table = 'border-collapse: collapse;',
            css.thead = 'border-top: 1px solid black !important;',
            '.col2' = 'border-bottom: 1px solid black !important;',  # Line above Random Effects
            'td' = 'border: none !important;',
            css.footer = 'border: none !important;'
          )
          
)

### simple effects analyses tables
## emmeans
# By Agent
emm_agent <- as.data.frame(emmeans_hVai$emmeans)
emm_agent_CI <- as.data.frame(CIhVai)

emm_agent <- emm_agent %>% 
  dplyr::select(Agent, PBH, PDE, emmean, SE, asymp.LCL, asymp.UCL)
cat('
<div class="apa-table-title">
  <span class="table-label">Table 3.</span> Estimated marginal means by PBH × PDE within Agent
</div>')
kable(
  emm_agent,
  booktabs = TRUE, 
  longtable = TRUE,
  escape = FALSE,
  digits = 2,
  label = NA,
  col.names = c("Agent", "PBH", "PDE", "EMM", "SE", "LCI", "UCI"),
  align = c("l", "c", "c", "c", "c", "c","c"),
  caption = NULL
) %>%
  row_spec(row = 0, align = "c") %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "EMMs are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )


# By PBH
emm_PBH <- as.data.frame(emmeans_PBHvN$emmeans)
emm_PBH_CI <- as.data.frame(CIPBHvN)
emm_PBH <- emm_PBH %>% 
  dplyr::select(PBH, Agent, PDE, emmean, SE, asymp.LCL, asymp.UCL)
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 4.</span> Estimated marginal means by PDE x Agent within PBH
</div>')
kable(
  emm_PBH,
  booktabs = TRUE, 
  longtable = TRUE,
  escape = FALSE,
  digits = 2,
  label = NA,
  col.names = c("PBH", "Agent", "PDE", "EMM", "SE", "LCI", "UCI"),
  align = c("l", "c", "c", "c", "c", "c","c"),
  caption = NULL
) %>%
  row_spec(row = 0, align = "c") %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "EMMs are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )

# By PDE
emm_PDE <- as.data.frame(emmeans_PDEvN$emmeans)
emm_PDE_CI <- as.data.frame(confint(emmeans_PDEvN))

emm_PDE <- emm_PDE %>% 
  dplyr::select(PDE, Agent, PBH, emmean, SE, asymp.LCL, asymp.UCL)

cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 5.</span> Estimated marginal means by Agent × PDE within PDE
</div>')



kable(
  emm_PDE,
  booktabs = TRUE, 
  longtable = TRUE,
  escape = FALSE,
  digits = 2,
  label = NA,
  caption = NULL,              # suppress internal caption
  col.names = c("PDE", "Agent", "PBH", "EMM", "SE", "LCI", "UCI"),
  align = c("l", "c", "c", "c", "c", "c", "c")
) %>%
  row_spec(0, align = "c") %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "EMMs are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
cat('<br></br>')



#### PBH x PDE interaction
emm_PBHvPDE <- as.data.frame(emmeans_PBHvPDE$emmeans)
emm_PBHvPDE_CI <- as.data.frame(CIhVai_PBHvPDE)
emm_PBHvPDE <- emm_PBHvPDE %>% 
  dplyr::select(PBH, PDE, emmean, SE, asymp.LCL, asymp.UCL)
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 4.</span> Estimated marginal means of PDE within PBH
</div>')
kable(
  emm_PBHvPDE,
  booktabs = TRUE, 
  longtable = TRUE,
  escape = FALSE,
  digits = 2,
  label = NA,
  col.names = c("PBH", "PDE", "EMM", "SE", "LCI", "UCI"),
  align = c("l", "c", "c", "c", "c", "c","c"),
  caption = NULL
) %>%
  row_spec(row = 0, align = "c") %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "EMMs are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )

## contrasts
contrast_agent <- as.data.frame(contrasts_hVai)

# Clean and rename
contrast_agent <- contrast_agent %>%
  dplyr::select(Agent, PBH_pairwise, PDE_pairwise, estimate, SE,  z.ratio, p.value) %>%
  rename(
    Agent = Agent, 
    PBH = PBH_pairwise,
    PDE = PDE_pairwise,
    Est      = estimate,
    SE = SE, 
    z = z.ratio,
    p        = p.value
  )

cat('
<div class="apa-table-title">
  <span class="table-label">Table 6.</span> Pairwise contrasts for PBH × PDE within Agent
</div>')
kable(
  contrast_agent,
  booktabs = TRUE,
  longtable = TRUE,
  digits = 3,
  col.names = c("Agent", "PBH", "PDE", "Estimate", "SE", "z", "p"),
  align = c("c","l","c","c","c","c","c","c"),
  caption = NULL
) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "Estimates are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )

contrast <- as.data.frame(contrasts_PBHvN)

# Clean and rename
contrast <- contrast %>%
  dplyr::select(PBH,Agent_pairwise, PDE_pairwise, estimate, SE,  z.ratio, p.value) %>%
  rename(
    PBH = PBH, 
    Agent = Agent_pairwise,
    PDE = PDE_pairwise,
    Est      = estimate,
    SE = SE, 
    z = z.ratio,
    p        = p.value
  )
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 7.</span> Pairwise contrasts for PBH × Agent within PBH
</div>')
kable(
  contrast,
  booktabs = TRUE,
  longtable = TRUE,
  digits = 3,
  col.names = c("PBH", "Agent", "PDE", "Estimate", "SE", "z", "p"),
  align = c("c","l","c","c","c","c","c","c"),
  caption = NULL
) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "Estimates are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )

contrast <- as.data.frame(contrasts_PDEvN)

# Clean and rename
contrast <- contrast %>%
  dplyr::select(PDE,Agent_pairwise, PBH_pairwise, estimate, SE,  z.ratio, p.value) %>%
  rename(
    PDE = PDE, 
    Agent = Agent_pairwise,
    PBH = PBH_pairwise,
    Est      = estimate,
    SE = SE, 
    z = z.ratio,
    p        = p.value
  )
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 8.</span> Pairwise contrasts for PDE × Agent within PDE
</div>')
kable(
  contrast,
  booktabs = TRUE,
  longtable = TRUE,
  digits = 3,
  col.names = c("PDE", "Agent", "PBH", "Estimate", "SE", "z", "p"),
  align = c("c","l","c","c","c","c","c","c"),
  caption = NULL
) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "Estimates are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
cat('<br></br>')


## PBH x PDE interaction
contrast <- as.data.frame(contrasts_PBHvPDE)

# Clean and rename
contrast <- contrast %>%
  dplyr::select(PBH, PDE_pairwise, estimate, SE,  z.ratio, p.value) %>%
  rename(
    PBH = PBH, 
    PDE = PDE_pairwise,
    Est      = estimate,
    SE = SE, 
    z = z.ratio,
    p        = p.value
  )
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 8.</span> Pairwise contrasts for PDE within PBH
</div>')
kable(
  contrast,
  booktabs = TRUE,
  longtable = TRUE,
  digits = 3,
  col.names = c("PBH",  "PDE", "Estimate", "SE", "z", "p"),
  align = c("c","l","c","c","c","c","c","c"),
  caption = NULL
) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "Estimates are on the log-odds scale.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
cat('<br></br>')


### table & figure for tost 
toast <- as.data.frame(TOaST)

toast <- toast %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )

cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 10.</span> TOST-test for Practical Equivalence for Main Model
</div>')
kable(
  toast,
  booktabs = TRUE,
  longtable = TRUE,
  digits = 3,
  col.names = c("Parameter", "LCI", "HCI", "Equivalence", "p"),
  align = c("l","c","c","c","c"),
  caption = NULL
) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "Note.",
    general = "Bounds set to mid-point value of small effect size boundary for log odds (±0.94). This is what the study was actually powered to detect for a three-way interaction. ",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
cat('<br></br>')

p <-plot(TOaST)

# Custom colours & fonts
p_custom <- p +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_family = "Georgia") +
  theme(
    text        = element_text(family = "Georgia"),
    axis.title  = element_text(family = "Georgia"),
    axis.text   = element_text(family = "Georgia"),
    legend.text = element_text(family = "Georgia"),
    legend.title= element_text(family = "Georgia")
  )
cat('<br></br>')
cat(' <div class="apa-figure-title">   <span class="figure-label">Figure 9.</span><br>
  TOST results for Main Analysis 
</div>
')
p_custom
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Bounds set to mid-point value of small effect size boundary for log odds (±0.94). This is what the study was actually powered to detect for a three-way interaction. 
</div>')
cat('<br></br>')

### model comparison table 
model_comp_df <- as.data.frame(model_comp)
model_comp_df <-  model_comp_df %>% 
  dplyr::select(
    Name, R2_marginal, R2_conditional, AIC, BIC
  ) 
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
cat('<br></br>')
cat('
<div class="apa-table-title">
  <span class="table-label">Table 17.</span> Comparison of logistic GLMMs with alternative fixed-factor combinations predicting moral judgments
</div>')
kable(model_comp_df,
      booktabs = TRUE,
      caption = NULL,
      digits = 2,
      col.names = c("Model","R² Marg*", "R² Cond**", "AIC", "BIC"),
      align = c("l","c","c","c","c"),) %>%
  kable_styling(full_width = TRUE) %>%
  footnote(
    general_title = "*",
    general = "Marg = Marginal, ** Cond = Conditional",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Lower AIC and BIC indicate better fit.
</div>')
cat('<br></br>')

### model checks

cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Posterior Predictive Check
</div>
')
diagnostic_plots[["PP_CHECK"]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Model-predicted intervals should include observed data points.
</div>')
cat('<br></br>')
### Linearity (The logit of the probability is a linear function of the predictors.)

# This plot helps assess whether the model’s mean structure is correctly specified. It shows whether the predicted probabilities systematically differ from the observed outcomes across the range of fitted values. If the model is appropriate, the binned residuals should cluster around zero with no clear pattern. Systematic curves or trends would suggest that the link function or the functional form of one or more predictors may be misspecified.
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Check of Linearity Assumption (Binned Residuals)
</div>
')
diagnostic_plots[[2]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Points should be within error bounds.
</div>')
cat('<br></br>')

### Dispersion
# (Binomial equivalent to checking for dispersion) The posterior predictive check also helps us check for dispersion. We already did this when checking the model fit. 
# We can also test with the nonparametric dispersion test from DHARMa.
cat('**Written output of DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated :**', 
    '\n\n',
    'Dispersion:', p[["statistic"]],
    '\n\n',
    '*p*:', p[["p.value"]])


### Outliers
# This plot helps assess whether the model’s mean structure is correctly specified. It shows whether the predicted probabilities systematically differ from the observed outcomes across the range of fitted values. If the model is appropriate, the binned residuals should cluster around zero with no clear pattern. Systematic curves or trends would suggest that the link function or the functional form of one or more predictors may be misspecified.
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Check for Outliers (Influential Observations)
</div>
')
diagnostic_plots[[3]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Points should be inside the contour lines.
</div>')
cat('<br></br>')

#Before checking these influential observations individually, we tested whether the residuals significantly deviate from the expected uniform distribution under the model.  
simResid <- simulate_residuals(hVai)
out_obj <- performance::check_outliers(simResid)
cat('**Written output of outlier detection test:**', 
    '\n\n',
    'Proportion of observed outliers:', out_obj[["Coefficient"]],
    '\n\n',
    'Proportion of expected outliers:', out_obj[["Expected"]], '95% CI [', out_obj[["CI_low"]],',',out_obj[["CI_high"]],']',
    '\n\n',
    'No outliers were detected (*p*:', out_obj[["p_value"]],').')

### Colinearity 
# This plot helps assess whether the model’s mean structure is correctly specified. It shows whether the predicted probabilities systematically differ from the observed outcomes across the range of fitted values. If the model is appropriate, the binned residuals should cluster around zero with no clear pattern. Systematic curves or trends would suggest that the link function or the functional form of one or more predictors may be misspecified.
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Check for Collinearity
</div>
')
diagnostic_plots[[4]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> High collinearity variation inflation factor (VIF) may inflate parameter uncertainty.
</div>')
cat('<br></br>')


### Normality of Residuals
# This plot helps assess whether the model’s mean structure is correctly specified. 
# It shows whether the predicted probabilities systematically differ from the observed outcomes across the range of fitted values. 
#If the model is appropriate, the binned residuals should cluster around zero with no clear pattern. 
#Systematic curves or trends would suggest that the link function or the functional form of one or more predictors may be misspecified.
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Checking for Uniformity of Residuals (QQ Plot) 
</div>
')
diagnostic_plots[[5]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Points should be inside the contour lines.
</div>')
cat('<br></br>')

### Normality of Random Effects
#### Participants
# Display the first plot
# Uniformity of Residuals
# Dots should fall along the lines
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Checking for Normality of Participants Random Effect 
</div>
')
diagnostic_plots[[6]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Dot should be plotted along the line.
</div>')
cat('<br></br>')

#### Dilemmas 
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  Checking for Normality of Dilemmas Random Effect 
</div>
')
diagnostic_plots[[7]]
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> Dot should be plotted along the line.
</div>')
cat('<br></br>')


## reduced df
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Reduced Model
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')



## reaction time modelling 
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 16.</span><br>
  Histograms Testing Log-RT Constants (0.1-1000 ms)
</div>
')

wrap
cat('<br></br>')

# RT plots
# row 1: PBH and PDE
#row1 <- plot_spacer() + p_PBH + plot_spacer() + p_PDE  + plot_spacer() +
# plot_layout(nrow = 1, widths = c(0.1,3,0.3, 3,0.1001))  # tiny spacer to preserve sizing

row1 <- plot_spacer() + p_PBH + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row1b <- plot_spacer() + p_PDE + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
# row 2: centred Agent plot
row2 <- plot_spacer() + p_Agent + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))

# combine rows
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 16.</span><br>
  Predicted log(RT) for PBH
</div>
')

row1
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 17.</span><br>
  Predicted log(RT) for PBH
</div>

')
row1b
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 18.</span><br>
  Predicted log(RT) for Agent
</div>
')
row2
cat('<br></br>')

# for the interactions 
# Apply formatting
p_int_list <- lapply(
  p_int_list,
  function(p) p + #georgia_theme +
    labs(title = NULL)
)

row1 <- plot_spacer() + p_int_list[[1]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row1b <- plot_spacer() + p_int_list[[2]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row2 <- plot_spacer() + p_int_list[[3]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row2b <- plot_spacer() + p_int_list[[4]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
#plot_spacer() + p_int_list[[1]]  + plot_spacer() + p_int_list[[2]]   + plot_spacer() +
#plot_layout(nrow = 1, widths = c(0.1,3,0.3, 3,0.1001))  # tiny spacer to preserve sizing

cat('<div class="apa-figure-title">   <span class="figure-label">Figure 19.</span><br>
  Predicted log(RT) for PBH x PDE 
</div>
')

row1
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 20.</span><br>
  Predicted log(RT) for PBH x Agent 
</div>
')

row1b
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 21.</span><br>
  Predicted log(RT) for PDE x Agent 
</div>
')

row2
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 22.</span><br>
  Predicted log(RT) for PBH x PDE x Agent
</div>
')

row2b
cat('<br></br>')

# adding logRT as RE
# diagnostic plots
# change theme & remove titles & subtitles

cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Model with RT as DV
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')



## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  reduced_df_temp <- reduced_df %>% mutate(logRT = log(RT + c))
  ggplot(reduced_df_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})

wrap <- wrap_plots(plots, nrow = 3)

cat('<div class="apa-figure-title">   <span class="figure-label">Figure 16.</span><br>
  Histograms Testing Log-RT Constants (0.1-1000 ms)
</div>
')

wrap
cat('<br></br>')

# main effects plots 
# row 1: PBH and PDE
#row1 <- plot_spacer() + p_PBH + plot_spacer() + p_PDE  + plot_spacer() +
# plot_layout(nrow = 1, widths = c(0.1,3,0.3, 3,0.1001))  # tiny spacer to preserve sizing

row1 <- plot_spacer() + p_PBH + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row1b <- plot_spacer() + p_PDE + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
# row 2: centred Agent plot
row2 <- plot_spacer() + p_Agent + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))

# combine rows
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 16.</span><br>
  Predicted log(RT) for PBH
</div>
')

row1
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 17.</span><br>
  Predicted log(RT) for PBH
</div>

')
row1b
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 18.</span><br>
  Predicted log(RT) for Agent
</div>
')
row2
cat('<br></br>')

# Apply formatting
p_int_list <- lapply(
  p_int_list,
  function(p) p + #georgia_theme +
    labs(title = NULL)
)

row1 <- plot_spacer() + p_int_list[[1]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row1b <- plot_spacer() + p_int_list[[2]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row2 <- plot_spacer() + p_int_list[[3]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
row2b <- plot_spacer() + p_int_list[[4]] + plot_spacer() +
  plot_layout(nrow = 1, widths = c(1, 3, 1))
#plot_spacer() + p_int_list[[1]]  + plot_spacer() + p_int_list[[2]]   + plot_spacer() +
#plot_layout(nrow = 1, widths = c(0.1,3,0.3, 3,0.1001))  # tiny spacer to preserve sizing

cat('<div class="apa-figure-title">   <span class="figure-label">Figure 19.</span><br>
  Predicted log(RT) for PBH x PDE 
</div>
')

row1
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 20.</span><br>
  Predicted log(RT) for PBH x Agent 
</div>
')

row1b
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 21.</span><br>
  Predicted log(RT) for PDE x Agent 
</div>
')

row2
cat('<br></br>')
cat('<div class="apa-figure-title">   <span class="figure-label">Figure 22.</span><br>
  Predicted log(RT) for PBH x PDE x Agent
</div>
')

row2b
cat('<br></br>')

# model w. logRT as RE w. reduced_df
# change theme & remove titles & subtitles

cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Model with RT as DV
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# intent 
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Intent Model
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# intent only predictor
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Intent-Only Model
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# intent x agent predictors w. reduced df
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Intent Model (Reduced Data)
</div>
')
diagnostic_plots_
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# intent only predictor
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Intent-Only Model (Reduced Data)
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')


# match df
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Model with Costless Rescue Exclusions
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# w. reduced data
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for Model with Costless Rescue Exclusions (Reduced Data)
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

# PBH x PDE only predictors 
cat('<div class="apa-figure-title">   <span class="figure-label">Figure X.</span><br>
  All Checks for PDE x PBH Model
</div>
')
diagnostic_plots
cat('<div style="color: black; font-size: 1em; margin-top: 0.25em;">
<span style="font-style: italic; ">Note.</span> See explanation of individual tests under model checks section for main model. 
</div>')
cat('<br></br>')

