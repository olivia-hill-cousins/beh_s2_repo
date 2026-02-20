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
packages <- c("lme4","lmerTest","performance","easystats","dplyr","emmeans","effects","tidyr","kableExtra", "sjPlot","qqplotr", "DHARMa", "ggeffects")
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
library(ggeffects)

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

set.platform("outputs")
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "fullRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropAgentdilRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## Drop PDE interaction for ID RF
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropPDEintRE_dropAgentdilRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## Drop PDE interaction for ID RF
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropAllintRE_dropAgentdilRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropPDEintRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## Drop PBH * Agent interaction slope for ID RF
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropintRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## drop PDE slope
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropPDEslope_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)
# w.o Agent dilemma slope
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropPDEslope_dropAgentDil_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## drop Agent ID slope
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropAgentIDRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)

## drop Agent ID & dilemma slope
hVai_full <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
              control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_full, "dropAgentIDdilRE_hVai_full_glmModel.rds")
isSingular(hVai_full)
summary(hVai_full)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_full <- readRDS("dropAgentIDdilRE_hVai_full_glmModel.rds")
#OR coefficients 
OR_hVai_full <- exp(fixef(hVai_full))
# confidence intervals for log-odds and OR
ci_hVai_full <- confint(hVai_full, method = "Wald")
ci_hVai_full.or <- exp(ci_hVai_full)
ses_hVai_full <- sqrt(diag(vcov(hVai_full)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_full_Agent <- emmeans(hVai_full, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_full_Agent

CI_hVai_full_Agent <- confint(emmeans_hVai_full_Agent)
CI_hVai_full_Agent

# by PBH
emmeans_hVai_full_PBH <- emmeans(hVai_full, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_full_PBH

CI_hVai_full_PBH <- confint(emmeans_hVai_full_PBH)
CI_hVai_full_PBH

# by PDE
emmeans_hVai_full_PDE <- emmeans(hVai_full, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_full_PDE

CI_hVai_full_PDE <- confint(emmeans_hVai_full_PDE)
CI_hVai_full_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_full_Agent <- contrast(emmeans_hVai_full_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_full_Agent

# # by PBH
contrasts_hVai_full_PBH <- contrast(emmeans_hVai_full_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_full_PBH

# # by PDE
contrasts_hVai_full_PDE <- contrast(emmeans_hVai_full_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_full_PDE

### Plots
set_theme(georgia_theme)
plot_model(hVai_full, terms = "PBH", type = "pred")
plot_model(hVai_full, terms = "PDE", type = "pred")
plot_model(hVai_full, terms = "Agent", type = "pred")
plot_model(hVai_full, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_full <- equivalence_test(hVai_full, rule = "classic", range = c(-0.94,0.94))

toast_hVai_full <- as.data.frame(TOaST_hVai_full)

toast_hVai_full <- toast_hVai_full %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_full
plot(TOaST_hVai_full)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_full,
  name = "Study2_full"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_full <- plot(check_model(hVai_full, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_full <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_full


p_hVai_full <- testDispersion(hVai_full,plot=F) 
p_hVai_full

pr_hVai_full <- predict_response(hVai_full, terms = c("PBH [all]","PDE [all] ","Agent [all]"))
pr_hVai_full

plot(pr_hVai_full, show_residuals=TRUE, show_residuals_line = TRUE)

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

set.platform("outputs")
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "reducedRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropAgentdilRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## Drop PDE interaction for ID RF
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropPDEintRE_dropAgentdilRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## Drop PDE interaction for ID RF
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropAllintRE_dropAgentdilRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropPDEintRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## Drop PBH * Agent interaction slope for ID RF
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropintRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## drop PDE slope
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropPDEslope_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)
# w.o Agent dilemma slope
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropPDEslope_dropAgentDil_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## drop Agent ID slope
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropAgentIDRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)

## drop Agent ID & dilemma slope
hVai_reduced <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                      control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced, "dropAgentIDdilRE_hVai_reduced_glmModel.rds")
isSingular(hVai_reduced)
summary(hVai_reduced)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced <- readRDS("dropAgentIDdilRE_hVai_reduced_glmModel.rds")
#OR coefficients 
OR_hVai_reduced <- exp(fixef(hVai_reduced))
# confidence intervals for log-odds and OR
ci_hVai_reduced <- confint(hVai_reduced, method = "Wald")
ci_hVai_reduced.or <- exp(ci_hVai_reduced)
ses_hVai_reduced <- sqrt(diag(vcov(hVai_reduced)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_Agent <- emmeans(hVai_reduced, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_Agent

CI_hVai_reduced_Agent <- confint(emmeans_hVai_reduced_Agent)
CI_hVai_reduced_Agent

# by PBH
emmeans_hVai_reduced_PBH <- emmeans(hVai_reduced, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_PBH

CI_hVai_reduced_PBH <- confint(emmeans_hVai_reduced_PBH)
CI_hVai_reduced_PBH

# by PDE
emmeans_hVai_reduced_PDE <- emmeans(hVai_reduced, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_PDE

CI_hVai_reduced_PDE <- confint(emmeans_hVai_reduced_PDE)
CI_hVai_reduced_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_Agent <- contrast(emmeans_hVai_reduced_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_Agent

# # by PBH
contrasts_hVai_reduced_PBH <- contrast(emmeans_hVai_reduced_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_PBH

# # by PDE
contrasts_hVai_reduced_PDE <- contrast(emmeans_hVai_reduced_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_PDE

### Plots
plot(allEffects(hVai_reduced))
set_theme(georgia_theme)
plot_model(hVai_reduced, terms = "PBH", type = "pred")
plot_model(hVai_reduced, terms = "PDE", type = "pred")
plot_model(hVai_reduced, terms = "Agent", type = "pred")
plot_model(hVai_reduced, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced <- equivalence_test(hVai_reduced, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced <- as.data.frame(TOaST_hVai_reduced)

toast_hVai_reduced <- toast_hVai_reduced %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced
plot(TOaST_hVai_reduced)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced,
  name = "Study2_reduced"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced <- plot(check_model(hVai_reduced, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced <- lapply(diagnostic_plots_hVai_reduced, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced


p_hVai_reduced <- testDispersion(hVai_reduced,plot=F) 
p_hVai_reduced


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

## drop Agent ID & dilemma slope
hVai_RT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df,
                 control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_RT, "dropAgentIDdilRE_hVai_RT_glmModel.rds")
isSingular(hVai_RT)
summary(hVai_RT)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_RT <- readRDS("dropAgentIDdilRE_hVai_RT_glmModel.rds")
#OR coefficients 
OR_hVai_RT <- exp(fixef(hVai_RT))
# confidence intervals for log-odds and OR
ci_hVai_RT <- confint(hVai_RT, method = "Wald")
ci_hVai_RT.or <- exp(ci_hVai_RT)
ses_hVai_RT <- sqrt(diag(vcov(hVai_RT)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_RT_Agent <- emmeans(hVai_RT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_RT_Agent

CI_hVai_RT_Agent <- confint(emmeans_hVai_RT_Agent)
CI_hVai_RT_Agent

# by PBH
emmeans_hVai_RT_PBH <- emmeans(hVai_RT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_RT_PBH

CI_hVai_RT_PBH <- confint(emmeans_hVai_RT_PBH)
CI_hVai_RT_PBH

# by PDE
emmeans_hVai_RT_PDE <- emmeans(hVai_RT, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_RT_PDE

CI_hVai_RT_PDE <- confint(emmeans_hVai_RT_PDE)
CI_hVai_RT_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_RT_Agent <- contrast(emmeans_hVai_RT_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_RT_Agent

# # by PBH
contrasts_hVai_RT_PBH <- contrast(emmeans_hVai_RT_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_RT_PBH

# # by PDE
contrasts_hVai_RT_PDE <- contrast(emmeans_hVai_RT_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_RT_PDE

### Plots
plot(allEffects(hVai_RT))
set_theme(georgia_theme)
plot_model(hVai_RT, terms = "PBH", type = "pred")
plot_model(hVai_RT, terms = "PDE", type = "pred")
plot_model(hVai_RT, terms = "Agent", type = "pred")
plot_model(hVai_RT, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_RT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_RT <- equivalence_test(hVai_RT, rule = "classic", range = c(-bound,bound))

toast_hVai_RT <- as.data.frame(TOaST_hVai_RT)

toast_hVai_RT <- toast_hVai_RT %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_RT
plot(TOaST_hVai_RT)

## Model comparisons
model_comparison_function(
  DV = "logRT",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = default,
  control_stuff = "continuous",
  full_model = hVai_RT,
  name = "Study2_RT"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_RT <- plot(check_model(hVai_RT, panel = FALSE))

# change theme & remove titles & subtitles
diagnostic_plots_hVai_RT <- lapply(diagnostic_plots_hVai_RT, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_RT


p_hVai_RT <- testDispersion(hVai_RT,plot=F) 
p_hVai_RT

## Reaction time modelling as Random Effect
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

## drop Agent ID & dilemma slope
hVai_RTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE+logRT|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_RTRE, "dropAgentIDdilRE_hVai_RTRE_glmModel.rds")
isSingular(hVai_RTRE)
summary(hVai_RTRE)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_RTRE <- readRDS("dropAgentIDdilRE_hVai_RTRE_glmModel.rds")
#OR coefficients 
OR_hVai_RTRE <- exp(fixef(hVai_RTRE))
# confidence intervals for log-odds and OR
ci_hVai_RTRE <- confint(hVai_RTRE, method = "Wald")
ci_hVai_RTRE.or <- exp(ci_hVai_RTRE)
ses_hVai_RTRE <- sqrt(diag(vcov(hVai_RTRE)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_RTRE_Agent <- emmeans(hVai_RTRE, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_RTRE_Agent

CI_hVai_RTRE_Agent <- confint(emmeans_hVai_RTRE_Agent)
CI_hVai_RTRE_Agent

# by PBH
emmeans_hVai_RTRE_PBH <- emmeans(hVai_RTRE, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_RTRE_PBH

CI_hVai_RTRE_PBH <- confint(emmeans_hVai_RTRE_PBH)
CI_hVai_RTRE_PBH

# by PDE
emmeans_hVai_RTRE_PDE <- emmeans(hVai_RTRE, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_RTRE_PDE

CI_hVai_RTRE_PDE <- confint(emmeans_hVai_RTRE_PDE)
CI_hVai_RTRE_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_RTRE_Agent <- contrast(emmeans_hVai_RTRE_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_RTRE_Agent

# # by PBH
contrasts_hVai_RTRE_PBH <- contrast(emmeans_hVai_RTRE_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_RTRE_PBH

# # by PDE
contrasts_hVai_RTRE_PDE <- contrast(emmeans_hVai_RTRE_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_RTRE_PDE

### Plots
plot(allEffects(hVai_RTRE))
set_theme(georgia_theme)
plot_model(hVai_RTRE, terms = "PBH", type = "pred")
plot_model(hVai_RTRE, terms = "PDE", type = "pred")
plot_model(hVai_RTRE, terms = "Agent", type = "pred")
plot_model(hVai_RTRE, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_RTRE)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_RTRE <- equivalence_test(hVai_RTRE, rule = "classic", range = c(-bound,bound))

toast_hVai_RTRE <- as.data.frame(TOaST_hVai_RTRE)

toast_hVai_RTRE <- toast_hVai_RTRE %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_RTRE
plot(TOaST_hVai_RTRE)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_RTRE,
  name = "Study2_RTRE"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_RTRE <- plot(check_model(hVai_RTRE, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_RTRE <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_RTRE


p_hVai_RTRE <- testDispersion(hVai_RTRE,plot=F) 
p_hVai_RTRE

## Intent x Agent
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "intentxAgentRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropAgentdilRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## Drop PDE interaction for ID RF
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropPDEintRE_dropAgentdilRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## Drop PDE interaction for ID RF
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropAllintRE_dropAgentdilRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropPDEintRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## Drop PBH * Agent interaction slope for ID RF
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropintRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## drop PDE slope
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropPDEslope_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)
# w.o Agent dilemma slope
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropPDEslope_dropAgentDil_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## drop Agent ID slope
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropAgentIDRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)

## drop Agent ID & dilemma slope
hVai_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intentxAgent, "dropAgentIDdilRE_hVai_intentxAgent_glmModel.rds")
isSingular(hVai_intentxAgent)
summary(hVai_intentxAgent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_intentxAgent <- readRDS("dropAgentIDdilRE_hVai_intentxAgent_glmModel.rds")
#OR coefficients 
OR_hVai_intentxAgent <- exp(fixef(hVai_intentxAgent))
# confidence intervals for log-odds and OR
ci_hVai_intentxAgent <- confint(hVai_intentxAgent, method = "Wald")
ci_hVai_intentxAgent.or <- exp(ci_hVai_intentxAgent)
ses_hVai_intentxAgent <- sqrt(diag(vcov(hVai_intentxAgent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_intentxAgent_Agent <- emmeans(hVai_intentxAgent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_intentxAgent_Agent

CI_hVai_intentxAgent_Agent <- confint(emmeans_hVai_intentxAgent_Agent)
CI_hVai_intentxAgent_Agent

# by PBH
emmeans_hVai_intentxAgent_PBH <- emmeans(hVai_intentxAgent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_intentxAgent_PBH

CI_hVai_intentxAgent_PBH <- confint(emmeans_hVai_intentxAgent_PBH)
CI_hVai_intentxAgent_PBH

# by PDE
emmeans_hVai_intentxAgent_PDE <- emmeans(hVai_intentxAgent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_intentxAgent_PDE

CI_hVai_intentxAgent_PDE <- confint(emmeans_hVai_intentxAgent_PDE)
CI_hVai_intentxAgent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_intentxAgent_Agent <- contrast(emmeans_hVai_intentxAgent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_intentxAgent_Agent

# # by PBH
contrasts_hVai_intentxAgent_PBH <- contrast(emmeans_hVai_intentxAgent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_intentxAgent_PBH

# # by PDE
contrasts_hVai_intentxAgent_PDE <- contrast(emmeans_hVai_intentxAgent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_intentxAgent_PDE

### Plots
plot(allEffects(hVai_intentxAgent))
set_theme(georgia_theme)
plot_model(hVai_intentxAgent, terms = "PBH", type = "pred")
plot_model(hVai_intentxAgent, terms = "PDE", type = "pred")
plot_model(hVai_intentxAgent, terms = "Agent", type = "pred")
plot_model(hVai_intentxAgent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_intentxAgent <- equivalence_test(hVai_intentxAgent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_intentxAgent <- as.data.frame(TOaST_hVai_intentxAgent)

toast_hVai_intentxAgent <- toast_hVai_intentxAgent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_intentxAgent
plot(TOaST_hVai_intentxAgent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_intentxAgent,
  name = "Study2_intentxAgent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_intentxAgent <- plot(check_model(hVai_intentxAgent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_intentxAgent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_intentxAgent


p_hVai_intentxAgent <- testDispersion(hVai_intentxAgent,plot=F) 
p_hVai_intentxAgent

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "intentRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropAgentdilRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## Drop PDE interaction for ID RF
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropPDEintRE_dropAgentdilRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## Drop PDE interaction for ID RF
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropAllintRE_dropAgentdilRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropPDEintRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## Drop PBH * Agent interaction slope for ID RF
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropintRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## drop PDE slope
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropPDEslope_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)
# w.o Agent dilemma slope
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropPDEslope_dropAgentDil_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## drop Agent ID slope
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropAgentIDRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)

## drop Agent ID & dilemma slope
hVai_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df, family = binomial(link = "logit"),
                     control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_intent, "dropAgentIDdilRE_hVai_intent_glmModel.rds")
isSingular(hVai_intent)
summary(hVai_intent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_intent <- readRDS("dropAgentIDdilRE_hVai_intent_glmModel.rds")
#OR coefficients 
OR_hVai_intent <- exp(fixef(hVai_intent))
# confidence intervals for log-odds and OR
ci_hVai_intent <- confint(hVai_intent, method = "Wald")
ci_hVai_intent.or <- exp(ci_hVai_intent)
ses_hVai_intent <- sqrt(diag(vcov(hVai_intent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_intent_Agent <- emmeans(hVai_intent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_intent_Agent

CI_hVai_intent_Agent <- confint(emmeans_hVai_intent_Agent)
CI_hVai_intent_Agent

# by PBH
emmeans_hVai_intent_PBH <- emmeans(hVai_intent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_intent_PBH

CI_hVai_intent_PBH <- confint(emmeans_hVai_intent_PBH)
CI_hVai_intent_PBH

# by PDE
emmeans_hVai_intent_PDE <- emmeans(hVai_intent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_intent_PDE

CI_hVai_intent_PDE <- confint(emmeans_hVai_intent_PDE)
CI_hVai_intent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_intent_Agent <- contrast(emmeans_hVai_intent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_intent_Agent

# # by PBH
contrasts_hVai_intent_PBH <- contrast(emmeans_hVai_intent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_intent_PBH

# # by PDE
contrasts_hVai_intent_PDE <- contrast(emmeans_hVai_intent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_intent_PDE

### Plots
plot(allEffects(hVai_intent))
set_theme(georgia_theme)
plot_model(hVai_intent, terms = "PBH", type = "pred")
plot_model(hVai_intent, terms = "PDE", type = "pred")
plot_model(hVai_intent, terms = "Agent", type = "pred")
plot_model(hVai_intent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_intent <- equivalence_test(hVai_intent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_intent <- as.data.frame(TOaST_hVai_intent)

toast_hVai_intent <- toast_hVai_intent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_intent
plot(TOaST_hVai_intent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_intent,
  name = "Study2_intent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_intent <- plot(check_model(hVai_intent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_intent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_intent


p_hVai_intent <- testDispersion(hVai_intent,plot=F) 
p_hVai_intent

#### REDUCED DATA
## Reaction time modelling 
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
wrap

## log-transform RT with selected constant
reduced_df$logRT <- log(reduced_df$RT + 1)

## drop Agent ID & dilemma slope
hVai_reduced_RT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df,
                        control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_RT, "dropAgentIDdilRE_hVai_reduced_RT_glmModel.rds")
isSingular(hVai_reduced_RT)
summary(hVai_reduced_RT)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_RT <- readRDS("dropAgentIDdilRE_hVai_reduced_RT_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_RT <- exp(fixef(hVai_reduced_RT))
# confidence intervals for log-odds and OR
ci_hVai_reduced_RT <- confint(hVai_reduced_RT, method = "Wald")
ci_hVai_reduced_RT.or <- exp(ci_hVai_reduced_RT)
ses_hVai_reduced_RT <- sqrt(diag(vcov(hVai_reduced_RT)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_RT_Agent <- emmeans(hVai_reduced_RT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_RT_Agent

CI_hVai_reduced_RT_Agent <- confint(emmeans_hVai_reduced_RT_Agent)
CI_hVai_reduced_RT_Agent

# by PBH
emmeans_hVai_reduced_RT_PBH <- emmeans(hVai_reduced_RT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_RT_PBH

CI_hVai_reduced_RT_PBH <- confint(emmeans_hVai_reduced_RT_PBH)
CI_hVai_reduced_RT_PBH

# by PDE
emmeans_hVai_reduced_RT_PDE <- emmeans(hVai_reduced_RT, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_RT_PDE

CI_hVai_reduced_RT_PDE <- confint(emmeans_hVai_reduced_RT_PDE)
CI_hVai_reduced_RT_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_RT_Agent <- contrast(emmeans_hVai_reduced_RT_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_RT_Agent

# # by PBH
contrasts_hVai_reduced_RT_PBH <- contrast(emmeans_hVai_reduced_RT_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_RT_PBH

# # by PDE
contrasts_hVai_reduced_RT_PDE <- contrast(emmeans_hVai_reduced_RT_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_RT_PDE

### Plots
plot(allEffects(hVai_reduced_RT))
set_theme(georgia_theme)
plot_model(hVai_reduced_RT, terms = "PBH", type = "pred")
plot_model(hVai_reduced_RT, terms = "PDE", type = "pred")
plot_model(hVai_reduced_RT, terms = "Agent", type = "pred")
plot_model(hVai_reduced_RT, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_reduced_RT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_reduced_RT <- equivalence_test(hVai_reduced_RT, rule = "classic", range = c(-bound,bound))

toast_hVai_reduced_RT <- as.data.frame(TOaST_hVai_reduced_RT)

toast_hVai_reduced_RT <- toast_hVai_reduced_RT %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_RT
plot(TOaST_hVai_reduced_RT)

## Model comparisons
model_comparison_function(
  DV = "logRT",
  data = reduced_df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = default,
  control_stuff = "continuous",
  full_model = hVai_reduced_RT,
  name = "Study2_reduced_RT"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_RT <- plot(check_model(hVai_reduced_RT, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_RT <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_RT


p_hVai_reduced_RT <- testDispersion(hVai_reduced_RT,plot=F) 
p_hVai_reduced_RT

## Reaction time modelling as Random Effect
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
wrap

## log-transform RT with selected constant
reduced_df$logRT <- log(reduced_df$RT + 1)

## drop Agent ID & dilemma slope
hVai_reduced_RTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_RTRE, "dropAgentIDdilRE_hVai_reduced_RTRE_glmModel.rds")
isSingular(hVai_reduced_RTRE)
summary(hVai_reduced_RTRE)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_RTRE <- readRDS("dropAgentIDdilRE_hVai_reduced_RTRE_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_RTRE <- exp(fixef(hVai_reduced_RTRE))
# confidence intervals for log-odds and OR
ci_hVai_reduced_RTRE <- confint(hVai_reduced_RTRE, method = "Wald")
ci_hVai_reduced_RTRE.or <- exp(ci_hVai_reduced_RTRE)
ses_hVai_reduced_RTRE <- sqrt(diag(vcov(hVai_reduced_RTRE)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_RTRE_Agent <- emmeans(hVai_reduced_RTRE, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_RTRE_Agent

CI_hVai_reduced_RTRE_Agent <- confint(emmeans_hVai_reduced_RTRE_Agent)
CI_hVai_reduced_RTRE_Agent

# by PBH
emmeans_hVai_reduced_RTRE_PBH <- emmeans(hVai_reduced_RTRE, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_RTRE_PBH

CI_hVai_reduced_RTRE_PBH <- confint(emmeans_hVai_reduced_RTRE_PBH)
CI_hVai_reduced_RTRE_PBH

# by PDE
emmeans_hVai_reduced_RTRE_PDE <- emmeans(hVai_reduced_RTRE, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_RTRE_PDE

CI_hVai_reduced_RTRE_PDE <- confint(emmeans_hVai_reduced_RTRE_PDE)
CI_hVai_reduced_RTRE_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_RTRE_Agent <- contrast(emmeans_hVai_reduced_RTRE_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_RTRE_Agent

# # by PBH
contrasts_hVai_reduced_RTRE_PBH <- contrast(emmeans_hVai_reduced_RTRE_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_RTRE_PBH

# # by PDE
contrasts_hVai_reduced_RTRE_PDE <- contrast(emmeans_hVai_reduced_RTRE_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_RTRE_PDE

### Plots
plot(allEffects(hVai_reduced_RTRE))
set_theme(georgia_theme)
plot_model(hVai_reduced_RTRE, terms = "PBH", type = "pred")
plot_model(hVai_reduced_RTRE, terms = "PDE", type = "pred")
plot_model(hVai_reduced_RTRE, terms = "Agent", type = "pred")
plot_model(hVai_reduced_RTRE, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_reduced_RTRE)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_reduced_RTRE <- equivalence_test(hVai_reduced_RTRE, rule = "classic", range = c(-bound,bound))

toast_hVai_reduced_RTRE <- as.data.frame(TOaST_hVai_reduced_RTRE)

toast_hVai_reduced_RTRE <- toast_hVai_reduced_RTRE %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_RTRE
plot(TOaST_hVai_reduced_RTRE)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_RTRE,
  name = "Study2_reduced_RTRE"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_RTRE <- plot(check_model(hVai_reduced_RTRE, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_RTRE <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_RTRE


p_hVai_reduced_RTRE <- testDispersion(hVai_reduced_RTRE,plot=F) 
p_hVai_reduced_RTRE

## Intent x Agent
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "intentxAgentRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropAgentdilRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## Drop PDE interaction for ID RF
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropPDEintRE_dropAgentdilRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## Drop PDE interaction for ID RF
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropAllintRE_dropAgentdilRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropPDEintRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## Drop PBH * Agent interaction slope for ID RF
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropintRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## drop PDE slope
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropPDEslope_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)
# w.o Agent dilemma slope
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropPDEslope_dropAgentDil_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## drop Agent ID slope
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropAgentIDRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)

## drop Agent ID & dilemma slope
hVai_reduced_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intentxAgent, "dropAgentIDdilRE_hVai_reduced_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_intentxAgent)
summary(hVai_reduced_intentxAgent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_intentxAgent <- readRDS("dropAgentIDdilRE_hVai_reduced_intentxAgent_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_intentxAgent <- exp(fixef(hVai_reduced_intentxAgent))
# confidence intervals for log-odds and OR
ci_hVai_reduced_intentxAgent <- confint(hVai_reduced_intentxAgent, method = "Wald")
ci_hVai_reduced_intentxAgent.or <- exp(ci_hVai_reduced_intentxAgent)
ses_hVai_reduced_intentxAgent <- sqrt(diag(vcov(hVai_reduced_intentxAgent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_intentxAgent_Agent <- emmeans(hVai_reduced_intentxAgent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_intentxAgent_Agent

CI_hVai_reduced_intentxAgent_Agent <- confint(emmeans_hVai_reduced_intentxAgent_Agent)
CI_hVai_reduced_intentxAgent_Agent

# by PBH
emmeans_hVai_reduced_intentxAgent_PBH <- emmeans(hVai_reduced_intentxAgent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_intentxAgent_PBH

CI_hVai_reduced_intentxAgent_PBH <- confint(emmeans_hVai_reduced_intentxAgent_PBH)
CI_hVai_reduced_intentxAgent_PBH

# by PDE
emmeans_hVai_reduced_intentxAgent_PDE <- emmeans(hVai_reduced_intentxAgent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_intentxAgent_PDE

CI_hVai_reduced_intentxAgent_PDE <- confint(emmeans_hVai_reduced_intentxAgent_PDE)
CI_hVai_reduced_intentxAgent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_intentxAgent_Agent <- contrast(emmeans_hVai_reduced_intentxAgent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_intentxAgent_Agent

# # by PBH
contrasts_hVai_reduced_intentxAgent_PBH <- contrast(emmeans_hVai_reduced_intentxAgent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_intentxAgent_PBH

# # by PDE
contrasts_hVai_reduced_intentxAgent_PDE <- contrast(emmeans_hVai_reduced_intentxAgent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_intentxAgent_PDE

### Plots
plot(allEffects(hVai_reduced_intentxAgent))
set_theme(georgia_theme)
plot_model(hVai_reduced_intentxAgent, terms = "PBH", type = "pred")
plot_model(hVai_reduced_intentxAgent, terms = "PDE", type = "pred")
plot_model(hVai_reduced_intentxAgent, terms = "Agent", type = "pred")
plot_model(hVai_reduced_intentxAgent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced_intentxAgent <- equivalence_test(hVai_reduced_intentxAgent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced_intentxAgent <- as.data.frame(TOaST_hVai_reduced_intentxAgent)

toast_hVai_reduced_intentxAgent <- toast_hVai_reduced_intentxAgent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_intentxAgent
plot(TOaST_hVai_reduced_intentxAgent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_intentxAgent,
  name = "Study2_reduced_intentxAgent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_intentxAgent <- plot(check_model(hVai_reduced_intentxAgent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_intentxAgent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_intentxAgent


p_hVai_reduced_intentxAgent <- testDispersion(hVai_reduced_intentxAgent,plot=F) 
p_hVai_reduced_intentxAgent

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "intentRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropAgentdilRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## Drop PDE interaction for ID RF
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropPDEintRE_dropAgentdilRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## Drop PDE interaction for ID RF
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropAllintRE_dropAgentdilRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropPDEintRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## Drop PBH * Agent interaction slope for ID RF
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropintRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## drop PDE slope
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropPDEslope_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)
# w.o Agent dilemma slope
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropPDEslope_dropAgentDil_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## drop Agent ID slope
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropAgentIDRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)

## drop Agent ID & dilemma slope
hVai_reduced_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df, family = binomial(link = "logit"),
                             control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_intent, "dropAgentIDdilRE_hVai_reduced_intent_glmModel.rds")
isSingular(hVai_reduced_intent)
summary(hVai_reduced_intent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_intent <- readRDS("dropAgentIDdilRE_hVai_reduced_intent_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_intent <- exp(fixef(hVai_reduced_intent))
# confidence intervals for log-odds and OR
ci_hVai_reduced_intent <- confint(hVai_reduced_intent, method = "Wald")
ci_hVai_reduced_intent.or <- exp(ci_hVai_reduced_intent)
ses_hVai_reduced_intent <- sqrt(diag(vcov(hVai_reduced_intent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_intent_Agent <- emmeans(hVai_reduced_intent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_intent_Agent

CI_hVai_reduced_intent_Agent <- confint(emmeans_hVai_reduced_intent_Agent)
CI_hVai_reduced_intent_Agent

# by PBH
emmeans_hVai_reduced_intent_PBH <- emmeans(hVai_reduced_intent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_intent_PBH

CI_hVai_reduced_intent_PBH <- confint(emmeans_hVai_reduced_intent_PBH)
CI_hVai_reduced_intent_PBH

# by PDE
emmeans_hVai_reduced_intent_PDE <- emmeans(hVai_reduced_intent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_intent_PDE

CI_hVai_reduced_intent_PDE <- confint(emmeans_hVai_reduced_intent_PDE)
CI_hVai_reduced_intent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_intent_Agent <- contrast(emmeans_hVai_reduced_intent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_intent_Agent

# # by PBH
contrasts_hVai_reduced_intent_PBH <- contrast(emmeans_hVai_reduced_intent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_intent_PBH

# # by PDE
contrasts_hVai_reduced_intent_PDE <- contrast(emmeans_hVai_reduced_intent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_intent_PDE

### Plots
plot(allEffects(hVai_reduced_intent))
set_theme(georgia_theme)
plot_model(hVai_reduced_intent, terms = "PBH", type = "pred")
plot_model(hVai_reduced_intent, terms = "PDE", type = "pred")
plot_model(hVai_reduced_intent, terms = "Agent", type = "pred")
plot_model(hVai_reduced_intent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced_intent <- equivalence_test(hVai_reduced_intent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced_intent <- as.data.frame(TOaST_hVai_reduced_intent)

toast_hVai_reduced_intent <- toast_hVai_reduced_intent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_intent
plot(TOaST_hVai_reduced_intent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_intent,
  name = "Study2_reduced_intent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_intent <- plot(check_model(hVai_reduced_intent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_intent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_intent


p_hVai_reduced_intent <- testDispersion(hVai_reduced_intent,plot=F) 
p_hVai_reduced_intent

## Match Preparation
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


### Prep variables for analysis
# make sure everything is factor
df_match$Task.Name <- as.factor(df_match$Task.Name)
df_match$Agent <- as.factor(df_match$Agent)
df_match$Context <- as.factor(df_match$Context)
df_match$PDE <- as.factor(df_match$PDE)
df_match$BA <- as.factor(df_match$BA)
df_match$MC <- as.factor(df_match$MC)
df_match$Intent <- as.factor(df_match$Intent)
df_match$PBH <- as.factor(df_match$PBH)
df_match$MG <- as.factor(df_match$MG)
df_match$RT <- abs(as.numeric(df_match$RT))

## Contrasts Coding
#set deviation contrasts for ease of interpretation -.5 vs .5
c<-contr.treatment(2)
my.coding<-matrix(rep(1/2, 2), ncol=1)
my.simple<-c-my.coding
my.simple
### Set the Contrast Coding Per Variable
contrasts(df_match$PDE)<- my.simple
contrasts(df_match$PDE)
contrasts(df_match$BA)<-my.simple
contrasts(df_match$BA)
contrasts(df_match$MC)<-my.simple
contrasts(df_match$MC)
contrasts(df_match$Intent)<-my.simple
contrasts(df_match$Intent)
contrasts(df_match$PBH)<-my.simple
contrasts(df_match$PBH)
contrasts(df_match$MG)<-my.simple
contrasts(df_match$MG)
contrasts(df_match$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
contrasts(df_match$Agent)


df_match$MJ <- as.numeric(as.character(df_match$MJ))

set.platform("outputs")

hVai_match <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                    control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match, "dropAgentIDdilRE_hVai_match_glmModel.rds")
isSingular(hVai_match)
summary(hVai_match)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_match <- readRDS("dropAgentIDdilRE_hVai_match_glmModel.rds")
#OR coefficients 
OR_hVai_match <- exp(fixef(hVai_match))
# confidence intervals for log-odds and OR
ci_hVai_match <- confint(hVai_match, method = "Wald")
ci_hVai_match.or <- exp(ci_hVai_match)
ses_hVai_match <- sqrt(diag(vcov(hVai_match)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_match_Agent <- emmeans(hVai_match, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_match_Agent

CI_hVai_match_Agent <- confint(emmeans_hVai_match_Agent)
CI_hVai_match_Agent

# by PBH
emmeans_hVai_match_PBH <- emmeans(hVai_match, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_match_PBH

CI_hVai_match_PBH <- confint(emmeans_hVai_match_PBH)
CI_hVai_match_PBH

# by PDE
emmeans_hVai_match_PDE <- emmeans(hVai_match, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_match_PDE

CI_hVai_match_PDE <- confint(emmeans_hVai_match_PDE)
CI_hVai_match_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_match_Agent <- contrast(emmeans_hVai_match_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_match_Agent

# # by PBH
contrasts_hVai_match_PBH <- contrast(emmeans_hVai_match_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_match_PBH

# # by PDE
contrasts_hVai_match_PDE <- contrast(emmeans_hVai_match_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_match_PDE

### Plots
set_theme(georgia_theme)
plot_model(hVai_match, terms = "PBH", type = "pred")
plot_model(hVai_match, terms = "PDE", type = "pred")
plot_model(hVai_match, terms = "Agent", type = "pred")
plot_model(hVai_match, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_match <- equivalence_test(hVai_match, rule = "classic", range = c(-0.94,0.94))

toast_hVai_match <- as.data.frame(TOaST_hVai_match)

toast_hVai_match <- toast_hVai_match %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_match
plot(TOaST_hVai_match)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_match,
  name = "Study2_match"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_match <- plot(check_model(hVai_match, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_match <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_match


p_hVai_match <- testDispersion(hVai_match,plot=F) 
p_hVai_match

pr_hVai_match <- predict_response(hVai_match, terms = c("PBH [all]","PDE [all] ","Agent [all]"))
pr_hVai_match

plot(pr_hVai_match, show_residuals=TRUE, show_residuals_line = TRUE)

## Excluding participants without matches for costless rescue dilemmas (reduced df)

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



### Prep variables for analysis
# make sure everything is factor
reduced_df_match$Task.Name <- as.factor(reduced_df_match$Task.Name)
reduced_df_match$Agent <- as.factor(reduced_df_match$Agent)
reduced_df_match$Context <- as.factor(reduced_df_match$Context)
reduced_df_match$PDE <- as.factor(reduced_df_match$PDE)
reduced_df_match$BA <- as.factor(reduced_df_match$BA)
reduced_df_match$MC <- as.factor(reduced_df_match$MC)
reduced_df_match$Intent <- as.factor(reduced_df_match$Intent)
reduced_df_match$PBH <- as.factor(reduced_df_match$PBH)
reduced_df_match$MG <- as.factor(reduced_df_match$MG)
reduced_df_match$RT <- abs(as.numeric(reduced_df_match$RT))

## Contrasts Coding
#set deviation contrasts for ease of interpretation -.5 vs .5
c<-contr.treatment(2)
my.coding<-matrix(rep(1/2, 2), ncol=1)
my.simple<-c-my.coding
my.simple
### Set the Contrast Coding Per Variable
contrasts(reduced_df_match$PDE)<- my.simple
contrasts(reduced_df_match$PDE)
contrasts(reduced_df_match$BA)<-my.simple
contrasts(reduced_df_match$BA)
contrasts(reduced_df_match$MC)<-my.simple
contrasts(reduced_df_match$MC)
contrasts(reduced_df_match$Intent)<-my.simple
contrasts(reduced_df_match$Intent)
contrasts(reduced_df_match$PBH)<-my.simple
contrasts(reduced_df_match$PBH)
contrasts(reduced_df_match$MG)<-my.simple
contrasts(reduced_df_match$MG)
contrasts(reduced_df_match$Agent)<- -1 * my.simple # -1 * makes human = -0.5, rather than AI
contrasts(reduced_df_match$Agent)


reduced_df_match$MJ <- as.numeric(as.character(reduced_df_match$MJ))

set.platform("outputs")

hVai_reduced_match <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                            control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match, "dropAgentIDdilRE_hVai_reduced_match_glmModel.rds")
isSingular(hVai_reduced_match)
summary(hVai_reduced_match)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_match <- readRDS("dropAgentIDdilRE_hVai_reduced_match_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_match <- exp(fixef(hVai_reduced_match))
# confidence intervals for log-odds and OR
ci_hVai_reduced_match <- confint(hVai_reduced_match, method = "Wald")
ci_hVai_reduced_match.or <- exp(ci_hVai_reduced_match)
ses_hVai_reduced_match <- sqrt(diag(vcov(hVai_reduced_match)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_match_Agent <- emmeans(hVai_reduced_match, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_match_Agent

CI_hVai_reduced_match_Agent <- confint(emmeans_hVai_reduced_match_Agent)
CI_hVai_reduced_match_Agent

# by PBH
emmeans_hVai_reduced_match_PBH <- emmeans(hVai_reduced_match, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_match_PBH

CI_hVai_reduced_match_PBH <- confint(emmeans_hVai_reduced_match_PBH)
CI_hVai_reduced_match_PBH

# by PDE
emmeans_hVai_reduced_match_PDE <- emmeans(hVai_reduced_match, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_match_PDE

CI_hVai_reduced_match_PDE <- confint(emmeans_hVai_reduced_match_PDE)
CI_hVai_reduced_match_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_match_Agent <- contrast(emmeans_hVai_reduced_match_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_match_Agent

# # by PBH
contrasts_hVai_reduced_match_PBH <- contrast(emmeans_hVai_reduced_match_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_match_PBH

# # by PDE
contrasts_hVai_reduced_match_PDE <- contrast(emmeans_hVai_reduced_match_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_match_PDE

### Plots
set_theme(georgia_theme)
plot_model(hVai_reduced_match, terms = "PBH", type = "pred")
plot_model(hVai_reduced_match, terms = "PDE", type = "pred")
plot_model(hVai_reduced_match, terms = "Agent", type = "pred")
plot_model(hVai_reduced_match, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced_match <- equivalence_test(hVai_reduced_match, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced_match <- as.data.frame(TOaST_hVai_reduced_match)

toast_hVai_reduced_match <- toast_hVai_reduced_match %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_match
plot(TOaST_hVai_reduced_match)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_match,
  name = "Study2_reduced_match"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_match <- plot(check_model(hVai_reduced_match, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_match <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_match


p_hVai_reduced_match <- testDispersion(hVai_reduced_match,plot=F) 
p_hVai_reduced_match

pr_hVai_reduced_match <- predict_response(hVai_reduced_match, terms = c("PBH [all]","PDE [all] ","Agent [all]"))
pr_hVai_reduced_match

plot(pr_hVai_reduced_match, show_residuals=TRUE, show_residuals_line = TRUE)

## Reaction time modelling 
## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  df_match_temp <- df_match %>% mutate(logRT = log(RT + c))
  ggplot(df_match_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})



wrap <- wrap_plots(plots, nrow = 3)
wrap

## log-transform RT with selected constant
df_match$logRT <- log(df_match$RT + 1)

## drop Agent ID & dilemma slope
hVai_match_RT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match,
                      control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_RT, "dropAgentIDdilRE_hVai_match_RT_glmModel.rds")
isSingular(hVai_match_RT)
summary(hVai_match_RT)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_match_RT <- readRDS("dropAgentIDdilRE_hVai_match_RT_glmModel.rds")
#OR coefficients 
OR_hVai_match_RT <- exp(fixef(hVai_match_RT))
# confidence intervals for log-odds and OR
ci_hVai_match_RT <- confint(hVai_match_RT, method = "Wald")
ci_hVai_match_RT.or <- exp(ci_hVai_match_RT)
ses_hVai_match_RT <- sqrt(diag(vcov(hVai_match_RT)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_match_RT_Agent <- emmeans(hVai_match_RT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_match_RT_Agent

CI_hVai_match_RT_Agent <- confint(emmeans_hVai_match_RT_Agent)
CI_hVai_match_RT_Agent

# by PBH
emmeans_hVai_match_RT_PBH <- emmeans(hVai_match_RT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_match_RT_PBH

CI_hVai_match_RT_PBH <- confint(emmeans_hVai_match_RT_PBH)
CI_hVai_match_RT_PBH

# by PDE
emmeans_hVai_match_RT_PDE <- emmeans(hVai_match_RT, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_match_RT_PDE

CI_hVai_match_RT_PDE <- confint(emmeans_hVai_match_RT_PDE)
CI_hVai_match_RT_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_match_RT_Agent <- contrast(emmeans_hVai_match_RT_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_match_RT_Agent

# # by PBH
contrasts_hVai_match_RT_PBH <- contrast(emmeans_hVai_match_RT_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_match_RT_PBH

# # by PDE
contrasts_hVai_match_RT_PDE <- contrast(emmeans_hVai_match_RT_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_match_RT_PDE

### Plots
plot(allEffects(hVai_match_RT))
set_theme(georgia_theme)
plot_model(hVai_match_RT, terms = "PBH", type = "pred")
plot_model(hVai_match_RT, terms = "PDE", type = "pred")
plot_model(hVai_match_RT, terms = "Agent", type = "pred")
plot_model(hVai_match_RT, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_match_RT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_match_RT <- equivalence_test(hVai_match_RT, rule = "classic", range = c(-bound,bound))

toast_hVai_match_RT <- as.data.frame(TOaST_hVai_match_RT)

toast_hVai_match_RT <- toast_hVai_match_RT %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_match_RT
plot(TOaST_hVai_match_RT)

## Model comparisons
model_comparison_function(
  DV = "logRT",
  data = df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = default,
  control_stuff = "continuous",
  full_model = hVai_match_RT,
  name = "Study2_RT"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_match_RT <- plot(check_model(hVai_match_RT, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_match_RT <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_match_RT


p_hVai_match_RT <- testDispersion(hVai_match_RT,plot=F) 
p_hVai_match_RT

## Reaction time modelling as Random Effect
## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  df_match_temp <- df_match %>% mutate(logRT = log(RT + c))
  ggplot(df_match_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})



wrap <- wrap_plots(plots, nrow = 3)
wrap

## log-transform RT with selected constant
df_match$logRT <- log(df_match$RT + 1)

## drop Agent ID & dilemma slope
hVai_match_RTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_RTRE, "dropAgentIDdilRE_hVai_match_RTRE_glmModel.rds")
isSingular(hVai_match_RTRE)
summary(hVai_match_RTRE)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_match_RTRE <- readRDS("dropAgentIDdilRE_hVai_match_RTRE_glmModel.rds")
#OR coefficients 
OR_hVai_match_RTRE <- exp(fixef(hVai_match_RTRE))
# confidence intervals for log-odds and OR
ci_hVai_match_RTRE <- confint(hVai_match_RTRE, method = "Wald")
ci_hVai_match_RTRE.or <- exp(ci_hVai_match_RTRE)
ses_hVai_match_RTRE <- sqrt(diag(vcov(hVai_match_RTRE)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_match_RTRE_Agent <- emmeans(hVai_match_RTRE, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_match_RTRE_Agent

CI_hVai_match_RTRE_Agent <- confint(emmeans_hVai_match_RTRE_Agent)
CI_hVai_match_RTRE_Agent

# by PBH
emmeans_hVai_match_RTRE_PBH <- emmeans(hVai_match_RTRE, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_match_RTRE_PBH

CI_hVai_match_RTRE_PBH <- confint(emmeans_hVai_match_RTRE_PBH)
CI_hVai_match_RTRE_PBH

# by PDE
emmeans_hVai_match_RTRE_PDE <- emmeans(hVai_match_RTRE, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_match_RTRE_PDE

CI_hVai_match_RTRE_PDE <- confint(emmeans_hVai_match_RTRE_PDE)
CI_hVai_match_RTRE_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_match_RTRE_Agent <- contrast(emmeans_hVai_match_RTRE_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_match_RTRE_Agent

# # by PBH
contrasts_hVai_match_RTRE_PBH <- contrast(emmeans_hVai_match_RTRE_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_match_RTRE_PBH

# # by PDE
contrasts_hVai_match_RTRE_PDE <- contrast(emmeans_hVai_match_RTRE_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_match_RTRE_PDE

### Plots
plot(allEffects(hVai_match_RTRE))
set_theme(georgia_theme)
plot_model(hVai_match_RTRE, terms = "PBH", type = "pred")
plot_model(hVai_match_RTRE, terms = "PDE", type = "pred")
plot_model(hVai_match_RTRE, terms = "Agent", type = "pred")
plot_model(hVai_match_RTRE, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_match_RTRE)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_match_RTRE <- equivalence_test(hVai_match_RTRE, rule = "classic", range = c(-bound,bound))

toast_hVai_match_RTRE <- as.data.frame(TOaST_hVai_match_RTRE)

toast_hVai_match_RTRE <- toast_hVai_match_RTRE %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_match_RTRE
plot(TOaST_hVai_match_RTRE)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_match_RTRE,
  name = "Study2_RTRE"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_match_RTRE <- plot(check_model(hVai_match_RTRE, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_match_RTRE <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_match_RTRE


p_hVai_match_RTRE <- testDispersion(hVai_match_RTRE,plot=F) 
p_hVai_match_RTRE

## Intent x Agent
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "intentxAgentRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropAgentdilRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## Drop PDE interaction for ID RF
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropPDEintRE_dropAgentdilRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## Drop PDE interaction for ID RF
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropAllintRE_dropAgentdilRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropPDEintRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## Drop PBH * Agent interaction slope for ID RF
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropintRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## drop PDE slope
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropPDEslope_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)
# w.o Agent dilemma slope
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropPDEslope_dropAgentDil_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## drop Agent ID slope
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropAgentIDRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)

## drop Agent ID & dilemma slope
hVai_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intentxAgent, "dropAgentIDdilRE_hVai_match_intentxAgent_glmModel.rds")
isSingular(hVai_match_intentxAgent)
summary(hVai_match_intentxAgent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_match_intentxAgent <- readRDS("dropAgentIDdilRE_hVai_match_intentxAgent_glmModel.rds")
#OR coefficients 
OR_hVai_match_intentxAgent <- exp(fixef(hVai_match_intentxAgent))
# confidence intervals for log-odds and OR
ci_hVai_match_intentxAgent <- confint(hVai_match_intentxAgent, method = "Wald")
ci_hVai_match_intentxAgent.or <- exp(ci_hVai_match_intentxAgent)
ses_hVai_match_intentxAgent <- sqrt(diag(vcov(hVai_match_intentxAgent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_match_intentxAgent_Agent <- emmeans(hVai_match_intentxAgent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_match_intentxAgent_Agent

CI_hVai_match_intentxAgent_Agent <- confint(emmeans_hVai_match_intentxAgent_Agent)
CI_hVai_match_intentxAgent_Agent

# by PBH
emmeans_hVai_match_intentxAgent_PBH <- emmeans(hVai_match_intentxAgent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_match_intentxAgent_PBH

CI_hVai_match_intentxAgent_PBH <- confint(emmeans_hVai_match_intentxAgent_PBH)
CI_hVai_match_intentxAgent_PBH

# by PDE
emmeans_hVai_match_intentxAgent_PDE <- emmeans(hVai_match_intentxAgent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_match_intentxAgent_PDE

CI_hVai_match_intentxAgent_PDE <- confint(emmeans_hVai_match_intentxAgent_PDE)
CI_hVai_match_intentxAgent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_match_intentxAgent_Agent <- contrast(emmeans_hVai_match_intentxAgent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_match_intentxAgent_Agent

# # by PBH
contrasts_hVai_match_intentxAgent_PBH <- contrast(emmeans_hVai_match_intentxAgent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_match_intentxAgent_PBH

# # by PDE
contrasts_hVai_match_intentxAgent_PDE <- contrast(emmeans_hVai_match_intentxAgent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_match_intentxAgent_PDE

### Plots
plot(allEffects(hVai_match_intentxAgent))
set_theme(georgia_theme)
plot_model(hVai_match_intentxAgent, terms = "PBH", type = "pred")
plot_model(hVai_match_intentxAgent, terms = "PDE", type = "pred")
plot_model(hVai_match_intentxAgent, terms = "Agent", type = "pred")
plot_model(hVai_match_intentxAgent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_match_intentxAgent <- equivalence_test(hVai_match_intentxAgent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_match_intentxAgent <- as.data.frame(TOaST_hVai_match_intentxAgent)

toast_hVai_match_intentxAgent <- toast_hVai_match_intentxAgent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_match_intentxAgent
plot(TOaST_hVai_match_intentxAgent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_match_intentxAgent,
  name = "Study2_intentxAgent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_match_intentxAgent <- plot(check_model(hVai_match_intentxAgent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_match_intentxAgent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_match_intentxAgent


p_hVai_match_intentxAgent <- testDispersion(hVai_match_intentxAgent,plot=F) 
p_hVai_match_intentxAgent

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "intentRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropAgentdilRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## Drop PDE interaction for ID RF
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropPDEintRE_dropAgentdilRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## Drop PDE interaction for ID RF
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropAllintRE_dropAgentdilRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropPDEintRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## Drop PBH * Agent interaction slope for ID RF
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropintRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## drop PDE slope
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropPDEslope_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)
# w.o Agent dilemma slope
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropPDEslope_dropAgentDil_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## drop Agent ID slope
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropAgentIDRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)

## drop Agent ID & dilemma slope
hVai_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=df_match, family = binomial(link = "logit"),
                           control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_match_intent, "dropAgentIDdilRE_hVai_match_intent_glmModel.rds")
isSingular(hVai_match_intent)
summary(hVai_match_intent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_match_intent <- readRDS("dropAgentIDdilRE_hVai_match_intent_glmModel.rds")
#OR coefficients 
OR_hVai_match_intent <- exp(fixef(hVai_match_intent))
# confidence intervals for log-odds and OR
ci_hVai_match_intent <- confint(hVai_match_intent, method = "Wald")
ci_hVai_match_intent.or <- exp(ci_hVai_match_intent)
ses_hVai_match_intent <- sqrt(diag(vcov(hVai_match_intent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_match_intent_Agent <- emmeans(hVai_match_intent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_match_intent_Agent

CI_hVai_match_intent_Agent <- confint(emmeans_hVai_match_intent_Agent)
CI_hVai_match_intent_Agent

# by PBH
emmeans_hVai_match_intent_PBH <- emmeans(hVai_match_intent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_match_intent_PBH

CI_hVai_match_intent_PBH <- confint(emmeans_hVai_match_intent_PBH)
CI_hVai_match_intent_PBH

# by PDE
emmeans_hVai_match_intent_PDE <- emmeans(hVai_match_intent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_match_intent_PDE

CI_hVai_match_intent_PDE <- confint(emmeans_hVai_match_intent_PDE)
CI_hVai_match_intent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_match_intent_Agent <- contrast(emmeans_hVai_match_intent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_match_intent_Agent

# # by PBH
contrasts_hVai_match_intent_PBH <- contrast(emmeans_hVai_match_intent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_match_intent_PBH

# # by PDE
contrasts_hVai_match_intent_PDE <- contrast(emmeans_hVai_match_intent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_match_intent_PDE

### Plots
plot(allEffects(hVai_match_intent))
set_theme(georgia_theme)
plot_model(hVai_match_intent, terms = "PBH", type = "pred")
plot_model(hVai_match_intent, terms = "PDE", type = "pred")
plot_model(hVai_match_intent, terms = "Agent", type = "pred")
plot_model(hVai_match_intent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_match_intent <- equivalence_test(hVai_match_intent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_match_intent <- as.data.frame(TOaST_hVai_match_intent)

toast_hVai_match_intent <- toast_hVai_match_intent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_match_intent
plot(TOaST_hVai_match_intent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_match_intent,
  name = "Study2_intent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_match_intent <- plot(check_model(hVai_match_intent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_match_intent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_match_intent


p_hVai_match_intent <- testDispersion(hVai_match_intent,plot=F) 
p_hVai_match_intent

## Reaction time modelling 
## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  reduced_df_match_temp <- reduced_df_match %>% mutate(logRT = log(RT + c))
  ggplot(reduced_df_match_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})



wrap <- wrap_plots(plots, nrow = 3)
wrap

## log-transform RT with selected constant
reduced_df_match$logRT <- log(reduced_df_match$RT + 1)

## drop Agent ID & dilemma slope
hVai_reduced_match_RT <- lmer(logRT ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match,
                              control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_RT, "dropAgentIDdilRE_hVai_reduced_match_RT_glmModel.rds")
isSingular(hVai_reduced_match_RT)
summary(hVai_reduced_match_RT)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_match_RT <- readRDS("dropAgentIDdilRE_hVai_reduced_match_RT_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_match_RT <- exp(fixef(hVai_reduced_match_RT))
# confidence intervals for log-odds and OR
ci_hVai_reduced_match_RT <- confint(hVai_reduced_match_RT, method = "Wald")
ci_hVai_reduced_match_RT.or <- exp(ci_hVai_reduced_match_RT)
ses_hVai_reduced_match_RT <- sqrt(diag(vcov(hVai_reduced_match_RT)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_match_RT_Agent <- emmeans(hVai_reduced_match_RT, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_match_RT_Agent

CI_hVai_reduced_match_RT_Agent <- confint(emmeans_hVai_reduced_match_RT_Agent)
CI_hVai_reduced_match_RT_Agent

# by PBH
emmeans_hVai_reduced_match_RT_PBH <- emmeans(hVai_reduced_match_RT, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_match_RT_PBH

CI_hVai_reduced_match_RT_PBH <- confint(emmeans_hVai_reduced_match_RT_PBH)
CI_hVai_reduced_match_RT_PBH

# by PDE
emmeans_hVai_reduced_match_RT_PDE <- emmeans(hVai_reduced_match_RT, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_match_RT_PDE

CI_hVai_reduced_match_RT_PDE <- confint(emmeans_hVai_reduced_match_RT_PDE)
CI_hVai_reduced_match_RT_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_match_RT_Agent <- contrast(emmeans_hVai_reduced_match_RT_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_match_RT_Agent

# # by PBH
contrasts_hVai_reduced_match_RT_PBH <- contrast(emmeans_hVai_reduced_match_RT_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_match_RT_PBH

# # by PDE
contrasts_hVai_reduced_match_RT_PDE <- contrast(emmeans_hVai_reduced_match_RT_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_match_RT_PDE

### Plots
plot(allEffects(hVai_reduced_match_RT))
set_theme(georgia_theme)
plot_model(hVai_reduced_match_RT, terms = "PBH", type = "pred")
plot_model(hVai_reduced_match_RT, terms = "PDE", type = "pred")
plot_model(hVai_reduced_match_RT, terms = "Agent", type = "pred")
plot_model(hVai_reduced_match_RT, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_reduced_match_RT)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_reduced_match_RT <- equivalence_test(hVai_reduced_match_RT, rule = "classic", range = c(-bound,bound))

toast_hVai_reduced_match_RT <- as.data.frame(TOaST_hVai_reduced_match_RT)

toast_hVai_reduced_match_RT <- toast_hVai_reduced_match_RT %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_match_RT
plot(TOaST_hVai_reduced_match_RT)

## Model comparisons
model_comparison_function(
  DV = "logRT",
  data = reduced_df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = default,
  control_stuff = "continuous",
  full_model = hVai_reduced_match_RT,
  name = "Study2_RT"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_match_RT <- plot(check_model(hVai_reduced_match_RT, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_match_RT <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_match_RT


p_hVai_reduced_match_RT <- testDispersion(hVai_reduced_match_RT,plot=F) 
p_hVai_reduced_match_RT

## Reaction time modelling as Random Effect
## apply log-transformation with different constants to see how it affects distribution
constants <- c(0.1, 1, 10, 100, 1000)
plots <- lapply(constants, function(c) {
  reduced_df_match_temp <- reduced_df_match %>% mutate(logRT = log(RT + c))
  ggplot(reduced_df_match_temp, aes(x = logRT)) +
    geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black") +
    #geom_density(color = pink_red, size = 1) +
    labs(title = paste("Constant =", c), x = "log(RT + constant)", y = "Count") #+
  #georgia_theme
})



wrap <- wrap_plots(plots, nrow = 3)
wrap

## log-transform RT with selected constant
reduced_df_match$logRT <- log(reduced_df_match$RT + 1)

## drop Agent ID & dilemma slope
hVai_reduced_match_RTRE <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                 control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_RTRE, "dropAgentIDdilRE_hVai_reduced_match_RTRE_glmModel.rds")
isSingular(hVai_reduced_match_RTRE)
summary(hVai_reduced_match_RTRE)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_match_RTRE <- readRDS("dropAgentIDdilRE_hVai_reduced_match_RTRE_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_match_RTRE <- exp(fixef(hVai_reduced_match_RTRE))
# confidence intervals for log-odds and OR
ci_hVai_reduced_match_RTRE <- confint(hVai_reduced_match_RTRE, method = "Wald")
ci_hVai_reduced_match_RTRE.or <- exp(ci_hVai_reduced_match_RTRE)
ses_hVai_reduced_match_RTRE <- sqrt(diag(vcov(hVai_reduced_match_RTRE)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_match_RTRE_Agent <- emmeans(hVai_reduced_match_RTRE, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_match_RTRE_Agent

CI_hVai_reduced_match_RTRE_Agent <- confint(emmeans_hVai_reduced_match_RTRE_Agent)
CI_hVai_reduced_match_RTRE_Agent

# by PBH
emmeans_hVai_reduced_match_RTRE_PBH <- emmeans(hVai_reduced_match_RTRE, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_match_RTRE_PBH

CI_hVai_reduced_match_RTRE_PBH <- confint(emmeans_hVai_reduced_match_RTRE_PBH)
CI_hVai_reduced_match_RTRE_PBH

# by PDE
emmeans_hVai_reduced_match_RTRE_PDE <- emmeans(hVai_reduced_match_RTRE, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_match_RTRE_PDE

CI_hVai_reduced_match_RTRE_PDE <- confint(emmeans_hVai_reduced_match_RTRE_PDE)
CI_hVai_reduced_match_RTRE_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_match_RTRE_Agent <- contrast(emmeans_hVai_reduced_match_RTRE_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_match_RTRE_Agent

# # by PBH
contrasts_hVai_reduced_match_RTRE_PBH <- contrast(emmeans_hVai_reduced_match_RTRE_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_match_RTRE_PBH

# # by PDE
contrasts_hVai_reduced_match_RTRE_PDE <- contrast(emmeans_hVai_reduced_match_RTRE_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_match_RTRE_PDE

### Plots
plot(allEffects(hVai_reduced_match_RTRE))
set_theme(georgia_theme)
plot_model(hVai_reduced_match_RTRE, terms = "PBH", type = "pred")
plot_model(hVai_reduced_match_RTRE, terms = "PDE", type = "pred")
plot_model(hVai_reduced_match_RTRE, terms = "Agent", type = "pred")
plot_model(hVai_reduced_match_RTRE, type = "int")

# two sided equivalence test for main analysis
# set bounds to small effect size for cohen's d but transformed to our logRT
# Extract residual SD from the model
resid_sd <- sigma(hVai_reduced_match_RTRE)
resid_sd
# Compute equivalence bound for Cohen's d = 0.2
bound <- 0.2 * resid_sd
bound

TOaST_hVai_reduced_match_RTRE <- equivalence_test(hVai_reduced_match_RTRE, rule = "classic", range = c(-bound,bound))

toast_hVai_reduced_match_RTRE <- as.data.frame(TOaST_hVai_reduced_match_RTRE)

toast_hVai_reduced_match_RTRE <- toast_hVai_reduced_match_RTRE %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_match_RTRE
plot(TOaST_hVai_reduced_match_RTRE)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_match_RTRE,
  name = "Study2_RTRE"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_match_RTRE <- plot(check_model(hVai_reduced_match_RTRE, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_match_RTRE <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_match_RTRE


p_hVai_reduced_match_RTRE <- testDispersion(hVai_reduced_match_RTRE,plot=F) 
p_hVai_reduced_match_RTRE

## Intent x Agent
## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "intentxAgentRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropAgentdilRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## Drop PDE interaction for ID RF
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropPDEintRE_dropAgentdilRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## Drop PDE interaction for ID RF
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropAllintRE_dropAgentdilRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropPDEintRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## Drop PBH * Agent interaction slope for ID RF
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropintRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## drop PDE slope
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropPDEslope_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)
# w.o Agent dilemma slope
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropPDEslope_dropAgentDil_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## drop Agent ID slope
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropAgentIDRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)

## drop Agent ID & dilemma slope
hVai_reduced_match_intentxAgent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                         control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intentxAgent, "dropAgentIDdilRE_hVai_reduced_match_intentxAgent_glmModel.rds")
isSingular(hVai_reduced_match_intentxAgent)
summary(hVai_reduced_match_intentxAgent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_match_intentxAgent <- readRDS("dropAgentIDdilRE_hVai_reduced_match_intentxAgent_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_match_intentxAgent <- exp(fixef(hVai_reduced_match_intentxAgent))
# confidence intervals for log-odds and OR
ci_hVai_reduced_match_intentxAgent <- confint(hVai_reduced_match_intentxAgent, method = "Wald")
ci_hVai_reduced_match_intentxAgent.or <- exp(ci_hVai_reduced_match_intentxAgent)
ses_hVai_reduced_match_intentxAgent <- sqrt(diag(vcov(hVai_reduced_match_intentxAgent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_match_intentxAgent_Agent <- emmeans(hVai_reduced_match_intentxAgent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_match_intentxAgent_Agent

CI_hVai_reduced_match_intentxAgent_Agent <- confint(emmeans_hVai_reduced_match_intentxAgent_Agent)
CI_hVai_reduced_match_intentxAgent_Agent

# by PBH
emmeans_hVai_reduced_match_intentxAgent_PBH <- emmeans(hVai_reduced_match_intentxAgent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_match_intentxAgent_PBH

CI_hVai_reduced_match_intentxAgent_PBH <- confint(emmeans_hVai_reduced_match_intentxAgent_PBH)
CI_hVai_reduced_match_intentxAgent_PBH

# by PDE
emmeans_hVai_reduced_match_intentxAgent_PDE <- emmeans(hVai_reduced_match_intentxAgent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_match_intentxAgent_PDE

CI_hVai_reduced_match_intentxAgent_PDE <- confint(emmeans_hVai_reduced_match_intentxAgent_PDE)
CI_hVai_reduced_match_intentxAgent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_match_intentxAgent_Agent <- contrast(emmeans_hVai_reduced_match_intentxAgent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_match_intentxAgent_Agent

# # by PBH
contrasts_hVai_reduced_match_intentxAgent_PBH <- contrast(emmeans_hVai_reduced_match_intentxAgent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_match_intentxAgent_PBH

# # by PDE
contrasts_hVai_reduced_match_intentxAgent_PDE <- contrast(emmeans_hVai_reduced_match_intentxAgent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_match_intentxAgent_PDE

### Plots
plot(allEffects(hVai_reduced_match_intentxAgent))
set_theme(georgia_theme)
plot_model(hVai_reduced_match_intentxAgent, terms = "PBH", type = "pred")
plot_model(hVai_reduced_match_intentxAgent, terms = "PDE", type = "pred")
plot_model(hVai_reduced_match_intentxAgent, terms = "Agent", type = "pred")
plot_model(hVai_reduced_match_intentxAgent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced_match_intentxAgent <- equivalence_test(hVai_reduced_match_intentxAgent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced_match_intentxAgent <- as.data.frame(TOaST_hVai_reduced_match_intentxAgent)

toast_hVai_reduced_match_intentxAgent <- toast_hVai_reduced_match_intentxAgent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_match_intentxAgent
plot(TOaST_hVai_reduced_match_intentxAgent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_match_intentxAgent,
  name = "Study2_intentxAgent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_match_intentxAgent <- plot(check_model(hVai_reduced_match_intentxAgent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_match_intentxAgent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_match_intentxAgent


p_hVai_reduced_match_intentxAgent <- testDispersion(hVai_reduced_match_intentxAgent,plot=F) 
p_hVai_reduced_match_intentxAgent

## Mixed Effects Logistic Regression (Three-Way) w. Moral Judgements as DV
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "intentRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## slope for Agent for dilemma is a perfect correlation with intercept, so we drop it and refit the model.
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*PDE*Agent|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropAgentdilRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## Drop PDE interaction for ID RF
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropPDEintRE_dropAgentdilRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## Drop PDE interaction for ID RF
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropAllintRE_dropAgentdilRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## drop slopes of RE structure
## Drop PDE interaction for ID RF
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH*Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropPDEintRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## Drop PBH * Agent interaction slope for ID RF
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropintRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## drop PDE slope
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropPDEslope_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)
# w.o Agent dilemma slope
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+Agent|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropPDEslope_dropAgentDil_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## drop Agent ID slope
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1+Agent|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropAgentIDRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)

## drop Agent ID & dilemma slope
hVai_reduced_match_intent <- glmer(MJ ~ PBH*PDE*Agent +  (1+PBH+PDE|ID) + (1|Dilemma),  data=reduced_df_match, family = binomial(link = "logit"),
                                   control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
saveRDS(hVai_reduced_match_intent, "dropAgentIDdilRE_hVai_reduced_match_intent_glmModel.rds")
isSingular(hVai_reduced_match_intent)
summary(hVai_reduced_match_intent)


### now we've found the best fitting model structure, we can conduct further analyses like simple effects
hVai_reduced_match_intent <- readRDS("dropAgentIDdilRE_hVai_reduced_match_intent_glmModel.rds")
#OR coefficients 
OR_hVai_reduced_match_intent <- exp(fixef(hVai_reduced_match_intent))
# confidence intervals for log-odds and OR
ci_hVai_reduced_match_intent <- confint(hVai_reduced_match_intent, method = "Wald")
ci_hVai_reduced_match_intent.or <- exp(ci_hVai_reduced_match_intent)
ses_hVai_reduced_match_intent <- sqrt(diag(vcov(hVai_reduced_match_intent)))

### simple effects
### Estimated Marginal Means

# by Agent
emmeans_hVai_reduced_match_intent_Agent <- emmeans(hVai_reduced_match_intent, pairwise ~ PBH*PDE|Agent, cov.reduce = range)
emmeans_hVai_reduced_match_intent_Agent

CI_hVai_reduced_match_intent_Agent <- confint(emmeans_hVai_reduced_match_intent_Agent)
CI_hVai_reduced_match_intent_Agent

# by PBH
emmeans_hVai_reduced_match_intent_PBH <- emmeans(hVai_reduced_match_intent, pairwise ~ Agent*PDE|PBH, cov.reduce = range)
emmeans_hVai_reduced_match_intent_PBH

CI_hVai_reduced_match_intent_PBH <- confint(emmeans_hVai_reduced_match_intent_PBH)
CI_hVai_reduced_match_intent_PBH

# by PDE
emmeans_hVai_reduced_match_intent_PDE <- emmeans(hVai_reduced_match_intent, pairwise ~ PBH*Agent|PDE, cov.reduce = range)
emmeans_hVai_reduced_match_intent_PDE

CI_hVai_reduced_match_intent_PDE <- confint(emmeans_hVai_reduced_match_intent_PDE)
CI_hVai_reduced_match_intent_PDE

### Simple Contrasts

# by Agent
contrasts_hVai_reduced_match_intent_Agent <- contrast(emmeans_hVai_reduced_match_intent_Agent, interaction = "pairwise", by=c("Agent"))
contrasts_hVai_reduced_match_intent_Agent

# # by PBH
contrasts_hVai_reduced_match_intent_PBH <- contrast(emmeans_hVai_reduced_match_intent_PBH, interaction = "pairwise", by=c("PBH"))
contrasts_hVai_reduced_match_intent_PBH

# # by PDE
contrasts_hVai_reduced_match_intent_PDE <- contrast(emmeans_hVai_reduced_match_intent_PDE, interaction = "pairwise", by=c("PDE"))
contrasts_hVai_reduced_match_intent_PDE

### Plots
plot(allEffects(hVai_reduced_match_intent))
set_theme(georgia_theme)
plot_model(hVai_reduced_match_intent, terms = "PBH", type = "pred")
plot_model(hVai_reduced_match_intent, terms = "PDE", type = "pred")
plot_model(hVai_reduced_match_intent, terms = "Agent", type = "pred")
plot_model(hVai_reduced_match_intent, type = "int")

# two sided equivalence test for main analysis
# set bounds to OR of 2.56 (log(2.56)=0.94) 
TOaST_hVai_reduced_match_intent <- equivalence_test(hVai_reduced_match_intent, rule = "classic", range = c(-0.94,0.94))

toast_hVai_reduced_match_intent <- as.data.frame(TOaST_hVai_reduced_match_intent)

toast_hVai_reduced_match_intent <- toast_hVai_reduced_match_intent %>% 
  dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>% 
  rename(
    Parameter = Parameter, 
    LCI = CI_low, 
    HCI = CI_high, 
    Equivalence = ROPE_Equivalence, 
    p = p
  )
toast_hVai_reduced_match_intent
plot(TOaST_hVai_reduced_match_intent)

## Model comparisons
model_comparison_function(
  DV = "MJ",
  data = reduced_df_match,
  re_structure = "(1 + PBH + PDE | ID) + (1 | Dilemma)",
  family_stuff = binomial(link = "logit"),
  control_stuff = "binary",
  full_model = hVai_reduced_match_intent,
  name = "Study2_intent"
)


## Model Checks

### Check model fit to distribution 
# return a list of single plots
diagnostic_plots_hVai_reduced_match_intent <- plot(check_model(hVai_reduced_match_intent, panel = FALSE),type="discrete_both")

# change theme & remove titles & subtitles
diagnostic_plots_hVai_reduced_match_intent <- lapply(diagnostic_plots, function(p) {
  if (inherits(p, "ggplot")) {
    p$labels$title <- NULL
    p$labels$subtitle <- NULL
    p <- p #+ georgia_theme
  }
  p
})
diagnostic_plots_hVai_reduced_match_intent


p_hVai_reduced_match_intent <- testDispersion(hVai_reduced_match_intent,plot=F) 
p_hVai_reduced_match_intent
