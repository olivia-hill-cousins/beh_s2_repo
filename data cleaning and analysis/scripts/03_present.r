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


### load some colours (colour-blind friendly)
colours <- brewer.pal(12, "Paired")

light_green <- "#B2DF8A" 
dark_green <- "#33A02C" 
pink_red <- "#FB9A99"
red <-  "#E31A1C"

# Add Georgia (adjust path if you downloaded manually; assumes system-installed)
font_add("Georgia", regular = "Georgia.ttf", bold = "Georgia Bold.ttf", 
         italic = "Georgia Italic.ttf", bolditalic = "Georgia Bold Italic.ttf")

showtext_auto()  # Enable for all plots

### set theme so everything formatted as preferred 
georgia_theme <- theme_minimal(base_family = "Georgia") +
  theme(
    text        = element_text(family = "Georgia"),
    axis.text   = element_text(family = "Georgia"),
    axis.title  = element_text(family = "Georgia"),
    legend.text = element_text(family = "Georgia"),
    legend.title= element_text(family = "Georgia")
  )
set_theme(georgia_theme)

# Methods

## Participants

# calculating mean age
dfDemowide$Age <- as.numeric(dfDemowide$Age)
mean_age <- mean(dfDemowide$Age)

# calculating standard deviation of age
sd_age <- sd(dfDemowide$Age)


# find the age of the youngest participant
youngest_age <- min(dfDemowide$Age)


# find the age of the oldest participant
oldest_age <- max(dfDemowide$Age)



# change Other (please specify) to what was specified in next question
dfDemowide$Gender <- ifelse(dfDemowide$Gender == "Other (please specify)", "Non-Binary", dfDemowide$Gender)



# get count of number of participants of each gender
gender_count <- table(dfDemowide$Gender)
gender_prop <- prop.table(gender_count) * 100


# Print APA-style
# Demographics summary
cat("\n\n",
    "*N* = ", length(unique(dfDemowide$ID)), "\n\n",
    "*M*~age~ = ", round(mean(dfDemowide$Age, na.rm=TRUE), 2), 
    ", *SD*~age~ = ", round(sd(dfDemowide$Age, na.rm=TRUE), 2), "\n\n",
    "*Age* range = ", min(dfDemowide$Age, na.rm=TRUE), "–", max(dfDemowide$Age, na.rm=TRUE), " years", "\n"
)


woman_count <- gender_count["Woman"]


man_count <- gender_count["Man"]


nonbinary_count <- gender_count["Non-binary"]

prefNoGender_count <- gender_count["Prefer Not To Say"]

anotherWayGender_count <- gender_count["In another way (specify, if you wish)"] 


### Gender Composition of Sample
gender_table <- data.frame(Gender = names(gender_count), 
                           n = as.vector(gender_count), 
                           pct = round(gender_prop, 1))

kable(gender_table[, c("Gender", "n", "pct.Freq")],
      col.names = c("Gender", "*n*", "%"),
      digits = 1)

# Summary table excluding NAs in primary_cultural
summary_table <- dfDemowide %>%
  filter(!is.na(primary_cultural)) %>% # Exclude NAs here
  count(primary_cultural, multiple_cultural) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(n, " (", percent, "%)"))
# Check unique combinations first
unique_combos <- dfDemowide %>%
  filter(!is.na(primary_cultural)) %>%
  count(primary_cultural, multiple_cultural) %>%
  pull(primary_cultural) %>% unique()

# Pivot only if consistent
summary_wide <- summary_table %>%
  dplyr::select(primary_cultural, multiple_cultural, label) %>%
  tidyr::pivot_wider(names_from = multiple_cultural, 
                     values_from = label, 
                     values_fill = "0 (0.0%)")
cat('
<div class="apa-table-title">
  <span class="table-label">Table 1.</span> Self‑Identified Cultural Ethnicity (Broad Categories)
</div>')
kable(summary_wide, col.names = c("Primary", "Single", "Multiple"))  
cat('</td></tr></table>')


# now add the demographics to the data
dfDemowide$variants_string <- as.factor(dfDemowide$variants_string)
cult_table <- dfDemowide %>%
  filter(!is.na(variants_string)) %>%  # Explicit NA exclusion
  count(variants_string, sort = TRUE) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(n, " (", Percent, "%)")) %>% 
  dplyr::arrange(desc(Percent))

# Build main table
cult_table <- dfDemowide %>%
  filter(!is.na(variants_string)) %>%
  count(variants_string, sort = TRUE) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1)
  ) %>%
  dplyr::arrange(desc(Percent))

# Create summary row
total_n <- sum(cult_table$n)
summary_row <- data.frame(
  variants_string = "Total sample *N*",
  n = total_n,
  Percent = "—"
)
cult_table$Percent <- as.character(cult_table$Percent)

# Bind rows
nat_with_total <- bind_rows(cult_table, summary_row)
cat('
<div class="apa-table-title">
  <span class="table-label">Appendix A: </span> Table Summarising Participants\' Specific Cultural Identities
</div>')
# Print table with a separating line above the final row
kable(
  nat_with_total,
  col.names = c("Cultural Ethnicities (Specific Preferences)", "*n*", "%"),
  digits = 1,
  escape = FALSE,
  align = c("l","r","r")
) %>%
  kable_styling(full_width = TRUE) %>%
  row_spec(nrow(nat_with_total)-1, hline_after = TRUE)   # draws a horizontal line above final row
cat('</td></tr></table>')
df$ID <- as.factor(df$ID)
dfDemowide$ID <- as.factor(dfDemowide$ID)

df <- df %>% 
  left_join(dfDemowide, by = "ID")

### Nationalities
nat_table <- dfDemowide %>%
  filter(!is.na(Nationality)) %>%  # Explicit NA exclusion
  count(Nationality, sort = TRUE) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(n, " (", Percent, "%)")) %>% 
  dplyr::arrange(desc(Percent))

# Build main table
nat_table <- dfDemowide %>%
  filter(!is.na(Nationality)) %>%
  count(Nationality, sort = TRUE) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1)
  ) %>%
  dplyr::arrange(desc(Percent))

# Create summary row
total_n <- sum(nat_table$n)
summary_row <- data.frame(
  Nationality = "Total sample *N*",
  n = total_n,
  Percent = "—"
)
nat_table$Percent <- as.character(nat_table$Percent)

# Bind rows
nat_with_total <- bind_rows(nat_table, summary_row)

# Print table with a separating line above the final row
cat('
<div class="apa-table-title">
  <span class="table-label">Appendix B: </span> Self‑Identified Nationality (Recorded on Prolific)
</div>')
kable(
  nat_with_total,
  col.names = c("Nationality", "*n*", "%"),
  digits = 1,
  escape = FALSE,
  align = c("l","r","r")
) %>%
  kable_styling(full_width = TRUE) %>%
  row_spec(nrow(nat_with_total)-1, hline_after = TRUE)   # draws a horizontal line above final row

# Results
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

hVai_report <- report(hVai)   
hVai_report

stargazer(hVai, type = "html", 
          ci.custom = list(ci_hVai),
          ci=TRUE, intercept.bottom = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", out = NULL)

stargazer(hVai, type = "html", 
          coef = list(OR_hVai),
          ci.custom = list(ci_hVai.or),
          se = list(ses_hVai),
          ci = TRUE, intercept.bottom = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "", out = NULL)

make_pred_plot <- function(term) {
  plot_model(
    hVai,
    terms = term,
    type = "pred",
    colors = colours
  ) + georgia_theme +
    labs(title = NULL)
}

p_PBH   <- make_pred_plot("PBH")
p_PDE   <- make_pred_plot("PDE")
p_Agent <- make_pred_plot("Agent")


library(patchwork)

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

row1
row2


############################################################
# 2. INTERACTION PLOTS
############################################################

p_int_list <- plot_model(hVai, type = "int", colors = colours)

# Apply formatting
p_int_list <- lapply(
  p_int_list,
  function(p) p + georgia_theme +
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

row1
row1b
row2
row2b

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


### Two Sided Equivalence Test for Main Analysis
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



          )