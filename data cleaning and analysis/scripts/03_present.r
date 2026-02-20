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
packages <- c("RColorBrewer","sysfonts","showtext", "ggplot2","patchwork", "kableExtra", "webshot2")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
library(RColorBrewer)
library(sysfonts)
library(showtext)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(webshot2)

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

###############################################################################
#============== Demographics
###############################################################################

# table summarising gender composition of sample 
gender_table <- data.frame(Gender = names(gender_count), 
                           n = as.vector(gender_count), 
                           pct = round(gender_prop, 1))

kable(gender_table[, c("Gender", "n", "pct.Freq")],
      col.names = c("Gender", "*n*", "%"),
      digits = 1)

# Summary table excluding NAs in primary_cultural
summary_table <- demo %>%
  filter(!is.na(primary_cultural)) %>% # Exclude NAs here
  count(primary_cultural, multiple_cultural) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(n, " (", percent, "%)"))
# Check unique combinations first
unique_combos <- demo %>%
  filter(!is.na(primary_cultural)) %>%
  count(primary_cultural, multiple_cultural) %>%
  pull(primary_cultural) %>% unique()

# Pivot only if consistent
summary_wide <- summary_table %>%
  dplyr::select(primary_cultural, multiple_cultural, label) %>%
  tidyr::pivot_wider(names_from = multiple_cultural, 
                     values_from = label, 
                     values_fill = "0 (0.0%)")


# now add the demographics to the data

demo$identified_prefs <- as.factor(demo$identified_prefs)
cult_table <- demo %>%
  filter(!is.na(identified_prefs)) %>%  # Explicit NA exclusion
  count(identified_prefs, sort = TRUE) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(n, " (", Percent, "%)")) %>% 
  dplyr::arrange(desc(Percent))

# Build main table
cult_table <- demo %>%
  filter(!is.na(identified_prefs)) %>%
  count(identified_prefs, sort = TRUE) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1)
  ) %>%
  dplyr::arrange(desc(Percent))

# Create summary row
total_n <- sum(cult_table$n)
summary_row <- data.frame(
  identified_prefs = "Total sample *N*",
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
demo$ID <- as.factor(demo$ID)





cat('
<div class="apa-table-title">
  <span class="table-label">Table 1.</span> Self‑Identified Cultural Ethnicity (Broad Categories)
</div>')
kable(summary_wide, col.names = c("Primary", "Single", "Multiple"))  
cat('</td></tr></table>')

## nationalities table 
nat_table <- demo %>%
  filter(!is.na(Nationality)) %>%  # Explicit NA exclusion
  count(Nationality, sort = TRUE) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(n, " (", Percent, "%)")) %>% 
  dplyr::arrange(desc(Percent))

# Build main table
nat_table <- demo %>%
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


###############################################################################
#============== Analyses
###############################################################################
present_analysis <- function(
    model,
    OR_labels,
    OR_title,
    logOdds_labels,
    logOdds_title,
    emmeans_list,
    contrasts_list,
    tost_obj,
    model_comp,
    diagnostic_plots,
    prefix = "analysis"
) {
  
  # Ensure folders exist
  if (!dir.exists("tables")) dir.create("tables")
  if (!dir.exists("figures")) dir.create("figures")
  
  css <- "
  <style>
    body, table, th, td {
      font-family: Georgia, serif;
      font-size: 11pt !important;
      color: black !important;
      line-height: 2 !important;
      margin-top: 2em !important;
      margin-bottom: 1em !important;
    }
    table, tbody, tr,  td { 
    border: none !important; 
    border-collapse: separate !important; }
    thead th {
    border-bottom: 1px solid black !important; 
    border-collapse: separate !important; }
    /* APA bottom rule (last data row, not the note) */ 
    tbody tr:last-child td { 
    border-bottom: 1px solid black !important; 
    } 
    /* Remove lines around NOTE row */ 
    tfoot td { border: none !important; }
  </style>
  "
  
  # Helper: save data-frame tables
  save_df_table <- function(df, name) {
    file <- file.path("tables", paste0(prefix, "_", name, ".html"))
    
    html <- knitr::kable(df, format = "html", digits = 3) %>% 
      add_header_above(c(" " = ncol(df)), line = TRUE) %>% 
      
      # APA header rule (row 0)
      row_spec(0, extra_css = "border-bottom: 2px solid black;") %>%
      
      # APA bottom rule (last row = nrow(df) + 1)
      row_spec(nrow(df) + 1, extra_css = "border-bottom: 1px solid black;") %>%
      
      # Apply minimal styling LAST
      kable_minimal()
    
    writeLines(html, file)
    message("Saved table: ", file)
  }
  
  
  # Helper: save figures
  # save_figure <- function(plot, name) {
  #   file <- file.path("figures", paste0(prefix, "_", name, ".png"))
  #   ggplot2::ggsave(file, plot, width = 5, height = 3, dpi = 300)
  #   message("Saved figure: ", file)
  # }
  
  save_fig <- function(plot, name) {
    file <- file.path("figures", paste0(prefix, "_", name, ".png"))
    png(filename = file,res = 300, width = 12, height = 8, units = "in")
    print(plot)
    dev.off()
    message("Saved figure: ", file)
  }
  # -------------------------
  # 1. Regression tables (saved directly)
  # -------------------------
  
  capture.output(
    tab_model(
      model,
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
      p.style = "numeric_stars",
      emph.p=FALSE,
      pred.labels = OR_labels,
      title = "", #paste0("Regression Table for ", OR_title, " (OR)"),
      CSS = list(
        css.table = "border-collapse: separate !important;",
        css.thead = "border-top: 1px solid black !important;",
        '.col2' = "border-bottom: none !important;",
        'td' = "border: none !important; font-size: 11pt !important;",
        'th' = "font-weight: normal !important; font-size: 11pt !important;",
        'body' = "font-family: Georgia, serif !important; font-size: 11pt !important;",
        css.footer = "border: none !important;"
      ),
      
      file = file.path("tables", paste0(prefix, "_regression_OR.html")),
      encoding = "UTF-8"
    )
  )
  
  
  capture.output(
    tab_model(
      model,
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
      p.style = "numeric_stars",
      emph.p=FALSE,
      pred.labels = logOdds_labels,
      title = " ", #paste0("Regression Table for ", logOdds_title, " (Log-Odds)"),
      CSS = list(
        css.table = "border-collapse: separate !important;",
        css.thead = "border-top: 1px solid black !important;",
        '.col2' = "border-bottom: none !important;",
        'td' = "border: none !important; font-size: 11pt !important;",
        'th' = "font-weight: normal !important; font-size: 11pt !important;",
        'body' = "font-family: Georgia, serif !important; font-size: 11pt !important;",
        css.footer = "border: none !important;"
      ),
      file = file.path("tables", paste0(prefix, "_regression_logodds.html")),
      encoding = "UTF-8"
    )
  )
  
  
  # 2. Reset theme for sjPlot 
  set_theme(georgia_theme)
  
  # -------------------------
  # 1b. Dynamic main-effect prediction plots
  # -------------------------
  
  message("Creating main-effect prediction plots...")
  
  # Extract all model terms
  all_terms <- attr(terms(model), "term.labels")
  
  # Identify main effects (no colons)
  main_terms <- grep("^[^:]+$", all_terms, value = TRUE)
  
  # Keep only categorical predictors (factors) 
  categorical_terms <- main_terms[sapply(main_terms, function(x) { 
    is.factor(model@frame[[x]]) })] 
  # Loop over each categorical main effect 
  for (mt in categorical_terms) { 
    p_main <- plot_model( model, terms = mt, type = "pred", colors = colours, title="") 
    fname <- paste0("pred_", mt) 
    save_fig(p_main, fname) 
    message("Saved main-effect plot: ", fname) 
  }
  
  # 6. Interaction plot
  # -------------------------
  # 1c. Dynamic two-way interaction plots
  # -------------------------
  
  message("Creating two-way interaction plots...")
  
  # Extract all model terms
  all_terms <- attr(terms(model), "term.labels")
  
  # Identify two-way interactions (exactly one colon)
  two_way_terms <- grep("^[^:]+:[^:]+$", all_terms, value = TRUE)
  
  # Loop over each two-way interaction
  for (tw in two_way_terms) {
    
    # Split "PBH:PDE" into c("PBH", "PDE")
    vars <- unlist(strsplit(tw, ":"))
    
    # Create the plot
    p_tw <- plot_model(
      model,
      type = "int",
      terms = vars,
      colors = colours,
      title = ""
    )
    
    # Save with a clean filename
    fname <- paste0("interaction_", vars[1], "_", vars[2])
    save_fig(p_tw, fname)
    
    message("Saved two-way interaction plot: ", fname)
  }
  
  p_int <- plot_model(model, type = "int", colors=colours, title= "")
  save_fig(p_int, "interaction_plot")
  
  message("Effect plots saved.")
  
  
  # -------------------------
  # 2. EMM tables (APA style)
  # -------------------------
  
  for (nm in names(emmeans_list)) {
    
    # Extract the actual EMM table
    df <- as.data.frame(emmeans_list[[nm]]$emmeans)
    
    # Keep only the columns you want (first 3 + numeric)
    df <- df %>%
      dplyr::select(1:3, emmean, SE, asymp.LCL, asymp.UCL)
    
    # Dynamic first 3 column names
    orig_names <- names(df)[1:3]
    
    # Build APA-style kable
    kable_obj <- kable(
      df,
      format = "html",
      booktabs = FALSE,   # APA does NOT use booktabs
      digits = 2,
      col.names = c(orig_names, "EMM", "SE", "LCI", "UCI"),
      align = c("l", rep("c", ncol(df)-1))
    ) %>%
      # # APA top rule
      # add_header_above(c(" " = ncol(df)), line = TRUE) %>%
      # # APA header rule
      # row_spec(0, extra_css = "border-bottom: 2px solid black;") %>%
      # # APA bottom rule
      # row_spec(nrow(df), extra_css = "border-bottom: 2px solid black;") %>%
      kable_styling(full_width = FALSE) %>%
      footnote(
        general_title = "Note.",
        general = "EMMs are on the log-odds scale.",
        threeparttable = TRUE,
        footnote_as_chunk = TRUE
      )
    
    # CSS for Georgia (no !important so APA lines remain)
    
    # Save HTML
    html_file <- file.path("tables", paste0(prefix, "_EMM_", nm, ".html"))
    
    kableExtra::save_kable(
      kable_obj,
      file = html_file,
      self_contained = TRUE,
      extra_dependencies = list(htmltools::HTML(css))
    )
    
    message("Saved EMM table: ", html_file)
  }
  
  
  
  # -------------------------
  # 3. Contrasts (APA style)
  # -------------------------
  
  for (nm in names(contrasts_list)) {
    
    df <- as.data.frame(contrasts_list[[nm]])
    df <- df %>%
      dplyr::select(1:3, estimate, SE, z.ratio, p.value) %>%
      rename(
        Est. = estimate,
        SE = SE,
        z = z.ratio,
        p = p.value
      )
    names(df) <- sub("_pairwise$", "", names(df))
    
    kable_obj <- kable(
      df,
      format = "html",
      booktabs = FALSE,
      digits = 3,
      align = c("l", rep("c", ncol(df)-1))
    ) %>%
      # APA top rule
      #add_header_above(c(" " = ncol(df)), line = TRUE) %>%
      kable_styling(full_width = TRUE) %>%
      footnote(
        general_title = "Note.",
        general = "Estimates are on the log-odds scale.",
        threeparttable = TRUE,
        footnote_as_chunk = TRUE
      )
    
    
    
    
    html_file <- file.path("tables", paste0(prefix, "_contrast_", nm, ".html"))
    
    kableExtra::save_kable(
      kable_obj,
      file = html_file,
      self_contained = TRUE,
      extra_dependencies = list(htmltools::HTML(css))
    )
    
    message("Saved contrast table: ", html_file)
  }
  
  
  # -------------------------
  # 4. TOST
  # -------------------------
  
  toast_df <- tost_obj %>%
    as.data.frame() %>%
    dplyr::select(Parameter, CI_low, CI_high, ROPE_Equivalence, p) %>%
    tibble::remove_rownames() %>% 
    rename(
      Parameter = Parameter,
      LCI = CI_low,
      UCI = CI_high,
      ROPE = ROPE_Equivalence,
      p = p
    )
  
  kable_obj <- kable(
    toast_df,
    format = "html",
    booktabs = FALSE,
    digits = 3,
    row.names = FALSE,
    align = c("l", rep("c", ncol(toast_df) - 1))
  ) %>%
    kable_styling(full_width = FALSE)
  
  
  
  html_file <- file.path("tables", paste0(prefix, "_TOST_table.html"))
  
  kableExtra::save_kable(
    kable_obj,
    file = html_file,
    self_contained = TRUE,
    extra_dependencies = list(htmltools::HTML(css))
  )
  
  message("Saved TOST table: ", html_file)
  
  
  tost_plot <- plot(tost_obj)
  tost_plot <- tost_plot +
    scale_color_brewer(palette = "Paired") +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal(base_family = "Georgia") +
    theme(
      text        = element_text(family = "Georgia"),
      axis.title  = element_text(family = "Georgia"),
      axis.text   = element_text(family = "Georgia"),
      legend.text = element_text(family = "Georgia"),
      legend.title= element_text(family = "Georgia"),
      title       = element_blank()
    )
  save_fig(tost_plot, "TOST_plot")
  
  # -------------------------
  # 5. Model comparison
  # -------------------------
  model_comp <- model_comp %>%
    dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC) %>%
    rename(
      Model = Name,
      'R2 (marg.)' = R2_marginal,
      'R2 (cond.)' = R2_conditional,
      AIC = AIC,
      BIC = BIC
    )
  
  kable_df <- kable(
    model_comp,
    format = "html",
    booktabs = FALSE,
    digits = 3,
    align = c("l", rep("c", ncol(model_comp) - 1))
  ) %>%
    kable_styling(full_width = FALSE) 
  
  
  save_kable(
    kable_df,
    file = file.path("tables", paste0(prefix, "_model_comparison.html")),
    self_contained = TRUE,
    extra_dependencies = list(htmltools::HTML(css))
  )
  
  
  # -------------------------
  # 6. Diagnostics
  # -------------------------
  # format them
  diagnostic_plots <- lapply(diagnostic_plots, function(p) {
    if (inherits(p, "ggplot")) {
      p$labels$title <- NULL
      p$labels$subtitle <- NULL
      p <- p + georgia_theme
    }
    p
  })
  for (i in seq_along(diagnostic_plots)) {
    #save_figure(diagnostic_plots[[i]], paste0("diagnostic_", i))
    save_fig(diagnostic_plots[[i]], paste0("diagnostic_", i))
  }
  
  # plot linearity of predicted response
  pr <- predict_response(model, terms = categorical_terms)
  plot_pr <- plot(pr, show_residuals = TRUE, show_residuals_line = TRUE, grid = FALSE) + georgia_theme +
    theme(title = element_blank())
  save_fig(plot_pr, "diagnostic_pred_response")
  message("Diagnostic plots saved.")
  
}

## main analysis
# prep names etc. 
model_comp <- readRDS("outputs/model_comp_Study2_full.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
# run function with full model
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_full_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_full_Agent,
    PBH = emmeans_hVai_full_PBH,
    PDE = emmeans_hVai_full_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_full_Agent,
    PBH = contrasts_hVai_full_PBH,
    PDE = contrasts_hVai_full_PDE
  ),
  tost_obj = TOaST_hVai_full,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_full,
  prefix = "Study2_full"
)

## reduced df 
# prep names etc. 
model_comp <- readRDS("outputs/model_comp_Study2_reduced.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_Agent,
    PBH = emmeans_hVai_reduced_PBH,
    PDE = emmeans_hVai_reduced_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_Agent,
    PBH = contrasts_hVai_reduced_PBH,
    PDE = contrasts_hVai_reduced_PDE
  ),
  tost_obj = TOaST_hVai_reduced,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced,
  prefix = "Study2_reduced"
)


## logRT as DV
model_comp <- readRDS("outputs/model_comp_Study2_RT.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_RT_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_RT_Agent,
    PBH = emmeans_hVai_RT_PBH,
    PDE = emmeans_hVai_RT_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_RT_Agent,
    PBH = contrasts_hVai_RT_PBH,
    PDE = contrasts_hVai_RT_PDE
  ),
  tost_obj = TOaST_hVai_RT,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_RT,
  prefix = "Study2_RT"
)

## logRT as RE
model_comp <- readRDS("outputs/model_comp_Study2_RTRE.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_RTRE_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_RTRE_Agent,
    PBH = emmeans_hVai_RTRE_PBH,
    PDE = emmeans_hVai_RTRE_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_RTRE_Agent,
    PBH = contrasts_hVai_RTRE_PBH,
    PDE = contrasts_hVai_RTRE_PDE
  ),
  tost_obj = TOaST_hVai_RTRE,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_RTRE,
  prefix = "Study2_RTRE"
)

## intent x Agent as fixed factors
model_comp <- readRDS("outputs/model_comp_Study2_intentxAgent.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_intentxAgent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_intentxAgent_Agent,
    PBH = emmeans_hVai_intentxAgent_PBH,
    PDE = emmeans_hVai_intentxAgent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_intentxAgent_Agent,
    PBH = contrasts_hVai_intentxAgent_PBH,
    PDE = contrasts_hVai_intentxAgent_PDE
  ),
  tost_obj = TOaST_hVai_intentxAgent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_intentxAgent,
  prefix = "Study2_intentxAgent"
)

## intent as fixed factor
model_comp <- readRDS("outputs/model_comp_Study2_intent.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_intent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_intent_Agent,
    PBH = emmeans_hVai_intent_PBH,
    PDE = emmeans_hVai_intent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_intent_Agent,
    PBH = contrasts_hVai_intent_PBH,
    PDE = contrasts_hVai_intent_PDE
  ),
  tost_obj = TOaST_hVai_intent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_intent,
  prefix = "Study2_intent"
)

## REDUCED DATA
## logRT as DV
model_comp <- readRDS("outputs/model_comp_Study2_reduced_RT.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_RT_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_RT_Agent,
    PBH = emmeans_hVai_reduced_RT_PBH,
    PDE = emmeans_hVai_reduced_RT_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_RT_Agent,
    PBH = contrasts_hVai_reduced_RT_PBH,
    PDE = contrasts_hVai_reduced_RT_PDE
  ),
  tost_obj = TOaST_hVai_reduced_RT,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_RT,
  prefix = "Study2_reduced_RT"
)

## logRT as RE
model_comp <- readRDS("outputs/model_comp_Study2_reduced_RTRE.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_RTRE_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_RTRE_Agent,
    PBH = emmeans_hVai_reduced_RTRE_PBH,
    PDE = emmeans_hVai_reduced_RTRE_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_RTRE_Agent,
    PBH = contrasts_hVai_reduced_RTRE_PBH,
    PDE = contrasts_hVai_reduced_RTRE_PDE
  ),
  tost_obj = TOaST_hVai_reduced_RTRE,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_RTRE,
  prefix = "Study2_reduced_RTRE"
)

## intent x Agent as fixed factors
model_comp <- readRDS("outputs/model_comp_Study2_reduced_intentxAgent.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_intentxAgent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_intentxAgent_Agent,
    PBH = emmeans_hVai_reduced_intentxAgent_PBH,
    PDE = emmeans_hVai_reduced_intentxAgent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_intentxAgent_Agent,
    PBH = contrasts_hVai_reduced_intentxAgent_PBH,
    PDE = contrasts_hVai_reduced_intentxAgent_PDE
  ),
  tost_obj = TOaST_hVai_reduced_intentxAgent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_intentxAgent,
  prefix = "Study2_reduced_intentxAgent"
)

## intent as fixed factor
model_comp <- readRDS("outputs/model_comp_Study2_reduced_intent.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "Full Model"
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_intent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_intent_Agent,
    PBH = emmeans_hVai_reduced_intent_PBH,
    PDE = emmeans_hVai_reduced_intent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_intent_Agent,
    PBH = contrasts_hVai_reduced_intent_PBH,
    PDE = contrasts_hVai_reduced_intent_PDE
  ),
  tost_obj = TOaST_hVai_reduced_intent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_intent,
  prefix = "Study2_reduced_intent"
)


## match df model
# prep names etc. 
model_comp <- readRDS("outputs/model_comp_Study2_match.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "match Model"
# run function with match model
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_match_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_match_Agent,
    PBH = emmeans_hVai_match_PBH,
    PDE = emmeans_hVai_match_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_match_Agent,
    PBH = contrasts_hVai_match_PBH,
    PDE = contrasts_hVai_match_PDE
  ),
  tost_obj = TOaST_hVai_match,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_match,
  prefix = "Study2_match"
)

## logRT as DV
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_match_RT_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_match_RT_Agent,
    PBH = emmeans_hVai_match_RT_PBH,
    PDE = emmeans_hVai_match_RT_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_match_RT_Agent,
    PBH = contrasts_hVai_match_RT_PBH,
    PDE = contrasts_hVai_match_RT_PDE
  ),
  tost_obj = TOaST_hVai_match_RT,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_match_RT,
  prefix = "Study2_match_RT"
)

## logRT as RE
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_match_RTRE_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_match_RTRE_Agent,
    PBH = emmeans_hVai_match_RTRE_PBH,
    PDE = emmeans_hVai_match_RTRE_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_match_RTRE_Agent,
    PBH = contrasts_hVai_match_RTRE_PBH,
    PDE = contrasts_hVai_match_RTRE_PDE
  ),
  tost_obj = TOaST_hVai_match_RTRE,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_match_RTRE,
  prefix = "Study2_match_RTRE"
)

## intent x Agent as fixed factors
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_match_intentxAgent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_match_intentxAgent_Agent,
    PBH = emmeans_hVai_match_intentxAgent_PBH,
    PDE = emmeans_hVai_match_intentxAgent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_match_intentxAgent_Agent,
    PBH = contrasts_hVai_match_intentxAgent_PBH,
    PDE = contrasts_hVai_match_intentxAgent_PDE
  ),
  tost_obj = TOaST_hVai_match_intentxAgent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_match_intentxAgent,
  prefix = "Study2_match_intentxAgent"
)

## intent as fixed factor
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_match_intent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_match_intent_Agent,
    PBH = emmeans_hVai_match_intent_PBH,
    PDE = emmeans_hVai_match_intent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_match_intent_Agent,
    PBH = contrasts_hVai_match_intent_PBH,
    PDE = contrasts_hVai_match_intent_PDE
  ),
  tost_obj = TOaST_hVai_match_intent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_match_intent,
  prefix = "Study2_match_intent"
)


## match reduced df
# prep names etc. 
model_comp <- readRDS("outputs/model_comp_Study2_reduced_match.rds")
model_comp_df <- as.data.frame(model_comp)
model_comp_df <- model_comp_df %>% 
  dplyr::select(Name, R2_marginal, R2_conditional, AIC, BIC)
model_comp_df[4,"Name"] <- "PBH x Agent"
model_comp_df[5,"Name"] <- "PDE x Agent"
model_comp_df[6,"Name"] <- "PBH x PDE"
model_comp_df[7,"Name"] <- "reduced_match Model"
# run function with reduced_match model
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_match_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_match_Agent,
    PBH = emmeans_hVai_reduced_match_PBH,
    PDE = emmeans_hVai_reduced_match_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_match_Agent,
    PBH = contrasts_hVai_reduced_match_PBH,
    PDE = contrasts_hVai_reduced_match_PDE
  ),
  tost_obj = TOaST_hVai_reduced_match,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_match,
  prefix = "Study2_reduced_match"
)

## logRT as DV
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_match_RT_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_match_RT_Agent,
    PBH = emmeans_hVai_reduced_match_RT_PBH,
    PDE = emmeans_hVai_reduced_match_RT_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_match_RT_Agent,
    PBH = contrasts_hVai_reduced_match_RT_PBH,
    PDE = contrasts_hVai_reduced_match_RT_PDE
  ),
  tost_obj = TOaST_hVai_reduced_match_RT,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_match_RT,
  prefix = "Study2_reduced_match_RT"
)

## logRT as RE
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_match_RTRE_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_match_RTRE_Agent,
    PBH = emmeans_hVai_reduced_match_RTRE_PBH,
    PDE = emmeans_hVai_reduced_match_RTRE_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_match_RTRE_Agent,
    PBH = contrasts_hVai_reduced_match_RTRE_PBH,
    PDE = contrasts_hVai_reduced_match_RTRE_PDE
  ),
  tost_obj = TOaST_hVai_reduced_match_RTRE,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_match_RTRE,
  prefix = "Study2_reduced_match_RTRE"
)

## intent x Agent as fixed factors
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_match_intentxAgent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_match_intentxAgent_Agent,
    PBH = emmeans_hVai_reduced_match_intentxAgent_PBH,
    PDE = emmeans_hVai_reduced_match_intentxAgent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_match_intentxAgent_Agent,
    PBH = contrasts_hVai_reduced_match_intentxAgent_PBH,
    PDE = contrasts_hVai_reduced_match_intentxAgent_PDE
  ),
  tost_obj = TOaST_hVai_reduced_match_intentxAgent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_match_intentxAgent,
  prefix = "Study2_reduced_match_intentxAgent"
)

## intent as fixed factor
present_analysis(
  model = readRDS("outputs/dropAgentIDdilRE_hVai_reduced_match_intent_glmModel.rds"),
  OR_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  OR_title = "Overall Model",
  logOdds_labels = c("(Intercept)","PBH Violated","PDE Criteria Met","Agent (AI)","PBH Violated x PDE Criteria Met","PBH Violated x Agent (AI)","PDE Criteria Met x Agent (AI)", "PBH Violated x PDE Criteria Met x Agent (AI)"),
  logOdds_title = "Overall Model",
  emmeans_list = list(
    Agent = emmeans_hVai_reduced_match_intent_Agent,
    PBH = emmeans_hVai_reduced_match_intent_PBH,
    PDE = emmeans_hVai_reduced_match_intent_PDE
  ),
  contrasts_list = list(
    Agent = contrasts_hVai_reduced_match_intent_Agent,
    PBH = contrasts_hVai_reduced_match_intent_PBH,
    PDE = contrasts_hVai_reduced_match_intent_PDE
  ),
  tost_obj = TOaST_hVai_reduced_match_intent,
  model_comp = model_comp_df,
  diagnostic_plots = diagnostic_plots_hVai_reduced_match_intent,
  prefix = "Study2_reduced_match_intent"
)