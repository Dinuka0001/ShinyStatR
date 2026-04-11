# =============================================================================
# ShinyStatR - Comprehensive Statistical Analysis App
# Version 1.1.0
# =============================================================================
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(ggsignif)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(car)
library(coin)
library(survival)
library(survminer)
library(pROC)
library(nortest)
library(multcomp)
library(broom)
library(svglite)
library(colourpicker)
library(cowplot)
library(scales)
library(ggtext)
library(RColorBrewer)

# Suppress false positives from NSE/data-masked symbols used by ggplot2/dplyr formulas.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("group", "value", "Group", "strata", "Parameter"))
}
group <- value <- Group <- strata <- Parameter <- NULL

# =============================================================================
# CUSTOM CSS
# =============================================================================
app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700;800&family=Source+Code+Pro:wght@400;500&display=swap');

:root {
  --bg-soft: #f3f7fb;
  --bg-soft-2: #eaf1f9;
  --card: #ffffff;
  --ink: #1c2d3d;
  --ink-muted: #5e7388;
  --accent: #0f6c9b;
  --accent-2: #0e89a6;
  --border: #d5e1ec;
  --ok: #1f8b5f;
  --warn: #c57a11;
  --danger: #b73f45;
}

body {
  font-family: 'Manrope', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  color: var(--ink);
}

.content-wrapper {
  background: radial-gradient(1200px 800px at 95% -10%, #dceeff 0%, transparent 60%),
              linear-gradient(180deg, var(--bg-soft) 0%, var(--bg-soft-2) 100%);
}

.skin-blue .main-header .logo {
  font-weight: 800;
  font-size: 18px;
  letter-spacing: 0.2px;
  background: linear-gradient(100deg, #0f6c9b, #0e89a6);
  color: #fff;
}

.skin-blue .main-header .navbar {
  background: linear-gradient(100deg, #184a72, #0f6c9b);
}

.main-sidebar,
.skin-blue .main-sidebar {
  background: linear-gradient(180deg, #13293d 0%, #173650 100%);
}

.sidebar-menu > li > a {
  font-size: 13px;
  letter-spacing: 0.15px;
  border-radius: 8px;
  margin: 2px 8px;
}

.sidebar-menu > li.active > a {
  background: rgba(255, 255, 255, 0.13);
  border-left: 4px solid #7ad3ff;
}

.stat-card {
  background: var(--card);
  border-radius: 14px;
  padding: 20px;
  margin-bottom: 16px;
  box-shadow: 0 10px 24px rgba(18, 55, 88, 0.08);
  border: 1px solid var(--border);
}

.stat-card h4 {
  margin-top: 0;
  color: var(--ink);
  font-weight: 700;
}

.stat-card.significant { border-left: 4px solid var(--danger); }
.stat-card.not-significant { border-left: 4px solid var(--ok); }

.badge-sig {
  padding: 5px 12px;
  border-radius: 999px;
  font-weight: 700;
  font-size: 12px;
  display: inline-block;
  margin: 6px 0;
}

.badge-sig.sig { background: #f8d9db; color: #8f2d32; }
.badge-sig.not-sig { background: #d8efe4; color: #1d6f4d; }

.section-header {
  background: linear-gradient(100deg, #184a72, #0f6c9b);
  color: #fff;
  padding: 13px 20px;
  border-radius: 12px;
  margin-bottom: 14px;
  font-size: 16px;
  font-weight: 700;
  box-shadow: 0 8px 20px rgba(17, 74, 114, 0.2);
}

.data-input-area textarea,
.group-name-input .form-control,
.form-control {
  border-radius: 10px;
  border: 1px solid var(--border);
}

.data-input-area textarea {
  font-family: 'Source Code Pro', 'Consolas', 'Courier New', monospace;
  font-size: 12px;
}

.data-input-area textarea:focus,
.group-name-input .form-control:focus,
.form-control:focus {
  border-color: #7abfe2;
  box-shadow: 0 0 0 3px rgba(15, 108, 155, 0.12);
}

.btn-primary {
  background: linear-gradient(100deg, #0f6c9b, #0e89a6);
  border: none;
  border-radius: 10px;
  font-weight: 700;
  padding: 8px 18px;
  transition: transform 0.15s ease, box-shadow 0.2s ease;
}

.btn-primary:hover {
  transform: translateY(-1px);
  box-shadow: 0 8px 18px rgba(15, 108, 155, 0.3);
}

.nav-tabs > li > a {
  border-radius: 10px 10px 0 0;
  font-weight: 700;
}

.nav-tabs > li.active > a {
  border-top: 3px solid var(--accent);
}

.dataTables_wrapper { font-size: 13px; }
table.dataTable thead th { background: #184a72; color: #fff; font-weight: 700; }

#plot_summary_text {
  background: #f8fbfe;
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 12px 15px;
  font-size: 12px;
  font-family: 'Source Code Pro', 'Consolas', 'Courier New', monospace;
  color: #2a4154;
  white-space: pre-wrap;
  word-wrap: break-word;
  min-height: 40px;
}

.sidebar-section-header {
  font-size: 11px;
  font-weight: 800;
  color: #7ad3ff;
  text-transform: uppercase;
  letter-spacing: 0.6px;
  margin: 12px 0 6px 0;
  padding-bottom: 4px;
  border-bottom: 1px solid rgba(122, 211, 255, 0.3);
}

.whisker-help { font-size: 11px; color: #6e7f8f; margin-top: -6px; margin-bottom: 8px; }

.test-info-box {
  background: #f0f8fd;
  border: 1px solid #c9e1ef;
  border-radius: 12px;
  padding: 15px 18px;
  margin: 10px 0 15px 0;
  font-size: 13px;
  color: #2d4659;
  line-height: 1.6;
}

.test-info-box h5 { margin-top: 0; color: #195a82; font-weight: 800; font-size: 14px; }
.test-info-box ul { margin: 5px 0; padding-left: 20px; }
.test-info-box .formula { font-family: 'Source Code Pro', monospace; background: #fff; padding: 2px 6px; border-radius: 4px; font-size: 12px; }
.info-toggle-link { color: #0f6c9b; font-weight: 700; cursor: pointer; text-decoration: none; font-size: 13px; }
.info-toggle-link:hover { color: #0c587f; text-decoration: underline; }

.data-source-banner {
  padding: 10px 12px;
  border-radius: 10px;
  margin-bottom: 12px;
  font-size: 12px;
  font-weight: 700;
  line-height: 1.4;
}

.data-source-banner.source-manual {
  background: #e0f4ea;
  border: 1px solid #89cbad;
  color: #1e6a48;
}

.data-source-banner.source-file {
  background: #e0eff9;
  border: 1px solid #90bde0;
  color: #1d4f78;
}

.data-source-banner.source-none {
  background: #fbeedb;
  border: 1px solid #ebc58a;
  color: #925f0f;
}

.data-source-sidebar { padding: 8px 15px; margin: 0 10px 5px 10px; }

.sidebar-status-panel {
  margin: 0 10px 8px 10px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.18);
  background: rgba(255, 255, 255, 0.06);
  padding: 8px 10px;
}

.sidebar-status-title {
  font-size: 10px;
  letter-spacing: 0.7px;
  text-transform: uppercase;
  color: #a9cce6;
  font-weight: 800;
  margin-bottom: 6px;
}

.sidebar-status-row {
  display: grid;
  grid-template-columns: minmax(0, 1fr);
  align-items: flex-start;
  gap: 4px;
  padding: 6px 0;
  border-top: 1px solid rgba(255, 255, 255, 0.1);
}

.sidebar-status-row .status-pill {
  justify-self: start;
}

.sidebar-status-row:first-of-type {
  border-top: none;
  padding-top: 2px;
}

.sidebar-status-main {
  color: #e5f2fc;
  font-size: 11px;
  font-weight: 700;
  line-height: 1.3;
  min-width: 0;
  overflow-wrap: anywhere;
  word-break: break-word;
}

.sidebar-status-detail {
  display: block;
  color: #b9d3e5;
  font-size: 9px;
  margin-top: 2px;
}

.status-pill {
  display: inline-block;
  border-radius: 999px;
  font-size: 9px;
  font-weight: 800;
  padding: 2px 6px;
  white-space: nowrap;
  text-transform: uppercase;
  letter-spacing: 0.35px;
}

.status-pill.ok {
  background: #d8efe4;
  color: #1d6f4d;
}

.status-pill.warn {
  background: #fbeedb;
  color: #925f0f;
}

.plot-config-panel,
.mp-options-panel {
  max-height: calc(100vh - 90px);
  overflow-y: auto;
  position: sticky;
  top: 66px;
  scrollbar-width: thin;
}

.panel-subtitle {
  margin: -3px 0 12px 0;
  color: var(--ink-muted);
  font-size: 12px;
  line-height: 1.45;
}

.plot-config-panel h5,
.mp-options-panel h5 {
  margin: 14px 0 8px 0;
  padding: 6px 10px;
  border-radius: 8px;
  background: #edf5fb;
  color: #1f4f74;
  border: 1px solid #d3e4f1;
}

.plot-config-panel hr,
.mp-options-panel hr {
  border-top: 1px solid #d7e3ee;
  margin-top: 14px;
  margin-bottom: 10px;
}

.cfg-section {
  border: 1px solid #d6e4f0;
  border-radius: 10px;
  background: #fbfdff;
  margin-bottom: 10px;
  overflow: hidden;
}

.cfg-section > summary {
  list-style: none;
  cursor: pointer;
  padding: 9px 12px;
  font-size: 13px;
  font-weight: 800;
  color: #1f4f74;
  background: #edf5fb;
  border-bottom: 1px solid #d6e4f0;
}

.cfg-section > summary::-webkit-details-marker {
  display: none;
}

.cfg-section > summary::after {
  content: '+';
  float: right;
  font-weight: 800;
}

.cfg-section[open] > summary::after {
  content: '-';
}

.cfg-section .form-group,
.cfg-section .shiny-input-container,
.cfg-section .help-block {
  margin-left: 12px;
  margin-right: 12px;
}

.cfg-section > .help-block {
  margin-top: 10px;
}

@media (max-width: 991px) {
  .section-header { font-size: 15px; padding: 11px 14px; }
  .stat-card { padding: 14px; border-radius: 12px; }
  .plot-config-panel,
  .mp-options-panel {
    max-height: none;
    overflow-y: visible;
    position: static;
  }
}
"

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
 skin = "blue",
 
 # --- Header ---
 dashboardHeader(
   title = span(icon("chart-bar"), " ShinyStatR"),
   titleWidth = 280
 ),
 
 # --- Sidebar ---
 dashboardSidebar(
   width = 280,
   sidebarMenu(
     id = "tabs",
     menuItem("Data Input", tabName = "data_input", icon = icon("database")),
     menuItem("Statistical Tests", tabName = "stat_tests", icon = icon("calculator"),
       menuSubItem("Parametric Tests", tabName = "parametric"),
       menuSubItem("Non-Parametric Tests", tabName = "nonparametric"),
       menuSubItem("Paired Tests", tabName = "paired"),
       menuSubItem("One-Sample Tests", tabName = "onesample"),
       menuSubItem("Proportion Tests", tabName = "proportion"),
       menuSubItem("Variance Tests", tabName = "variance"),
       menuSubItem("Normality Tests", tabName = "normality"),
       menuSubItem("Survival & ROC", tabName = "survival_roc"),
       menuSubItem("Multi-Factor Tests", tabName = "multifactor")
     ),
     menuItem("Results", tabName = "results", icon = icon("table")),
     menuItem("Plots", tabName = "plots", icon = icon("chart-area")),
     menuItem("Multi-Parameter", tabName = "multi_param", icon = icon("layer-group")),
     menuItem("About", tabName = "about", icon = icon("info-circle")),
     hr(),
     uiOutput("sidebar_data_source"),
     div(style = "padding: 10px 15px; color: #b8c7ce; font-size: 11px;",
       "ShinyStatR v1.1.0", br(),
       "Developed by Dinuka Adasooriya"
     )
   )
 ),
 
 # --- Body ---
 dashboardBody(
   tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(paste(c(
      "Shiny.addCustomMessageHandler('resizePlotContainer', function(msg) {",
      "  var wrapper = document.getElementById('main_plot_wrapper');",
      "  var plotEl = document.getElementById('main_plot');",
      "  if (wrapper) { wrapper.style.height = msg.height + 'px'; }",
      "  if (plotEl) { plotEl.style.height = msg.height + 'px'; }",
      "});",
      "Shiny.addCustomMessageHandler('clearMpInputs', function(msg) {",
      "  var fileEl = document.getElementById('mp_file_input');",
      "  if (fileEl) {",
      "    fileEl.value = '';",
      "    fileEl.dispatchEvent(new Event('change', { bubbles: true }));",
      "    var fileWrapper = fileEl.closest('.input-group');",
      "    if (fileWrapper) {",
      "      var fileText = fileWrapper.querySelector('input.form-control');",
      "      if (fileText) {",
      "        fileText.value = '';",
      "      }",
      "    }",
      "  }",
      "});",
      "$(document).on('shiny:connected', function() {",
      "  function disableHeaders() {",
      "    $('#mp_test_method option, #plot_signif_method option').each(function() {",
      "      if ($(this).val().indexOf('header_') === 0) {",
      "        $(this).prop('disabled', true);",
      "        $(this).css({'font-weight': 'bold', 'color': '#2c6fa0', 'background': '#eef5fb'});",
      "      }",
      "    });",
      "  }",
      "  disableHeaders();",
      "  $(document).on('shiny:updateinput', function(e) {",
      "    if (e.binding && e.binding.name === 'shiny.selectInput') {",
      "      setTimeout(disableHeaders, 100);",
      "    }",
      "  });",
      "});"
    ), collapse = '\n')))
   ),
   
   tabItems(
     # =========================================================================
     # TAB: DATA INPUT (merged: Manual Entry + File Upload)
     # =========================================================================
     tabItem(tabName = "data_input",
       fluidRow(
         column(12,
           div(class = "section-header", icon("database"), " Data Input"),
           div(class = "stat-card",
             tabsetPanel(id = "data_input_mode", type = "tabs",
               # --- Sub-tab: Paste Data ---
               tabPanel("Paste Data", icon = icon("keyboard"),
                 br(),
                 fluidRow(
                   column(4,
                     numericInput("num_groups", "Number of Groups:",
                       value = 2, min = 1, max = 20, step = 1)
                   ),
                   column(4,
                     selectInput("data_separator", "Data Separator:",
                       choices = c("New line" = "\n", "Comma" = ",",
                                   "Space" = " ", "Tab" = "\t", "Semicolon" = ";"),
                       selected = "\n")
                   ),
                   column(4,
                     br(),
                     actionButton("load_manual_data", "Load Data",
                       icon = icon("check-circle"),
                       class = "btn-primary", style = "margin-top: 5px;"),
                     actionButton("clear_all_data", "Clear All",
                       icon = icon("trash"), class = "btn-danger",
                       style = "margin-top: 5px; margin-left: 5px;")
                   )
                 ),
                 hr(),
                 uiOutput("group_inputs"),
                 hr(),
                 h4(icon("eye"), " Data Preview"),
                 DTOutput("manual_data_preview")
               ),
               # --- Sub-tab: Upload File ---
               tabPanel("Upload File", icon = icon("file-upload"),
                 br(),
                 fluidRow(
                   column(6,
                     fileInput("file_upload_input", "Choose File:",
                       accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx"),
                       multiple = FALSE),
                     helpText("Supported formats: CSV, TXT, TSV, Excel (.xls, .xlsx)")
                   ),
                   column(3,
                     selectInput("file_sep", "Separator (text files):",
                       choices = c("Comma" = ",", "Tab" = "\t",
                                   "Semicolon" = ";", "Space" = " "))
                   ),
                   column(3,
                     checkboxInput("file_header", "File has header row", TRUE),
                     selectInput("file_sheet", "Excel Sheet:", choices = NULL)
                   )
                 ),
                 fluidRow(
                   column(6,
                     selectInput("file_data_format", "Data Format:",
                       choices = c("Wide (groups as columns)" = "wide",
                                   "Long (group label + value columns)" = "long"))
                   ),
                   column(3,
                     conditionalPanel(
                       condition = "input.file_data_format == 'long'",
                       selectInput("file_group_col", "Group Column:", choices = NULL)
                     )
                   ),
                   column(3,
                     conditionalPanel(
                       condition = "input.file_data_format == 'long'",
                       selectInput("file_value_col", "Value Column:", choices = NULL)
                     )
                   )
                 ),
                 actionButton("load_file_data", "Load File Data",
                   icon = icon("upload"), class = "btn-primary"),
                 actionButton("clear_all_data2", "Clear All",
                   icon = icon("trash"), class = "btn-danger",
                   style = "margin-left: 5px;"),
                 hr(),
                 h4(icon("table"), " Uploaded Data Preview"),
                 DTOutput("file_data_preview")
               )
             )
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: PARAMETRIC TESTS
     # =========================================================================
     tabItem(tabName = "parametric",
       fluidRow(
         column(12,
           uiOutput("ds_banner_param"),
           div(class = "section-header", icon("calculator"),
             " Parametric Tests (assume normal distribution)"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("parametric_test", "Select Test:",
                   choices = c(
                     "One Sample T-Test" = "one_t",
                     "One Sample Z-Test" = "one_z",
                     "Two Sample T-Test (Pooled)" = "two_t_pooled",
                     "Two Sample T-Test (Welch's)" = "two_t_welch",
                     "Two Sample Z-Test" = "two_z",
                     "One Way ANOVA" = "anova_one",
                     "Repeated Measures ANOVA" = "rm_anova"
                   ))
               ),
               column(3,
                 selectInput("param_alternative", "Alternative:",
                   choices = c("two.sided", "less", "greater"))
               ),
               column(3,
                 numericInput("param_conf_level", "Confidence Level:",
                   value = 0.95, min = 0.80, max = 0.99, step = 0.01)
               )
             ),
             fluidRow(
               column(3,
                 conditionalPanel(
                   condition = "input.parametric_test == 'one_t' ||
                                input.parametric_test == 'one_z'",
                   numericInput("param_mu", "Hypothesized Mean (mu):", value = 0)
                 )
               ),
               column(3,
                 conditionalPanel(
                   condition = "input.parametric_test == 'one_z' ||
                                input.parametric_test == 'two_z'",
                   numericInput("param_sigma", "Known Sigma:", value = 1, min = 0.001)
                 )
               ),
               column(3,
                 conditionalPanel(
                   condition = "input.parametric_test == 'two_z'",
                   numericInput("param_sigma2", "Known Sigma (Group 2):",
                     value = 1, min = 0.001)
                 )
               ),
               column(3,
                 uiOutput("param_group_selectors")
               )
             ),
             br(),
             checkboxInput("show_info_parametric", "Show test details", FALSE),
             uiOutput("test_info_parametric"),
             actionButton("run_parametric", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("parametric_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: NON-PARAMETRIC TESTS
     # =========================================================================
     tabItem(tabName = "nonparametric",
       fluidRow(
         column(12,
           uiOutput("ds_banner_nonparam"),
           div(class = "section-header", icon("calculator"),
             " Non-Parametric Tests (no distribution assumptions)"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("nonparam_test", "Select Test:",
                   choices = c(
                     "Mann-Whitney U Test" = "mann_whitney",
                     "Kruskal-Wallis Test" = "kruskal",
                     "Chi-Squared Goodness of Fit" = "chi_gof"
                   ))
               ),
               column(3,
                 selectInput("nonparam_alternative", "Alternative:",
                   choices = c("two.sided", "less", "greater"))
               ),
               column(3,
                 numericInput("nonparam_conf_level", "Confidence Level:",
                   value = 0.95, min = 0.80, max = 0.99, step = 0.01)
               )
             ),
             fluidRow(
               column(3,
                 checkboxInput("np_exact", "Exact p-value", TRUE)
               ),
               column(3,
                 checkboxInput("np_correct", "Continuity correction", FALSE)
               ),
               column(6,
                 helpText(em("Exact is recommended for small samples (n < 50).",
                   "Continuity correction only applies when exact = FALSE."))
               )
             ),
             fluidRow(
               column(6,
                 uiOutput("nonparam_group_selectors")
               ),
               column(6,
                 conditionalPanel(
                   condition = "input.nonparam_test == 'chi_gof'",
                   textInput("chi_expected_probs",
                     "Expected Proportions (comma-separated):",
                     placeholder = "e.g., 0.25, 0.25, 0.25, 0.25")
                 )
               )
             ),
             br(),
             checkboxInput("show_info_nonparam", "Show test details", FALSE),
             uiOutput("test_info_nonparam"),
             actionButton("run_nonparam", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("nonparam_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: PAIRED TESTS
     # =========================================================================
     tabItem(tabName = "paired",
       fluidRow(
         column(12,
           uiOutput("ds_banner_paired"),
           div(class = "section-header", icon("calculator"),
             " Paired / Repeated Measures Tests"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("paired_test", "Select Test:",
                   choices = c(
                     "Paired T-Test" = "paired_t",
                     "Wilcoxon Signed Rank Test" = "wilcoxon_sr",
                     "Friedman Test" = "friedman"
                   ))
               ),
               column(3,
                 selectInput("paired_alternative", "Alternative:",
                   choices = c("two.sided", "less", "greater"))
               ),
               column(3,
                 numericInput("paired_conf_level", "Confidence Level:",
                   value = 0.95, min = 0.80, max = 0.99, step = 0.01)
               )
             ),
             fluidRow(
               column(3,
                 checkboxInput("pd_exact", "Exact p-value", TRUE)
               ),
               column(3,
                 checkboxInput("pd_correct", "Continuity correction", FALSE)
               ),
               column(6,
                 helpText(em("Exact is recommended for small samples."))
               )
             ),
             uiOutput("paired_group_selectors"),
             br(),
             checkboxInput("show_info_paired", "Show test details", FALSE),
             uiOutput("test_info_paired"),
             actionButton("run_paired", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("paired_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: ONE-SAMPLE TESTS
     # =========================================================================
     tabItem(tabName = "onesample",
       fluidRow(
         column(12,
           uiOutput("ds_banner_onesample"),
           div(class = "section-header", icon("calculator"),
             " One-Sample Tests"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("onesample_test", "Select Test:",
                   choices = c(
                     "One Sample T-Test" = "one_t",
                     "One Sample Z-Test" = "one_z",
                     "One Sample Proportion Test" = "one_prop"
                   ))
               ),
               column(3,
                 numericInput("os_mu", "Hypothesized value:", value = 0)
               ),
               column(3,
                 conditionalPanel(
                   condition = "input.onesample_test == 'one_z'",
                   numericInput("os_sigma", "Known sigma:", value = 1, min = 0.001)
                 ),
                 conditionalPanel(
                   condition = "input.onesample_test == 'one_prop'",
                   numericInput("os_prop_x", "Number of successes:", value = 10, min = 0),
                   numericInput("os_prop_n", "Number of trials:", value = 100, min = 1)
                 )
               )
             ),
             uiOutput("onesample_group_selector"),
             br(),
             checkboxInput("show_info_onesample", "Show test details", FALSE),
             uiOutput("test_info_onesample"),
             actionButton("run_onesample", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("onesample_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: PROPORTION TESTS
     # =========================================================================
     tabItem(tabName = "proportion",
       fluidRow(
         column(12,
           uiOutput("ds_banner_prop"),
           div(class = "section-header", icon("calculator"),
             " Proportion Tests"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("prop_test", "Select Test:",
                   choices = c(
                     "One Sample Proportion Test" = "one_prop",
                     "Two Sample Proportion Test" = "two_prop"
                   ))
               ),
               column(3,
                 selectInput("prop_alternative", "Alternative:",
                   choices = c("two.sided", "less", "greater"))
               ),
               column(3,
                 numericInput("prop_conf_level", "Confidence Level:",
                   value = 0.95, min = 0.80, max = 0.99, step = 0.01)
               )
             ),
             fluidRow(
               column(6,
                 h5(strong("Group 1:")),
                 numericInput("prop_x1", "Successes:", value = 10, min = 0),
                 numericInput("prop_n1", "Trials:", value = 100, min = 1)
               ),
               column(6,
                 conditionalPanel(
                   condition = "input.prop_test == 'two_prop'",
                   h5(strong("Group 2:")),
                   numericInput("prop_x2", "Successes:", value = 15, min = 0),
                   numericInput("prop_n2", "Trials:", value = 100, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.prop_test == 'one_prop'",
                   numericInput("prop_p0", "Hypothesized proportion:",
                     value = 0.5, min = 0, max = 1, step = 0.01)
                 )
               )
             ),
             br(),
             checkboxInput("show_info_prop", "Show test details", FALSE),
             uiOutput("test_info_prop"),
             actionButton("run_prop", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("prop_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: VARIANCE TESTS
     # =========================================================================
     tabItem(tabName = "variance",
       fluidRow(
         column(12,
           uiOutput("ds_banner_var"),
           div(class = "section-header", icon("calculator"),
             " Tests for Variances"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("var_test", "Select Test:",
                   choices = c(
                     "Chi-Squared Test for Variance" = "chi_var",
                     "F Test for Variances" = "f_test",
                     "Levene's Test for Variances" = "levene"
                   ))
               ),
               column(3,
                 selectInput("var_alternative", "Alternative:",
                   choices = c("two.sided", "less", "greater"))
               ),
               column(3,
                 conditionalPanel(
                   condition = "input.var_test == 'chi_var'",
                   numericInput("var_sigma0", "Hypothesized Variance:",
                     value = 1, min = 0.001)
                 )
               )
             ),
             uiOutput("var_group_selectors"),
             br(),
             checkboxInput("show_info_var", "Show test details", FALSE),
             uiOutput("test_info_var"),
             actionButton("run_var", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("var_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: NORMALITY TESTS
     # =========================================================================
     tabItem(tabName = "normality",
       fluidRow(
         column(12,
           uiOutput("ds_banner_norm"),
           div(class = "section-header", icon("calculator"),
             " Normality & Distribution Tests"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("norm_test", "Select Test:",
                   choices = c(
                     "Shapiro-Wilk Test" = "shapiro",
                     "Kolmogorov-Smirnov Test" = "ks_one",
                     "Two-Sample Kolmogorov-Smirnov Test" = "ks_two"
                   ))
               ),
               column(6,
                 uiOutput("norm_group_selectors")
               )
             ),
             br(),
             checkboxInput("show_info_norm", "Show test details", FALSE),
             uiOutput("test_info_norm"),
             actionButton("run_norm", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("norm_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: SURVIVAL & ROC
     # =========================================================================
     tabItem(tabName = "survival_roc",
       fluidRow(
         column(12,
           uiOutput("ds_banner_surv"),
           div(class = "section-header", icon("heartbeat"),
             " Survival Analysis & ROC Curves"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("surv_test", "Select Analysis:",
                   choices = c(
                     "Kaplan-Meier Survival Analysis" = "km",
                     "DeLong Test - Independent Curves" = "delong_ind",
                     "DeLong Test - Paired Curves" = "delong_paired"
                   ))
               ),
               column(6,
                 uiOutput("surv_group_selectors")
               )
             ),
             conditionalPanel(
               condition = "input.surv_test == 'km'",
               fluidRow(
                 column(6,
                   helpText("Data format: Group should contain time values.",
                     "Create a separate group for event/status (1=event, 0=censored).")
                ),
                column(3,
                  numericInput("surv_event_code", "Event Code:", value = 1)
                ),
                column(3,
                  numericInput("surv_censor_code", "Censor Code:", value = 0)
                 )
               ),
              uiOutput("surv_km_detect_hint")
             ),
             conditionalPanel(
               condition = "input.surv_test == 'delong_ind' ||
                            input.surv_test == 'delong_paired'",
               fluidRow(
                 column(12,
                   helpText("Data format: Need groups for actual class labels (0/1),",
                     "predictor scores from Model 1, and predictor scores from Model 2.")
                ),
                column(4,
                  numericInput("surv_positive_label", "Positive Class Label:", value = 1)
                 )
               ),
              uiOutput("surv_roc_detect_hint")
             ),
             br(),
             checkboxInput("show_info_surv", "Show test details", FALSE),
             uiOutput("test_info_surv"),
             actionButton("run_surv", "Run Analysis",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("surv_result"),
             plotOutput("surv_plot", height = "500px")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: MULTI-FACTOR TESTS
     # =========================================================================
     tabItem(tabName = "multifactor",
       fluidRow(
         column(12,
           uiOutput("ds_banner_multi"),
           div(class = "section-header", icon("calculator"),
             " Multi-Factor & Multivariate Tests"),
           div(class = "stat-card",
             fluidRow(
               column(6,
                 selectInput("multi_test", "Select Test:",
                   choices = c(
                     "Two Way ANOVA" = "two_anova",
                     "One Way MANOVA" = "manova"
                   ))
               ),
               column(6,
                 uiOutput("multi_group_selectors")
               )
             ),
             conditionalPanel(
               condition = "input.multi_test == 'two_anova'",
               helpText("Data format: Need columns for Factor A, Factor B,",
                 "and the response variable. Use the file upload for structured data.")
             ),
             conditionalPanel(
               condition = "input.multi_test == 'manova'",
               helpText("Data format: Need a grouping column and multiple",
                 "response variable columns. Use the file upload for structured data.")
             ),
            checkboxInput("multi_remove_missing", "Remove rows with missing values", TRUE),
             br(),
             checkboxInput("show_info_multi", "Show test details", FALSE),
             uiOutput("test_info_multi"),
             actionButton("run_multi", "Run Test",
               icon = icon("play"), class = "btn-primary"),
             hr(),
             uiOutput("multi_result")
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: RESULTS
     # =========================================================================
     tabItem(tabName = "results",
       fluidRow(
         column(12,
           uiOutput("ds_banner_results"),
           div(class = "section-header", icon("table"), " Results & Data Summary"),
           tabsetPanel(
             tabPanel("Data Summary",
               br(),
               div(class = "stat-card",
                 fluidRow(
                   column(6, h4(icon("info-circle"), " Descriptive Statistics")),
                   column(6, align = "right",
                     downloadButton("download_desc_csv", "CSV",
                       class = "btn-primary"),
                     downloadButton("download_desc_xlsx", "Excel",
                       class = "btn-success", style = "margin-left: 5px;")
                   )
                 ),
                 hr(),
                 DTOutput("desc_stats_table")
               )
             ),
             tabPanel("Test History",
               br(),
               div(class = "stat-card",
                 fluidRow(
                   column(6, h4(icon("history"), " All Test Results")),
                   column(6, align = "right",
                     downloadButton("download_results_txt", "Download as TXT",
                       class = "btn-primary"),
                     actionButton("clear_results", "Clear History",
                       icon = icon("trash"), class = "btn-danger",
                       style = "margin-left: 5px;")
                   )
                 ),
                 hr(),
                 uiOutput("all_results_display")
               )
             ),
             tabPanel("Raw Data",
               br(),
               div(class = "stat-card",
                 h4(icon("database"), " Current Dataset"),
                 DTOutput("raw_data_table"),
                 hr(),
                 downloadButton("download_data_csv", "Download as CSV",
                   class = "btn-primary")
               )
             )
           )
         )
       )
     ),
     
     # =========================================================================
     # TAB: PLOTS
     # =========================================================================
     tabItem(tabName = "plots",
       uiOutput("ds_banner_plots"),
       fluidRow(
         column(3,
          div(class = "stat-card plot-config-panel",
             h4(icon("chart-area"), " Plot Configuration"),
            p(class = "panel-subtitle",
              "Tune appearance, labels, significance, and export settings. Sections below are grouped for faster scanning."),
             
             selectInput("plot_type", "Plot Type:",
               choices = c(
                 "Box Plot" = "box",
                 "Violin Plot" = "violin",
                 "Dot Plot (Strip)" = "dot",
                 "Bee Swarm Plot" = "swarm",
                 "Bar Plot (Mean + SE)" = "bar",
                 "Bar Plot (Mean + SD)" = "bar_sd",
                 "Box + Jitter" = "box_jitter",
                 "Violin + Jitter" = "violin_jitter",
                 "Violin + Box" = "violin_box",
                 "Dot Plot (Mean + SD)" = "mean_dot",
                 "Histogram" = "histogram",
                 "Density Plot" = "density",
                 "QQ Plot" = "qq"
               )),
             
             conditionalPanel(
               condition = "input.plot_type == 'box' || input.plot_type == 'box_jitter' || input.plot_type == 'violin_box'",
               selectInput("plot_whisker_type", "Whiskers Representation:",
                 choices = c("Default (1.5 IQR)" = "default",
                             "Min / Max" = "minmax",
                             "Percentiles (5th-95th)" = "percentile",
                             "Std Deviation (Mean \u00b1 SD)" = "sd"),
                 selected = "default"),
               helpText(style = "font-size:11px; color:#888; margin-top:-8px;",
                 "Controls how far box whiskers extend.")
             ),
             
             tags$details(class = "cfg-section", open = "open",
               tags$summary("Aesthetics"),
               colourInput("plot_bg_color", "Background Color:", "#ffffff"),
               checkboxInput("plot_use_group_colors",
                 "Use different colors per group", TRUE),
               conditionalPanel(
                 condition = "input.plot_use_group_colors == false",
                 colourInput("plot_fill_color", "Fill Color:", "#3c8dbc",
                   allowTransparent = TRUE),
                 colourInput("plot_border_color", "Border/Line Color:", "#2c3e50"),
                 colourInput("plot_point_color", "Point Color:", "#e74c3c")
               ),
               conditionalPanel(
                 condition = "input.plot_use_group_colors == true",
                 selectInput("plot_palette", "Color Palette:",
                   choices = c(
                     "Default" = "default",
                     "Custom" = "custom",
                     "Black & White" = "bw_pal",
                     "Greyscale" = "grey_pal",
                     "Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                     "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                     "Dark2" = "Dark2", "Accent" = "Accent",
                     "Paired" = "Paired",
                     "npg (Nature)" = "npg",
                     "aaas (Science)" = "aaas",
                     "lancet" = "lancet",
                     "jco (JCO)" = "jco",
                     "nejm (NEJM)" = "nejm"
                   )),
                 uiOutput("palette_preview"),
                 uiOutput("custom_group_colors_ui")
               ),
               sliderInput("plot_alpha", "Fill Transparency:",
                 min = 0, max = 1, value = 0.7, step = 0.05),
               sliderInput("plot_line_width", "Line Width:",
                 min = 0.1, max = 3, value = 0.8, step = 0.1),
               sliderInput("plot_point_size", "Point Size:",
                 min = 0.5, max = 6, value = 2, step = 0.25)
             ),

             tags$details(class = "cfg-section",
               tags$summary("Layout & Labels"),
               textInput("plot_title", "Title:", ""),
               textInput("plot_xlab", "X-axis Label:", ""),
               textInput("plot_ylab", "Y-axis Label:", ""),
               helpText(
                 style = "font-size:11px; color:#607d8b; margin-top:-4px; margin-bottom:8px;",
                 "HTML markup supported: ",
                 tags$code("<b>bold</b>"), ", ",
                 tags$code("<i>italic</i>"), ", ",
                 tags$code("<sup>sup</sup>"), ", ",
                 tags$code("<sub>sub</sub>")
               ),
               sliderInput("plot_title_size", "Title Font Size:",
                 min = 8, max = 30, value = 16, step = 1),
               sliderInput("plot_axis_title_size", "Axis Title Font Size:",
                 min = 8, max = 24, value = 14, step = 1),
               sliderInput("plot_axis_text_size", "Axis Text Font Size:",
                 min = 6, max = 20, value = 12, step = 1),
               sliderInput("plot_legend_size", "Legend Font Size:",
                 min = 6, max = 20, value = 11, step = 1),
               checkboxInput("plot_title_bold", "Bold Title", TRUE),
               checkboxInput("plot_title_italic", "Italic Title", FALSE),
               checkboxInput("plot_axis_title_bold", "Bold Axis Titles", FALSE),
               checkboxInput("plot_axis_title_italic", "Italic Axis Titles", FALSE),
               selectInput("plot_theme", "Theme:",
                 choices = c(
                   "Classic" = "classic",
                   "Minimal" = "minimal",
                   "BW" = "bw",
                   "Light" = "light",
                   "Publication (theme_pubr)" = "pubr",
                   "void" = "void"
                 ), selected = "classic"),
               checkboxInput("plot_show_legend", "Show Legend", TRUE),
               checkboxInput("plot_coord_flip", "Flip Coordinates", FALSE),
               checkboxInput("plot_underline_title", "Underline Title", FALSE),
               uiOutput("plot_group_labels_ui")
             ),

             tags$details(class = "cfg-section",
               tags$summary("Error Bars & Overlays"),
               checkboxInput("plot_show_mean", "Show Mean Point", FALSE),
               checkboxInput("plot_show_errorbar", "Show Error Bars", FALSE),
               conditionalPanel(
                 condition = "input.plot_show_errorbar == true",
                 selectInput("plot_errorbar_type", "Error Bar Type:",
                   choices = c("SE" = "se", "SD" = "sd",
                               "95% CI" = "ci", "IQR" = "iqr")),
                 sliderInput("plot_errorbar_width", "Error Bar Width:",
                   min = 0, max = 0.8, value = 0.2, step = 0.05)
               ),
               checkboxInput("plot_show_jitter", "Overlay Jitter Points", FALSE),
               conditionalPanel(
                 condition = "input.plot_show_jitter == true",
                 sliderInput("plot_jitter_width", "Jitter Width:",
                   min = 0, max = 0.5, value = 0.15, step = 0.01),
                 sliderInput("plot_jitter_alpha", "Jitter Alpha:",
                   min = 0, max = 1, value = 0.5, step = 0.05)
               )
             ),

             tags$details(class = "cfg-section",
               tags$summary("Significance"),
               checkboxInput("plot_show_signif", "Show Significance Bars", FALSE),
               conditionalPanel(
                 condition = "input.plot_show_signif == true",
                 selectInput("plot_signif_method", "Comparison Method:",
                   choices = c(
                     "--- Parametric (2 groups) ---" = "header_param2",
                     "Student's t-test (pooled)" = "t.test.pooled",
                     "Welch's t-test" = "t.test",
                     "--- Non-Parametric (2 groups) ---" = "header_np2",
                     "Mann-Whitney U Test" = "wilcox.test",
                     "--- Parametric (3+ groups) ---" = "header_param3",
                     "One-Way ANOVA" = "anova",
                     "--- Non-Parametric (3+ groups) ---" = "header_np3",
                     "Kruskal-Wallis Test" = "kruskal.test"
                   ), selected = "t.test", selectize = FALSE),
                 selectInput("plot_signif_label", "Significance Label:",
                   choices = c("Stars (*, **, ***)" = "p.signif",
                               "Exact p-value" = "p.format"),
                   selected = "p.signif"),
                 sliderInput("plot_signif_step", "Bar Step Increase:",
                   min = 0, max = 0.15, value = 0.05, step = 0.01),
                 sliderInput("plot_signif_text_size", "Significance Text Size:",
                   min = 2, max = 8, value = 3.5, step = 0.5)
               )
             ),

             tags$details(class = "cfg-section",
               tags$summary("Spacing & Export"),
               sliderInput("plot_width_adj", "Group Width (dodge):",
                 min = 0.2, max = 1.2, value = 0.75, step = 0.05),
               numericInput("plot_download_width", "Download Width (cm):",
                 value = 20, min = 5, max = 50, step = 1),
               numericInput("plot_download_height", "Download Height (cm):",
                 value = 15, min = 5, max = 50, step = 1),
               numericInput("plot_download_dpi", "Download DPI:",
                 value = 300, min = 72, max = 600, step = 50)
             )
           )
         ),
         column(9,
           div(class = "stat-card",
             fluidRow(
               column(6, h4(icon("image"), " Plot Output")),
               column(6, align = "right",
                 downloadButton("download_plot_png", "PNG",
                   class = "btn-primary"),
                 downloadButton("download_plot_svg", "SVG",
                   class = "btn-primary", style = "margin-left: 5px;")
               )
             ),
             hr(),
             div(id = "main_plot_wrapper",
               style = "width: 100%; min-height: 400px; position: relative;",
               plotOutput("main_plot", height = "576px", width = "100%")
             ),
             hr(),
             verbatimTextOutput("plot_summary_text")
           )
         )
       )
     )
     ,
     # =========================================================================
     # TAB: ABOUT
     # =========================================================================
     tabItem(tabName = "about",
       fluidRow(
         column(8, offset = 2,
           div(class = "section-header", icon("info-circle"), " About ShinyStatR"),
           div(class = "stat-card",
             h3(icon("chart-bar"), " ShinyStatR",
               tags$small(" v1.1.0", style = "color: #95a5a6;")),
             p(style = "font-size: 15px; line-height: 1.7;",
               "ShinyStatR is a comprehensive, web-based statistical analysis tool built ",
               "with R and Shiny. It provides an intuitive interface for performing a wide ",
               "range of statistical tests, generating publication-ready plots, and exploring ",
               "data — all without writing a single line of code."
             ),
             hr(),
             h4(icon("flask"), " Key Features"),
             tags$ul(style = "font-size: 14px; line-height: 1.8;",
               tags$li(strong("26 statistical tests"), " covering parametric, non-parametric, ",
                 "paired, one-sample, proportion, variance, normality, survival/ROC, and multi-factor analyses"),
               tags$li(strong("Flexible data input"), " — manual entry or file upload (CSV, Excel, TXT)"),
               tags$li(strong("Exact p-values"), " for Mann-Whitney U and Wilcoxon Signed Rank tests ",
                 "using the ", code("coin"), " package, matching SPSS/Prism output"),
               tags$li(strong("13 customisable plot types"), " with full control over colours, fonts, ",
                 "themes, error bars, significance annotations, and custom per-group colours"),
               tags$li(strong("Publication-ready exports"), " — download plots as high-resolution PNG or SVG, ",
                 "and descriptive statistics as CSV or Excel")
             ),
             hr(),
             h4(icon("cogs"), " R Packages Used"),
             p(style = "font-size: 13px; line-height: 1.6;",
               "This application relies on the following open-source R packages:"),
             fluidRow(
               column(4,
                 tags$ul(style = "font-size: 13px;",
                   tags$li(code("shiny"), " / ", code("shinydashboard")),
                   tags$li(code("shinydashboardPlus")),
                   tags$li(code("shinyWidgets")),
                   tags$li(code("DT")),
                   tags$li(code("ggplot2")),
                   tags$li(code("ggpubr")),
                   tags$li(code("ggsignif")),
                   tags$li(code("ggbeeswarm")),
                   tags$li(code("colourpicker"))
                 )
               ),
               column(4,
                 tags$ul(style = "font-size: 13px;",
                   tags$li(code("dplyr"), " / ", code("tidyr")),
                   tags$li(code("purrr"), " / ", code("tibble")),
                   tags$li(code("readxl"), " / ", code("writexl")),
                   tags$li(code("car")),
                   tags$li(code("coin")),
                   tags$li(code("nortest")),
                   tags$li(code("multcomp")),
                   tags$li(code("broom"))
                 )
               ),
               column(4,
                 tags$ul(style = "font-size: 13px;",
                   tags$li(code("survival")),
                   tags$li(code("survminer")),
                   tags$li(code("pROC")),
                   tags$li(code("svglite")),
                   tags$li(code("RColorBrewer")),
                   tags$li(code("cowplot")),
                   tags$li(code("scales")),
                   tags$li(code("ggtext"))
                 )
               )
             ),
             hr(),
             h4(icon("user"), " Developer"),
             p(style = "font-size: 14px; line-height: 1.8;",
               strong("Dinuka Adasooriya"), br(),
               "Department of Oral Biology", br(),
               "Yonsei University College of Dentistry", br(),
               "Seoul, Republic of Korea", br(),
               icon("envelope"), " ",
               tags$a(href = "mailto:dinuka90@yuhs.ac", "dinuka90@yuhs.ac")
             ),
             hr(),
             h4(icon("link"), " Links"),
             tags$ul(style = "font-size: 14px; line-height: 2;",
               tags$li(
                 icon("github"), " ",
                 strong("Source Code: "),
                 tags$a(href = "https://github.com/Dinuka0001/ShinyStatR",
                        target = "_blank",
                        "github.com/Dinuka0001/ShinyStatR"),
                 " — View the source code, report issues, or contribute"
               ),
               tags$li(
                 icon("globe"), " ",
                 strong("Live App: "),
                 tags$a(href = "https://dinuka-shinystatr.share.connect.posit.cloud/",
                        target = "_blank",
                        "dinuka-shinystatr.share.connect.posit.cloud"),
                 " — Try ShinyStatR online without installation"
               )
             ),
             hr(),
             div(style = "text-align: center; color: #95a5a6; font-size: 12px;",
               p("Built with ", icon("heart", style = "color: #e74c3c;"),
                 " using R and Shiny"),
               p(paste0("\u00A9 ", format(Sys.Date(), "%Y"),
                 " Dinuka Adasooriya. All rights reserved."))
             )
           )
         )
       )
     ),
     # =========================================================================
     # TAB: MULTI-PARAMETER COMPARISON
     # =========================================================================
     tabItem(tabName = "multi_param",
       fluidRow(
         column(12,
           div(class = "section-header", icon("layer-group"), " Multi-Parameter Group Comparison")
         )
       ),
       fluidRow(
         # --- Left: Data Input & Options ---
         column(4,
           div(class = "stat-card",
             h4(icon("database"), " Step 1: Load Data"),
             tabsetPanel(id = "mp_input_mode", type = "tabs",
               tabPanel("Paste Data", icon = icon("keyboard"),
                 br(),
                 helpText("Paste your data (any column order). Use the separator matching your data.",
                          "The first row should be a header."),
                 fluidRow(
                   column(6,
                     selectInput("mp_paste_sep", "Separator:",
                       choices = c("Tab" = "\t", "Comma" = ",", "Semicolon" = ";", "Space" = " "),
                       selected = "\t")
                   ),
                   column(6,
                     checkboxInput("mp_paste_header", "First row is header", TRUE)
                   )
                 ),
                textAreaInput("mp_raw_text", label = NULL,
                  rows = 10,
                  width = "100%",
                  value = "",
                  placeholder = "Paste data here...\nSample\tGroup\tParam1\tParam2\nS1\tCtrl\t1.2\t3.4\nS2\tTreat\t2.1\t4.5"
                ),
                 br(),
                 actionButton("mp_load", "Load Pasted Data", icon = icon("check-circle"),
                   class = "btn-primary")
               ),
               tabPanel("Upload File", icon = icon("file-upload"),
                 br(),
                 fileInput("mp_file_input", "Choose File:",
                   accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),
                 helpText("Supported: CSV, TXT, TSV, Excel. Any column arrangement."),
                 fluidRow(
                   column(6,
                     selectInput("mp_file_sep", "Separator (text files):",
                       choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";"),
                       selected = ",")
                   ),
                   column(6,
                     checkboxInput("mp_file_header", "File has header", TRUE)
                   )
                 ),
                 selectInput("mp_file_sheet", "Excel Sheet:", choices = NULL),
                 actionButton("mp_load_file", "Load File Data", icon = icon("upload"),
                   class = "btn-primary")
               )
             ),
             div(style = "margin-top:10px;",
               actionButton("mp_clear", "Clear All Data", icon = icon("trash"),
                 class = "btn-danger btn-sm")
             )
           ),
           # --- Step 2: Column Configuration ---
           uiOutput("mp_col_config_ui"),
           # --- Data Summary & Preview ---
           div(class = "stat-card",
             uiOutput("mp_data_summary"),
             DTOutput("mp_data_preview")
           ),
           div(class = "stat-card mp-options-panel",
             h4(icon("sliders-h"), " Plot Options"),
             p(class = "panel-subtitle",
               "Configure multi-parameter visual style, significance testing, and download dimensions."),
             tags$details(class = "cfg-section", open = "open",
               tags$summary("Data mapping & plot type"),
               uiOutput("mp_param_selector_ui"),
               uiOutput("mp_group_selector_ui"),
               uiOutput("mp_group_order_ui"),
               radioButtons("mp_view_mode", "Plot Layout:",
                 choices = c("Separate plots (one per parameter)" = "separate",
                             "Single combined plot (all parameters)" = "combined",
                             "Grouped single plot (parameters on x-axis)" = "grouped"),
                 selected = "separate", inline = FALSE),
               selectInput("mp_plot_type", "Plot Type:",
                 choices = c(
                   "Box Plot" = "box",
                   "Violin Plot" = "violin",
                   "Dot Plot (Strip)" = "dot",
                   "Bee Swarm Plot" = "swarm",
                   "Bar Plot (Mean + SE)" = "bar",
                   "Bar Plot (Mean + SD)" = "bar_sd",
                   "Box + Jitter" = "box_jitter",
                   "Violin + Jitter" = "violin_jitter",
                   "Violin + Box" = "violin_box",
                   "Dot Plot (Mean + SD)" = "mean_dot"
                 ), selected = "box_jitter"),
               conditionalPanel(
                 condition = "input.mp_plot_type == 'box' || input.mp_plot_type == 'box_jitter' || input.mp_plot_type == 'violin_box'",
                 selectInput("mp_whisker_type", "Whiskers Representation:",
                   choices = c("Default (1.5 IQR)" = "default",
                               "Min / Max" = "minmax",
                               "Percentiles (5th-95th)" = "percentile",
                               "Std Deviation (Mean \u00b1 SD)" = "sd"),
                   selected = "default"),
                 helpText(style = "font-size:11px; color:#888; margin-top:-8px;",
                   "Controls how far box whiskers extend.")
               ),
               checkboxInput("mp_custom_ylim", "Set custom Y-axis limits", FALSE),
               conditionalPanel(
                 condition = "input.mp_custom_ylim == true",
                 helpText("Applied per-parameter in separate mode, or to the full combined plot."),
                 fluidRow(
                   column(6, numericInput("mp_ymin", "Y Min:", value = NA)),
                   column(6, numericInput("mp_ymax", "Y Max:", value = NA))
                 )
               ),
               conditionalPanel(
                 condition = "input.mp_view_mode == 'combined' || input.mp_view_mode == 'grouped'",
                 checkboxInput("mp_free_y", "Free Y-axis scales per parameter", TRUE)
               )
             ),

             tags$details(class = "cfg-section", open = "open",
               tags$summary("Aesthetics"),
               colourInput("mp_bg_color", "Background Color:", "#ffffff"),
               sliderInput("mp_alpha", "Fill Transparency:", min = 0, max = 1, value = 0.7, step = 0.05),
               sliderInput("mp_line_width", "Line Width:",
                 min = 0.1, max = 3, value = 0.8, step = 0.1),
               sliderInput("mp_point_size", "Point Size:", min = 0.5, max = 6, value = 2, step = 0.25),
               selectInput("mp_palette", "Color Palette:",
                 choices = c(
                   "Default" = "default",
                   "Custom" = "custom",
                   "Black & White" = "bw_pal",
                   "Greyscale" = "grey_pal",
                   "Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                   "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                   "Dark2" = "Dark2", "Accent" = "Accent",
                   "Paired" = "Paired",
                   "npg (Nature)" = "npg", "aaas (Science)" = "aaas",
                   "lancet" = "lancet", "jco (JCO)" = "jco",
                   "nejm (NEJM)" = "nejm"
                 ), selected = "default"),
               uiOutput("mp_palette_preview"),
               uiOutput("mp_custom_group_colors_ui")
             ),

             tags$details(class = "cfg-section",
               tags$summary("Layout & Labels"),
               textInput("mp_xlab", "X-axis Label:", ""),
               textInput("mp_ylab", "Y-axis Label:", ""),
               helpText(
                 style = "font-size:11px; color:#607d8b; margin-top:-4px; margin-bottom:8px;",
                 "HTML markup supported: ",
                 tags$code("<b>bold</b>"), ", ",
                 tags$code("<i>italic</i>"), ", ",
                 tags$code("<sup>sup</sup>"), ", ",
                 tags$code("<sub>sub</sub>")
               ),
               sliderInput("mp_title_size", "Title Font Size:",
                 min = 8, max = 28, value = 16, step = 1),
               checkboxInput("mp_title_bold", "Bold Title", FALSE),
               checkboxInput("mp_title_italic", "Italic Title", FALSE),
               checkboxInput("mp_underline_title", "Underline Title", FALSE),
               sliderInput("mp_axis_title_size", "Axis Title Font Size:",
                 min = 8, max = 24, value = 14, step = 1),
               sliderInput("mp_axis_text_size", "Axis Text Font Size:",
                 min = 6, max = 20, value = 12, step = 1),
               sliderInput("mp_legend_size", "Legend Font Size:",
                 min = 6, max = 20, value = 11, step = 1),
               checkboxInput("mp_axis_title_bold", "Bold Axis Titles", FALSE),
               checkboxInput("mp_axis_title_italic", "Italic Axis Titles", FALSE),
               selectInput("mp_theme", "Theme:",
                 choices = c(
                   "Classic" = "classic",
                   "Minimal" = "minimal",
                   "BW" = "bw",
                   "Light" = "light",
                   "Publication (theme_pubr)" = "pubr",
                   "void" = "void"
                 ), selected = "classic"),
               checkboxInput("mp_show_legend", "Show Legend", TRUE),
               checkboxInput("mp_coord_flip", "Flip Coordinates", FALSE),
               uiOutput("mp_group_labels_ui")
             ),

             tags$details(class = "cfg-section",
               tags$summary("Per-Parameter Labels"),
               helpText(style = "font-size:11px; color:#607d8b; margin-bottom:6px;",
                 "Set custom title and Y-axis label for each parameter.",
                 "Select a parameter, edit labels, then click Apply.",
                 "HTML markup is supported."),
               uiOutput("mp_param_label_selector_ui"),
               textInput("mp_pp_title", "Parameter Title:", ""),
               textInput("mp_pp_ylab", "Parameter Y-axis Label:", ""),
               actionButton("mp_pp_apply", "Apply",
                 icon = icon("check"), class = "btn-primary btn-sm"),
               actionButton("mp_pp_clear_all", "Clear All",
                 icon = icon("eraser"), class = "btn-default btn-sm",
                 style = "margin-left:5px;"),
               uiOutput("mp_pp_status")
             ),

             tags$details(class = "cfg-section",
               tags$summary("Error Bars & Overlays"),
               checkboxInput("mp_show_mean", "Show Mean Point", FALSE),
               checkboxInput("mp_show_errorbar", "Show Error Bars", FALSE),
               conditionalPanel(
                 condition = "input.mp_show_errorbar == true",
                 selectInput("mp_errorbar_type", "Error Bar Type:",
                   choices = c("SE" = "se", "SD" = "sd",
                               "95% CI" = "ci", "IQR" = "iqr")),
                 sliderInput("mp_errorbar_width", "Error Bar Width:",
                   min = 0, max = 0.8, value = 0.2, step = 0.05)
               ),
               checkboxInput("mp_show_jitter", "Overlay Jitter Points", FALSE),
               conditionalPanel(
                 condition = "input.mp_show_jitter == true",
                 sliderInput("mp_jitter_width", "Jitter Width:",
                   min = 0, max = 0.5, value = 0.15, step = 0.01),
                 sliderInput("mp_jitter_alpha", "Jitter Alpha:",
                   min = 0, max = 1, value = 0.5, step = 0.05)
               )
             ),

             tags$details(class = "cfg-section",
               tags$summary("Significance"),
               selectInput("mp_test_method", "Comparison Method:",
                 choices = c(
                   "--- Parametric (2 groups) ---" = "header_param2",
                   "Student's t-test (pooled)" = "t.test",
                   "Welch's t-test" = "welch",
                   "Two Sample Z-Test" = "two_z",
                   "--- Non-Parametric (2 groups) ---" = "header_np2",
                   "Mann-Whitney U Test" = "wilcox.test",
                   "Kolmogorov-Smirnov (two-sample)" = "ks_two",
                   "--- Paired (2 groups) ---" = "header_paired",
                   "Paired t-test" = "paired_t",
                   "Wilcoxon Signed-Rank Test" = "wilcoxon_sr",
                   "--- Parametric (3+ groups) ---" = "header_param3",
                   "One-Way ANOVA" = "anova",
                   "--- Non-Parametric (3+ groups) ---" = "header_np3",
                   "Kruskal-Wallis Test" = "kruskal",
                   "--- Variance Tests ---" = "header_var",
                   "F-Test for Variances" = "f_test",
                   "Levene's Test" = "levene",
                   "--- Normality Tests ---" = "header_norm",
                   "Shapiro-Wilk Test" = "shapiro",
                   "--- Other ---" = "header_other",
                   "None" = "none"
                 ), selected = "t.test", selectize = FALSE),
               conditionalPanel(
                 condition = "input.mp_test_method == 'two_z'",
                 numericInput("mp_known_sigma", "Known Sigma:", value = 1, min = 0.001)
               ),
               conditionalPanel(
                 condition = "input.mp_test_method == 'two_z'",
                 numericInput("mp_known_sigma2", "Known Sigma (Group 2):", value = 1, min = 0.001)
               ),
               checkboxInput("mp_show_signif", "Show significance on plots", TRUE),
               conditionalPanel(
                 condition = "input.mp_show_signif == true",
                 selectInput("mp_signif_style", "Significance Display Style:",
                   choices = c("Text annotation" = "text",
                               "Bracket bars" = "bars"),
                   selected = "text"),
                 selectInput("mp_signif_label", "Significance Label:",
                   choices = c("Stars (*, **, ***)" = "p.signif",
                               "Exact p-value" = "p.format"), selected = "p.signif"),
                 sliderInput("mp_signif_step", "Bar Step Increase:",
                   min = 0, max = 0.15, value = 0.05, step = 0.01),
                 sliderInput("mp_signif_text_size", "Significance Text Size:",
                   min = 2, max = 8, value = 3.5, step = 0.5)
               )
             ),

             tags$details(class = "cfg-section",
               tags$summary("Spacing & Export"),
               sliderInput("mp_width_adj", "Group Width (dodge):",
                 min = 0.2, max = 1.2, value = 0.75, step = 0.05),
               numericInput("mp_dl_width", "Download Width (cm):",
                 value = 18, min = 5, max = 50, step = 1),
               numericInput("mp_dl_height", "Download Height (cm):",
                 value = 14, min = 5, max = 50, step = 1),
               numericInput("mp_dl_dpi", "Download DPI:",
                 value = 300, min = 72, max = 600, step = 50)
             ),

             downloadButton("mp_download_png", "PNG", class = "btn-primary"),
             downloadButton("mp_download_svg", "SVG", class = "btn-primary",
               style = "margin-left:5px;")
           )
         ),
         # --- Right: Plots & Results ---
         column(8,
           div(class = "stat-card",
             h4(icon("chart-bar"), " Comparison Plots"),
             uiOutput("mp_plots_ui")
           ),
           div(class = "stat-card",
             fluidRow(
               column(6, h4(icon("table"), " Statistical Results")),
               column(6, align = "right", style = "padding-top: 10px;",
                 downloadButton("mp_dl_stats_csv", "CSV",
                   class = "btn-primary btn-sm"),
                 downloadButton("mp_dl_stats_xlsx", "Excel",
                   class = "btn-success btn-sm", style = "margin-left: 5px;")
               )
             ),
             DTOutput("mp_stats_table"),
             hr(),
             fluidRow(
               column(6, h4(icon("list"), " Descriptive Statistics")),
               column(6, align = "right", style = "padding-top: 10px;",
                 downloadButton("mp_dl_desc_csv", "CSV",
                   class = "btn-primary btn-sm"),
                 downloadButton("mp_dl_desc_xlsx", "Excel",
                   class = "btn-success btn-sm", style = "margin-left: 5px;")
               )
             ),
             DTOutput("mp_desc_table")
           )
         )
       )
     )
   ) # end tabItems
 ) # end body
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
 
 # --- Reactive values --------------------------------------------------------
 rv <- reactiveValues(
   data = NULL,            # named list of numeric vectors
   group_names = NULL,     # character vector of group names
   results_history = list(), # list of result strings
   raw_uploaded = NULL,    # raw uploaded data frame
   current_result = NULL,  # most recent test result text
  data_source = NULL,     # "manual" or "file"
  data_source_detail = NULL, # extra info (e.g. filename)
  mp_data_source = NULL,     # "paste" or "file" for multi-parameter loader
  mp_data_detail = NULL,     # descriptive detail (e.g. filename)
  mp_configured = FALSE,     # TRUE after column config is successfully applied
  mp_param_labels = list()   # per-parameter labels: list(param = list(title=, ylab=))
 )
 
 # --- Sidebar data source indicator ------------------------------------------
 output$sidebar_data_source <- renderUI({
   src <- rv$data_source
  mp_src <- rv$mp_data_source

  main_label <- "No data"
  main_detail <- "Use Data Input"
  main_icon <- icon("exclamation-triangle")
  main_badge <- span(class = "status-pill warn", "None")

  if (!is.null(src) && src == "manual") {
    main_label <- "Manual active"
    main_detail <- if (!is.null(rv$data_source_detail) && nzchar(rv$data_source_detail)) {
      rv$data_source_detail
    } else {
      "Main data source"
    }
    main_icon <- icon("keyboard")
    main_badge <- span(class = "status-pill ok", "Ready")
  } else if (!is.null(src) && src == "file") {
    main_label <- "File active"
    main_detail <- if (!is.null(rv$data_source_detail) && nzchar(rv$data_source_detail)) {
      rv$data_source_detail
    } else {
      "Main data source"
    }
    main_icon <- icon("file")
    main_badge <- span(class = "status-pill ok", "Ready")
  }

  mp_label <- "No multi-parameter"
  mp_detail <- "Load in Multi-Parameter tab"
  mp_icon <- icon("layer-group")
  mp_badge <- span(class = "status-pill warn", "Empty")

  if (!is.null(mp_src)) {
    mode_text <- if (mp_src == "paste") "Paste" else "File"
    state_text <- if (isTRUE(rv$mp_configured)) "Ready" else "Raw"
    mp_label <- paste0(mode_text, " (", state_text, ")")
    mp_detail <- if (!is.null(rv$mp_data_detail) && nzchar(rv$mp_data_detail)) {
      rv$mp_data_detail
    } else {
      "Multi-Parameter module"
    }
    mp_icon <- if (mp_src == "paste") icon("keyboard") else icon("file")
    mp_badge <- if (isTRUE(rv$mp_configured)) {
      span(class = "status-pill ok", "Ready")
    } else {
      span(class = "status-pill warn", "Pending")
    }
  }

  div(class = "sidebar-status-panel",
    div(class = "sidebar-status-title", "Data Status"),
    div(class = "sidebar-status-row",
      div(class = "sidebar-status-main",
        main_icon, " Main: ", main_label,
        span(class = "sidebar-status-detail", main_detail)
      ),
      main_badge
    ),
    div(class = "sidebar-status-row",
      div(class = "sidebar-status-main",
        mp_icon, " Multi: ", mp_label,
        span(class = "sidebar-status-detail", mp_detail)
      ),
      mp_badge
    )
  )
 })
 
 # --- Inline data source banner (reusable) -----------------------------------
 # --- Data source banner helper -----------------------------------------------
 make_data_source_banner <- function() {
   src <- rv$data_source
   if (is.null(src)) {
     div(class = "data-source-banner source-none",
       icon("exclamation-triangle"),
       " No data loaded. Go to ", strong("Data Input"),
       " to paste data or upload a file first."
     )
   } else if (src == "manual") {
     n_groups <- if (!is.null(rv$data)) length(rv$data) else 0
     n_obs <- if (!is.null(rv$data)) sum(sapply(rv$data, length)) else 0
     div(class = "data-source-banner source-manual",
       icon("keyboard"),
       paste0(" Analysing: Manual entry data (",
         n_groups, " groups, ", n_obs, " total observations)")
     )
   } else if (src == "file") {
     n_groups <- if (!is.null(rv$data)) length(rv$data) else 0
     n_obs <- if (!is.null(rv$data)) sum(sapply(rv$data, length)) else 0
     div(class = "data-source-banner source-file",
       icon("file"),
       paste0(" Analysing: File data (",
         n_groups, " groups, ", n_obs, " total observations)"),
       br(), tags$small(rv$data_source_detail)
     )
   }
 }
 
 # Render banners for all tabs that need them
 output$data_source_banner <- renderUI({ make_data_source_banner() })
 output$ds_banner_param     <- renderUI({ make_data_source_banner() })
 output$ds_banner_nonparam  <- renderUI({ make_data_source_banner() })
 output$ds_banner_paired    <- renderUI({ make_data_source_banner() })
 output$ds_banner_onesample <- renderUI({ make_data_source_banner() })
 output$ds_banner_prop      <- renderUI({ make_data_source_banner() })
 output$ds_banner_var       <- renderUI({ make_data_source_banner() })
 output$ds_banner_norm      <- renderUI({ make_data_source_banner() })
 output$ds_banner_surv      <- renderUI({ make_data_source_banner() })
 output$ds_banner_multi     <- renderUI({ make_data_source_banner() })
 output$ds_banner_results   <- renderUI({ make_data_source_banner() })
 output$ds_banner_plots     <- renderUI({ make_data_source_banner() })
 
 # ===========================================================================
 # DATA INPUT - Dynamic group fields
 # ===========================================================================
 output$group_inputs <- renderUI({
   n <- input$num_groups
   req(n)
   
   lapply(seq_len(n), function(i) {
     fluidRow(
       column(3,
         div(class = "group-name-input",
           textInput(paste0("group_name_", i),
             label = NULL,
             value = paste0("Group ", i),
             placeholder = paste0("Group ", i, " name"))
         )
       ),
       column(9,
         div(class = "data-input-area",
           textAreaInput(paste0("group_data_", i),
             label = NULL,
             placeholder = paste0("Paste data for Group ", i,
               " (one value per line, or use selected separator)"),
             rows = 4, resize = "vertical")
         )
       )
     )
   })
 })
 
 # --- Parse manual data ------------------------------------------------------
 parse_group_data <- function(text, sep) {
   if (is.null(text) || trimws(text) == "") return(numeric(0))
   vals <- unlist(strsplit(text, sep, fixed = (sep != "\n")))
   vals <- trimws(vals)
   vals <- vals[vals != ""]
   suppressWarnings(as.numeric(vals))
 }
 
 observeEvent(input$load_manual_data, {
   n <- input$num_groups
   sep <- input$data_separator
   data_list <- list()
   names_vec <- character(0)
   
   for (i in seq_len(n)) {
     gname <- input[[paste0("group_name_", i)]]
     gdata <- input[[paste0("group_data_", i)]]
     if (is.null(gname) || gname == "") gname <- paste0("Group ", i)
     parsed <- parse_group_data(gdata, sep)
     parsed <- parsed[!is.na(parsed)]
     if (length(parsed) > 0) {
       data_list[[gname]] <- parsed
       names_vec <- c(names_vec, gname)
     }
   }
   
   if (length(data_list) == 0) {
     showNotification("No valid numeric data found.", type = "error")
     return()
   }
   
   rv$data <- data_list
   rv$group_names <- names_vec
   rv$data_source <- "manual"
   rv$data_source_detail <- paste0(length(data_list), " group(s) entered manually")
   showNotification(
     paste0("\u2705 Loaded ", length(data_list),
       " group(s) from manual entry. This is now the active dataset."),
     type = "message", duration = 5
   )
 })
 
 # Clear data handler (shared logic)
 clear_all_data_fn <- function() {
   rv$data <- NULL
   rv$group_names <- NULL
   rv$raw_uploaded <- NULL
   rv$data_source <- NULL
   rv$data_source_detail <- NULL
   n <- input$num_groups
   if (!is.null(n)) {
     for (i in seq_len(n)) {
       updateTextInput(session, paste0("group_name_", i),
         value = paste0("Group ", i))
       updateTextAreaInput(session, paste0("group_data_", i),
         value = "")
     }
   }
   showNotification("All data cleared.", type = "warning")
 }
 
 observeEvent(input$clear_all_data, { clear_all_data_fn() })
 observeEvent(input$clear_all_data2, { clear_all_data_fn() })
 
 # --- Manual data preview table ------------------------------------------------
 output$manual_data_preview <- renderDT({
   req(rv$data)
   max_len <- max(sapply(rv$data, length))
   df <- as.data.frame(lapply(rv$data, function(x) {
     c(x, rep(NA, max_len - length(x)))
   }))
   datatable(df, options = list(
     pageLength = 15, scrollX = TRUE, dom = 'frtip'),
     rownames = FALSE) %>%
     formatRound(columns = names(df), digits = 4)
 })
 
 # ===========================================================================
 # FILE UPLOAD
 # ===========================================================================
 observeEvent(input$file_upload_input, {
   req(input$file_upload_input)
   file <- input$file_upload_input
   ext <- tools::file_ext(file$name)
   
   tryCatch({
     if (ext %in% c("xls", "xlsx")) {
       sheets <- readxl::excel_sheets(file$datapath)
       updateSelectInput(session, "file_sheet", choices = sheets)
       df <- readxl::read_excel(file$datapath, sheet = 1)
     } else {
       df <- read.csv(file$datapath, 
         sep = input$file_sep, header = input$file_header,
         stringsAsFactors = FALSE)
       updateSelectInput(session, "file_sheet", choices = NULL)
     }
     
     rv$raw_uploaded <- df
     updateSelectInput(session, "file_group_col", choices = names(df))
     updateSelectInput(session, "file_value_col", choices = names(df))
     
     showNotification("File loaded successfully!", type = "message")
   }, error = function(e) {
     showNotification(paste("Error reading file:", e$message), type = "error")
   })
 })
 
 observeEvent(input$file_sheet, {
   req(input$file_upload_input, input$file_sheet)
   file <- input$file_upload_input
   ext <- tools::file_ext(file$name)
   if (ext %in% c("xls", "xlsx")) {
     tryCatch({
       df <- readxl::read_excel(file$datapath, sheet = input$file_sheet)
       rv$raw_uploaded <- df
       updateSelectInput(session, "file_group_col", choices = names(df))
       updateSelectInput(session, "file_value_col", choices = names(df))
     }, error = function(e) {
       showNotification(paste("Error:", e$message), type = "error")
     })
   }
 })
 
 output$file_data_preview <- renderDT({
   req(rv$raw_uploaded)
   datatable(rv$raw_uploaded, options = list(
     pageLength = 15, scrollX = TRUE, dom = 'frtip'),
     rownames = FALSE)
 })
 
 observeEvent(input$load_file_data, {
   req(rv$raw_uploaded)
   df <- rv$raw_uploaded
   
   tryCatch({
     if (input$file_data_format == "wide") {
       data_list <- list()
       names_vec <- character(0)
       for (col in names(df)) {
         vals <- suppressWarnings(as.numeric(df[[col]]))
         vals <- vals[!is.na(vals)]
         if (length(vals) > 0) {
           data_list[[col]] <- vals
           names_vec <- c(names_vec, col)
         }
       }
     } else {
       req(input$file_group_col, input$file_value_col)
       gcol <- input$file_group_col
       vcol <- input$file_value_col
       groups <- unique(df[[gcol]])
      groups <- groups[!is.na(groups) & trimws(as.character(groups)) != ""]
       data_list <- list()
       names_vec <- character(0)
       for (g in groups) {
        group_name <- trimws(as.character(g))
         vals <- suppressWarnings(
           as.numeric(df[[vcol]][df[[gcol]] == g]))
         vals <- vals[!is.na(vals)]
         if (length(vals) > 0) {
          data_list[[group_name]] <- vals
          names_vec <- c(names_vec, group_name)
         }
       }
     }
     
     if (length(data_list) == 0) {
       showNotification("No valid numeric data found in file.", type = "error")
       return()
     }
     
     rv$data <- data_list
     rv$group_names <- names_vec
     rv$data_source <- "file"
     rv$data_source_detail <- paste0(
       length(data_list), " group(s) from file: ",
       input$file_upload_input$name)
     showNotification(
       paste0("\u2705 Loaded ", length(data_list),
         " group(s) from file. This is now the active dataset.",
         " Any previously entered manual data is no longer active."),
       type = "message", duration = 5
     )
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
   })
 })
 
 # ===========================================================================
 # HELPER: Dynamic group selectors for tests
 # ===========================================================================
 get_group_choices <- reactive({
   req(rv$group_names)
   setNames(rv$group_names, rv$group_names)
 })
 
 # Parametric group selectors
 output$param_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$parametric_test
   
   if (test %in% c("one_t", "one_z")) {
     selectInput("param_group1", "Select Group:", choices = gc)
   } else if (test %in% c("two_t_pooled", "two_t_welch", "two_z")) {
     tagList(
       selectInput("param_group1", "Group 1:", choices = gc,
         selected = gc[1]),
       selectInput("param_group2", "Group 2:", choices = gc,
         selected = if (length(gc) > 1) gc[2] else gc[1])
     )
   } else {
     checkboxGroupInput("param_groups_multi", "Select Groups:",
       choices = gc, selected = gc)
   }
 })
 
 # Non-parametric group selectors
 output$nonparam_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$nonparam_test
   
   if (test == "mann_whitney") {
     fluidRow(
       column(6, selectInput("np_group1", "Group 1:", choices = gc,
         selected = gc[1])),
       column(6, selectInput("np_group2", "Group 2:", choices = gc,
         selected = if (length(gc) > 1) gc[2] else gc[1]))
     )
   } else {
     checkboxGroupInput("np_groups_multi", "Select Groups:",
       choices = gc, selected = gc)
   }
 })
 
 # Paired group selectors
 output$paired_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$paired_test
   
   if (test %in% c("paired_t", "wilcoxon_sr")) {
     fluidRow(
       column(6, selectInput("pd_group1", "Group 1:", choices = gc,
         selected = gc[1])),
       column(6, selectInput("pd_group2", "Group 2:", choices = gc,
         selected = if (length(gc) > 1) gc[2] else gc[1]))
     )
   } else {
     checkboxGroupInput("pd_groups_multi", "Select Groups (3+):",
       choices = gc, selected = gc)
   }
 })
 
 # One-sample group selector
 output$onesample_group_selector <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   if (input$onesample_test != "one_prop") {
     selectInput("os_group1", "Select Group:", choices = gc)
   }
 })
 
 # Variance group selectors
 output$var_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$var_test
   
   if (test == "chi_var") {
     selectInput("var_group1", "Select Group:", choices = gc)
   } else if (test == "f_test") {
     fluidRow(
       column(6, selectInput("var_group1", "Group 1:", choices = gc,
         selected = gc[1])),
       column(6, selectInput("var_group2", "Group 2:", choices = gc,
         selected = if (length(gc) > 1) gc[2] else gc[1]))
     )
   } else {
     checkboxGroupInput("var_groups_multi", "Select Groups:",
       choices = gc, selected = gc)
   }
 })
 
 # Normality group selectors
 output$norm_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$norm_test
   
   if (test == "ks_two") {
     fluidRow(
       column(6, selectInput("norm_group1", "Group 1:", choices = gc,
         selected = gc[1])),
       column(6, selectInput("norm_group2", "Group 2:", choices = gc,
         selected = if (length(gc) > 1) gc[2] else gc[1]))
     )
   } else {
     selectInput("norm_group1", "Select Group:", choices = gc)
   }
 })
 
 # Survival / ROC selectors
 output$surv_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$surv_test
   
   if (test == "km") {
     fluidRow(
       column(4, selectInput("surv_time", "Time Variable (Group):",
        choices = gc,
        selected = gc[1])),
       column(4, selectInput("surv_event", "Event/Status (Group):",
        choices = gc,
        selected = if (length(gc) > 1) gc[2] else gc[1])),
       column(4, selectInput("surv_strata", "Strata/Group (optional):",
        choices = c("None" = "none", gc),
        selected = "none"))
     )
   } else {
     fluidRow(
       column(3, selectInput("surv_actual", "Actual Labels (0/1):",
        choices = gc,
        selected = gc[1])),
       column(3, selectInput("surv_pred1", "Predictor 1 Scores:",
        choices = gc,
        selected = if (length(gc) > 1) gc[2] else gc[1])),
       column(3, selectInput("surv_pred2", "Predictor 2 Scores:",
        choices = gc,
        selected = if (length(gc) > 2) gc[3] else if (length(gc) > 1) gc[2] else gc[1])),
       column(3,
         conditionalPanel(
           condition = "input.surv_test == 'delong_paired'",
           helpText("Paired: same subjects, two predictors")
         )
       )
     )
   }
 })
 
 # Multi-factor selectors
 output$multi_group_selectors <- renderUI({
   req(rv$group_names)
   gc <- get_group_choices()
   test <- input$multi_test
   
   if (test == "two_anova") {
     fluidRow(
       column(4, selectInput("multi_factorA", "Factor A (Group):",
        choices = gc,
        selected = gc[1])),
       column(4, selectInput("multi_factorB", "Factor B (Group):",
        choices = gc,
        selected = if (length(gc) > 1) gc[2] else gc[1])),
       column(4, selectInput("multi_response", "Response (Group):",
        choices = gc,
        selected = if (length(gc) > 2) gc[3] else if (length(gc) > 1) gc[2] else gc[1]))
     )
   } else {
    manova_default <- if (length(gc) > 2) gc[2:3] else if (length(gc) > 1) gc[2] else gc[1]
     fluidRow(
       column(4, selectInput("multi_manova_group", "Grouping Variable:",
        choices = gc,
        selected = gc[1])),
       column(8, checkboxGroupInput("multi_manova_resp",
        "Response Variables:", choices = gc, selected = manova_default, inline = TRUE))
     )
   }
 })

# Auto-detect coding values for survival/ROC selectors
detect_surv_codes <- function(values) {
  x <- suppressWarnings(as.numeric(values))
  x <- x[is.finite(x)]
  ux <- sort(unique(x))
  if (length(ux) < 2) return(NULL)
  list(
    event = ux[length(ux)],
    censor = ux[1],
    positive = ux[length(ux)]
  )
}

observeEvent(input$surv_event, {
  req(rv$data, input$surv_event)
  vals <- rv$data[[input$surv_event]]
  det <- detect_surv_codes(vals)
  if (!is.null(det)) {
    updateNumericInput(session, "surv_event_code", value = det$event)
    updateNumericInput(session, "surv_censor_code", value = det$censor)
  }
}, ignoreInit = TRUE)

observeEvent(input$surv_actual, {
  req(rv$data, input$surv_actual)
  vals <- rv$data[[input$surv_actual]]
  det <- detect_surv_codes(vals)
  if (!is.null(det)) {
    updateNumericInput(session, "surv_positive_label", value = det$positive)
  }
}, ignoreInit = TRUE)

output$surv_km_detect_hint <- renderUI({
  if (!identical(input$surv_test, "km")) return(NULL)
  req(rv$data, input$surv_event)
  vals <- suppressWarnings(as.numeric(rv$data[[input$surv_event]]))
  vals <- vals[is.finite(vals)]
  ux <- sort(unique(vals))
  if (length(ux) == 0) {
    return(helpText("No numeric values detected in selected Event/Status group."))
  }
  preview <- paste(head(ux, 6), collapse = ", ")
  if (length(ux) > 6) preview <- paste0(preview, ", ...")
  helpText(
    paste0("Detected event values: ", preview,
      " | Current mapping: event=", input$surv_event_code,
      ", censor=", input$surv_censor_code)
  )
})

output$surv_roc_detect_hint <- renderUI({
  if (!(identical(input$surv_test, "delong_ind") || identical(input$surv_test, "delong_paired"))) return(NULL)
  req(rv$data, input$surv_actual)
  vals <- suppressWarnings(as.numeric(rv$data[[input$surv_actual]]))
  vals <- vals[is.finite(vals)]
  ux <- sort(unique(vals))
  if (length(ux) == 0) {
    return(helpText("No numeric values detected in selected Actual Labels group."))
  }
  preview <- paste(head(ux, 6), collapse = ", ")
  if (length(ux) > 6) preview <- paste0(preview, ", ...")
  helpText(
    paste0("Detected label values: ", preview,
      " | Current positive label=", input$surv_positive_label)
  )
})
 
 # ===========================================================================
 # HELPER: Format result output
 # ===========================================================================
 format_result <- function(test_name, result_obj, extra_text = "") {
   p_val <- NULL
   stat_val <- NULL
   
   if (is.list(result_obj) && !is.null(result_obj$p.value)) {
     p_val <- result_obj$p.value
     stat_val <- result_obj$statistic
   }
   
   lines <- paste0(
     "═══════════════════════════════════════════════════════\n",
     "  TEST: ", test_name, "\n",
     "═══════════════════════════════════════════════════════\n"
   )
   
   if (!is.null(stat_val)) {
     lines <- paste0(lines, "  Statistic: ",
       paste(names(stat_val), "=", round(stat_val, 6), collapse = ", "), "\n")
   }
   if (!is.null(p_val)) {
     lines <- paste0(lines, "  p-value: ", format.pval(p_val, digits = 6), "\n")
     sig <- ifelse(p_val < 0.05, "SIGNIFICANT (p < 0.05)", 
                   "NOT SIGNIFICANT (p >= 0.05)")
     lines <- paste0(lines, "  Decision: ", sig, "\n")
   }
   if (!is.null(result_obj$conf.int)) {
     ci <- result_obj$conf.int
     lines <- paste0(lines, "  Confidence Interval: [",
       round(ci[1], 6), ", ", round(ci[2], 6), "]\n")
   }
   if (!is.null(result_obj$estimate)) {
     est <- result_obj$estimate
     lines <- paste0(lines, "  Estimate: ",
       paste(names(est), "=", round(est, 6), collapse = ", "), "\n")
   }
   if (extra_text != "") {
     lines <- paste0(lines, "\n", extra_text, "\n")
   }
   lines <- paste0(lines, "───────────────────────────────────────────────────────\n")
   
   lines
 }
 
 format_result_html <- function(test_name, result_obj, extra_html = "") {
   p_val <- NULL
   stat_val <- NULL
   
   if (is.list(result_obj) && !is.null(result_obj$p.value)) {
     p_val <- result_obj$p.value
     stat_val <- result_obj$statistic
   }
   
   sig_badge <- ""
   if (!is.null(p_val)) {
     if (p_val < 0.05) {
       sig_badge <- '<span class="badge-sig sig">SIGNIFICANT (p &lt; 0.05)</span>'
     } else {
       sig_badge <- '<span class="badge-sig not-sig">NOT SIGNIFICANT (p &ge; 0.05)</span>'
     }
   }
   
   card_class <- "stat-card"
   if (!is.null(p_val)) {
     card_class <- paste(card_class,
       ifelse(p_val < 0.05, "significant", "not-significant"))
   }
   
   html <- paste0('<div class="', card_class, '">')
   html <- paste0(html, '<h4>', test_name, '</h4>')
   html <- paste0(html, sig_badge)
   
   if (!is.null(stat_val)) {
     html <- paste0(html, '<p><strong>Test Statistic:</strong> ',
       paste(names(stat_val), "=", round(stat_val, 6), collapse = ", "),
       '</p>')
   }
   if (!is.null(p_val)) {
     html <- paste0(html, '<p><strong>p-value:</strong> ',
       format.pval(p_val, digits = 6), '</p>')
   }
   if (!is.null(result_obj$conf.int)) {
     ci <- result_obj$conf.int
     html <- paste0(html, '<p><strong>Confidence Interval:</strong> [',
       round(ci[1], 6), ', ', round(ci[2], 6), ']</p>')
   }
   if (!is.null(result_obj$estimate)) {
     est <- result_obj$estimate
     html <- paste0(html, '<p><strong>Estimate:</strong> ',
       paste(names(est), "=", round(est, 6), collapse = ", "), '</p>')
   }
   if (!is.null(result_obj$method)) {
     html <- paste0(html, '<p><em>Method: ', result_obj$method, '</em></p>')
   }
   if (extra_html != "") {
     html <- paste0(html, extra_html)
   }
   html <- paste0(html, '</div>')
   
   html
 }
 
 store_result <- function(text) {
   rv$results_history <- c(rv$results_history, list(text))
   rv$current_result <- text
 }

validate_distinct_groups <- function(group1, group2,
                                     label = "Please select two different groups.") {
  if (is.null(group1) || is.null(group2) || identical(group1, group2)) {
    showNotification(label, type = "error")
    return(FALSE)
  }
  TRUE
}

validate_min_sample <- function(x, min_n, label) {
  if (length(x) < min_n) {
    showNotification(paste0(label, " requires at least ", min_n, " observations."),
      type = "error")
    return(FALSE)
  }
  TRUE
}

validate_min_sample_pair <- function(x, y, min_n, label) {
  if (length(x) < min_n || length(y) < min_n) {
    showNotification(paste0(label, " requires at least ", min_n,
      " observations in each selected group."), type = "error")
    return(FALSE)
  }
  TRUE
}
 
 # ===========================================================================
 # PARAMETRIC TESTS
 # ===========================================================================
 observeEvent(input$run_parametric, {
   req(rv$data)
   test <- input$parametric_test
   alt <- input$param_alternative
   conf <- input$param_conf_level
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "one_t") {
       req(input$param_group1)
       x <- rv$data[[input$param_group1]]
      if (!validate_min_sample(x, 2, "One Sample T-Test")) return()
       result <- t.test(x, mu = input$param_mu,
         alternative = alt, conf.level = conf)
       test_name <- "One Sample T-Test"
       
     } else if (test == "one_z") {
       req(input$param_group1)
       x <- rv$data[[input$param_group1]]
       n <- length(x)
       z <- (mean(x) - input$param_mu) / (input$param_sigma / sqrt(n))
       p <- switch(alt,
         "two.sided" = 2 * pnorm(-abs(z)),
         "less" = pnorm(z),
         "greater" = pnorm(z, lower.tail = FALSE)
       )
       result <- list(
         statistic = c(z = z), p.value = p,
         estimate = c("sample mean" = mean(x)),
         method = "One Sample Z-Test",
         conf.int = mean(x) + c(-1, 1) * qnorm(1 - (1 - conf) / 2) *
           input$param_sigma / sqrt(n)
       )
       test_name <- "One Sample Z-Test"
       
     } else if (test == "two_t_pooled") {
       req(input$param_group1, input$param_group2)
      if (!validate_distinct_groups(input$param_group1, input$param_group2,
        "Two Sample T-Test requires two different groups.")) return()
       x <- rv$data[[input$param_group1]]
       y <- rv$data[[input$param_group2]]
      if (!validate_min_sample_pair(x, y, 2, "Two Sample T-Test")) return()
       result <- t.test(x, y, var.equal = TRUE,
         alternative = alt, conf.level = conf)
       test_name <- "Two Sample T-Test (Pooled Variance)"
       
     } else if (test == "two_t_welch") {
       req(input$param_group1, input$param_group2)
      if (!validate_distinct_groups(input$param_group1, input$param_group2,
        "Welch's T-Test requires two different groups.")) return()
       x <- rv$data[[input$param_group1]]
       y <- rv$data[[input$param_group2]]
      if (!validate_min_sample_pair(x, y, 2, "Welch's T-Test")) return()
       result <- t.test(x, y, var.equal = FALSE,
         alternative = alt, conf.level = conf)
       test_name <- "Two Sample T-Test (Welch's)"
       
     } else if (test == "two_z") {
       req(input$param_group1, input$param_group2)
      if (!validate_distinct_groups(input$param_group1, input$param_group2,
        "Two Sample Z-Test requires two different groups.")) return()
       x <- rv$data[[input$param_group1]]
       y <- rv$data[[input$param_group2]]
      if (!validate_min_sample_pair(x, y, 1, "Two Sample Z-Test")) return()
      if (input$param_sigma <= 0 || input$param_sigma2 <= 0) {
        showNotification("Known sigma values must be greater than 0 for Two Sample Z-Test.",
          type = "error")
        return()
      }
       nx <- length(x); ny <- length(y)
       z <- (mean(x) - mean(y)) /
         sqrt(input$param_sigma^2 / nx + input$param_sigma2^2 / ny)
       p <- switch(alt,
         "two.sided" = 2 * pnorm(-abs(z)),
         "less" = pnorm(z),
         "greater" = pnorm(z, lower.tail = FALSE)
       )
       diff_mean <- mean(x) - mean(y)
       se <- sqrt(input$param_sigma^2 / nx + input$param_sigma2^2 / ny)
       ci <- diff_mean + c(-1, 1) * qnorm(1 - (1 - conf) / 2) * se
       result <- list(
         statistic = c(z = z), p.value = p,
         estimate = c("difference in means" = diff_mean),
         method = "Two Sample Z-Test",
         conf.int = ci
       )
       test_name <- "Two Sample Z-Test"
       
     } else if (test == "anova_one") {
       req(input$param_groups_multi)
       groups_sel <- input$param_groups_multi
       req(length(groups_sel) >= 2)
       
       df_long <- do.call(rbind, lapply(groups_sel, function(g) {
         data.frame(group = g, value = rv$data[[g]],
           stringsAsFactors = FALSE)
       }))
       df_long$group <- factor(df_long$group)
       
       aov_res <- aov(value ~ group, data = df_long)
       aov_summary <- summary(aov_res)
       f_val <- aov_summary[[1]]$`F value`[1]
       p_val <- aov_summary[[1]]$`Pr(>F)`[1]
       
       # Post-hoc Tukey
       tukey_res <- TukeyHSD(aov_res)
       tukey_text <- paste(capture.output(print(tukey_res)), collapse = "\n")
       
       result <- list(
         statistic = c(F = f_val), p.value = p_val,
         method = "One Way ANOVA"
       )
       test_name <- "One Way ANOVA"
       extra <- paste0("<pre>", tukey_text, "</pre>")
       
     } else if (test == "rm_anova") {
       req(input$param_groups_multi)
       groups_sel <- input$param_groups_multi
       req(length(groups_sel) >= 2)
       
       # Build repeated measures data
       min_n <- min(sapply(groups_sel, function(g) length(rv$data[[g]])))
       df_long <- do.call(rbind, lapply(groups_sel, function(g) {
         data.frame(
           subject = seq_len(min_n),
           group = g,
           value = rv$data[[g]][seq_len(min_n)],
           stringsAsFactors = FALSE
         )
       }))
       df_long$subject <- factor(df_long$subject)
       df_long$group <- factor(df_long$group)
       
       aov_res <- aov(value ~ group + Error(subject/group), data = df_long)
       aov_text <- paste(capture.output(summary(aov_res)), collapse = "\n")
       
       # Extract p-value from within-subject error stratum
       s <- summary(aov_res)
       p_val <- NA
       f_val <- NA
       for (stratum in s) {
         tbl <- stratum[[1]]
         if ("group" %in% rownames(tbl)) {
           f_val <- tbl["group", "F value"]
           p_val <- tbl["group", "Pr(>F)"]
         }
       }
       
       if (is.na(p_val) || is.na(f_val)) {
         showNotification("Could not extract F/p-value from RM ANOVA. Check that groups have sufficient data.", type = "error")
         return()
       }
       result <- list(
         statistic = c(F = f_val), p.value = p_val,
         method = "Repeated Measures ANOVA"
       )
       test_name <- "Repeated Measures ANOVA"
       extra <- paste0("<pre>", aov_text, "</pre>")
     }
     
     # Store and render
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       
       output$parametric_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$parametric_result <- renderUI({
       div(class = "stat-card significant",
         h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # NON-PARAMETRIC TESTS
 # ===========================================================================
 observeEvent(input$run_nonparam, {
   req(rv$data)
   test <- input$nonparam_test
   alt <- input$nonparam_alternative
   conf <- input$nonparam_conf_level
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "mann_whitney") {
       req(input$np_group1, input$np_group2)
      if (!validate_distinct_groups(input$np_group1, input$np_group2,
        "Mann-Whitney U Test requires two different groups.")) return()
       x <- rv$data[[input$np_group1]]
       y <- rv$data[[input$np_group2]]
      if (!validate_min_sample_pair(x, y, 1, "Mann-Whitney U Test")) return()
       nx <- length(x)
       ny <- length(y)
       use_exact <- input$np_exact
       use_correct <- input$np_correct
       
       # Use coin::wilcox_test for exact p-values (handles ties correctly)
       # This matches SPSS and GraphPad Prism behavior
       if (use_exact && (nx + ny) <= 200) {
         tryCatch({
           df_coin <- data.frame(
             value = c(x, y),
             group = factor(c(rep("A", nx), rep("B", ny)))
           )
           coin_res <- coin::wilcox_test(value ~ group, data = df_coin,
             alternative = alt, distribution = "exact")
           z_stat <- coin::statistic(coin_res, type = "standardized")
           p_val <- coin::pvalue(coin_res)
           
           # Compute U statistic: U = R1 - n1*(n1+1)/2
           # where R1 = sum of ranks of group 1
           all_vals <- c(x, y)
           ranks <- rank(all_vals)
           R1 <- sum(ranks[1:nx])
           U1 <- R1 - nx * (nx + 1) / 2
           U2 <- nx * ny - U1
           U <- min(U1, U2)
           
           # Effect size r = Z / sqrt(N)
           r_effect <- abs(as.numeric(z_stat)) / sqrt(nx + ny)
           
           result <- list(
             statistic = c(U = U, W = R1, Z = as.numeric(z_stat)),
             p.value = as.numeric(p_val),
             estimate = c("U1" = U1, "U2" = U2),
             method = "Mann-Whitney U Test (Exact)",
             conf.int = NULL
           )
           extra <- paste0(
             "<p><strong>Effect size r:</strong> ", round(r_effect, 4),
             " (", ifelse(r_effect < 0.1, "negligible",
               ifelse(r_effect < 0.3, "small",
                 ifelse(r_effect < 0.5, "medium", "large"))), ")</p>",
             "<p><strong>Sum of ranks (Group 1):</strong> ", round(R1, 2),
             "</p><p><strong>n1 =", nx, ", n2 =", ny, "</strong></p>",
             "<p><em>Exact permutation-based p-value (handles ties correctly)</em></p>"
           )
         }, error = function(e) {
           # Fallback to wilcox.test if coin fails
           result <<- wilcox.test(x, y, alternative = alt,
             conf.level = conf, exact = TRUE, correct = use_correct)
           
           ranks <- rank(c(x, y))
           R1 <- sum(ranks[1:nx])
           U1 <- R1 - nx * (nx + 1) / 2
           U2 <- nx * ny - U1
           U <- min(U1, U2)
           r_effect <- abs(qnorm(result$p.value / 2)) / sqrt(nx + ny)
           
           extra <<- paste0(
             "<p><strong>U statistic:</strong> ", round(U, 2),
             "</p><p><strong>Effect size r:</strong> ", round(r_effect, 4),
             "</p><p><em>Exact via wilcox.test (fallback)</em></p>"
           )
         })
       } else {
         # Normal approximation for large samples
         result <- wilcox.test(x, y, alternative = alt,
           conf.level = conf, exact = FALSE, correct = use_correct)
         
         ranks <- rank(c(x, y))
         R1 <- sum(ranks[1:nx])
         U1 <- R1 - nx * (nx + 1) / 2
         U2 <- nx * ny - U1
         U <- min(U1, U2)
         
         # Z-score from normal approximation
         mean_U <- nx * ny / 2
         sd_U <- sqrt(nx * ny * (nx + ny + 1) / 12)
         z_approx <- (U1 - mean_U) / sd_U
         r_effect <- abs(z_approx) / sqrt(nx + ny)
         
         # Add U to result
         result$statistic <- c(U = U, W = result$statistic[[1]], Z = z_approx)
         result$estimate <- c("U1" = U1, "U2" = U2)
         result$method <- paste0("Mann-Whitney U Test (Normal Approximation",
           ifelse(use_correct, ", continuity corrected", ""), ")")
         
         extra <- paste0(
           "<p><strong>Effect size r:</strong> ", round(r_effect, 4),
           " (", ifelse(r_effect < 0.1, "negligible",
             ifelse(r_effect < 0.3, "small",
               ifelse(r_effect < 0.5, "medium", "large"))), ")</p>",
           "<p><strong>Sum of ranks (Group 1):</strong> ", round(R1, 2),
           "</p><p><strong>n1 =", nx, ", n2 =", ny, "</strong></p>",
           ifelse(use_correct,
             "<p><em>Continuity correction applied</em></p>", "")
         )
       }
       test_name <- "Mann-Whitney U Test"
       
     } else if (test == "kruskal") {
       req(input$np_groups_multi)
       groups_sel <- input$np_groups_multi
       req(length(groups_sel) >= 2)
       
       df_long <- do.call(rbind, lapply(groups_sel, function(g) {
         data.frame(group = g, value = rv$data[[g]],
           stringsAsFactors = FALSE)
       }))
       df_long$group <- factor(df_long$group)
       result <- kruskal.test(value ~ group, data = df_long)
       test_name <- "Kruskal-Wallis Test"
       
       # Post-hoc Dunn test approximation
       if (result$p.value < 0.05) {
         pw <- pairwise.wilcox.test(df_long$value, df_long$group,
           p.adjust.method = "bonferroni")
         pw_text <- paste(capture.output(print(pw)), collapse = "\n")
         extra <- paste0("<p><strong>Post-hoc pairwise Wilcoxon tests",
           " (Bonferroni adjusted):</strong></p><pre>", pw_text, "</pre>")
       }
       
     } else if (test == "chi_gof") {
       req(input$np_groups_multi)
       groups_sel <- input$np_groups_multi
       
       # Use counts (length of each group as observed frequencies)
       observed <- sapply(groups_sel, function(g) length(rv$data[[g]]))
       
       # Parse expected probabilities
       exp_probs <- NULL
       if (!is.null(input$chi_expected_probs) &&
           trimws(input$chi_expected_probs) != "") {
         exp_probs <- as.numeric(
           unlist(strsplit(input$chi_expected_probs, ",")))
         if (length(exp_probs) != length(observed))
           exp_probs <- NULL
       }
       
       if (!is.null(exp_probs)) {
         result <- chisq.test(observed, p = exp_probs)
       } else {
         result <- chisq.test(observed)
       }
       test_name <- "Chi-Squared Goodness of Fit"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$nonparam_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$nonparam_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # PAIRED TESTS
 # ===========================================================================
 observeEvent(input$run_paired, {
   req(rv$data)
   test <- input$paired_test
   alt <- input$paired_alternative
   conf <- input$paired_conf_level
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "paired_t") {
       req(input$pd_group1, input$pd_group2)
      if (!validate_distinct_groups(input$pd_group1, input$pd_group2,
        "Paired T-Test requires two different groups.")) return()
       x <- rv$data[[input$pd_group1]]
       y <- rv$data[[input$pd_group2]]
       min_n <- min(length(x), length(y))
      if (min_n < 2) {
        showNotification("Paired T-Test requires at least 2 paired observations.",
          type = "error")
        return()
      }
      if (length(x) != length(y)) {
        showNotification(paste0("Groups have unequal lengths (", length(x), " vs ", length(y),
          "). Truncating to ", min_n, " paired observations."), type = "warning")
      }
       result <- t.test(x[1:min_n], y[1:min_n], paired = TRUE,
         alternative = alt, conf.level = conf)
       test_name <- "Paired T-Test"
       
     } else if (test == "wilcoxon_sr") {
       req(input$pd_group1, input$pd_group2)
      if (!validate_distinct_groups(input$pd_group1, input$pd_group2,
        "Wilcoxon Signed Rank Test requires two different groups.")) return()
       x <- rv$data[[input$pd_group1]]
       y <- rv$data[[input$pd_group2]]
       min_n <- min(length(x), length(y))
      if (min_n < 1) {
        showNotification("Wilcoxon Signed Rank Test requires at least 1 paired observation.",
          type = "error")
        return()
      }
      if (length(x) != length(y)) {
        showNotification(paste0("Groups have unequal lengths (", length(x), " vs ", length(y),
          "). Truncating to ", min_n, " paired observations."), type = "warning")
      }
       use_exact <- input$pd_exact
       use_correct <- input$pd_correct
       
       # Use coin::wilcoxsign_test for exact p-values (handles ties/zeros)
       if (use_exact && min_n <= 200) {
         tryCatch({
           df_coin <- data.frame(
             value = c(x[1:min_n], y[1:min_n]),
             group = factor(rep(c("pre", "post"), each = min_n)),
             id = factor(rep(seq_len(min_n), 2))
           )
           coin_res <- coin::wilcoxsign_test(
             value ~ group | id, data = df_coin,
             alternative = alt, distribution = "exact")
           z_stat <- coin::statistic(coin_res, type = "standardized")
           p_val <- coin::pvalue(coin_res)
           
           # Effect size r = Z / sqrt(N)
           r_effect <- abs(as.numeric(z_stat)) / sqrt(min_n)
           
           result <- list(
             statistic = c(Z = as.numeric(z_stat)),
             p.value = as.numeric(p_val),
             method = "Wilcoxon Signed Rank Test (Exact)",
             estimate = c("median diff" = median(x[1:min_n] - y[1:min_n]))
           )
           extra <- paste0(
             "<p><strong>Effect size r:</strong> ", round(r_effect, 4),
             " (", ifelse(r_effect < 0.1, "negligible",
               ifelse(r_effect < 0.3, "small",
                 ifelse(r_effect < 0.5, "medium", "large"))), ")</p>",
             "<p><strong>n (pairs):</strong> ", min_n, "</p>",
             "<p><em>Exact permutation-based p-value</em></p>"
           )
         }, error = function(e) {
           result <<- wilcox.test(x[1:min_n], y[1:min_n], paired = TRUE,
             alternative = alt, conf.level = conf,
             exact = TRUE, correct = use_correct)
           extra <<- "<p><em>Fallback: wilcox.test exact method</em></p>"
         })
       } else {
         result <- wilcox.test(x[1:min_n], y[1:min_n], paired = TRUE,
           alternative = alt, conf.level = conf,
           exact = FALSE, correct = use_correct)
         result$method <- paste0("Wilcoxon Signed Rank Test (Normal Approx.",
           ifelse(use_correct, ", continuity corrected", ""), ")")
         extra <- paste0(
           "<p><strong>n (pairs):</strong> ", min_n, "</p>",
           ifelse(use_correct,
             "<p><em>Continuity correction applied</em></p>", "")
         )
       }
       test_name <- "Paired Wilcoxon Signed Rank Test"
       
     } else if (test == "friedman") {
       req(input$pd_groups_multi)
       groups_sel <- input$pd_groups_multi
       req(length(groups_sel) >= 3)
       
       min_n <- min(sapply(groups_sel, function(g) length(rv$data[[g]])))
       if (min_n < 2) {
         showNotification("Friedman test requires at least 2 observations per group.", type = "error")
         return()
       }
       mat <- sapply(groups_sel, function(g) rv$data[[g]][1:min_n])
       if (!is.matrix(mat)) {
         showNotification("Could not form a valid data matrix for Friedman test.", type = "error")
         return()
       }
       result <- friedman.test(mat)
       test_name <- "Friedman Test"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$paired_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$paired_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # ONE-SAMPLE TESTS
 # ===========================================================================
 observeEvent(input$run_onesample, {
   req(rv$data)
   test <- input$onesample_test
   
   tryCatch({
     result <- NULL
     test_name <- ""
     
     if (test == "one_t") {
       req(input$os_group1)
       x <- rv$data[[input$os_group1]]
       result <- t.test(x, mu = input$os_mu)
       test_name <- "One Sample T-Test"
       
     } else if (test == "one_z") {
       req(input$os_group1)
       x <- rv$data[[input$os_group1]]
       n <- length(x)
       if (is.null(input$os_sigma) || input$os_sigma <= 0) {
         showNotification("Known sigma must be greater than 0 for Z-Test.", type = "error")
         return()
       }
       z <- (mean(x) - input$os_mu) / (input$os_sigma / sqrt(n))
       p <- 2 * pnorm(-abs(z))  # two-sided (one-sample tab)
       ci <- mean(x) + c(-1, 1) * qnorm(0.975) * input$os_sigma / sqrt(n)
       result <- list(
         statistic = c(z = z), p.value = p,
         estimate = c("sample mean" = mean(x)),
         method = "One Sample Z-Test",
         conf.int = ci
       )
       test_name <- "One Sample Z-Test"
       
     } else if (test == "one_prop") {
       p0 <- input$os_mu
       if (is.null(p0) || p0 <= 0 || p0 >= 1) {
         showNotification(
           "Hypothesized proportion must be between 0 and 1 (exclusive).",
           type = "error")
         return()
       }
       if (is.null(input$os_prop_n) || input$os_prop_n < 1) {
         showNotification("Number of trials must be at least 1.", type = "error")
         return()
       }
       if (is.null(input$os_prop_x) || input$os_prop_x < 0 || input$os_prop_x > input$os_prop_n) {
         showNotification("Successes must be between 0 and total trials.", type = "error")
         return()
       }
       result <- prop.test(input$os_prop_x, input$os_prop_n, p = p0)
       test_name <- "One Sample Proportion Test"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$onesample_result <- renderUI({
         HTML(format_result_html(test_name, result))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$onesample_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # PROPORTION TESTS
 # ===========================================================================
 observeEvent(input$run_prop, {
   tryCatch({
     result <- NULL
     test_name <- ""
     alt <- input$prop_alternative
     conf <- input$prop_conf_level
     
     if (input$prop_test == "one_prop") {
       result <- prop.test(input$prop_x1, input$prop_n1,
         p = input$prop_p0, alternative = alt, conf.level = conf)
       test_name <- "One Sample Proportion Test"
       
     } else if (input$prop_test == "two_prop") {
       result <- prop.test(
         c(input$prop_x1, input$prop_x2),
         c(input$prop_n1, input$prop_n2),
         alternative = alt, conf.level = conf
       )
       test_name <- "Two Sample Proportion Test"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$prop_result <- renderUI({
         HTML(format_result_html(test_name, result))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$prop_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # VARIANCE TESTS
 # ===========================================================================
 observeEvent(input$run_var, {
   req(rv$data)
   test <- input$var_test
   alt <- input$var_alternative
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "chi_var") {
       req(input$var_group1)
       x <- rv$data[[input$var_group1]]
       n <- length(x)
       if (is.null(input$var_sigma0) || input$var_sigma0 <= 0) {
         showNotification("Hypothesized variance must be greater than 0.", type = "error")
         return()
       }
       s2 <- var(x)
       chi2 <- (n - 1) * s2 / input$var_sigma0
       df <- n - 1
       p <- switch(alt,
         "two.sided" = 2 * min(pchisq(chi2, df), pchisq(chi2, df,
           lower.tail = FALSE)),
         "less" = pchisq(chi2, df),
         "greater" = pchisq(chi2, df, lower.tail = FALSE)
       )
       result <- list(
         statistic = c("Chi-squared" = chi2), p.value = p,
         estimate = c("sample variance" = s2),
         method = paste("Chi-Squared Test for Variance (df =", df, ")")
       )
       test_name <- "Chi-Squared Test for Variance"
       
     } else if (test == "f_test") {
       req(input$var_group1, input$var_group2)
      if (!validate_distinct_groups(input$var_group1, input$var_group2,
        "F Test for Variances requires two different groups.")) return()
       x <- rv$data[[input$var_group1]]
       y <- rv$data[[input$var_group2]]
      if (!validate_min_sample_pair(x, y, 2, "F Test for Variances")) return()
       result <- var.test(x, y, alternative = alt)
       test_name <- "F Test for Variances"
       
     } else if (test == "levene") {
       req(input$var_groups_multi)
       groups_sel <- input$var_groups_multi
       req(length(groups_sel) >= 2)
       
       df_long <- do.call(rbind, lapply(groups_sel, function(g) {
         data.frame(group = g, value = rv$data[[g]],
           stringsAsFactors = FALSE)
       }))
       df_long$group <- factor(df_long$group)
       
       lev_res <- car::leveneTest(value ~ group, data = df_long)
       f_val <- lev_res$`F value`[1]
       p_val <- lev_res$`Pr(>F)`[1]
       
       result <- list(
         statistic = c(F = f_val), p.value = p_val,
         method = "Levene's Test for Homogeneity of Variance"
       )
       lev_text <- paste(capture.output(print(lev_res)), collapse = "\n")
       extra <- paste0("<pre>", lev_text, "</pre>")
       test_name <- "Levene's Test for Variances"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$var_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$var_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # NORMALITY TESTS
 # ===========================================================================
 observeEvent(input$run_norm, {
   req(rv$data)
   test <- input$norm_test
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "shapiro") {
       req(input$norm_group1)
       x <- rv$data[[input$norm_group1]]
       if (length(x) < 3 || length(x) > 5000) {
         showNotification(
           "Shapiro-Wilk requires 3 to 5000 observations.",
           type = "error"
         )
         return()
       }
       result <- shapiro.test(x)
       test_name <- "Shapiro-Wilk Normality Test"
       
     } else if (test == "ks_one") {
       req(input$norm_group1)
       x <- rv$data[[input$norm_group1]]
       # Lilliefors test: correct KS test when parameters are estimated
       # ks.test with estimated mean/sd gives INVALID p-values
       lillie_result <- nortest::lillie.test(x)
       
       # Also compute standard KS for reference
       ks_raw <- suppressWarnings(
         ks.test(x, "pnorm", mean(x), sd(x)))
       
       result <- list(
         statistic = c(D = lillie_result$statistic),
         p.value = lillie_result$p.value,
         method = "Lilliefors (Kolmogorov-Smirnov) Normality Test"
       )
       extra <- paste0(
         "<p><em>Note: The Lilliefors correction is applied because parameters ",
         "(mean, sd) are estimated from the data. A standard KS test with ",
         "estimated parameters gives invalid (anti-conservative) p-values.</em></p>",
         "<p>Uncorrected KS D = ", round(ks_raw$statistic, 6),
         ", p = ", format.pval(ks_raw$p.value, digits = 6),
         " (unreliable)</p>"
       )
       test_name <- "Kolmogorov-Smirnov Normality Test (Lilliefors corrected)"
       
     } else if (test == "ks_two") {
       req(input$norm_group1, input$norm_group2)
      if (!validate_distinct_groups(input$norm_group1, input$norm_group2,
        "Two-Sample KS Test requires two different groups.")) return()
       x <- rv$data[[input$norm_group1]]
       y <- rv$data[[input$norm_group2]]
      if (!validate_min_sample_pair(x, y, 1, "Two-Sample KS Test")) return()
       result <- ks.test(x, y)
       test_name <- "Two-Sample Kolmogorov-Smirnov Test"
     }
     
     if (!is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$norm_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$norm_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # SURVIVAL & ROC TESTS
 # ===========================================================================
 observeEvent(input$run_surv, {
   req(rv$data)
   test <- input$surv_test
   
   tryCatch({
     test_name <- ""
     extra <- ""
     
     if (test == "km") {
      req(input$surv_time, input$surv_event, input$surv_strata)
      time_vals <- suppressWarnings(as.numeric(rv$data[[input$surv_time]]))
      event_raw <- suppressWarnings(as.numeric(rv$data[[input$surv_event]]))
      min_n <- min(length(time_vals), length(event_raw))
      time_vals <- time_vals[1:min_n]
      event_raw <- event_raw[1:min_n]

      event_code <- input$surv_event_code
      censor_code <- input$surv_censor_code
      if (is.null(event_code) || is.null(censor_code) || identical(event_code, censor_code)) {
        showNotification("Event and censor codes must be different.", type = "error")
        return()
      }
      event_vals <- ifelse(event_raw == event_code, 1,
        ifelse(event_raw == censor_code, 0, NA_real_))
      n_mapped <- sum(!is.na(event_vals))
      if (n_mapped == 0) {
        showNotification("No event values matched the event/censor codes. Check your codes.", type = "error")
        return()
      }
      if (n_mapped < length(event_raw) * 0.5) {
        showNotification(paste0("Warning: only ", n_mapped, " of ", length(event_raw),
          " event values matched the codes. Unmatched rows will be excluded."), type = "warning")
      }

      strata_selected <- !is.null(input$surv_strata) && input$surv_strata != "none"
      if (strata_selected) {
        strata_vals <- rv$data[[input$surv_strata]]
        strata_vals <- strata_vals[1:min_n]
        valid <- is.finite(time_vals) & !is.na(event_vals) & !is.na(strata_vals)
        surv_df <- data.frame(time = time_vals[valid], event = event_vals[valid],
          strata = factor(strata_vals[valid]))

        if (nrow(surv_df) < 2 || sum(surv_df$event %in% c(0, 1)) < 2) {
          showNotification("Not enough valid time/event rows for Kaplan-Meier analysis.", type = "error")
          return()
        }
        if (nlevels(surv_df$strata) < 2) {
          showNotification("Selected strata has fewer than 2 valid groups; running without strata.", type = "warning")
          strata_selected <- FALSE
        }
      }

      if (strata_selected) {
        fit <- survfit(Surv(surv_df$time, surv_df$event) ~ strata, data = surv_df)
         
         # Log-rank test
        lr <- survdiff(Surv(surv_df$time, surv_df$event) ~ strata, data = surv_df)
         p_val <- 1 - pchisq(lr$chisq, length(lr$n) - 1)
         
         result <- list(
           statistic = c("Chi-sq" = lr$chisq), p.value = p_val,
           method = "Kaplan-Meier with Log-Rank Test"
         )
         
         output$surv_plot <- renderPlot({
           ggsurvplot(fit, data = surv_df, pval = TRUE,
             risk.table = TRUE, conf.int = TRUE)
         })
       } else {
        valid <- is.finite(time_vals) & !is.na(event_vals)
        surv_df <- data.frame(time = time_vals[valid], event = event_vals[valid])
        if (nrow(surv_df) < 2 || sum(surv_df$event %in% c(0, 1)) < 2) {
          showNotification("Not enough valid time/event rows for Kaplan-Meier analysis.", type = "error")
          return()
        }
        fit <- survfit(Surv(surv_df$time, surv_df$event) ~ 1, data = surv_df)
         
         result <- list(
           statistic = NULL, p.value = NULL,
           method = "Kaplan-Meier Survival Estimate (no strata)"
         )
         
         fit_summary <- paste(capture.output(print(fit)), collapse = "\n")
         extra <- paste0("<pre>", fit_summary, "</pre>")
         
         output$surv_plot <- renderPlot({
           ggsurvplot(fit, data = surv_df, conf.int = TRUE)
         })
       }
       
       test_name <- "Kaplan-Meier Survival Analysis"
       txt <- format_result(test_name, result)
       store_result(txt)
       output$surv_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
       
     } else if (test %in% c("delong_ind", "delong_paired")) {
       req(input$surv_actual, input$surv_pred1, input$surv_pred2)
      actual_raw <- suppressWarnings(as.numeric(rv$data[[input$surv_actual]]))
      pred1 <- suppressWarnings(as.numeric(rv$data[[input$surv_pred1]]))
      pred2 <- suppressWarnings(as.numeric(rv$data[[input$surv_pred2]]))
      min_n <- min(length(actual_raw), length(pred1), length(pred2))
      actual_raw <- actual_raw[1:min_n]
      pred1 <- pred1[1:min_n]
      pred2 <- pred2[1:min_n]

      valid <- is.finite(actual_raw) & is.finite(pred1) & is.finite(pred2)
      if (sum(valid) < 4) {
        showNotification("Not enough valid rows for ROC/DeLong analysis.", type = "error")
        return()
      }
      actual_raw <- actual_raw[valid]
      pred1 <- pred1[valid]
      pred2 <- pred2[valid]

      pos_label <- input$surv_positive_label
      if (is.null(pos_label)) pos_label <- 1
      actual <- ifelse(actual_raw == pos_label, 1, 0)
      if (length(unique(actual)) < 2) {
        showNotification("Actual labels must contain both classes after mapping positive label.", type = "error")
        return()
      }
       
       roc1 <- pROC::roc(actual, pred1, quiet = TRUE)
       roc2 <- pROC::roc(actual, pred2, quiet = TRUE)
       
       paired_flag <- (test == "delong_paired")
       delong <- pROC::roc.test(roc1, roc2, method = "delong",
         paired = paired_flag)
       
       result <- list(
         statistic = c(Z = delong$statistic), p.value = delong$p.value,
         estimate = c("AUC1" = auc(roc1), "AUC2" = auc(roc2)),
         method = paste("DeLong Test -",
           ifelse(paired_flag, "Paired", "Independent"), "Curves")
       )
       test_name <- result$method
       txt <- format_result(test_name, result)
       store_result(txt)
       
       output$surv_result <- renderUI({
         HTML(format_result_html(test_name, result))
       })
       
       output$surv_plot <- renderPlot({
         plot(roc1, col = "#3c8dbc", lwd = 2,
           main = "ROC Curve Comparison")
         plot(roc2, col = "#e74c3c", lwd = 2, add = TRUE)
         legend("bottomright",
           legend = c(
             paste("Model 1 (AUC =", round(auc(roc1), 4), ")"),
             paste("Model 2 (AUC =", round(auc(roc2), 4), ")")
           ),
           col = c("#3c8dbc", "#e74c3c"), lwd = 2
         )
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$surv_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # MULTI-FACTOR TESTS
 # ===========================================================================
 observeEvent(input$run_multi, {
   req(rv$data)
   test <- input$multi_test
   
   tryCatch({
     result <- NULL
     test_name <- ""
     extra <- ""
     
     if (test == "two_anova") {
       req(input$multi_factorA, input$multi_factorB, input$multi_response)
       fa <- rv$data[[input$multi_factorA]]
       fb <- rv$data[[input$multi_factorB]]
      resp <- suppressWarnings(as.numeric(rv$data[[input$multi_response]]))
       min_n <- min(length(fa), length(fb), length(resp))
       
       df <- data.frame(
         factorA = factor(fa[1:min_n]),
         factorB = factor(fb[1:min_n]),
         response = resp[1:min_n]
       )

      if (isTRUE(input$multi_remove_missing)) {
        df <- df[stats::complete.cases(df), , drop = FALSE]
      }
      if (nrow(df) < 3) {
        showNotification("Two Way ANOVA requires at least 3 complete rows.", type = "error")
        return()
      }
      if (nlevels(df$factorA) < 2 || nlevels(df$factorB) < 2) {
        showNotification("Factor A and Factor B each need at least 2 levels.", type = "error")
        return()
      }
      if (length(unique(df$response)) < 2) {
        showNotification("Response variable must have variation for ANOVA.", type = "error")
        return()
      }
       
       aov_res <- aov(response ~ factorA * factorB, data = df)
       aov_text <- paste(capture.output(summary(aov_res)), collapse = "\n")
       
       s <- summary(aov_res)[[1]]
       # Report all terms: Factor A, Factor B, and interaction
       terms <- rownames(s)
       terms <- terms[!grepl("Residuals", terms)]
       f_vals <- s$`F value`[seq_along(terms)]
       p_vals <- s$`Pr(>F)`[seq_along(terms)]
       
       # Use interaction p-value as main result if available, else Factor A
       main_idx <- which(grepl(":", terms))
       if (length(main_idx) == 0) main_idx <- 1
       
       result <- list(
         statistic = c(F = f_vals[main_idx]),
         p.value = p_vals[main_idx],
         method = "Two Way ANOVA"
       )
       
       # Build summary of all terms for extra display
       terms_summary <- paste(sapply(seq_along(terms), function(i) {
         sig <- if (!is.na(p_vals[i]) && p_vals[i] < 0.05) "***" else "ns"
         paste0("<strong>", terms[i], ":</strong> F = ",
           round(f_vals[i], 4), ", p = ",
           format.pval(p_vals[i], digits = 4), " ", sig)
       }), collapse = "<br/>")
       extra <- paste0(
         "<p>", terms_summary, "</p>",
         "<pre>", aov_text, "</pre>"
       )
       test_name <- "Two Way ANOVA"
       
     } else if (test == "manova") {
       req(input$multi_manova_group, input$multi_manova_resp)
       resp_names <- input$multi_manova_resp
       grp_name <- input$multi_manova_group
       req(length(resp_names) >= 2)
       
       grp <- rv$data[[grp_name]]
       min_n <- min(length(grp),
         min(sapply(resp_names, function(r) length(rv$data[[r]]))))

      df <- data.frame(group = factor(grp[1:min_n]), stringsAsFactors = FALSE)
      for (r in resp_names) {
        df[[r]] <- suppressWarnings(as.numeric(rv$data[[r]][1:min_n]))
      }
      if (isTRUE(input$multi_remove_missing)) {
        df <- df[stats::complete.cases(df), , drop = FALSE]
      }
      if (nrow(df) < 3) {
        showNotification("MANOVA requires at least 3 complete rows.", type = "error")
        return()
      }
      if (nlevels(df$group) < 2) {
        showNotification("Grouping variable must have at least 2 levels.", type = "error")
        return()
      }
      if (any(vapply(df[resp_names], function(x) length(unique(x)) < 2, logical(1)))) {
        showNotification("Each MANOVA response variable must have variation.", type = "error")
        return()
      }

      manova_res <- manova(do.call(cbind, df[resp_names]) ~ group, data = df)
       manova_summ <- summary(manova_res)
       manova_text <- paste(capture.output(print(manova_summ)),
         collapse = "\n")
       
       pillai_stat <- manova_summ$stats[1, "approx F"]
       p_val <- manova_summ$stats[1, "Pr(>F)"]
       
       result <- list(
         statistic = c("approx F" = pillai_stat), p.value = p_val,
         method = "One Way MANOVA (Pillai's trace)"
       )
       extra <- paste0("<pre>", manova_text, "</pre>")
       test_name <- "One Way MANOVA"
     }
     
     if (exists("result") && !is.null(result)) {
       txt <- format_result(test_name, result)
       store_result(txt)
       output$multi_result <- renderUI({
         HTML(format_result_html(test_name, result, extra))
       })
     }
   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
     output$multi_result <- renderUI({
       div(class = "stat-card significant", h4("Error"), p(e$message))
     })
   })
 })
 
 # ===========================================================================
 # TEST INFORMATION PANELS
 # ===========================================================================
 
 test_descriptions <- list(
   # Parametric tests
   one_t = list(
     name = "One Sample T-Test",
     desc = "Tests whether the mean of a single group differs from a known or hypothesized value.",
     assumptions = c("Data is continuous", "Sample is randomly drawn", "Data is approximately normally distributed", "No significant outliers"),
     when = "Use when you have one group of continuous data and want to test if its mean equals a specific value.",
     formula = "t = (x\u0304 - \u03bc\u2080) / (s / \u221an)",
     interpret = "If p < 0.05, the sample mean is significantly different from the hypothesized mean."
   ),
   one_z = list(
     name = "One Sample Z-Test",
     desc = "Tests whether the mean of a single group differs from a hypothesized value when the population standard deviation is known.",
     assumptions = c("Data is continuous", "Population standard deviation (sigma) is known", "Data is normally distributed or n is large (n > 30)"),
     when = "Use when you know the population standard deviation. Rarely used in practice; T-test is preferred when sigma is unknown.",
     formula = "Z = (x\u0304 - \u03bc\u2080) / (\u03c3 / \u221an)",
     interpret = "If p < 0.05, the sample mean significantly differs from the hypothesized mean."
   ),
   two_t_pooled = list(
     name = "Two Sample T-Test (Pooled Variance)",
     desc = "Compares means of two independent groups assuming equal variances (Student's t-test).",
     assumptions = c("Both samples are independent", "Data is continuous and approximately normal", "Variances of both groups are equal (homoscedasticity)", "No significant outliers"),
     when = "Use when comparing two independent groups with similar variances. Check with F-test or Levene's test first.",
     formula = "t = (x\u0304\u2081 - x\u0304\u2082) / (sp \u00d7 \u221a(1/n\u2081 + 1/n\u2082)), where sp = pooled SD",
     interpret = "If p < 0.05, the two group means are significantly different."
   ),
   two_t_welch = list(
     name = "Two Sample T-Test (Welch's)",
     desc = "Compares means of two independent groups without assuming equal variances. More robust than the pooled t-test.",
     assumptions = c("Both samples are independent", "Data is continuous and approximately normal", "No assumption about equal variances"),
     when = "Preferred over pooled t-test when variances may differ. Generally recommended as the default two-sample t-test.",
     formula = "t = (x\u0304\u2081 - x\u0304\u2082) / \u221a(s\u2081\u00b2/n\u2081 + s\u2082\u00b2/n\u2082)",
     interpret = "If p < 0.05, the two group means are significantly different."
   ),
   two_z = list(
     name = "Two Sample Z-Test",
     desc = "Compares means of two independent groups when population standard deviations are known.",
     assumptions = c("Both samples are independent", "Population standard deviations are known", "Data is normal or n is large"),
     when = "Use when population sigmas are known for both groups. Rarely used in practice.",
     formula = "Z = (x\u0304\u2081 - x\u0304\u2082) / \u221a(\u03c3\u2081\u00b2/n\u2081 + \u03c3\u2082\u00b2/n\u2082)",
     interpret = "If p < 0.05, the means of the two populations differ significantly."
   ),
   anova_one = list(
     name = "One Way ANOVA",
     desc = "Tests whether the means of three or more independent groups are all equal. Extends the two-sample t-test to multiple groups.",
     assumptions = c("Observations are independent", "Data in each group is approximately normal", "Variances are approximately equal across groups (homogeneity)"),
     when = "Use when comparing means across 3+ independent groups. Follow up with post-hoc tests (Tukey HSD) if significant.",
     formula = "F = MS_between / MS_within",
     interpret = "If p < 0.05, at least one group mean differs. Use Tukey HSD to identify which pairs differ."
   ),
   rm_anova = list(
     name = "Repeated Measures ANOVA",
     desc = "Tests whether the means differ across conditions when the same subjects are measured multiple times.",
     assumptions = c("Same subjects measured under all conditions", "Data is approximately normal", "Sphericity (equal variances of differences between conditions)"),
     when = "Use when the same subjects/items are measured under 2+ conditions (within-subject design).",
     formula = "F = MS_condition / MS_error (within-subject)",
     interpret = "If p < 0.05, there is a significant effect of condition on the measures."
   ),
   # Non-parametric tests
   mann_whitney = list(
     name = "Mann-Whitney U Test (Wilcoxon Rank-Sum)",
     desc = "Non-parametric test comparing two independent groups based on ranks. Tests whether one group tends to have larger values than the other. Equivalent to the Wilcoxon rank-sum test.",
     assumptions = c("Two independent samples", "Ordinal or continuous data", "Observations are independent", "Similar shape of distributions (for median comparison interpretation)"),
     when = "Use instead of a two-sample t-test when data is non-normal, ordinal, or has outliers. Compares distributions/medians.",
     formula = "U = R\u2081 - n\u2081(n\u2081+1)/2, where R\u2081 = sum of ranks in group 1",
     interpret = "If p < 0.05, the distributions of the two groups differ significantly. Report U statistic, Z, and effect size r."
   ),
   kruskal = list(
     name = "Kruskal-Wallis Test",
     desc = "Non-parametric alternative to one-way ANOVA. Tests whether the distributions of 3+ independent groups differ.",
     assumptions = c("Independent samples", "Ordinal or continuous data", "Similar distribution shapes across groups"),
     when = "Use instead of ANOVA when data is non-normal or ordinal with 3+ groups.",
     formula = "H = [12/(N(N+1))] \u00d7 \u03a3(R\u1d62\u00b2/n\u1d62) - 3(N+1)",
     interpret = "If p < 0.05, at least one group differs. Follow up with pairwise Wilcoxon tests."
   ),
   chi_gof = list(
     name = "Chi-Squared Goodness of Fit",
     desc = "Tests whether observed frequencies match expected frequencies (proportions).",
     assumptions = c("Observations are independent", "Expected frequency in each category \u2265 5", "Data is categorical/count data"),
     when = "Use to test if observed category counts deviate from expected proportions.",
     formula = "\u03c7\u00b2 = \u03a3 (O\u1d62 - E\u1d62)\u00b2 / E\u1d62",
     interpret = "If p < 0.05, the observed distribution differs significantly from expected."
   ),
   # Paired tests
   paired_t = list(
     name = "Paired T-Test",
     desc = "Compares means from the same subjects measured at two time points or under two conditions.",
     assumptions = c("Paired/matched observations", "Differences are approximately normally distributed", "No significant outliers in differences"),
     when = "Use when comparing two related measurements (before/after, matched pairs). Each subject contributes one pair.",
     formula = "t = d\u0304 / (s_d / \u221an), where d = paired differences",
     interpret = "If p < 0.05, the mean difference between paired observations is significantly different from zero."
   ),
   wilcoxon_sr = list(
     name = "Wilcoxon Signed Rank Test",
     desc = "Non-parametric alternative to the paired t-test. Tests whether ranked paired differences are symmetric around zero.",
     assumptions = c("Paired/matched observations", "Differences are continuous and symmetric", "Data is at least ordinal"),
     when = "Use instead of paired t-test when differences are non-normal or data is ordinal.",
     formula = "W = sum of signed ranks of differences",
     interpret = "If p < 0.05, there is a significant difference between the paired conditions."
   ),
   friedman = list(
     name = "Friedman Test",
     desc = "Non-parametric alternative to repeated measures ANOVA. Tests differences across 3+ related groups using ranks.",
     assumptions = c("Same subjects measured under 3+ conditions", "Data is at least ordinal", "Observations within blocks are independent"),
     when = "Use instead of repeated measures ANOVA when data is non-normal or ordinal.",
     formula = "\u03c7\u00b2_F = [12/(nk(k+1))] \u00d7 \u03a3R\u1d62\u00b2 - 3n(k+1)",
     interpret = "If p < 0.05, at least one condition differs. Follow up with pairwise Wilcoxon signed-rank tests."
   ),
   # One-sample tests (duplicates with different context)
   one_prop = list(
     name = "One Sample Proportion Test",
     desc = "Tests whether an observed proportion equals a hypothesized value.",
     assumptions = c("Independent binary observations", "np \u2265 10 and n(1-p) \u2265 10", "Random sampling"),
     when = "Use to test if a proportion (success rate) equals a specific value.",
     formula = "\u03c7\u00b2 = (p\u0302 - p\u2080)\u00b2 / [p\u2080(1-p\u2080)/n]",
     interpret = "If p < 0.05, the observed proportion differs significantly from the hypothesized value."
   ),
   # Proportion tests
   two_prop = list(
     name = "Two Sample Proportion Test",
     desc = "Compares proportions from two independent groups.",
     assumptions = c("Independent samples", "Binary outcome", "Sufficient sample size (np \u2265 10)"),
     when = "Use when comparing success rates between two independent groups.",
     formula = "\u03c7\u00b2 compares observed vs. expected under H\u2080: p\u2081 = p\u2082",
     interpret = "If p < 0.05, the two proportions are significantly different."
   ),
   # Variance tests
   chi_var = list(
     name = "Chi-Squared Test for Variance",
     desc = "Tests whether the variance of a population equals a hypothesized value.",
     assumptions = c("Data is normally distributed", "Random sample", "Continuous data"),
     when = "Use to test if a population variance equals a specific value. Very sensitive to non-normality.",
     formula = "\u03c7\u00b2 = (n-1)s\u00b2 / \u03c3\u2080\u00b2",
     interpret = "If p < 0.05, the population variance differs significantly from the hypothesized variance."
   ),
   f_test = list(
     name = "F Test for Variances",
     desc = "Compares variances of two independent groups using the ratio of sample variances.",
     assumptions = c("Independent samples", "Both populations are normally distributed", "Continuous data"),
     when = "Use to check if two groups have equal variances (homogeneity). Often used before t-tests.",
     formula = "F = s\u2081\u00b2 / s\u2082\u00b2",
     interpret = "If p < 0.05, the variances of the two groups are significantly different."
   ),
   levene = list(
     name = "Levene's Test for Variances",
     desc = "Tests equality of variances across 2+ groups. More robust than the F-test as it is less sensitive to non-normality.",
     assumptions = c("Independent samples", "Continuous data", "No strong assumption of normality"),
     when = "Preferred over F-test for checking homogeneity of variances, especially with non-normal data.",
     formula = "Based on deviations from group medians (or means)",
     interpret = "If p < 0.05, the variances differ across groups \u2014 consider Welch's t-test or non-parametric tests."
   ),
   # Normality tests
   shapiro = list(
     name = "Shapiro-Wilk Normality Test",
     desc = "Tests whether data follows a normal distribution. One of the most powerful normality tests for small to moderate samples.",
     assumptions = c("Sample size between 3 and 5000", "Continuous data"),
     when = "Use to check normality before applying parametric tests. Most powerful for n < 50.",
     formula = "W = (\u03a3 a_i x_(i))\u00b2 / \u03a3(x_i - x\u0304)\u00b2",
     interpret = "If p < 0.05, the data significantly deviates from normality. Consider non-parametric tests."
   ),
   ks_one = list(
     name = "Kolmogorov-Smirnov Normality Test (Lilliefors)",
     desc = "Tests whether data follows a normal distribution using the maximum difference between empirical and theoretical CDF. Lilliefors correction is applied when parameters are estimated from the data.",
     assumptions = c("Continuous data", "Sample size \u2265 5"),
     when = "Use for larger samples. Less powerful than Shapiro-Wilk for small samples. Lilliefors correction is essential when mean and SD are estimated.",
     formula = "D = max|F_n(x) - F(x)| with Lilliefors correction",
     interpret = "If p < 0.05, the data does not follow a normal distribution."
   ),
   ks_two = list(
     name = "Two-Sample Kolmogorov-Smirnov Test",
     desc = "Tests whether two independent samples come from the same distribution.",
     assumptions = c("Independent samples", "Continuous data"),
     when = "Use to compare the overall distribution of two groups (not just location/spread, but entire shape).",
     formula = "D = max|F\u2081(x) - F\u2082(x)|",
     interpret = "If p < 0.05, the two samples come from significantly different distributions."
   ),
   # Survival & ROC
   km = list(
     name = "Kaplan-Meier Survival Analysis",
     desc = "Estimates survival function from time-to-event data. Log-rank test compares survival curves between groups.",
     assumptions = c("Censoring is non-informative", "Survival probabilities are the same regardless of early or late entry", "Events occur at specified times"),
     when = "Use for time-to-event data (survival, failure, relapse). Add strata to compare groups with log-rank test.",
     formula = "S(t) = \u220f[t_i \u2264 t] (1 - d_i/n_i)",
     interpret = "Log-rank p < 0.05 means survival curves differ significantly between groups."
   ),
   delong_ind = list(
     name = "DeLong Test - Independent ROC Curves",
     desc = "Compares AUCs of two ROC curves from independent models using DeLong's non-parametric method.",
     assumptions = c("Independent ROC curves", "Binary outcome variable", "Continuous predictor scores"),
     when = "Use to compare predictive performance of two independent diagnostic models.",
     formula = "Z = (AUC\u2081 - AUC\u2082) / SE(AUC\u2081 - AUC\u2082)",
     interpret = "If p < 0.05, the diagnostic accuracy of the two models differs significantly."
   ),
   delong_paired = list(
     name = "DeLong Test - Paired ROC Curves",
     desc = "Compares AUCs of two ROC curves derived from the same subjects using DeLong's method with paired variance estimation.",
     assumptions = c("Same subjects, two different predictors", "Binary outcome variable", "Continuous predictor scores"),
     when = "Use to compare two diagnostic tests applied to the same patients.",
     formula = "Z = (AUC\u2081 - AUC\u2082) / SE_paired(AUC\u2081 - AUC\u2082)",
     interpret = "If p < 0.05, one model has significantly better diagnostic performance."
   ),
   # Multi-factor tests
   two_anova = list(
     name = "Two Way ANOVA",
     desc = "Tests the effects of two factors and their interaction on a continuous outcome variable.",
     assumptions = c("Independent observations", "Data is approximately normal in each cell", "Homogeneity of variances", "Balanced or near-balanced design preferred"),
     when = "Use when two categorical factors may influence the outcome. Tests main effects and interaction.",
     formula = "F_A, F_B, F_AB = MS_effect / MS_error for each term",
     interpret = "Examines: (1) main effect of A, (2) main effect of B, (3) A\u00d7B interaction. p < 0.05 indicates a significant effect."
   ),
   manova = list(
     name = "One Way MANOVA",
     desc = "Tests whether group means differ on a combination of multiple dependent variables simultaneously.",
     assumptions = c("Multivariate normality", "Homogeneity of covariance matrices", "Independent observations", "No severe multicollinearity"),
     when = "Use when you have multiple response variables and one grouping factor. Controls Type I error better than running separate ANOVAs.",
     formula = "Pillai's trace, Wilks' lambda, or other multivariate statistics",
     interpret = "If p < 0.05, the groups differ significantly on the combined dependent variables."
   )
 )
 
 # Helper to render test info HTML
 render_test_info <- function(test_id) {
   info <- test_descriptions[[test_id]]
   if (is.null(info)) return(NULL)
   
   assumptions_html <- paste0("<li>", info$assumptions, "</li>", collapse = "")
   
   div(class = "test-info-box",
     h5(icon("info-circle"), " ", info$name),
     p(info$desc),
     p(strong("When to use: "), info$when),
     p(strong("Formula: "), span(class = "formula", info$formula)),
     p(strong("Assumptions:")),
     HTML(paste0("<ul>", assumptions_html, "</ul>")),
     p(strong("Interpretation: "), info$interpret)
   )
 }
 
 # --- Parametric test info ---
 output$test_info_parametric <- renderUI({
   req(input$show_info_parametric)
   if (!input$show_info_parametric) return(NULL)
   render_test_info(input$parametric_test)
 })
 
 # --- Non-parametric test info ---
 output$test_info_nonparam <- renderUI({
   req(input$show_info_nonparam)
   if (!input$show_info_nonparam) return(NULL)
   render_test_info(input$nonparam_test)
 })
 
 # --- Paired test info ---
 output$test_info_paired <- renderUI({
   req(input$show_info_paired)
   if (!input$show_info_paired) return(NULL)
   render_test_info(input$paired_test)
 })
 
 # --- One-sample test info ---
 output$test_info_onesample <- renderUI({
   req(input$show_info_onesample)
   if (!input$show_info_onesample) return(NULL)
   test_id <- input$onesample_test
   render_test_info(test_id)
 })
 
 # --- Proportion test info ---
 output$test_info_prop <- renderUI({
   req(input$show_info_prop)
   if (!input$show_info_prop) return(NULL)
   test_id <- input$prop_test
   render_test_info(test_id)
 })
 
 # --- Variance test info ---
 output$test_info_var <- renderUI({
   req(input$show_info_var)
   if (!input$show_info_var) return(NULL)
   render_test_info(input$var_test)
 })
 
 # --- Normality test info ---
 output$test_info_norm <- renderUI({
   req(input$show_info_norm)
   if (!input$show_info_norm) return(NULL)
   render_test_info(input$norm_test)
 })
 
 # --- Survival/ROC test info ---
 output$test_info_surv <- renderUI({
   req(input$show_info_surv)
   if (!input$show_info_surv) return(NULL)
   render_test_info(input$surv_test)
 })
 
 # --- Multi-factor test info ---
 output$test_info_multi <- renderUI({
   req(input$show_info_multi)
   if (!input$show_info_multi) return(NULL)
   render_test_info(input$multi_test)
 })
 
 # ===========================================================================
 # RESULTS TAB
 # ===========================================================================
 
 # Descriptive statistics (shared reactive)
 desc_stats_data <- reactive({
   req(rv$data)
   stats_df <- do.call(rbind, lapply(names(rv$data), function(g) {
     x <- rv$data[[g]]
     data.frame(
       Group = g,
       N = length(x),
       Mean = mean(x, na.rm = TRUE),
       SD = sd(x, na.rm = TRUE),
       SE = sd(x, na.rm = TRUE) / sqrt(length(x)),
       Median = median(x, na.rm = TRUE),
       Q1 = quantile(x, 0.25, na.rm = TRUE),
       Q3 = quantile(x, 0.75, na.rm = TRUE),
       IQR = IQR(x, na.rm = TRUE),
       Min = min(x, na.rm = TRUE),
       Max = max(x, na.rm = TRUE),
       Skewness = {
         n <- length(x)
         if (n < 3) NA_real_ else {
           m <- mean(x); s <- sd(x)
           (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
         }
       },
       Kurtosis = {
         n <- length(x)
         if (n < 4) NA_real_ else {
           m <- mean(x); s <- sd(x)
           (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - m) / s)^4) -
             3 * (n - 1)^2 / ((n - 2) * (n - 3))
         }
       },
       stringsAsFactors = FALSE
     )
   }))
   rownames(stats_df) <- NULL
   stats_df
 })
 
 output$desc_stats_table <- renderDT({
   datatable(desc_stats_data(), options = list(
     pageLength = 20, scrollX = TRUE, dom = 'frtip'),
     rownames = FALSE) %>%
     formatRound(columns = c("Mean", "SD", "SE", "Median", "Q1", "Q3",
       "IQR", "Min", "Max", "Skewness", "Kurtosis"), digits = 4)
 })
 
 # Download descriptive stats as CSV
 output$download_desc_csv <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_DescStats_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
   },
   content = function(file) {
     write.csv(desc_stats_data(), file, row.names = FALSE)
   }
 )
 
 # Download descriptive stats as Excel
 output$download_desc_xlsx <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_DescStats_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
   },
   content = function(file) {
     if (requireNamespace("writexl", quietly = TRUE)) {
       writexl::write_xlsx(desc_stats_data(), file)
     } else {
       write.csv(desc_stats_data(), file, row.names = FALSE)
       showNotification("writexl not installed. Saved as CSV instead.",
         type = "warning")
     }
   }
 )
 
 # All test results display
 output$all_results_display <- renderUI({
   if (length(rv$results_history) == 0) {
     return(div(class = "stat-card",
       p(em("No tests have been run yet. Go to a test tab and run a test."))))
   }
   
   result_blocks <- lapply(rev(rv$results_history), function(txt) {
     div(style = "margin-bottom: 10px;",
       pre(style = "background: #f8f9fa; border: 1px solid #e0e0e0;
                    border-radius: 6px; padding: 12px; font-size: 12px;
                    white-space: pre-wrap; word-wrap: break-word;", txt)
     )
   })
   do.call(tagList, result_blocks)
 })
 
 # Download results as TXT
 output$download_results_txt <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_Results_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
   },
   content = function(file) {
     header <- paste0(
       "ShinyStatR - Statistical Analysis Results\n",
       "Generated: ", Sys.time(), "\n",
       "═══════════════════════════════════════════════════════\n\n"
     )
     txt <- paste(c(header, unlist(rv$results_history)), collapse = "\n\n")
     writeLines(txt, file)
   }
 )
 
 # Clear results
 observeEvent(input$clear_results, {
   rv$results_history <- list()
   rv$current_result <- NULL
   showNotification("Results history cleared.", type = "warning")
 })
 
 # Raw data table
 output$raw_data_table <- renderDT({
   req(rv$data)
   max_len <- max(sapply(rv$data, length))
   df <- as.data.frame(lapply(rv$data, function(x) {
     c(x, rep(NA, max_len - length(x)))
   }))
   datatable(df, options = list(
     pageLength = 25, scrollX = TRUE, dom = 'frtip'),
     rownames = FALSE) %>%
     formatRound(columns = names(df), digits = 4)
 })
 
 # Download data as CSV
 output$download_data_csv <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_Data_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
   },
   content = function(file) {
     max_len <- max(sapply(rv$data, length))
     df <- as.data.frame(lapply(rv$data, function(x) {
       c(x, rep(NA, max_len - length(x)))
     }))
     write.csv(df, file, row.names = FALSE)
   }
 )
 
 # ===========================================================================
 # PLOTS
 # ===========================================================================
 
 # Build the long-format data for plotting
 plot_data <- reactive({
   req(rv$data)
   df <- do.call(rbind, lapply(names(rv$data), function(g) {
     data.frame(group = g, value = rv$data[[g]],
       stringsAsFactors = FALSE)
   }))
   df$group <- factor(df$group, levels = names(rv$data))
   df
 })
 
 # Helper: custom whisker stat function for percentile/SD box plots
 whisker_stat_fun <- function(wtype) {
   function(x) {
     qs <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
     if (wtype == "percentile") {
       lo <- quantile(x, 0.05, na.rm = TRUE)
       hi <- quantile(x, 0.95, na.rm = TRUE)
     } else {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       lo <- m - s; hi <- m + s
     }
     data.frame(ymin = unname(lo), lower = unname(qs[1]),
                middle = unname(qs[2]), upper = unname(qs[3]),
                ymax = unname(hi))
   }
 }

 # Build the main plot
 build_plot <- reactive({
   req(plot_data())
   df <- plot_data()
   pt <- input$plot_type
   
   # Base aesthetics
   p <- ggplot(df, aes(x = group, y = value))
   
   # Color / fill logic (only map fill, not color — border is user-controlled)
   if (input$plot_use_group_colors) {
     p <- ggplot(df, aes(x = group, y = value, fill = group))
   }
   
   # === Plot geometries ===
   width <- input$plot_width_adj
   alpha <- input$plot_alpha
   lw <- input$plot_line_width
   ps <- input$plot_point_size
   
   # Border color logic
   # Helper: darken a hex color for a matching border
   darken_hex <- function(hex, factor = 0.35) {
     rgb_vals <- col2rgb(hex)
     darkened <- rgb_vals * (1 - factor)
     rgb(darkened[1], darkened[2], darkened[3], maxColorValue = 255)
   }
   
   if (input$plot_use_group_colors) {
     n_groups <- length(unique(df$group))
     pal <- input$plot_palette
     
     if (!is.null(pal) && pal == "custom") {
       # Custom: use user-picked border colors
       border_cols <- sapply(seq_len(n_groups), function(i) {
         val <- input[[paste0("custom_border_", i)]]
         if (is.null(val)) "#2c3e50" else val
       })
     } else {
       # All other palettes: auto-darken fill colors for borders
       fill_colors <- get_palette_colors(
         if (is.null(pal)) "default" else pal, n_groups)
       if (!is.null(fill_colors)) {
         border_cols <- unname(sapply(fill_colors, darken_hex))
       } else {
         border_cols <- rep("#2c3e50", n_groups)
       }
     }
     border_col <- border_cols[1]
   } else {
     border_col <- if (!is.null(input$plot_border_color))
       input$plot_border_color else "#2c3e50"
     border_cols <- NULL
   }
   
   # Whisker type: default, min/max, percentile, or SD
   whisker_type <- if (!is.null(input$plot_whisker_type)) input$plot_whisker_type else "default"
   bx_coef <- if (whisker_type == "minmax") Inf else 1.5
   use_stat_whisker <- whisker_type %in% c("percentile", "sd")
   
   if (pt == "box") {
     if (use_stat_whisker) {
       if (!is.null(border_cols)) {
         p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
           aes(color = group), width = width, alpha = alpha, linewidth = lw)
       } else {
         p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
           width = width, alpha = alpha, linewidth = lw, color = border_col,
           fill = if (!input$plot_use_group_colors) input$plot_fill_color else NA)
       }
     } else {
       if (!is.null(border_cols)) {
         p <- p + geom_boxplot(aes(color = group), width = width, alpha = alpha,
           linewidth = lw, outlier.size = ps, coef = bx_coef)
       } else {
         p <- p + geom_boxplot(width = width, alpha = alpha,
           linewidth = lw, outlier.size = ps, color = border_col, coef = bx_coef,
           fill = if (!input$plot_use_group_colors) input$plot_fill_color else NA)
       }
     }
     
   } else if (pt == "violin") {
     if (!is.null(border_cols)) {
       p <- p + geom_violin(aes(color = group), width = width, alpha = alpha,
         linewidth = lw, trim = FALSE)
     } else {
       p <- p + geom_violin(width = width, alpha = alpha,
         linewidth = lw, trim = FALSE, color = border_col,
         fill = if (!input$plot_use_group_colors) input$plot_fill_color else NA)
     }
     
   } else if (pt == "dot") {
     if (!is.null(border_cols)) {
       p <- p + geom_jitter(aes(color = group), width = 0.15,
         size = ps, alpha = alpha)
     } else {
       p <- p + geom_jitter(width = 0.15, size = ps, alpha = alpha,
         color = border_col)
     }
     
   } else if (pt == "swarm") {
     if (!is.null(border_cols)) {
       p <- p + ggbeeswarm::geom_beeswarm(aes(color = group),
         size = ps, alpha = alpha, cex = 2)
     } else {
       p <- p + ggbeeswarm::geom_beeswarm(size = ps, alpha = alpha,
         cex = 2, color = border_col)
     }
     
   } else if (pt == "bar" || pt == "bar_sd") {
     # Use stat_summary on raw df so p always has raw data (fixes significance)
     err_fun <- if (pt == "bar") {
       mean_se
     } else {
       function(x, ...) {
         m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
         data.frame(y = m, ymin = m - s, ymax = m + s)
       }
     }
     eb_w <- if (!is.null(input$plot_errorbar_width)) input$plot_errorbar_width else 0.2
     
     if (input$plot_use_group_colors) {
       if (!is.null(border_cols)) {
         p <- p +
           stat_summary(fun = mean, geom = "col", aes(color = group),
             width = width, alpha = alpha, linewidth = lw) +
           stat_summary(fun.data = err_fun, geom = "errorbar",
             aes(color = group), width = eb_w, linewidth = lw)
       } else {
         p <- p +
           stat_summary(fun = mean, geom = "col",
             width = width, alpha = alpha, color = border_col,
             linewidth = lw) +
           stat_summary(fun.data = err_fun, geom = "errorbar",
             width = eb_w, linewidth = lw, color = border_col)
       }
     } else {
       p <- p +
         stat_summary(fun = mean, geom = "col",
           width = width, alpha = alpha,
           fill = input$plot_fill_color,
           color = border_col, linewidth = lw) +
         stat_summary(fun.data = err_fun, geom = "errorbar",
           width = eb_w, linewidth = lw, color = border_col)
     }
     
   } else if (pt == "box_jitter") {
     if (use_stat_whisker) {
       if (!is.null(border_cols)) {
         p <- p +
           stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
             aes(color = group), width = width, alpha = alpha, linewidth = lw) +
           geom_jitter(aes(color = group), width = 0.15, size = ps, alpha = 0.5)
       } else {
         fill_val <- if (!input$plot_use_group_colors) input$plot_fill_color else NA
         p <- p +
           stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
             width = width, alpha = alpha, linewidth = lw, color = border_col, fill = fill_val) +
           geom_jitter(width = 0.15, size = ps, alpha = 0.5, color = border_col)
       }
     } else {
       if (!is.null(border_cols)) {
         p <- p +
           geom_boxplot(aes(color = group), width = width, alpha = alpha,
             linewidth = lw, outlier.shape = NA, coef = bx_coef) +
           geom_jitter(aes(color = group), width = 0.15, size = ps, alpha = 0.5)
       } else {
         fill_val <- if (!input$plot_use_group_colors) input$plot_fill_color else NA
         p <- p +
           geom_boxplot(width = width, alpha = alpha, linewidth = lw,
             outlier.shape = NA, color = border_col, fill = fill_val, coef = bx_coef) +
           geom_jitter(width = 0.15, size = ps, alpha = 0.5, color = border_col)
       }
     }
     
   } else if (pt == "violin_jitter") {
     if (!is.null(border_cols)) {
       p <- p +
         geom_violin(aes(color = group), width = width, alpha = alpha,
           linewidth = lw, trim = FALSE) +
         geom_jitter(aes(color = group), width = 0.15,
           size = ps, alpha = 0.5)
     } else {
       fill_val <- if (!input$plot_use_group_colors) input$plot_fill_color else NA
       p <- p +
         geom_violin(width = width, alpha = alpha, linewidth = lw,
           trim = FALSE, color = border_col, fill = fill_val) +
         geom_jitter(width = 0.15, size = ps, alpha = 0.5,
           color = border_col)
     }
     
   } else if (pt == "violin_box") {
     if (use_stat_whisker) {
       if (!is.null(border_cols)) {
         p <- p +
           geom_violin(aes(color = group), width = width, alpha = alpha,
             linewidth = lw, trim = FALSE) +
           stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
             aes(color = group), width = width * 0.2, alpha = 0.8, linewidth = lw)
       } else {
         fill_val <- if (!input$plot_use_group_colors) input$plot_fill_color else NA
         p <- p +
           geom_violin(width = width, alpha = alpha, linewidth = lw,
             trim = FALSE, color = border_col, fill = fill_val) +
           stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
             width = width * 0.2, alpha = 0.8, linewidth = lw, color = border_col, fill = fill_val)
       }
     } else {
       if (!is.null(border_cols)) {
         p <- p +
           geom_violin(aes(color = group), width = width, alpha = alpha,
             linewidth = lw, trim = FALSE) +
           geom_boxplot(aes(color = group), width = width * 0.2,
             alpha = 0.8, linewidth = lw, outlier.shape = NA, coef = bx_coef)
       } else {
         fill_val <- if (!input$plot_use_group_colors) input$plot_fill_color else NA
         p <- p +
           geom_violin(width = width, alpha = alpha, linewidth = lw,
             trim = FALSE, color = border_col, fill = fill_val) +
           geom_boxplot(width = width * 0.2, alpha = 0.8, linewidth = lw,
             outlier.shape = NA, color = border_col, fill = fill_val, coef = bx_coef)
       }
     }
     
   } else if (pt == "mean_dot") {
     # Use stat_summary on raw df so p always has raw data (fixes significance)
     sd_err_fun <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     if (input$plot_use_group_colors) {
       if (!is.null(border_cols)) {
         p <- p +
           stat_summary(fun = mean, geom = "point", aes(color = group),
             size = ps * 2) +
           stat_summary(fun.data = sd_err_fun, geom = "errorbar",
             aes(color = group), width = 0.2, linewidth = lw)
       } else {
         p <- p +
           stat_summary(fun = mean, geom = "point",
             size = ps * 2, color = border_col) +
           stat_summary(fun.data = sd_err_fun, geom = "errorbar",
             width = 0.2, linewidth = lw, color = border_col)
       }
     } else {
       p <- p +
         stat_summary(fun = mean, geom = "point",
           size = ps * 2, color = input$plot_point_color) +
         stat_summary(fun.data = sd_err_fun, geom = "errorbar",
           width = 0.2, linewidth = lw, color = border_col)
     }
     
   } else if (pt == "histogram") {
     if (!is.null(border_cols)) {
       p <- ggplot(df, aes(x = value, color = group)) +
         geom_histogram(aes(fill = group), alpha = alpha,
           position = "identity", bins = 30, linewidth = lw) +
         facet_wrap(~group, scales = "free_y")
     } else {
       p <- ggplot(df, aes(x = value)) +
         geom_histogram(aes(fill = group), alpha = alpha,
           position = "identity", bins = 30, linewidth = lw,
           color = border_col) +
         facet_wrap(~group, scales = "free_y")
     }
     
   } else if (pt == "density") {
     if (!is.null(border_cols)) {
       p <- ggplot(df, aes(x = value, fill = group, color = group)) +
         geom_density(alpha = alpha, linewidth = lw)
     } else {
       p <- ggplot(df, aes(x = value, fill = group)) +
         geom_density(alpha = alpha, linewidth = lw,
           color = border_col)
     }
     
   } else if (pt == "qq") {
     if (!is.null(border_cols)) {
       p <- ggplot(df, aes(sample = value, color = group)) +
         stat_qq(size = ps) +
         stat_qq_line(linewidth = lw) +
         facet_wrap(~group, scales = "free")
     } else {
       p <- ggplot(df, aes(sample = value)) +
         stat_qq(size = ps, color = border_col) +
         stat_qq_line(linewidth = lw, color = border_col) +
         facet_wrap(~group, scales = "free")
     }
   }
   
   # === Compute group label overrides (used for x-ticks, legend, fills) ===
   grp_labels <- NULL
   if (!is.null(rv$group_names) && length(rv$group_names) > 0 &&
       !(pt %in% c("histogram", "density", "qq"))) {
     grp_labels <- sapply(seq_along(rv$group_names), function(i) {
       val <- input[[paste0("plot_grp_label_", i)]]
       if (!is.null(val) && nzchar(val)) val else rv$group_names[i]
     })
     names(grp_labels) <- rv$group_names
   }
   
   # === Non-grouped fill/color override ===
   if (!input$plot_use_group_colors &&
       !(pt %in% c("bar", "bar_sd", "mean_dot", "histogram", "density"))) {
     p <- p +
       scale_fill_manual(values = rep(input$plot_fill_color,
         length(unique(df$group))),
         labels = if (!is.null(grp_labels)) grp_labels else waiver())
   }
   
   # === Color palette (fill only — border handled per-geom) ===
   if (input$plot_use_group_colors) {
     pal <- input$plot_palette
     n_groups <- length(unique(df$group))
     if (pal == "custom") {
       # Custom user-defined colors per group
       custom_cols <- sapply(seq_len(n_groups), function(i) {
         val <- input[[paste0("custom_col_", i)]]
         if (is.null(val)) "#3c8dbc" else val
       })
       p <- p + scale_fill_manual(values = custom_cols,
         labels = if (!is.null(grp_labels)) grp_labels else waiver())
     } else {
       pal_colors <- get_palette_colors(pal, n_groups)
       if (!is.null(pal_colors)) {
         p <- p + scale_fill_manual(values = pal_colors,
           labels = if (!is.null(grp_labels)) grp_labels else waiver())
       }
     }
   }
   
   # === Per-group border colors ===
   if (!is.null(border_cols)) {
     p <- p + scale_color_manual(values = border_cols, guide = "none")
   }
   
   # === Overlays ===
   if (input$plot_show_mean &&
       !(pt %in% c("bar", "bar_sd", "mean_dot", "histogram",
                    "density", "qq"))) {
     p <- p + stat_summary(fun = mean, geom = "point",
       shape = 18, size = ps * 1.5, color = "#e74c3c")
   }
   
   if (input$plot_show_errorbar &&
       !(pt %in% c("bar", "bar_sd", "mean_dot", "histogram",
                    "density", "qq"))) {
     eb_type <- input$plot_errorbar_type
     eb_fun <- switch(eb_type,
       "se" = mean_se,
       "sd" = function(x, ...) { m <- mean(x); s <- sd(x); data.frame(y = m, ymin = m - s, ymax = m + s) },
       "ci" = mean_cl_normal,
       "iqr" = median_hilow
     )
     p <- p + stat_summary(fun.data = eb_fun, geom = "errorbar",
       width = input$plot_errorbar_width, linewidth = lw)
   }
   
   if (input$plot_show_jitter &&
       !(pt %in% c("dot", "swarm", "box_jitter", "violin_jitter",
                    "histogram", "density", "qq"))) {
     p <- p + geom_jitter(width = input$plot_jitter_width,
       alpha = input$plot_jitter_alpha, size = ps * 0.7)
   }
   
   # === Significance bars ===
   # Uses mp_compute_pvalue / mp_compute_pvalue_multi for consistent p-values
   # across bracket bars and text annotation styles.
   if (input$plot_show_signif &&
       !(pt %in% c("histogram", "density", "qq"))) {
     tryCatch({
       method <- input$plot_signif_method
       label <- input$plot_signif_label
       
       # Skip disabled header selections
       if (!is.null(method) && !grepl("^header_", method)) {
         # Map main-plot method names to mp_compute_pvalue method names
         mp_method <- switch(method,
           "t.test.pooled" = "t.test",
           "t.test"        = "welch",
           "wilcox.test"   = "wilcox.test",
           NULL
         )
         
         if (!is.null(mp_method)) {
           groups <- levels(df$group)
           if (length(groups) >= 2) {
             comps <- combn(groups, 2, simplify = FALSE)
             # Compute p-values & labels for each comparison
             annotations <- sapply(comps, function(pair) {
               g1 <- df$value[df$group == pair[1]]
               g2 <- df$value[df$group == pair[2]]
               pv <- mp_compute_pvalue(g1, g2, mp_method)
               mp_format_p_label(pv, label)
             })
             # Compute y positions for brackets
             y_max <- max(df$value, na.rm = TRUE)
             y_range <- diff(range(df$value, na.rm = TRUE))
             y_start <- y_max + y_range * 0.05
             y_positions <- y_start + seq(0, length(comps) - 1) * y_range * input$plot_signif_step
             
             p <- p + ggsignif::geom_signif(
               comparisons = comps,
               annotations = annotations,
               y_position = y_positions,
               textsize = input$plot_signif_text_size,
               vjust = -0.2,
               tip_length = 0.02
             )
           }
         } else if (method %in% c("anova", "kruskal.test")) {
           # Multi-group: single overall annotation
           mp_multi_method <- switch(method,
             "anova" = "anova", "kruskal.test" = "kruskal", method)
           pv <- mp_compute_pvalue_multi(df$value, df$group, mp_multi_method)
           p_label <- if (!is.na(pv)) mp_format_p_label(pv, label) else ""
           if (nzchar(p_label)) {
             y_pos <- max(df$value, na.rm = TRUE) +
               diff(range(df$value, na.rm = TRUE)) * 0.08
             p <- p + annotate("text", x = Inf, y = y_pos, label = p_label,
                               hjust = 1.1, size = input$plot_signif_text_size,
                               fontface = "bold", color = "#333333")
           }
         }
       }
     }, error = function(e) {
       message("Significance annotation error: ", e$message)
     })
   }
   
   # === Labels ===
   title_text <- if (input$plot_title != "") input$plot_title else NULL
   xlab_text <- if (input$plot_xlab != "") input$plot_xlab else "Group"
   ylab_text <- if (input$plot_ylab != "") input$plot_ylab else "Value"
   
   p <- p + labs(title = title_text, x = xlab_text, y = ylab_text)
   
   # === Group label overrides (x-axis ticks) ===
   if (!is.null(grp_labels)) {
     p <- p + scale_x_discrete(labels = grp_labels)
   }
   
   # === Theme ===
   base_theme <- switch(input$plot_theme,
     "classic" = theme_classic(),
     "minimal" = theme_minimal(),
     "bw" = theme_bw(),
     "light" = theme_light(),
     "pubr" = theme_pubr(),
     "void" = theme_void()
   )
   
   # Title face
   title_face <- "plain"
   if (input$plot_title_bold && input$plot_title_italic) {
     title_face <- "bold.italic"
   } else if (input$plot_title_bold) {
     title_face <- "bold"
   } else if (input$plot_title_italic) {
     title_face <- "italic"
   }
   
   axis_title_face <- "plain"
   if (input$plot_axis_title_bold && input$plot_axis_title_italic) {
     axis_title_face <- "bold.italic"
   } else if (input$plot_axis_title_bold) {
     axis_title_face <- "bold"
   } else if (input$plot_axis_title_italic) {
     axis_title_face <- "italic"
   }
   
   p <- p + base_theme +
     theme(
       plot.title = element_markdown(
         size = input$plot_title_size, face = title_face,
         hjust = 0.5),
       axis.title.x = element_markdown(
         size = input$plot_axis_title_size, face = axis_title_face),
       axis.title.y = element_markdown(
         size = input$plot_axis_title_size, face = axis_title_face),
       axis.text.x = element_markdown(size = input$plot_axis_text_size),
       axis.text.y = element_markdown(size = input$plot_axis_text_size),
       legend.text = element_markdown(size = input$plot_legend_size),
       legend.title = element_markdown(size = input$plot_legend_size + 1),
       panel.background = element_rect(fill = input$plot_bg_color),
       plot.background = element_rect(fill = input$plot_bg_color,
         color = NA)
     )
   
   # Underline title
   if (input$plot_underline_title && !is.null(title_text)) {
     p <- p + theme(
       plot.title = element_markdown(
         size = input$plot_title_size, face = title_face,
         hjust = 0.5,
         margin = margin(b = 5),
         lineheight = 1.2
       )
     )
   }
   
   # Legend
   if (!input$plot_show_legend) {
     p <- p + theme(legend.position = "none")
   }
   
   # Coord flip
   if (input$plot_coord_flip &&
       !(pt %in% c("histogram", "density", "qq"))) {
     p <- p + coord_flip()
   }
   
   p
 })
 
 # -----------------------------------------------------------------------
 # Palette color definitions (shared helper)
 # -----------------------------------------------------------------------
 get_palette_colors <- function(pal, n = 8) {
   colors <- switch(pal,
     "default" = c("#3c8dbc", "#e74c3c", "#00a65a", "#f39c12",
       "#605ca8", "#d81b60", "#39cccc", "#3d9970",
       "#ff851b", "#001f3f", "#85144b", "#7fdbff"),
     "bw_pal" = {
       # White fills with increasing grey for distinction, black borders
       greys <- c("#ffffff", "#d9d9d9", "#969696", "#636363",
         "#bdbdbd", "#252525", "#f0f0f0", "#cccccc")
       greys
     },
     "grey_pal" = {
       if (n <= 1) "#737373"
       else grDevices::grey.colors(n, start = 0.3, end = 0.85)
     },
     "npg" = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488",
       "#F39B7F", "#8491B4", "#91D1C2", "#DC0000",
       "#7E6148", "#B09C85"),
     "aaas" = c("#3B4992", "#EE0000", "#008B45", "#631879",
       "#008280", "#BB0021", "#5F559B", "#A20056",
       "#808180", "#1B1919"),
     "lancet" = c("#00468B", "#ED0000", "#42B540", "#0099B4",
       "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919"),
     "jco" = c("#0073C2", "#EFC000", "#868686", "#CD534C",
       "#7AA6DC", "#003C67", "#8F7700", "#3B3B3B",
       "#A73030", "#4A6990"),
     "nejm" = c("#BC3C29", "#0072B5", "#E18727", "#20854E",
       "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
     NULL  # for RColorBrewer palettes, handled below
   )
   if (is.null(colors)) {
     colors <- tryCatch(
       RColorBrewer::brewer.pal(max(3, n), pal),
       error = function(e) NULL
     )
   }
   if (!is.null(colors)) {
     if (n > length(colors)) colors <- rep_len(colors, n)
     colors[1:n]
   } else NULL
 }
 
 # -----------------------------------------------------------------------
 # Group label overrides (Plots tab — allows HTML markup in tick labels)
 # -----------------------------------------------------------------------
 output$plot_group_labels_ui <- renderUI({
   gnames <- rv$group_names
   if (is.null(gnames) || length(gnames) == 0) return(NULL)
   tagList(
     hr(),
     tags$label(style = "font-weight:700; font-size:13px;", "Group Display Labels"),
     helpText(style = "font-size:11px; color:#607d8b; margin-top:2px; margin-bottom:6px;",
       "Override x-axis tick labels. HTML markup supported."),
     lapply(seq_along(gnames), function(i) {
       textInput(paste0("plot_grp_label_", i),
         label = paste0("Group ", i, " (", gnames[i], "):"),
         value = gnames[i])
     })
   )
 })

 # MP tab group label overrides
 output$mp_group_labels_ui <- renderUI({
   df <- mp_data()
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(grp_levels) || length(grp_levels) == 0) return(NULL)
   tagList(
     hr(),
     tags$label(style = "font-weight:700; font-size:13px;", "Group Display Labels"),
     helpText(style = "font-size:11px; color:#607d8b; margin-top:2px; margin-bottom:6px;",
       "Override x-axis group tick labels. HTML markup supported",
       "(e.g. <b>Treatment</b> for bold)."),
     lapply(seq_along(grp_levels), function(i) {
       textInput(paste0("mp_grp_label_", i),
         label = paste0("Group ", i, " (", grp_levels[i], "):"),
         value = grp_levels[i])
     })
   )
 })

 # -----------------------------------------------------------------------
 # Palette preview swatches
 # -----------------------------------------------------------------------
 output$palette_preview <- renderUI({
   pal <- input$plot_palette
   if (is.null(pal) || pal == "custom") return(NULL)
   n <- if (!is.null(rv$data)) length(rv$data) else 6
   cols <- get_palette_colors(pal, max(n, 3))
   if (is.null(cols)) return(NULL)
   swatches <- lapply(cols, function(col) {
     tags$span(style = paste0(
       "display:inline-block; width:24px; height:18px; margin:1px;",
       "border-radius:3px; border:1px solid #aaa;",
       "background-color:", col, ";"
     ))
   })
   div(style = "margin: 5px 0 10px 0;",
     tags$small(tags$strong("Palette colors:")),
     br(),
     do.call(tagList, swatches)
   )
 })
 
 # -----------------------------------------------------------------------
 # Custom per-group color pickers
 # -----------------------------------------------------------------------
 output$custom_group_colors_ui <- renderUI({
   pal <- input$plot_palette
   if (is.null(pal) || pal != "custom") return(NULL)
   req(rv$data)
   groups <- names(rv$data)
   n <- length(groups)
   fallback <- get_palette_colors("default", n)
   
   # Helper: darken a hex colour for matching border
   darken_color <- function(hex, factor = 0.35) {
     rgb_vals <- col2rgb(hex)
     darkened <- rgb_vals * (1 - factor)
     rgb(darkened[1], darkened[2], darkened[3], maxColorValue = 255)
   }
   
   color_inputs <- lapply(seq_along(groups), function(i) {
     fill_val <- fallback[min(i, length(fallback))]
     border_val <- darken_color(fill_val)
     fluidRow(
       column(6,
         colourInput(
           inputId = paste0("custom_col_", i),
           label = paste0(groups[i], " Fill:"),
           value = fill_val
         )
       ),
       column(6,
         colourInput(
           inputId = paste0("custom_border_", i),
           label = paste0(groups[i], " Border:"),
           value = border_val
         )
       )
     )
   })
   div(style = "margin-top: 8px;",
     tags$small(tags$strong("Custom group colors:")),
     do.call(tagList, color_inputs)
   )
 })
 
 # -----------------------------------------------------------------------
 # Multi-Parameter: Palette preview swatches
 # -----------------------------------------------------------------------
 output$mp_palette_preview <- renderUI({
   pal <- input$mp_palette
   if (is.null(pal) || pal == "custom") return(NULL)
   grp_levels <- mp_grp_levels()
   n <- if (!is.null(grp_levels)) length(grp_levels) else 6
   cols <- get_palette_colors(pal, max(n, 3))
   if (is.null(cols)) return(NULL)
   swatches <- lapply(cols, function(col) {
     tags$span(style = paste0(
       "display:inline-block; width:24px; height:18px; margin:1px;",
       "border-radius:3px; border:1px solid #aaa;",
       "background-color:", col, ";"
     ))
   })
   div(style = "margin: 5px 0 10px 0;",
     tags$small(tags$strong("Palette colors:")),
     br(),
     do.call(tagList, swatches)
   )
 })
 
 # -----------------------------------------------------------------------
 # Multi-Parameter: Custom per-group color pickers
 # -----------------------------------------------------------------------
 output$mp_custom_group_colors_ui <- renderUI({
   pal <- input$mp_palette
   if (is.null(pal) || pal != "custom") return(NULL)
   grp_levels <- mp_grp_levels()
   req(grp_levels)
   n <- length(grp_levels)
   fallback <- get_palette_colors("default", n)
   
   color_inputs <- lapply(seq_along(grp_levels), function(i) {
     fill_val <- fallback[min(i, length(fallback))]
     fluidRow(
       column(12,
         colourInput(
           inputId = paste0("mp_custom_col_", i),
           label = paste0(grp_levels[i], " Color:"),
           value = fill_val
         )
       )
     )
   })
   div(style = "margin-top: 8px;",
     tags$small(tags$strong("Custom group colors:")),
     do.call(tagList, color_inputs)
   )
 })
 
 # -----------------------------------------------------------------------
 # Dynamic plot container resize (WYSIWYG) — capped to avoid overflow
 # -----------------------------------------------------------------------
 MAX_DISPLAY_H <- 700  # max pixel height for preview
 
 observe({
   w <- input$plot_download_width
   h <- input$plot_download_height
   if (is.null(w) || is.null(h) || w <= 0 || h <= 0) return()
   cw <- session$clientData$output_main_plot_width
   if (is.null(cw) || cw <= 0) cw <- 700
   display_h <- min(round(cw * (h / w)), MAX_DISPLAY_H)
   session$sendCustomMessage("resizePlotContainer", list(height = display_h))
 })
 
 # Render plot (WYSIWYG: match download aspect ratio, capped height)
 output$main_plot <- renderPlot({
   build_plot()
 }, res = 96,
   width = function() {
     cw <- session$clientData$output_main_plot_width
     if (is.null(cw) || cw <= 0) cw <- 700
     w <- input$plot_download_width
     h <- input$plot_download_height
     if (is.null(w) || is.null(h) || w <= 0 || h <= 0) return(as.integer(cw))
     target_h <- round(cw * (h / w))
     if (target_h > MAX_DISPLAY_H) {
       # Scale width down proportionally
       as.integer(round(MAX_DISPLAY_H * (w / h)))
     } else {
       as.integer(cw)
     }
   },
   height = function() {
     cw <- session$clientData$output_main_plot_width
     if (is.null(cw) || cw <= 0) cw <- 700
     w <- input$plot_download_width
     h <- input$plot_download_height
     if (is.null(w) || is.null(h) || w <= 0 || h <= 0) return(525L)
     as.integer(min(round(cw * (h / w)), MAX_DISPLAY_H))
   }
 )
 
 # Plot summary text
 output$plot_summary_text <- renderText({
   req(rv$data)
   df <- plot_data()
   
   summ_lines <- sapply(names(rv$data), function(g) {
     x <- rv$data[[g]]
     paste0(g, ": n=", length(x),
       ", mean=", round(mean(x), 4),
       ", sd=", round(sd(x), 4),
       ", median=", round(median(x), 4))
   })
   paste(summ_lines, collapse = "\n")
 })
 
 # Download PNG
 output$download_plot_png <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_Plot_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
   },
   content = function(file) {
     ggsave(file, plot = build_plot(),
       width = input$plot_download_width / 2.54,
       height = input$plot_download_height / 2.54,
       dpi = input$plot_download_dpi,
       device = "png", bg = input$plot_bg_color)
   }
 )
 
 # Download SVG
 output$download_plot_svg <- downloadHandler(
   filename = function() {
     paste0("ShinyStatR_Plot_",
       format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
   },
   content = function(file) {
     svglite::svglite(file,
       width = input$plot_download_width / 2.54,
       height = input$plot_download_height / 2.54,
       bg = input$plot_bg_color)
     print(build_plot())
     dev.off()
   }
 )
 
 # -----------------------------------------------------------------------
 # Sync color pickers with palette / theme
 # -----------------------------------------------------------------------
 observeEvent(input$plot_palette, {
   pal <- input$plot_palette
   if (pal == "custom") return()  # don't override for custom
   n <- if (!is.null(rv$data)) length(rv$data) else 6
   cols <- get_palette_colors(pal, max(n, 3))
   if (!is.null(cols)) {
     colourpicker::updateColourInput(session, "plot_fill_color",
       value = cols[1])
     border_col <- if (pal == "bw_pal") "#000000"
       else if (pal == "grey_pal") "#333333"
       else cols[min(2, length(cols))]
     colourpicker::updateColourInput(session, "plot_border_color",
       value = border_col)
     point_col <- if (pal %in% c("bw_pal", "grey_pal")) "#000000"
       else cols[min(3, length(cols))]
     colourpicker::updateColourInput(session, "plot_point_color",
       value = point_col)
   }
 }, ignoreInit = TRUE)
 
 observeEvent(input$plot_theme, {
   theme <- input$plot_theme
   bg <- switch(theme,
     "classic" = "#ffffff",
     "minimal" = "#ffffff",
     "bw" = "#ffffff",
     "light" = "#ffffff",
     "pubr" = "#ffffff",
     "void" = "#ffffff",
     "#ffffff"
   )
   colourpicker::updateColourInput(session, "plot_bg_color", value = bg)
 }, ignoreInit = TRUE)
 
 # ===========================================================================
 # MULTI-PARAMETER COMPARISON — Server Logic
 # ===========================================================================
 
 mp_raw   <- reactiveVal(NULL)   # raw data.frame as loaded (staging)
 mp_data  <- reactiveVal(NULL)   # final data.frame with Sample, Group, + parameters
 mp_params <- reactiveVal(NULL)  # character vector of parameter column names
 mp_groups <- reactiveVal(NULL)  # character vector of unique group names
 
 # ---- Auto-detection helpers ------------------------------------------------
 # Classify every column as "id", "group", or "numeric"
 mp_classify_cols <- function(df) {
   sapply(colnames(df), function(cn) {
     vals <- df[[cn]]
     # Try numeric conversion
     num_vals <- suppressWarnings(as.numeric(vals))
     pct_numeric <- sum(!is.na(num_vals)) / max(length(vals), 1)
     n_unique <- length(unique(na.omit(vals)))
     n_rows   <- nrow(df)
     
     if (pct_numeric >= 0.8 && n_unique > 2) {
       "numeric"   # likely a measurement parameter
     } else if (is.character(vals) || is.factor(vals)) {
       if (n_unique <= max(10, n_rows * 0.3)) {
         "group"   # few unique text values → grouping column
       } else {
         "id"      # many unique text values → sample ID
       }
     } else if (pct_numeric >= 0.8 && n_unique <= 2) {
       "group"     # binary numeric codes (e.g. 0/1) → group
     } else {
       "id"
     }
   }, USE.NAMES = TRUE)
 }
 
 # Pick the best auto-detected column for each role
 mp_auto_detect <- function(df) {
   classes <- mp_classify_cols(df)
   cols <- colnames(df)
   
   # Group: first column classified as "group"
   grp_candidates <- cols[classes == "group"]
   grp_col <- if (length(grp_candidates) > 0) grp_candidates[1] else cols[min(2, length(cols))]
   
   # Sample: first column classified as "id" that is not the group col
   id_candidates <- cols[classes == "id" & cols != grp_col]
   sample_col <- if (length(id_candidates) > 0) id_candidates[1] else "__auto__"
   
   # Parameters: all columns classified as "numeric" (excluding group/sample)
   param_candidates <- cols[classes == "numeric" & cols != grp_col & cols != sample_col]
   if (length(param_candidates) == 0) {
     # Fallback: pick all columns that are not group/sample
     param_candidates <- setdiff(cols, c(grp_col, sample_col))
   }
   
   list(sample_col = sample_col, group_col = grp_col, param_cols = param_candidates)
 }
 
 # ---- Stage raw data (shared by paste & file loaders) -----------------------
mp_stage_raw <- function(df, source = NULL, detail = NULL) {
   if (is.null(df) || nrow(df) < 2 || ncol(df) < 2) {
     showNotification("Could not parse data. Need at least 2 rows and 2 columns.",
                      type = "error", duration = 5)
     return()
   }
   df <- as.data.frame(df, stringsAsFactors = FALSE)
   mp_raw(df)

  if (is.null(detail) || length(detail) == 0 || !nzchar(as.character(detail)[1])) {
    detail <- paste0(nrow(df), " rows, ", ncol(df), " columns")
  }
  rv$mp_data_source <- source
  rv$mp_data_detail <- as.character(detail)[1]
  rv$mp_configured <- FALSE
   # Clear processed data so user applies config
   mp_data(NULL); mp_params(NULL); mp_groups(NULL)
   showNotification(
     paste0("Raw data loaded: ", nrow(df), " rows \u00d7 ", ncol(df),
            " columns. Configure column roles below, then click Apply."),
     type = "message", duration = 5)
 }
 
 # ---- Apply column configuration and produce final data ---------------------
 mp_apply_config <- function(sample_col, group_col, param_cols) {
   df <- mp_raw()
   if (is.null(df)) { showNotification("No raw data loaded.", type = "error"); return() }
   if (is.null(group_col) || !nzchar(group_col)) {
     showNotification("Please select a Group column.", type = "error"); return()
   }
   if (is.null(param_cols) || length(param_cols) == 0) {
     showNotification("Please select at least one parameter column.", type = "error"); return()
   }
   
   # Build the final data frame
   out <- data.frame(row.names = seq_len(nrow(df)))
   
   # Sample column
   if (!is.null(sample_col) && sample_col != "__auto__" && sample_col %in% colnames(df)) {
     out$Sample <- as.character(df[[sample_col]])
   } else {
     out$Sample <- paste0("S", seq_len(nrow(df)))
   }
   
   # Group column
   out$Group <- as.character(df[[group_col]])
   
   # Parameter columns
   valid_params <- c()
   for (p in param_cols) {
     if (p %in% colnames(df)) {
       out[[p]] <- suppressWarnings(as.numeric(df[[p]]))
       if (!all(is.na(out[[p]]))) valid_params <- c(valid_params, p)
     }
   }
   
   if (length(valid_params) == 0) {
     showNotification("No valid numeric data in the selected parameter columns.", type = "error")
     return()
   }
   
   out <- out[, c("Sample", "Group", valid_params), drop = FALSE]
   mp_data(out)
   mp_params(valid_params)
   mp_groups(unique(out$Group))
  rv$mp_configured <- TRUE
  if (is.null(rv$mp_data_detail) || !nzchar(rv$mp_data_detail)) {
    rv$mp_data_detail <- paste0(nrow(out), " samples, ", length(valid_params), " parameters")
  }
   showNotification(paste0("\u2705 Configured: ", nrow(out), " samples, ",
                           length(valid_params), " parameters, ",
                           length(unique(out$Group)), " groups."),
                    type = "message", duration = 4)
 }
 
 # ---- Load pasted data -------------------------------------------------------
 observeEvent(input$mp_load, {
   req(nzchar(trimws(input$mp_raw_text)))
   txt <- input$mp_raw_text
   sep <- input$mp_paste_sep
   has_header <- isTRUE(input$mp_paste_header)
   df <- tryCatch(
     read.delim(textConnection(txt), header = has_header, sep = sep,
                stringsAsFactors = FALSE, check.names = FALSE),
     error = function(e) NULL
   )
  mp_stage_raw(
    df,
    source = "paste",
    detail = "Pasted text"
  )
 })
 
 # ---- Detect Excel sheets on file select ------------------------------------
 observeEvent(input$mp_file_input, {
   req(input$mp_file_input)
   fpath <- input$mp_file_input$datapath
   fname <- input$mp_file_input$name
   ext <- tolower(tools::file_ext(fname))
   if (ext %in% c("xls", "xlsx")) {
     sheets <- tryCatch(readxl::excel_sheets(fpath), error = function(e) NULL)
     updateSelectInput(session, "mp_file_sheet", choices = sheets)
   } else {
     updateSelectInput(session, "mp_file_sheet", choices = NULL)
   }
 })
 
 # ---- Load file data --------------------------------------------------------
 observeEvent(input$mp_load_file, {
   req(input$mp_file_input)
   fpath <- input$mp_file_input$datapath
   fname <- input$mp_file_input$name
   ext <- tolower(tools::file_ext(fname))
   
   df <- tryCatch({
     if (ext %in% c("xls", "xlsx")) {
       sh <- input$mp_file_sheet
       if (is.null(sh) || !nzchar(sh)) sh <- 1
       readxl::read_excel(fpath, sheet = sh, col_names = input$mp_file_header)
     } else {
       read.delim(fpath, header = input$mp_file_header,
                  sep = input$mp_file_sep, stringsAsFactors = FALSE,
                  check.names = FALSE)
     }
   }, error = function(e) { NULL })
   
   if (!is.null(df)) df <- as.data.frame(df)
  sheet_detail <- if (ext %in% c("xls", "xlsx") && !is.null(input$mp_file_sheet) &&
      nzchar(as.character(input$mp_file_sheet))) {
      paste0(" | Sheet: ", input$mp_file_sheet)
    } else {
      ""
    }
  mp_stage_raw(
    df,
    source = "file",
    detail = paste0(fname, sheet_detail)
  )
 })
 
 # ---- Clear -----------------------------------------------------------------
 observeEvent(input$mp_clear, {
   mp_raw(NULL); mp_data(NULL); mp_params(NULL); mp_groups(NULL)
  rv$mp_data_source <- NULL
  rv$mp_data_detail <- NULL
  rv$mp_configured <- FALSE
  updateTextAreaInput(session, "mp_raw_text", value = "")
  updateSelectInput(session, "mp_file_sheet", choices = NULL, selected = character(0))
  session$sendCustomMessage("clearMpInputs", list())
  showNotification("Multi-parameter inputs and loaded data cleared.", type = "message", duration = 3)
 })
 
 # ---- Column configuration UI (Step 2) -------------------------------------
 output$mp_col_config_ui <- renderUI({
   df <- mp_raw()
   if (is.null(df)) return(NULL)
   
   cols <- colnames(df)
   det  <- mp_auto_detect(df)
   
   # Determine auto-detected selections
   auto_sample <- det$sample_col
   auto_group  <- det$group_col
   auto_params <- det$param_cols
   
   sample_choices <- c("Auto-generate (S1, S2, ...)" = "__auto__", setNames(cols, cols))
   group_choices  <- setNames(cols, cols)
   param_choices  <- setNames(cols, cols)
   
   div(class = "stat-card",
     h4(icon("cogs"), " Step 2: Configure Columns"),
     div(class = "test-info-box", style = "margin-bottom:12px;",
       icon("magic"), strong(" Auto-detected:"),
       tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px;",
         tags$li("Sample: ", tags$code(if (auto_sample == "__auto__") "(auto-generate)" else auto_sample)),
         tags$li("Group: ", tags$code(auto_group)),
         tags$li("Parameters: ", tags$code(paste(auto_params, collapse = ", ")))
       ),
       p(style = "margin:6px 0 0 0; font-size:12px; color:#666;",
         "Adjust below if the auto-detection is not correct.")
     ),
     selectInput("mp_cfg_sample", "Sample / ID Column:",
       choices = sample_choices, selected = auto_sample),
     selectInput("mp_cfg_group", "Group Column:",
       choices = group_choices, selected = auto_group),
     checkboxGroupInput("mp_cfg_params", "Parameter Columns (numeric):",
       choices = param_choices, selected = auto_params, inline = TRUE),
     fluidRow(
       column(6,
         actionButton("mp_apply_cfg", "Apply Configuration",
           icon = icon("check"), class = "btn-primary",
           style = "width:100%;")
       ),
       column(6,
         actionButton("mp_auto_apply", "Auto-Detect & Apply",
           icon = icon("magic"), class = "btn-info",
           style = "width:100%;")
       )
     ),
     hr(),
     h5(icon("eye"), " Raw Data Preview"),
     DTOutput("mp_raw_preview")
   )
 })
 
 # ---- Raw data preview table ------------------------------------------------
 output$mp_raw_preview <- renderDT({
   df <- mp_raw()
   req(df)
   datatable(df, rownames = FALSE,
     options = list(pageLength = 5, dom = "tp", scrollX = TRUE),
     class = "display compact", style = "bootstrap")
 })
 
 # ---- Apply configuration button -------------------------------------------
 observeEvent(input$mp_apply_cfg, {
   mp_apply_config(input$mp_cfg_sample, input$mp_cfg_group, input$mp_cfg_params)
 })
 
 # ---- Auto-detect & apply button -------------------------------------------
 observeEvent(input$mp_auto_apply, {
   df <- mp_raw()
   req(df)
   det <- mp_auto_detect(df)
   # Update UI selectors to match auto-detection
   updateSelectInput(session, "mp_cfg_sample", selected = det$sample_col)
   updateSelectInput(session, "mp_cfg_group", selected = det$group_col)
   updateCheckboxGroupInput(session, "mp_cfg_params", selected = det$param_cols)
   # Apply immediately
   mp_apply_config(det$sample_col, det$group_col, det$param_cols)
 })
 
 # ---- Data summary & preview (final configured data) -----------------------
 output$mp_data_summary <- renderUI({
   df <- mp_data()
   if (is.null(df)) {
     raw <- mp_raw()
     if (!is.null(raw)) {
       return(div(class = "data-source-banner source-none",
         icon("arrow-up"), " Raw data loaded. Configure columns above, then click ",
         strong("Apply Configuration"), "."))
     }
     return(div(class = "data-source-banner source-none",
                icon("info-circle"), " No data loaded yet."))
   }
   div(class = "data-source-banner source-file",
     icon("check-circle"),
     sprintf(" %d samples | %d parameters (%s) | Groups: %s",
             nrow(df), length(mp_params()),
             paste(mp_params(), collapse = ", "),
             paste(mp_groups(), collapse = ", "))
   )
 })
 
 output$mp_data_preview <- renderDT({
   df <- mp_data()
   req(df)
   datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = "tp", scrollX = TRUE),
             class = "display compact", style = "bootstrap")
 })
 
 # --- Dynamic UI: parameter selector -----------------------------------------
 output$mp_param_selector_ui <- renderUI({
   params <- mp_params()
   if (is.null(params)) return(NULL)
   checkboxGroupInput("mp_sel_params", "Parameters to display:",
     choices = params, selected = params, inline = TRUE)
 })
 
 # --- Dynamic UI: group selector (when >2 groups) ----------------------------
 output$mp_group_selector_ui <- renderUI({
   grps <- mp_groups()
   if (is.null(grps) || length(grps) <= 2) return(NULL)
   checkboxGroupInput("mp_sel_groups", "Groups to display:",
     choices = grps, selected = grps, inline = TRUE)
 })

 # --- Helper: currently selected groups (respects selector when >2 groups) ----
 mp_active_groups <- reactive({
   all_grps <- mp_groups()
   if (is.null(all_grps)) return(NULL)
   if (length(all_grps) <= 2) return(all_grps)
   sel <- input$mp_sel_groups
   if (is.null(sel) || length(sel) == 0) return(all_grps)
   # Preserve original order
   all_grps[all_grps %in% sel]
 })

 # --- Dynamic UI: group order selector ----------------------------------------
 output$mp_group_order_ui <- renderUI({
   grps <- mp_active_groups()
   if (is.null(grps) || length(grps) < 2) return(NULL)
   opt1 <- paste(grps, collapse = ", ")
   opt2 <- paste(rev(grps), collapse = ", ")
   val1 <- paste(grps, collapse = "|||")
   val2 <- paste(rev(grps), collapse = "|||")
   choices <- stats::setNames(c(val1, val2), c(opt1, opt2))
   selectInput("mp_group_order", "Group Order on X-axis:", choices = choices, selected = val1)
 })
 
 # --- Parse current group levels from selector --------------------------------
 mp_grp_levels <- reactive({
   go <- input$mp_group_order
   active <- mp_active_groups()
   if (is.null(go) || !nzchar(go)) return(active)
   parsed <- strsplit(go, "\\|\\|\\|")[[1]]
   # Ensure only currently selected groups are included
   if (!is.null(active)) parsed <- parsed[parsed %in% active]
   if (length(parsed) == 0) return(active)
   parsed
 })
 
 # --- Build individual parameter plot -----------------------------------------
 # --- Shared helper: compute p-value matching the stats table exactly --------
 # Helper: classify test type for the multiparametric module
 mp_test_type <- function(test_method) {
   two_grp <- c("t.test", "welch", "two_z", "wilcox.test", "ks_two",
                "paired_t", "wilcoxon_sr", "f_test")
   multi_grp <- c("anova", "kruskal")
   per_group <- c("shapiro")
   overall   <- c("levene")
   if (test_method %in% two_grp)   return("two_group")
   if (test_method %in% multi_grp) return("multi_group")
   if (test_method %in% per_group) return("per_group")
   if (test_method %in% overall)   return("overall")
   return("none")
 }

 mp_compute_pvalue <- function(g1, g2, test_method, sigma1 = 1, sigma2 = 1) {
   tryCatch({
     if (test_method == "t.test") {
       tt <- t.test(g1, g2, var.equal = TRUE)
       tt$p.value
     } else if (test_method == "welch") {
       tt <- t.test(g1, g2, var.equal = FALSE)
       tt$p.value
     } else if (test_method == "two_z") {
       n1 <- length(g1); n2 <- length(g2)
       z <- (mean(g1) - mean(g2)) / sqrt(sigma1^2 / n1 + sigma2^2 / n2)
       2 * pnorm(-abs(z))
     } else if (test_method == "wilcox.test") {
       nx <- length(g1); ny <- length(g2)
       df_coin <- data.frame(
         value = c(g1, g2),
         group = factor(c(rep("A", nx), rep("B", ny)))
       )
       if ((nx + ny) <= 200) {
         coin_res <- coin::wilcox_test(value ~ group, data = df_coin,
                                        distribution = "exact")
       } else {
         coin_res <- coin::wilcox_test(value ~ group, data = df_coin,
                                        distribution = "asymptotic")
       }
       as.numeric(coin::pvalue(coin_res))
     } else if (test_method == "ks_two") {
       ks <- ks.test(g1, g2)
       ks$p.value
     } else if (test_method == "paired_t") {
       if (length(g1) != length(g2)) return(NA_real_)
       tt <- t.test(g1, g2, paired = TRUE)
       tt$p.value
     } else if (test_method == "wilcoxon_sr") {
       if (length(g1) != length(g2)) return(NA_real_)
       wt <- wilcox.test(g1, g2, paired = TRUE)
       wt$p.value
     } else if (test_method == "f_test") {
       ft <- var.test(g1, g2)
       ft$p.value
     } else {
       NA_real_
     }
   }, error = function(e) NA_real_)
 }

 # Multi-group p-value computation
 mp_compute_pvalue_multi <- function(values, groups, test_method) {
   tryCatch({
     df_tmp <- data.frame(value = values, group = factor(groups))
     df_tmp <- df_tmp[!is.na(df_tmp$value), ]
     if (test_method == "anova") {
       fit <- aov(value ~ group, data = df_tmp)
       summary(fit)[[1]][["Pr(>F)"]][1]
     } else if (test_method == "kruskal") {
       kt <- kruskal.test(value ~ group, data = df_tmp)
       kt$p.value
     } else if (test_method == "levene") {
       lt <- car::leveneTest(value ~ group, data = df_tmp)
       lt$`Pr(>F)`[1]
     } else {
       NA_real_
     }
   }, error = function(e) NA_real_)
 }

 # --- Per-parameter label selector UI (MP tab) --------------------------------
 output$mp_param_label_selector_ui <- renderUI({
   params <- input$mp_sel_params
   if (is.null(params) || length(params) == 0)
     return(helpText("Select parameters first."))
   selectInput("mp_pp_param", "Parameter:",
     choices = params, selected = params[1])
 })

 # When user selects a parameter, populate the text inputs with stored values
 observeEvent(input$mp_pp_param, {
   param <- input$mp_pp_param
   if (is.null(param)) return()
   stored <- rv$mp_param_labels[[param]]
   updateTextInput(session, "mp_pp_title",
     value = if (!is.null(stored$title)) stored$title else "")
   updateTextInput(session, "mp_pp_ylab",
     value = if (!is.null(stored$ylab)) stored$ylab else "")
 })

 # Apply button: store per-param labels
 observeEvent(input$mp_pp_apply, {
   param <- input$mp_pp_param
   if (is.null(param) || !nzchar(param)) return()
   rv$mp_param_labels[[param]] <- list(
     title = input$mp_pp_title,
     ylab = input$mp_pp_ylab
   )
 })

 # Clear all per-param labels
 observeEvent(input$mp_pp_clear_all, {
   rv$mp_param_labels <- list()
   updateTextInput(session, "mp_pp_title", value = "")
   updateTextInput(session, "mp_pp_ylab", value = "")
 })

 # Status display: show which parameters have custom labels
 output$mp_pp_status <- renderUI({
   labels <- rv$mp_param_labels
   configured <- names(labels)[sapply(labels, function(l) {
     nzchar(l$title) || nzchar(l$ylab)
   })]
   if (length(configured) == 0) return(NULL)
   helpText(style = "font-size:10px; color:#1f8b5f; margin-top:6px;",
     icon("check-circle"),
     paste("Custom labels set for:", paste(configured, collapse = ", ")))
 })

 # --- Format p-value for plot annotation ------------------------------------
 mp_format_p_label <- function(p_val, style = "p.signif") {
   if (is.na(p_val)) return("")
   if (style == "p.signif") {
     if (p_val < 0.001)  return("***")
     if (p_val < 0.01)   return("**")
     if (p_val < 0.05)   return("*")
     return("ns")
   } else {
     # p.format: show exact value
     if (p_val < 0.001) return(paste0("p = ", formatC(p_val, format = "e", digits = 2)))
     return(paste0("p = ", formatC(p_val, format = "f", digits = 4)))
   }
 }
 
 # --- Build individual parameter plot -----------------------------------------
 mp_build_one_plot <- function(df, param, grp_levels, plot_type, palette, alpha_val,
                                pt_size, theme_name, show_signif, test_method,
                                signif_label, custom_ylim, ymin, ymax,
                                line_width = 0.8, width_adj = 0.75,
                                bg_color = "#ffffff",
                                title_text = "", xlab_text = "", ylab_text = "",
                                title_size = 16, axis_title_size = 14,
                                axis_text_size = 12, legend_size = 11,
                                title_bold = TRUE, title_italic = FALSE,
                                axis_title_bold = FALSE, axis_title_italic = FALSE,
                                underline_title = FALSE,
                                show_legend = TRUE, coord_flip = FALSE,
                                show_mean = FALSE,
                                show_errorbar = FALSE, errorbar_type = "se",
                                errorbar_width = 0.2,
                                show_jitter = FALSE, jitter_width = 0.15,
                                jitter_alpha = 0.5,
                                signif_step = 0.05, signif_text_size = 3.5,
                                custom_colors = NULL,
                                whisker_type = "default",
                                signif_style = "text",
                                grp_labels = NULL) {
   # Validate param exists in df
   if (!param %in% colnames(df)) {
     return(ggplot() + theme_void() + labs(title = paste(param, "(column not found)")))
   }
   sub <- df[, c("Sample", "Group", param), drop = FALSE]
   colnames(sub)[3] <- "value"
   sub$value <- suppressWarnings(as.numeric(sub$value))
   sub$Group <- factor(sub$Group, levels = grp_levels)
   sub <- sub[!is.na(sub$value) & !is.na(sub$Group), ]
   if (nrow(sub) == 0) return(ggplot() + theme_void() + labs(title = paste(param, "(no data)")))
   
   n_grps <- length(grp_levels)
   if (!is.null(custom_colors) && length(custom_colors) >= n_grps) {
     cols <- custom_colors[seq_len(n_grps)]
   } else {
     cols <- tryCatch(get_palette_colors(palette, max(n_grps, 3)), error = function(e) NULL)
     if (is.null(cols)) cols <- scales::hue_pal()(n_grps)
     cols <- cols[seq_len(n_grps)]
   }
   
   lw <- line_width
   w <- width_adj
   bx_coef <- if (whisker_type == "minmax") Inf else 1.5
   use_stat_whisker <- whisker_type %in% c("percentile", "sd")
   
   p <- ggplot(sub, aes(x = Group, y = value, fill = Group)) +
     scale_fill_manual(values = cols,
       labels = if (!is.null(grp_labels)) grp_labels else waiver())
   
   if (!is.null(grp_labels)) {
     p <- p + scale_x_discrete(labels = grp_labels)
   }
   
   if (plot_type == "box") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw)
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = 21,
                             linewidth = lw, outlier.size = pt_size, coef = bx_coef)
     }
   } else if (plot_type == "violin") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw)
   } else if (plot_type == "box_jitter") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw) +
         geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                     aes(fill = Group), color = "black", stroke = 0.3)
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = NA, linewidth = lw, coef = bx_coef) +
         geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                     aes(fill = Group), color = "black", stroke = 0.3)
     }
   } else if (plot_type == "violin_jitter") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
       geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                   aes(fill = Group), color = "black", stroke = 0.3)
   } else if (plot_type == "violin_box") {
     if (use_stat_whisker) {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
         stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                      width = w * 0.2, alpha = 0.9, linewidth = lw)
     } else {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
         geom_boxplot(width = w * 0.2, alpha = 0.9, outlier.shape = NA, linewidth = lw, coef = bx_coef)
     }
   } else if (plot_type == "bar") {
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw) +
       stat_summary(fun.data = mean_se, geom = "errorbar",
                    width = errorbar_width, linewidth = lw)
   } else if (plot_type == "bar_sd") {
     sd_err_fun_bar <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw) +
       stat_summary(fun.data = sd_err_fun_bar, geom = "errorbar",
                    width = errorbar_width, linewidth = lw)
   } else if (plot_type == "dot") {
     p <- p + geom_jitter(width = 0.2, size = pt_size, shape = 21,
                          color = "black", stroke = 0.3, alpha = 0.8)
   } else if (plot_type == "swarm") {
     p <- p + ggbeeswarm::geom_beeswarm(size = pt_size, shape = 21,
                                         color = "black", stroke = 0.3, alpha = 0.8)
   } else if (plot_type == "mean_dot") {
     sd_err_fun <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "point", size = pt_size * 2) +
       stat_summary(fun.data = sd_err_fun, geom = "errorbar",
         width = errorbar_width, linewidth = lw)
   }
   
   # Overlays: Mean point
   if (show_mean && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     p <- p + stat_summary(fun = mean, geom = "point",
       shape = 18, size = pt_size * 1.5, color = "#e74c3c")
   }
   
   # Overlays: Error bars
   if (show_errorbar && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     eb_fun <- switch(errorbar_type,
       "se" = mean_se,
       "sd" = function(x, ...) { m <- mean(x); s <- sd(x); data.frame(y = m, ymin = m - s, ymax = m + s) },
       "ci" = mean_cl_normal,
       "iqr" = median_hilow
     )
     p <- p + stat_summary(fun.data = eb_fun, geom = "errorbar",
       width = errorbar_width, linewidth = lw)
   }
   
   # Overlays: Jitter
   if (show_jitter && !(plot_type %in% c("dot", "swarm", "box_jitter", "violin_jitter"))) {
     p <- p + geom_jitter(width = jitter_width,
       alpha = jitter_alpha, size = pt_size * 0.7)
   }
   
   # Significance annotation (uses same computation as stats table)
   if (show_signif && test_method != "none" && !grepl("^header_", test_method)) {
     ttype <- mp_test_type(test_method)
     
     if (signif_style == "bars" && ttype == "two_group" && n_grps >= 2) {
       # Bracket bar style using geom_signif with manually computed p-values
       tryCatch({
         comps <- combn(grp_levels, 2, simplify = FALSE)
         annotations <- sapply(comps, function(pair) {
           g1 <- sub$value[sub$Group == pair[1]]
           g2 <- sub$value[sub$Group == pair[2]]
           pv <- mp_compute_pvalue(g1, g2, test_method)
           mp_format_p_label(pv, signif_label)
         })
         y_max <- max(sub$value, na.rm = TRUE)
         y_range <- diff(range(sub$value, na.rm = TRUE))
         y_start <- y_max + y_range * 0.05
         y_positions <- y_start + seq(0, length(comps) - 1) * y_range * signif_step
         
         p <- p + ggsignif::geom_signif(
           comparisons = comps,
           annotations = annotations,
           y_position = y_positions,
           textsize = signif_text_size,
           vjust = -0.2,
           tip_length = 0.02
         )
       }, error = function(e) {
         message("Bracket bar annotation error: ", e$message)
       })
     } else {
       # Text annotation style (original)
       p_val <- NA_real_
       if (ttype == "two_group" && n_grps >= 2) {
         g1 <- sub$value[sub$Group == grp_levels[1]]
         g2 <- sub$value[sub$Group == grp_levels[2]]
         p_val <- mp_compute_pvalue(g1, g2, test_method)
       } else if (ttype %in% c("multi_group", "overall") && n_grps >= 2) {
         p_val <- mp_compute_pvalue_multi(sub$value, sub$Group, test_method)
       }
       if (!is.na(p_val)) {
         p_label <- mp_format_p_label(p_val, signif_label)
         if (nzchar(p_label)) {
           # For bar plots, compute y_pos from summary (mean + error) not raw values
           if (plot_type %in% c("bar", "bar_sd")) {
             summ_y <- sub %>% dplyr::group_by(Group) %>%
               dplyr::summarise(
                 top = mean(value, na.rm = TRUE) + if (plot_type == "bar")
                   sd(value, na.rm = TRUE) / sqrt(dplyr::n()) else sd(value, na.rm = TRUE),
                 .groups = "drop")
             y_pos <- max(summ_y$top, na.rm = TRUE) * 1.08
           } else {
             y_pos <- max(sub$value, na.rm = TRUE) + diff(range(sub$value, na.rm = TRUE)) * 0.08
           }
           x_pos <- (1 + n_grps) / 2
           p <- p + annotate("text", x = x_pos, y = y_pos, label = p_label,
                              size = signif_text_size, fontface = "bold", color = "#333333")
         }
       }
     }
   }
   
   # Y-axis limits / coord_flip
   if (custom_ylim && !is.na(ymin) && !is.na(ymax)) {
     if (coord_flip) {
       p <- p + coord_flip(ylim = c(ymin, ymax))
     } else {
       p <- p + coord_cartesian(ylim = c(ymin, ymax))
     }
   } else if (coord_flip) {
     p <- p + coord_flip()
   }
   
   # Labels
   plot_title <- if (nzchar(title_text)) title_text else param
   xl <- if (nzchar(xlab_text)) xlab_text else ""
   yl <- if (nzchar(ylab_text)) ylab_text else param
   
   # Title face
   title_face <- "plain"
   if (title_bold && title_italic) title_face <- "bold.italic"
   else if (title_bold) title_face <- "bold"
   else if (title_italic) title_face <- "italic"
   
   axis_title_face <- "plain"
   if (axis_title_bold && axis_title_italic) axis_title_face <- "bold.italic"
   else if (axis_title_bold) axis_title_face <- "bold"
   else if (axis_title_italic) axis_title_face <- "italic"
   
   theme_fn <- switch(theme_name,
     "classic" = theme_classic, "minimal" = theme_minimal,
     "bw" = theme_bw, "light" = theme_light,
     "pubr" = theme_pubr, "void" = theme_void, theme_classic)
   p <- p + theme_fn() +
     labs(title = plot_title, x = xl, y = yl) +
     theme(
       plot.title = element_markdown(face = title_face, size = title_size, hjust = 0.5),
       axis.text.x = element_markdown(size = axis_text_size),
       axis.text.y = element_markdown(size = axis_text_size),
       axis.title.x = element_markdown(size = axis_title_size, face = axis_title_face),
       axis.title.y = element_markdown(size = axis_title_size, face = axis_title_face),
       legend.text = element_markdown(size = legend_size),
       legend.title = element_markdown(size = legend_size + 1),
       panel.background = element_rect(fill = bg_color),
       plot.background = element_rect(fill = bg_color, color = NA)
     )
   
   # Legend
   if (!show_legend) {
     p <- p + theme(legend.position = "none")
   } else {
     p <- p + theme(legend.position = "bottom")
   }
   
   p
 }
 
 # --- Build single combined faceted plot --------------------------------------
 mp_build_combined_plot <- function(df, params, grp_levels, plot_type, palette, alpha_val,
                                     pt_size, theme_name, show_signif, test_method,
                                     signif_label, custom_ylim, ymin, ymax, free_y,
                                     line_width = 0.8, width_adj = 0.75,
                                     bg_color = "#ffffff",
                                     title_text = "", xlab_text = "", ylab_text = "",
                                     title_size = 16, axis_title_size = 14,
                                     axis_text_size = 12, legend_size = 11,
                                     title_bold = TRUE, title_italic = FALSE,
                                     axis_title_bold = FALSE, axis_title_italic = FALSE,
                                     underline_title = FALSE,
                                     show_legend = TRUE, coord_flip = FALSE,
                                     show_mean = FALSE,
                                     show_errorbar = FALSE, errorbar_type = "se",
                                     errorbar_width = 0.2,
                                     show_jitter = FALSE, jitter_width = 0.15,
                                     jitter_alpha = 0.5,
                                     signif_step = 0.05, signif_text_size = 3.5,
                                     custom_colors = NULL,
                                     whisker_type = "default",
                                     signif_style = "text",
                                     param_labels = NULL,
                                     grp_labels = NULL) {
   # Validate params exist in df
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(ggplot() + theme_void() + labs(title = "No valid parameters"))
   # Build param display label lookup
   plbl <- setNames(params, params)
   if (!is.null(param_labels)) {
     for (pp in names(param_labels)) {
       ttl <- param_labels[[pp]]$title
       if (!is.null(ttl) && nzchar(ttl)) plbl[pp] <- ttl
     }
   }
   # Melt to long format
   sub_df <- df[, c("Sample", "Group", params), drop = FALSE]
   # Ensure all param cols are numeric
   for (p in params) sub_df[[p]] <- suppressWarnings(as.numeric(sub_df[[p]]))
   long <- tidyr::pivot_longer(sub_df,
                                cols = all_of(params),
                                names_to = "Parameter", values_to = "value")
   long$Group <- factor(long$Group, levels = grp_levels)
   long$Parameter <- factor(long$Parameter, levels = params)
   long <- long[!is.na(long$value) & !is.na(long$Group), ]
   if (nrow(long) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
   
   n_grps <- length(grp_levels)
   if (!is.null(custom_colors) && length(custom_colors) >= n_grps) {
     cols <- custom_colors[seq_len(n_grps)]
   } else {
     cols <- tryCatch(get_palette_colors(palette, max(n_grps, 3)), error = function(e) NULL)
     if (is.null(cols)) cols <- scales::hue_pal()(n_grps)
     cols <- cols[seq_len(n_grps)]
   }
   
   lw <- line_width
   w <- width_adj
   bx_coef <- if (whisker_type == "minmax") Inf else 1.5
   use_stat_whisker <- whisker_type %in% c("percentile", "sd")
   
   p <- ggplot(long, aes(x = Group, y = value, fill = Group)) +
     scale_fill_manual(values = cols,
       labels = if (!is.null(grp_labels)) grp_labels else waiver())
   
   if (!is.null(grp_labels)) {
     p <- p + scale_x_discrete(labels = grp_labels)
   }
   
   if (plot_type == "box") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw)
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = 21,
                             linewidth = lw, outlier.size = pt_size, coef = bx_coef)
     }
   } else if (plot_type == "violin") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw)
   } else if (plot_type == "box_jitter") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw) +
         geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                     aes(fill = Group), color = "black", stroke = 0.3)
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = NA, linewidth = lw, coef = bx_coef) +
         geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                     aes(fill = Group), color = "black", stroke = 0.3)
     }
   } else if (plot_type == "violin_jitter") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
       geom_jitter(width = 0.15, size = pt_size, alpha = 0.7, shape = 21,
                   aes(fill = Group), color = "black", stroke = 0.3)
   } else if (plot_type == "violin_box") {
     if (use_stat_whisker) {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
         stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                      width = w * 0.2, alpha = 0.9, linewidth = lw)
     } else {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw) +
         geom_boxplot(width = w * 0.2, alpha = 0.9, outlier.shape = NA, linewidth = lw, coef = bx_coef)
     }
   } else if (plot_type == "bar") {
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw) +
       stat_summary(fun.data = mean_se, geom = "errorbar",
                    width = errorbar_width, linewidth = lw)
   } else if (plot_type == "bar_sd") {
     sd_err_fun_bar <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw) +
       stat_summary(fun.data = sd_err_fun_bar, geom = "errorbar",
                    width = errorbar_width, linewidth = lw)
   } else if (plot_type == "dot") {
     p <- p + geom_jitter(width = 0.2, size = pt_size, shape = 21,
                          color = "black", stroke = 0.3, alpha = 0.8)
   } else if (plot_type == "swarm") {
     p <- p + ggbeeswarm::geom_beeswarm(size = pt_size, shape = 21,
                                         color = "black", stroke = 0.3, alpha = 0.8)
   } else if (plot_type == "mean_dot") {
     sd_err_fun <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "point", size = pt_size * 2) +
       stat_summary(fun.data = sd_err_fun, geom = "errorbar",
         width = errorbar_width, linewidth = lw)
   }
   
   # Overlays: Mean point
   if (show_mean && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     p <- p + stat_summary(fun = mean, geom = "point",
       shape = 18, size = pt_size * 1.5, color = "#e74c3c")
   }
   
   # Overlays: Error bars
   if (show_errorbar && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     eb_fun <- switch(errorbar_type,
       "se" = mean_se,
       "sd" = function(x, ...) { m <- mean(x); s <- sd(x); data.frame(y = m, ymin = m - s, ymax = m + s) },
       "ci" = mean_cl_normal,
       "iqr" = median_hilow
     )
     p <- p + stat_summary(fun.data = eb_fun, geom = "errorbar",
       width = errorbar_width, linewidth = lw)
   }
   
   # Overlays: Jitter
   if (show_jitter && !(plot_type %in% c("dot", "swarm", "box_jitter", "violin_jitter"))) {
     p <- p + geom_jitter(width = jitter_width,
       alpha = jitter_alpha, size = pt_size * 0.7)
   }
   
   # Significance (compute per-parameter, annotate consistently with stats table)
   if (show_signif && test_method != "none" && !grepl("^header_", test_method) && n_grps >= 2) {
     ttype <- mp_test_type(test_method)
     
     if (signif_style == "bars" && ttype == "two_group" && n_grps >= 2) {
       # Bracket bar style per facet using geom_signif with manual p-values
       tryCatch({
         comps <- combn(grp_levels, 2, simplify = FALSE)
         # Build per-parameter annotation data for geom_signif
         signif_layers <- lapply(params, function(param) {
           sub_p <- long[long$Parameter == param, ]
           annotations <- sapply(comps, function(pair) {
             g1 <- sub_p$value[sub_p$Group == pair[1]]
             g2 <- sub_p$value[sub_p$Group == pair[2]]
             pv <- mp_compute_pvalue(g1, g2, test_method)
             mp_format_p_label(pv, signif_label)
           })
           y_max <- max(sub_p$value, na.rm = TRUE)
           y_range <- diff(range(sub_p$value, na.rm = TRUE))
           if (y_range == 0) y_range <- abs(y_max) * 0.1 + 1
           y_start <- y_max + y_range * 0.05
           y_positions <- y_start + seq(0, length(comps) - 1) * y_range * signif_step
           
           sub_signif <- data.frame(
             Parameter = param,
             stringsAsFactors = FALSE
           )
           sub_signif$Parameter <- factor(sub_signif$Parameter, levels = params)
           
           ggsignif::geom_signif(
             data = sub_p,
             comparisons = comps,
             annotations = annotations,
             y_position = y_positions,
             textsize = signif_text_size,
             vjust = -0.2,
             tip_length = 0.02
           )
         })
         for (layer in signif_layers) p <- p + layer
       }, error = function(e) {
         message("Bracket bar annotation error: ", e$message)
       })
     } else {
       # Text annotation style (original)
       annot_rows <- lapply(params, function(param) {
         sub_p <- long[long$Parameter == param, ]
         p_val <- NA_real_
         if (ttype == "two_group" && n_grps >= 2) {
           g1 <- sub_p$value[sub_p$Group == grp_levels[1]]
           g2 <- sub_p$value[sub_p$Group == grp_levels[2]]
           p_val <- mp_compute_pvalue(g1, g2, test_method)
         } else if (ttype %in% c("multi_group", "overall")) {
           p_val <- mp_compute_pvalue_multi(sub_p$value, sub_p$Group, test_method)
         }
         p_label <- if (!is.na(p_val)) mp_format_p_label(p_val, signif_label) else ""
         # For bar plots, use summary (mean + error) for y_pos
         if (plot_type %in% c("bar", "bar_sd")) {
           summ_y <- sub_p %>% dplyr::group_by(Group) %>%
             dplyr::summarise(
               top = mean(value, na.rm = TRUE) + if (plot_type == "bar")
                 sd(value, na.rm = TRUE) / sqrt(dplyr::n()) else sd(value, na.rm = TRUE),
               .groups = "drop")
           y_pos <- max(summ_y$top, na.rm = TRUE) * 1.08
         } else {
           y_pos <- max(sub_p$value, na.rm = TRUE) +
             diff(range(sub_p$value, na.rm = TRUE)) * 0.08
         }
         x_pos <- (1 + n_grps) / 2
         data.frame(Parameter = param, x = x_pos, y = y_pos, label = p_label,
                    stringsAsFactors = FALSE)
       })
       annot_df <- do.call(rbind, annot_rows)
       annot_df$Parameter <- factor(annot_df$Parameter, levels = params)
       annot_df <- annot_df[nzchar(annot_df$label), , drop = FALSE]
       if (nrow(annot_df) > 0) {
         p <- p + geom_text(data = annot_df, aes(x = x, y = y, label = label),
                            inherit.aes = FALSE, size = signif_text_size, fontface = "bold",
                            color = "#333333")
       }
     }
   }
   # Facet
   scales_arg <- if (free_y) "free_y" else "fixed"
   p <- p + facet_wrap(~ Parameter, scales = scales_arg,
     labeller = labeller(Parameter = plbl))
   
   # Y-axis limits / coord_flip (only if not free)
   if (custom_ylim && !is.na(ymin) && !is.na(ymax) && !free_y) {
     if (coord_flip) {
       p <- p + coord_flip(ylim = c(ymin, ymax))
     } else {
       p <- p + coord_cartesian(ylim = c(ymin, ymax))
     }
   } else if (coord_flip) {
     p <- p + coord_flip()
   }
   
   # Labels
   plot_title <- if (nzchar(title_text)) title_text else NULL
   xl <- if (nzchar(xlab_text)) xlab_text else ""
   yl <- if (nzchar(ylab_text)) ylab_text else "Value"
   
   # Title face
   title_face <- "plain"
   if (title_bold && title_italic) title_face <- "bold.italic"
   else if (title_bold) title_face <- "bold"
   else if (title_italic) title_face <- "italic"
   
   axis_title_face <- "plain"
   if (axis_title_bold && axis_title_italic) axis_title_face <- "bold.italic"
   else if (axis_title_bold) axis_title_face <- "bold"
   else if (axis_title_italic) axis_title_face <- "italic"
   
   theme_fn <- switch(theme_name,
     "classic" = theme_classic, "minimal" = theme_minimal,
     "bw" = theme_bw, "light" = theme_light,
     "pubr" = theme_pubr, "void" = theme_void, theme_classic)
   p <- p + theme_fn() +
     labs(title = plot_title, x = xl, y = yl) +
     theme(
       plot.title = element_markdown(face = title_face, size = title_size, hjust = 0.5),
       strip.text = element_markdown(face = "bold", size = axis_text_size + 1),
       axis.text.x = element_markdown(size = axis_text_size),
       axis.text.y = element_markdown(size = axis_text_size),
       axis.title.x = element_markdown(size = axis_title_size, face = axis_title_face),
       axis.title.y = element_markdown(size = axis_title_size, face = axis_title_face),
       legend.text = element_markdown(size = legend_size),
       legend.title = element_blank(),
       panel.background = element_rect(fill = bg_color),
       plot.background = element_rect(fill = bg_color, color = NA)
     )
   
   # Legend
   if (show_legend) {
     p <- p + theme(legend.position = "bottom")
   } else {
     p <- p + theme(legend.position = "none")
   }
   
   p
 }
 
 # --- Build grouped single plot (all params on x-axis, groups dodged) ---------
 mp_build_grouped_plot <- function(df, params, grp_levels, plot_type, palette, alpha_val,
                                    pt_size, theme_name, show_signif, test_method,
                                    signif_label, custom_ylim, ymin, ymax, free_y,
                                    line_width = 0.8, width_adj = 0.75,
                                    bg_color = "#ffffff",
                                    title_text = "", xlab_text = "", ylab_text = "",
                                    title_size = 16, axis_title_size = 14,
                                    axis_text_size = 12, legend_size = 11,
                                    title_bold = TRUE, title_italic = FALSE,
                                    axis_title_bold = FALSE, axis_title_italic = FALSE,
                                    underline_title = FALSE,
                                    show_legend = TRUE, coord_flip = FALSE,
                                    show_mean = FALSE,
                                    show_errorbar = FALSE, errorbar_type = "se",
                                    errorbar_width = 0.2,
                                    show_jitter = FALSE, jitter_width = 0.15,
                                    jitter_alpha = 0.5,
                                    signif_step = 0.05, signif_text_size = 3.5,
                                    custom_colors = NULL,
                                    whisker_type = "default",
                                    signif_style = "text",
                                    param_labels = NULL,
                                    grp_labels = NULL) {
   # Validate params
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(ggplot() + theme_void() + labs(title = "No valid parameters"))
   
   # Build param display label lookup
   plbl <- setNames(params, params)
   if (!is.null(param_labels)) {
     for (pp in names(param_labels)) {
       ttl <- param_labels[[pp]]$title
       if (!is.null(ttl) && nzchar(ttl)) plbl[pp] <- ttl
     }
   }
   # Melt to long format
   sub_df <- df[, c("Sample", "Group", params), drop = FALSE]
   for (pp in params) sub_df[[pp]] <- suppressWarnings(as.numeric(sub_df[[pp]]))
   long <- tidyr::pivot_longer(sub_df, cols = all_of(params),
                                names_to = "Parameter", values_to = "value")
   long$Group <- factor(long$Group, levels = grp_levels)
   long$Parameter <- factor(long$Parameter, levels = params)
   long <- long[!is.na(long$value) & !is.na(long$Group), ]
   if (nrow(long) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
   
   n_grps <- length(grp_levels)
   if (!is.null(custom_colors) && length(custom_colors) >= n_grps) {
     cols <- custom_colors[seq_len(n_grps)]
   } else {
     cols <- tryCatch(get_palette_colors(palette, max(n_grps, 3)), error = function(e) NULL)
     if (is.null(cols)) cols <- scales::hue_pal()(n_grps)
     cols <- cols[seq_len(n_grps)]
   }
   
   lw <- line_width
   w <- width_adj
   bx_coef <- if (whisker_type == "minmax") Inf else 1.5
   use_stat_whisker <- whisker_type %in% c("percentile", "sd")
   dodge_w <- w
   
   # x = Parameter, fill = Group, dodged
   p <- ggplot(long, aes(x = Parameter, y = value, fill = Group)) +
     scale_fill_manual(values = cols,
       labels = if (!is.null(grp_labels)) grp_labels else waiver())
   
   if (plot_type == "box") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw,
                             position = position_dodge(width = dodge_w))
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = 21,
                             linewidth = lw, outlier.size = pt_size, coef = bx_coef,
                             position = position_dodge(width = dodge_w))
     }
   } else if (plot_type == "violin") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw,
                          position = position_dodge(width = dodge_w))
   } else if (plot_type == "box_jitter") {
     if (use_stat_whisker) {
       p <- p + stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                             alpha = alpha_val, width = w, linewidth = lw,
                             position = position_dodge(width = dodge_w)) +
         geom_point(size = pt_size, alpha = 0.7, shape = 21, color = "black", stroke = 0.3,
                    position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_w))
     } else {
       p <- p + geom_boxplot(alpha = alpha_val, width = w, outlier.shape = NA,
                             linewidth = lw, coef = bx_coef,
                             position = position_dodge(width = dodge_w)) +
         geom_point(size = pt_size, alpha = 0.7, shape = 21, color = "black", stroke = 0.3,
                    position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_w))
     }
   } else if (plot_type == "violin_jitter") {
     p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw,
                          position = position_dodge(width = dodge_w)) +
       geom_point(size = pt_size, alpha = 0.7, shape = 21, color = "black", stroke = 0.3,
                  position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_w))
   } else if (plot_type == "violin_box") {
     if (use_stat_whisker) {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw,
                            position = position_dodge(width = dodge_w)) +
         stat_summary(fun.data = whisker_stat_fun(whisker_type), geom = "boxplot",
                      width = w * 0.2, alpha = 0.9, linewidth = lw,
                      position = position_dodge(width = dodge_w))
     } else {
       p <- p + geom_violin(alpha = alpha_val, width = w, trim = FALSE, linewidth = lw,
                            position = position_dodge(width = dodge_w)) +
         geom_boxplot(width = w * 0.2, alpha = 0.9, outlier.shape = NA, linewidth = lw,
                      coef = bx_coef, position = position_dodge(width = dodge_w))
     }
   } else if (plot_type == "bar") {
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw,
                    position = position_dodge(width = dodge_w)) +
       stat_summary(fun.data = mean_se, geom = "errorbar",
                    width = errorbar_width, linewidth = lw,
                    position = position_dodge(width = dodge_w))
   } else if (plot_type == "bar_sd") {
     sd_err_fun_bar <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "col", alpha = alpha_val, width = w,
                    color = "black", linewidth = lw,
                    position = position_dodge(width = dodge_w)) +
       stat_summary(fun.data = sd_err_fun_bar, geom = "errorbar",
                    width = errorbar_width, linewidth = lw,
                    position = position_dodge(width = dodge_w))
   } else if (plot_type == "dot") {
     p <- p + geom_point(size = pt_size, shape = 21, color = "black", stroke = 0.3, alpha = 0.8,
                         position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_w))
   } else if (plot_type == "swarm") {
     p <- p + ggbeeswarm::geom_beeswarm(size = pt_size, shape = 21,
                                         color = "black", stroke = 0.3, alpha = 0.8,
                                         dodge.width = dodge_w)
   } else if (plot_type == "mean_dot") {
     sd_err_fun <- function(x, ...) {
       m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
       data.frame(y = m, ymin = m - s, ymax = m + s)
     }
     p <- p +
       stat_summary(fun = mean, geom = "point", size = pt_size * 2,
                    position = position_dodge(width = dodge_w)) +
       stat_summary(fun.data = sd_err_fun, geom = "errorbar",
                    width = errorbar_width, linewidth = lw,
                    position = position_dodge(width = dodge_w))
   }
   
   # Overlays: Mean point
   if (show_mean && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     p <- p + stat_summary(fun = mean, geom = "point",
       shape = 18, size = pt_size * 1.5, color = "#e74c3c",
       position = position_dodge(width = dodge_w))
   }
   
   # Overlays: Error bars
   if (show_errorbar && !(plot_type %in% c("bar", "bar_sd", "mean_dot"))) {
     eb_fun <- switch(errorbar_type,
       "se" = mean_se,
       "sd" = function(x, ...) { m <- mean(x); s <- sd(x); data.frame(y = m, ymin = m - s, ymax = m + s) },
       "ci" = mean_cl_normal,
       "iqr" = median_hilow
     )
     p <- p + stat_summary(fun.data = eb_fun, geom = "errorbar",
       width = errorbar_width, linewidth = lw,
       position = position_dodge(width = dodge_w))
   }
   
   # Overlays: Jitter
   if (show_jitter && !(plot_type %in% c("dot", "swarm", "box_jitter", "violin_jitter"))) {
     p <- p + geom_point(alpha = jitter_alpha, size = pt_size * 0.7,
       position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_w))
   }
   
   # Significance: annotate per parameter
   if (show_signif && test_method != "none" && !grepl("^header_", test_method) && n_grps >= 2) {
     ttype <- mp_test_type(test_method)
     annot_rows <- lapply(params, function(param) {
       sub_p <- long[long$Parameter == param, ]
       p_val <- NA_real_
       if (ttype == "two_group" && n_grps >= 2) {
         g1 <- sub_p$value[sub_p$Group == grp_levels[1]]
         g2 <- sub_p$value[sub_p$Group == grp_levels[2]]
         p_val <- mp_compute_pvalue(g1, g2, test_method)
       } else if (ttype %in% c("multi_group", "overall")) {
         p_val <- mp_compute_pvalue_multi(sub_p$value, sub_p$Group, test_method)
       }
       p_label <- if (!is.na(p_val)) mp_format_p_label(p_val, signif_label) else ""
       if (plot_type %in% c("bar", "bar_sd")) {
         summ_y <- sub_p %>% dplyr::group_by(Group) %>%
           dplyr::summarise(
             top = mean(value, na.rm = TRUE) + if (plot_type == "bar")
               sd(value, na.rm = TRUE) / sqrt(dplyr::n()) else sd(value, na.rm = TRUE),
             .groups = "drop")
         y_pos <- max(summ_y$top, na.rm = TRUE) * 1.08
       } else {
         y_pos <- max(sub_p$value, na.rm = TRUE) +
           diff(range(sub_p$value, na.rm = TRUE)) * 0.08
       }
       data.frame(Parameter = param, y = y_pos, label = p_label,
                  stringsAsFactors = FALSE)
     })
     annot_df <- do.call(rbind, annot_rows)
     annot_df$Parameter <- factor(annot_df$Parameter, levels = params)
     annot_df <- annot_df[nzchar(annot_df$label), , drop = FALSE]
     if (nrow(annot_df) > 0) {
       p <- p + geom_text(data = annot_df, aes(x = Parameter, y = y, label = label),
                          inherit.aes = FALSE, size = signif_text_size, fontface = "bold",
                          color = "#333333")
     }
   }
   
   # Y-axis limits / coord_flip
   if (custom_ylim && !is.na(ymin) && !is.na(ymax)) {
     if (coord_flip) {
       p <- p + coord_flip(ylim = c(ymin, ymax))
     } else {
       p <- p + coord_cartesian(ylim = c(ymin, ymax))
     }
   } else if (coord_flip) {
     p <- p + coord_flip()
   }
   
   # Labels
   plot_title <- if (nzchar(title_text)) title_text else NULL
   xl <- if (nzchar(xlab_text)) xlab_text else ""
   yl <- if (nzchar(ylab_text)) ylab_text else "Value"
   
   # Title face
   title_face <- "plain"
   if (title_bold && title_italic) title_face <- "bold.italic"
   else if (title_bold) title_face <- "bold"
   else if (title_italic) title_face <- "italic"
   
   axis_title_face <- "plain"
   if (axis_title_bold && axis_title_italic) axis_title_face <- "bold.italic"
   else if (axis_title_bold) axis_title_face <- "bold"
   else if (axis_title_italic) axis_title_face <- "italic"
   
   theme_fn <- switch(theme_name,
     "classic" = theme_classic, "minimal" = theme_minimal,
     "bw" = theme_bw, "light" = theme_light,
     "pubr" = theme_pubr, "void" = theme_void, theme_classic)
   p <- p + theme_fn() +
     labs(title = plot_title, x = xl, y = yl) +
     scale_x_discrete(labels = plbl) +
     theme(
       plot.title = element_markdown(face = title_face, size = title_size, hjust = 0.5),
       axis.text.x = element_markdown(size = axis_text_size),
       axis.text.y = element_markdown(size = axis_text_size),
       axis.title.x = element_markdown(size = axis_title_size, face = axis_title_face),
       axis.title.y = element_markdown(size = axis_title_size, face = axis_title_face),
       legend.text = element_markdown(size = legend_size),
       legend.title = element_blank(),
       panel.background = element_rect(fill = bg_color),
       plot.background = element_rect(fill = bg_color, color = NA)
     )
   
   # Legend
   if (show_legend) {
     p <- p + theme(legend.position = "bottom")
   } else {
     p <- p + theme(legend.position = "none")
   }
   
   p
 }
 
 # --- Reactive: list of separate plots ----------------------------------------
 mp_plot_list <- reactive({
   df <- mp_data()
   params <- input$mp_sel_params
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(params) || length(params) == 0 || is.null(grp_levels)) return(NULL)
   # Filter to params that actually exist in df
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(NULL)
   
   ymin_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymin else NA
   ymax_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymax else NA
   
   # Gather custom colors if palette is "custom"
   custom_cols <- NULL
   if (!is.null(input$mp_palette) && input$mp_palette == "custom") {
     n_grps <- length(grp_levels)
     custom_cols <- sapply(seq_len(n_grps), function(i) {
       val <- input[[paste0("mp_custom_col_", i)]]
       if (is.null(val)) "#3c8dbc" else val
     })
   }
   
   plots <- lapply(params, function(param) {
     pp <- rv$mp_param_labels[[param]]
     pp_title <- if (!is.null(pp$title) && nzchar(pp$title)) pp$title else ""
     pp_ylab <- if (!is.null(pp$ylab) && nzchar(pp$ylab)) pp$ylab else input$mp_ylab
     
     # Compute group label overrides
     mp_gl <- NULL
     if (length(grp_levels) > 0) {
       mp_gl <- sapply(seq_along(grp_levels), function(i) {
         val <- input[[paste0("mp_grp_label_", i)]]
         if (!is.null(val) && nzchar(val)) val else grp_levels[i]
       })
       names(mp_gl) <- grp_levels
       if (all(mp_gl == grp_levels)) mp_gl <- NULL
     }
     
     mp_build_one_plot(
       df = df, param = param, grp_levels = grp_levels,
       plot_type = input$mp_plot_type, palette = input$mp_palette,
       alpha_val = input$mp_alpha, pt_size = input$mp_point_size,
       theme_name = input$mp_theme, show_signif = isTRUE(input$mp_show_signif),
       test_method = input$mp_test_method, signif_label = input$mp_signif_label,
       custom_ylim = isTRUE(input$mp_custom_ylim), ymin = ymin_val, ymax = ymax_val,
       line_width = input$mp_line_width,
       width_adj = input$mp_width_adj,
       bg_color = input$mp_bg_color,
       title_text = pp_title,
       xlab_text = input$mp_xlab,
       ylab_text = pp_ylab,
       title_size = input$mp_title_size,
       axis_title_size = input$mp_axis_title_size,
       axis_text_size = input$mp_axis_text_size,
       legend_size = input$mp_legend_size,
       title_bold = isTRUE(input$mp_title_bold),
       title_italic = isTRUE(input$mp_title_italic),
       axis_title_bold = isTRUE(input$mp_axis_title_bold),
       axis_title_italic = isTRUE(input$mp_axis_title_italic),
       underline_title = isTRUE(input$mp_underline_title),
       show_legend = isTRUE(input$mp_show_legend),
       coord_flip = isTRUE(input$mp_coord_flip),
       show_mean = isTRUE(input$mp_show_mean),
       show_errorbar = isTRUE(input$mp_show_errorbar),
       errorbar_type = if (!is.null(input$mp_errorbar_type)) input$mp_errorbar_type else "se",
       errorbar_width = if (!is.null(input$mp_errorbar_width)) input$mp_errorbar_width else 0.2,
       show_jitter = isTRUE(input$mp_show_jitter),
       jitter_width = if (!is.null(input$mp_jitter_width)) input$mp_jitter_width else 0.15,
       jitter_alpha = if (!is.null(input$mp_jitter_alpha)) input$mp_jitter_alpha else 0.5,
       signif_step = if (!is.null(input$mp_signif_step)) input$mp_signif_step else 0.05,
       signif_text_size = if (!is.null(input$mp_signif_text_size)) input$mp_signif_text_size else 3.5,
       custom_colors = custom_cols,
       whisker_type = if (!is.null(input$mp_whisker_type)) input$mp_whisker_type else "default",
       signif_style = if (!is.null(input$mp_signif_style)) input$mp_signif_style else "text",
       grp_labels = mp_gl
     )
   })
   names(plots) <- params
   plots
 })
 
 # --- Reactive: single combined plot ------------------------------------------
 mp_single_plot <- reactive({
   df <- mp_data()
   params <- input$mp_sel_params
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(params) || length(params) == 0 || is.null(grp_levels)) return(NULL)
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(NULL)
   
   ymin_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymin else NA
   ymax_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymax else NA
   
   # Gather custom colors if palette is "custom"
   custom_cols <- NULL
   if (!is.null(input$mp_palette) && input$mp_palette == "custom") {
     n_grps <- length(grp_levels)
     custom_cols <- sapply(seq_len(n_grps), function(i) {
       val <- input[[paste0("mp_custom_col_", i)]]
       if (is.null(val)) "#3c8dbc" else val
     })
   }
   
   # Compute group label overrides
   mp_gl <- NULL
   if (length(grp_levels) > 0) {
     mp_gl <- sapply(seq_along(grp_levels), function(i) {
       val <- input[[paste0("mp_grp_label_", i)]]
       if (!is.null(val) && nzchar(val)) val else grp_levels[i]
     })
     names(mp_gl) <- grp_levels
     if (all(mp_gl == grp_levels)) mp_gl <- NULL
   }
   
   mp_build_combined_plot(
     df = df, params = params, grp_levels = grp_levels,
     plot_type = input$mp_plot_type, palette = input$mp_palette,
     alpha_val = input$mp_alpha, pt_size = input$mp_point_size,
     theme_name = input$mp_theme, show_signif = isTRUE(input$mp_show_signif),
     test_method = input$mp_test_method, signif_label = input$mp_signif_label,
     custom_ylim = isTRUE(input$mp_custom_ylim), ymin = ymin_val, ymax = ymax_val,
     free_y = isTRUE(input$mp_free_y),
     line_width = input$mp_line_width,
     width_adj = input$mp_width_adj,
     bg_color = input$mp_bg_color,
     title_text = "",
     xlab_text = input$mp_xlab,
     ylab_text = input$mp_ylab,
     title_size = input$mp_title_size,
     axis_title_size = input$mp_axis_title_size,
     axis_text_size = input$mp_axis_text_size,
     legend_size = input$mp_legend_size,
     title_bold = isTRUE(input$mp_title_bold),
     title_italic = isTRUE(input$mp_title_italic),
     axis_title_bold = isTRUE(input$mp_axis_title_bold),
     axis_title_italic = isTRUE(input$mp_axis_title_italic),
     underline_title = isTRUE(input$mp_underline_title),
     show_legend = isTRUE(input$mp_show_legend),
     coord_flip = isTRUE(input$mp_coord_flip),
     show_mean = isTRUE(input$mp_show_mean),
     show_errorbar = isTRUE(input$mp_show_errorbar),
     errorbar_type = if (!is.null(input$mp_errorbar_type)) input$mp_errorbar_type else "se",
     errorbar_width = if (!is.null(input$mp_errorbar_width)) input$mp_errorbar_width else 0.2,
     show_jitter = isTRUE(input$mp_show_jitter),
     jitter_width = if (!is.null(input$mp_jitter_width)) input$mp_jitter_width else 0.15,
     jitter_alpha = if (!is.null(input$mp_jitter_alpha)) input$mp_jitter_alpha else 0.5,
     signif_step = if (!is.null(input$mp_signif_step)) input$mp_signif_step else 0.05,
     signif_text_size = if (!is.null(input$mp_signif_text_size)) input$mp_signif_text_size else 3.5,
     custom_colors = custom_cols,
     whisker_type = if (!is.null(input$mp_whisker_type)) input$mp_whisker_type else "default",
     signif_style = if (!is.null(input$mp_signif_style)) input$mp_signif_style else "text",
     param_labels = rv$mp_param_labels,
     grp_labels = mp_gl
   )
 })
 
 # --- Reactive: grouped single plot (params on x-axis, groups dodged) --------
 mp_grouped_plot <- reactive({
   df <- mp_data()
   params <- input$mp_sel_params
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(params) || length(params) == 0 || is.null(grp_levels)) return(NULL)
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(NULL)
   
   ymin_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymin else NA
   ymax_val <- if (isTRUE(input$mp_custom_ylim)) input$mp_ymax else NA
   
   custom_cols <- NULL
   if (!is.null(input$mp_palette) && input$mp_palette == "custom") {
     n_grps <- length(grp_levels)
     custom_cols <- sapply(seq_len(n_grps), function(i) {
       val <- input[[paste0("mp_custom_col_", i)]]
       if (is.null(val)) "#3c8dbc" else val
     })
   }
   
   # Compute group label overrides
   mp_gl <- NULL
   if (length(grp_levels) > 0) {
     mp_gl <- sapply(seq_along(grp_levels), function(i) {
       val <- input[[paste0("mp_grp_label_", i)]]
       if (!is.null(val) && nzchar(val)) val else grp_levels[i]
     })
     names(mp_gl) <- grp_levels
     if (all(mp_gl == grp_levels)) mp_gl <- NULL
   }
   
   mp_build_grouped_plot(
     df = df, params = params, grp_levels = grp_levels,
     plot_type = input$mp_plot_type, palette = input$mp_palette,
     alpha_val = input$mp_alpha, pt_size = input$mp_point_size,
     theme_name = input$mp_theme, show_signif = isTRUE(input$mp_show_signif),
     test_method = input$mp_test_method, signif_label = input$mp_signif_label,
     custom_ylim = isTRUE(input$mp_custom_ylim), ymin = ymin_val, ymax = ymax_val,
     free_y = isTRUE(input$mp_free_y),
     line_width = input$mp_line_width,
     width_adj = input$mp_width_adj,
     bg_color = input$mp_bg_color,
     title_text = "",
     xlab_text = input$mp_xlab,
     ylab_text = input$mp_ylab,
     title_size = input$mp_title_size,
     axis_title_size = input$mp_axis_title_size,
     axis_text_size = input$mp_axis_text_size,
     legend_size = input$mp_legend_size,
     title_bold = isTRUE(input$mp_title_bold),
     title_italic = isTRUE(input$mp_title_italic),
     axis_title_bold = isTRUE(input$mp_axis_title_bold),
     axis_title_italic = isTRUE(input$mp_axis_title_italic),
     underline_title = isTRUE(input$mp_underline_title),
     show_legend = isTRUE(input$mp_show_legend),
     coord_flip = isTRUE(input$mp_coord_flip),
     show_mean = isTRUE(input$mp_show_mean),
     show_errorbar = isTRUE(input$mp_show_errorbar),
     errorbar_type = if (!is.null(input$mp_errorbar_type)) input$mp_errorbar_type else "se",
     errorbar_width = if (!is.null(input$mp_errorbar_width)) input$mp_errorbar_width else 0.2,
     show_jitter = isTRUE(input$mp_show_jitter),
     jitter_width = if (!is.null(input$mp_jitter_width)) input$mp_jitter_width else 0.15,
     jitter_alpha = if (!is.null(input$mp_jitter_alpha)) input$mp_jitter_alpha else 0.5,
     signif_step = if (!is.null(input$mp_signif_step)) input$mp_signif_step else 0.05,
     signif_text_size = if (!is.null(input$mp_signif_text_size)) input$mp_signif_text_size else 3.5,
     custom_colors = custom_cols,
     whisker_type = if (!is.null(input$mp_whisker_type)) input$mp_whisker_type else "default",
     signif_style = if (!is.null(input$mp_signif_style)) input$mp_signif_style else "text",
     param_labels = rv$mp_param_labels,
     grp_labels = mp_gl
   )
 })
 
 # --- Render plot outputs (dynamic UI) ----------------------------------------
 output$mp_plots_ui <- renderUI({
   df <- mp_data()
   params <- input$mp_sel_params
   if (is.null(df) || is.null(params) || length(params) == 0)
     return(div(style = "padding:30px; text-align:center; color:#999;",
                icon("chart-bar", style = "font-size:48px;"), br(), br(),
                "Load data and select parameters to see plots."))
   
   view <- input$mp_view_mode
   if (is.null(view)) view <- "separate"
   
   if (view == "combined") {
     # Single combined faceted plot
     n <- length(params)
     h <- max(380, 280 * ceiling(n / 3))
     tagList(plotOutput("mp_combined_plot_out", height = paste0(h, "px")))
   } else if (view == "grouped") {
     # Grouped single plot: all parameters on x-axis, groups dodged
     n <- length(params)
     h <- max(450, 100 * n + 200)
     tagList(plotOutput("mp_grouped_plot_out", height = paste0(h, "px")))
   } else {
     # Separate individual plots
     n <- length(params)
     ncols <- if (n <= 2) n else if (n <= 4) 2 else 3
     plot_height <- 380
     plot_tags <- lapply(seq_along(params), function(i) {
       pid <- paste0("mp_plot_", i)
       column(12 / ncols,
         plotOutput(pid, height = paste0(plot_height, "px"))
       )
     })
     rows <- split(plot_tags, ceiling(seq_along(plot_tags) / ncols))
     do.call(tagList, lapply(rows, function(r) fluidRow(r)))
   }
 })
 
 # Render individual plots dynamically
 observe({
   plots <- mp_plot_list()
   if (is.null(plots)) return()
   lapply(seq_along(plots), function(i) {
     local({
       idx <- i
       pid <- paste0("mp_plot_", idx)
       output[[pid]] <- renderPlot({ mp_plot_list()[[idx]] }, res = 96)
     })
   })
 })
 
 # Render combined plot
 output$mp_combined_plot_out <- renderPlot({
   p <- mp_single_plot()
   req(p)
   p
 }, res = 96)
 
 # Render grouped plot
 output$mp_grouped_plot_out <- renderPlot({
   p <- mp_grouped_plot()
   req(p)
   p
 }, res = 96)
 
 # --- Statistical results (reactive data.frame) -------------------------------
 mp_stats_data <- reactive({
   df <- mp_data()
   params <- input$mp_sel_params
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(params) || is.null(grp_levels)) return(NULL)
   # Filter data to selected groups
   df <- df[df$Group %in% grp_levels, , drop = FALSE]
   test_method <- input$mp_test_method
   if (is.null(test_method) || test_method == "none" ||
       grepl("^header_", test_method)) return(NULL)
   
   params <- params[params %in% colnames(df)]
   if (length(params) == 0) return(NULL)
   
   n_grps <- length(grp_levels)
   ttype <- mp_test_type(test_method)
   
   # Validate group count vs test type
   if (ttype == "two_group" && n_grps < 2) return(NULL)
   if (ttype == "multi_group" && n_grps < 2) return(NULL)
   if (ttype == "overall" && n_grps < 2) return(NULL)
   
   sigma1 <- if (!is.null(input$mp_known_sigma)) input$mp_known_sigma else 1
   sigma2 <- if (!is.null(input$mp_known_sigma2)) input$mp_known_sigma2 else 1
   
   results <- lapply(params, function(param) {
     vals <- suppressWarnings(as.numeric(df[[param]]))
     grps <- factor(df$Group, levels = grp_levels)
     
     test_res <- tryCatch({
       if (ttype == "two_group") {
         # Two-group tests: use first two groups
         g1 <- vals[grps == grp_levels[1] & !is.na(vals)]
         g2 <- vals[grps == grp_levels[2] & !is.na(vals)]
         
         if (test_method == "t.test") {
           tt <- t.test(g1, g2, var.equal = TRUE)
           list(statistic = round(tt$statistic, 4), p = tt$p.value,
                method = "Student's t-test")
         } else if (test_method == "welch") {
           tt <- t.test(g1, g2, var.equal = FALSE)
           list(statistic = round(tt$statistic, 4), p = tt$p.value,
                method = "Welch's t-test")
         } else if (test_method == "two_z") {
           n1 <- length(g1); n2 <- length(g2)
           z <- (mean(g1) - mean(g2)) / sqrt(sigma1^2 / n1 + sigma2^2 / n2)
           p_val <- 2 * pnorm(-abs(z))
           list(statistic = round(z, 4), p = p_val, method = "Two Sample Z-Test")
         } else if (test_method == "wilcox.test") {
           nx <- length(g1); ny <- length(g2)
           df_coin <- data.frame(
             value = c(g1, g2),
             group = factor(c(rep("A", nx), rep("B", ny)))
           )
           if ((nx + ny) <= 200) {
             coin_res <- coin::wilcox_test(value ~ group, data = df_coin,
                                            distribution = "exact")
           } else {
             coin_res <- coin::wilcox_test(value ~ group, data = df_coin,
                                            distribution = "asymptotic")
           }
           p_val <- as.numeric(coin::pvalue(coin_res))
           z_stat <- as.numeric(coin::statistic(coin_res, type = "standardized"))
           ranks <- rank(c(g1, g2))
           R1 <- sum(ranks[1:nx])
           U1 <- R1 - nx * (nx + 1) / 2
           U2 <- nx * ny - U1
           U  <- min(U1, U2)
           meth <- if ((nx + ny) <= 200) "Mann-Whitney U (Exact)" else "Mann-Whitney U (Approx)"
           list(statistic = U, p = p_val, method = meth, Z = z_stat)
         } else if (test_method == "ks_two") {
           ks <- ks.test(g1, g2)
           list(statistic = round(ks$statistic, 4), p = ks$p.value,
                method = "Kolmogorov-Smirnov (two-sample)")
         } else if (test_method == "paired_t") {
           if (length(g1) != length(g2))
             list(statistic = NA, p = NA, method = "Paired t-test (unequal n)")
           else {
             tt <- t.test(g1, g2, paired = TRUE)
             list(statistic = round(tt$statistic, 4), p = tt$p.value,
                  method = "Paired t-test")
           }
         } else if (test_method == "wilcoxon_sr") {
           if (length(g1) != length(g2))
             list(statistic = NA, p = NA, method = "Wilcoxon Signed-Rank (unequal n)")
           else {
             wt <- wilcox.test(g1, g2, paired = TRUE)
             list(statistic = round(wt$statistic, 4), p = wt$p.value,
                  method = "Wilcoxon Signed-Rank")
           }
         } else if (test_method == "f_test") {
           ft <- var.test(g1, g2)
           list(statistic = round(ft$statistic, 4), p = ft$p.value,
                method = "F-Test for Variances")
         } else {
           list(statistic = NA, p = NA, method = "Unknown")
         }
       } else if (ttype == "multi_group") {
         # Multi-group tests: use all groups
         valid <- !is.na(vals)
         df_tmp <- data.frame(value = vals[valid], group = grps[valid])
         if (test_method == "anova") {
           fit <- aov(value ~ group, data = df_tmp)
           s <- summary(fit)[[1]]
           list(statistic = round(s$`F value`[1], 4), p = s$`Pr(>F)`[1],
                method = "One-Way ANOVA")
         } else if (test_method == "kruskal") {
           kt <- kruskal.test(value ~ group, data = df_tmp)
           list(statistic = round(kt$statistic, 4), p = kt$p.value,
                method = "Kruskal-Wallis")
         } else {
           list(statistic = NA, p = NA, method = "Unknown")
         }
       } else if (ttype == "overall") {
         valid <- !is.na(vals)
         df_tmp <- data.frame(value = vals[valid], group = grps[valid])
         if (test_method == "levene") {
           lt <- car::leveneTest(value ~ group, data = df_tmp)
           list(statistic = round(lt$`F value`[1], 4), p = lt$`Pr(>F)`[1],
                method = "Levene's Test")
         } else {
           list(statistic = NA, p = NA, method = "Unknown")
         }
       } else if (ttype == "per_group") {
         # Shapiro-Wilk per group: return combined results
         grp_results <- lapply(grp_levels, function(gl) {
           gv <- vals[grps == gl & !is.na(vals)]
           if (length(gv) < 3 || length(gv) > 5000)
             return(data.frame(Parameter = param, Method = paste0("Shapiro-Wilk (", gl, ")"),
               Statistic = NA, Z = NA, p.value = NA, Significance = "\u2014",
               stringsAsFactors = FALSE))
           sw <- shapiro.test(gv)
           p_val <- sw$p.value
           sig <- if (is.na(p_val)) "\u2014"
                  else if (p_val < 0.001) "***"
                  else if (p_val < 0.01) "**"
                  else if (p_val < 0.05) "*"
                  else "ns"
           data.frame(Parameter = param, Method = paste0("Shapiro-Wilk (", gl, ")"),
             Statistic = round(sw$statistic, 4), Z = NA,
             p.value = signif(p_val, 4), Significance = sig,
             stringsAsFactors = FALSE)
         })
         return(do.call(rbind, grp_results))
       } else {
         list(statistic = NA, p = NA, method = "Unknown")
       }
     }, error = function(e) list(statistic = NA, p = NA, method = paste0("Error: ", e$message)))
     
     # Per-group tests already return data.frame
     if (is.data.frame(test_res)) return(test_res)
     
     sig <- if (is.na(test_res$p)) "\u2014"
            else if (test_res$p < 0.001) "***"
            else if (test_res$p < 0.01) "**"
            else if (test_res$p < 0.05) "*"
            else "ns"
     
     z_val <- if (!is.null(test_res$Z) && !is.na(test_res$Z)) round(test_res$Z, 4) else NA
     
     data.frame(Parameter = param, Method = test_res$method,
       Statistic = round(as.numeric(test_res$statistic), 4),
       Z = z_val,
       p.value = if (is.na(test_res$p)) NA else signif(test_res$p, 4),
       Significance = sig, stringsAsFactors = FALSE)
   })
   
   do.call(rbind, results)
 })
 
 # --- Statistical results table -----------------------------------------------
 output$mp_stats_table <- renderDT({
   res_df <- mp_stats_data()
   req(res_df)
   datatable(res_df, rownames = FALSE, options = list(dom = "t", pageLength = 50),
     class = "display compact") %>%
     formatStyle("Significance",
       color = styleEqual(c("***", "**", "*", "ns", "\u2014"),
                          c("#c0392b", "#c0392b", "#e67e22", "#27ae60", "#999")),
       fontWeight = "bold")
 })
 
 # --- Descriptive statistics (reactive data.frame) ----------------------------
 mp_desc_data <- reactive({
   df <- mp_data()
   params <- input$mp_sel_params
   grp_levels <- mp_grp_levels()
   if (is.null(df) || is.null(params) || is.null(grp_levels)) return(NULL)
   
   desc <- lapply(params, function(param) {
     if (!param %in% colnames(df)) return(NULL)
     lapply(grp_levels, function(grp) {
       vals <- df[[param]][df$Group == grp]
       vals <- suppressWarnings(as.numeric(vals))
       vals <- vals[!is.na(vals)]
       if (length(vals) == 0) {
         return(data.frame(
           Parameter = param, Group = grp, N = 0,
           Mean = NA, SD = NA, Median = NA, Min = NA, Max = NA,
           stringsAsFactors = FALSE))
       }
       data.frame(
         Parameter = param, Group = grp, N = length(vals),
         Mean = round(mean(vals), 4), SD = round(sd(vals), 4),
         Median = round(median(vals), 4),
         Min = round(min(vals), 4), Max = round(max(vals), 4),
         stringsAsFactors = FALSE)
     })
   })
   do.call(rbind, unlist(desc, recursive = FALSE))
 })
 
 # --- Descriptive statistics table --------------------------------------------
 output$mp_desc_table <- renderDT({
   desc_df <- mp_desc_data()
   req(desc_df)
   datatable(desc_df, rownames = FALSE, options = list(dom = "t", pageLength = 50),
     class = "display compact")
 })
 
 # --- Download: Export statistical results tables -----------------------------
 output$mp_dl_stats_csv <- downloadHandler(
   filename = function() paste0("multi_param_stats_", Sys.Date(), ".csv"),
   content = function(file) {
     res <- mp_stats_data()
     if (is.null(res)) res <- data.frame(Note = "No results to export")
     write.csv(res, file, row.names = FALSE)
   }
 )
 
 output$mp_dl_stats_xlsx <- downloadHandler(
   filename = function() paste0("multi_param_stats_", Sys.Date(), ".xlsx"),
   content = function(file) {
     res <- mp_stats_data()
     if (is.null(res)) res <- data.frame(Note = "No results to export")
     if (requireNamespace("writexl", quietly = TRUE)) {
       writexl::write_xlsx(res, file)
     } else {
       write.csv(res, file, row.names = FALSE)
       showNotification("writexl not installed. Saved as CSV instead.", type = "warning")
     }
   }
 )
 
 output$mp_dl_desc_csv <- downloadHandler(
   filename = function() paste0("multi_param_descriptive_", Sys.Date(), ".csv"),
   content = function(file) {
     desc <- mp_desc_data()
     if (is.null(desc)) desc <- data.frame(Note = "No data to export")
     write.csv(desc, file, row.names = FALSE)
   }
 )
 
 output$mp_dl_desc_xlsx <- downloadHandler(
   filename = function() paste0("multi_param_descriptive_", Sys.Date(), ".xlsx"),
   content = function(file) {
     desc <- mp_desc_data()
     if (is.null(desc)) desc <- data.frame(Note = "No data to export")
     if (requireNamespace("writexl", quietly = TRUE)) {
       writexl::write_xlsx(desc, file)
     } else {
       write.csv(desc, file, row.names = FALSE)
       showNotification("writexl not installed. Saved as CSV instead.", type = "warning")
     }
   }
 )
 
 # --- Download: use combined or grid of separate plots ------------------------
 mp_download_plot <- reactive({
   if (isTRUE(input$mp_view_mode == "combined")) {
     mp_single_plot()
   } else if (isTRUE(input$mp_view_mode == "grouped")) {
     mp_grouped_plot()
   } else {
     plots <- mp_plot_list()
     req(plots)
     cowplot::plot_grid(plotlist = plots, ncol = min(length(plots), 3),
                        align = "h", axis = "tb")
   }
 })
 
 output$mp_download_png <- downloadHandler(
   filename = function() paste0("multi_param_comparison_", Sys.Date(), ".png"),
   content = function(file) {
     p <- mp_download_plot()
     req(p)
     n <- length(input$mp_sel_params)
     ncols <- min(n, 3)
     nrows <- ceiling(n / 3)
     view <- input$mp_view_mode
     if (isTRUE(view == "grouped") || isTRUE(view == "combined")) {
       w <- input$mp_dl_width
       h <- input$mp_dl_height
     } else {
       w <- input$mp_dl_width * ncols / 2
       h <- (input$mp_dl_height + 2) * max(nrows, 1)
     }
     ggplot2::ggsave(file, plot = p, width = w, height = h,
       units = "cm", dpi = input$mp_dl_dpi, device = "png")
   }
 )
 
 output$mp_download_svg <- downloadHandler(
   filename = function() paste0("multi_param_comparison_", Sys.Date(), ".svg"),
   content = function(file) {
     p <- mp_download_plot()
     req(p)
     n <- length(input$mp_sel_params)
     ncols <- min(n, 3)
     nrows <- ceiling(n / 3)
     view <- input$mp_view_mode
     if (isTRUE(view == "grouped") || isTRUE(view == "combined")) {
       w <- input$mp_dl_width
       h <- input$mp_dl_height
     } else {
       w <- input$mp_dl_width * ncols / 2
       h <- (input$mp_dl_height + 2) * max(nrows, 1)
     }
     ggplot2::ggsave(file, plot = p, width = w, height = h,
       units = "cm", device = svglite::svglite)
   }
 )
 
} # end server

# =============================================================================
# RUN APP
# =============================================================================
shinyApp(ui = ui, server = server)
