# ============================================
# CKM-CRA Shiny App (完整版 v3)
# ============================================

library(shiny)
library(bslib)
library(ggplot2)
library(ckmCRA)

# ============================================
# Helper functions
# ============================================

interpret_cra <- function(cra) {
  if (cra >= 4) {
    list(
      label  = "High accessibility",
      color  = "#2E7D32",
      icon   = "circle-check",
      text   = "This patient is ELIGIBLE for treatment de-escalation.",
      detail = "Most risk factors are well-controlled. Guidelines suggest reducing treatment intensity to avoid overtreatment."
    )
  } else if (cra == 3) {
    list(
      label  = "Moderate accessibility",
      color  = "#F9A825",
      icon   = "circle-exclamation",
      text   = "Partially eligible for de-escalation.",
      detail = "Some risk factors are controlled, but not enough for full de-escalation. Targeted intervention on remaining factors may help."
    )
  } else {
    list(
      label  = "Low accessibility",
      color  = "#C62828",
      icon   = "circle-xmark",
      text   = "NOT eligible for de-escalation.",
      detail = "Multiple risk factors are uncontrolled. Continue or intensify treatment."
    )
  }
}

interpret_risk <- function(cat) {
  switch(as.character(cat),
         "Low" = list(
           color  = "#2E7D32",
           text   = "Lower than average mortality risk for CKM 3-4 patients.",
           detail = "Continue current management and routine follow-up."
         ),
         "Moderate" = list(
           color  = "#F9A825",
           text   = "Average mortality risk for CKM 3-4 patients.",
           detail = "Standard follow-up recommended. Address modifiable factors."
         ),
         "High" = list(
           color  = "#EF6C00",
           text   = "Elevated mortality risk.",
           detail = "Intensify risk factor management. Consider specialist referral."
         ),
         "Very High" = list(
           color  = "#C62828",
           text   = "Substantially elevated mortality risk.",
           detail = "Urgent comprehensive evaluation. Aggressive risk factor control."
         ),
         list(color = "#607D8B", text = "", detail = "")
  )
}

# ============================================
# UI
# ============================================
ui <- page_navbar(
  title = tags$span(icon("heart-pulse"), " CKM-CRA Risk Calculator"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1565C0",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  fillable = FALSE,
  
  # ============ TAB 1: Patient Assessment ============
  nav_panel(
    "Patient Assessment",
    icon = icon("user-doctor"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        
        h5(icon("clipboard-list"), " Patient Information"),
        accordion(
          id = "acc", open = "demo",
          
          accordion_panel("Demographics", icon = icon("id-card"),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("age", "Age (years)", 60, 20, 90),
                                         selectInput("sex", "Sex",
                                                     c("Male" = 1, "Female" = 2), 1)
                          ),
                          selectInput("race", "Race/Ethnicity",
                                      c("Mexican American" = 1, "Other Hispanic" = 2,
                                        "NH White" = 3, "NH Black" = 4, "Other" = 5), 3),
                          layout_columns(col_widths = c(6, 6),
                                         selectInput("educ", "Education",
                                                     c("<HS" = 1, "HS" = 2, "College+" = 3), 2),
                                         selectInput("pir_cat", "Income (PIR)",
                                                     c("Low" = 1, "Middle" = 2, "High" = 3), 2)
                          ),
                          selectInput("dm", "Diabetes",
                                      c("No" = 0, "Yes" = 1), 0)
          ),
          
          accordion_panel("Blood Pressure & Glycemia", icon = icon("heart"),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("BPXSY1", "Systolic BP (mmHg)", 120, 70, 250),
                                         numericInput("BPXDI1", "Diastolic BP (mmHg)", 75, 40, 150)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXGH", "HbA1c (%)", 5.5, 3, 15, 0.1),
                                         numericInput("EGFR", "eGFR (mL/min/1.73m2)", 90, 5, 150)
                          )
          ),
          
          accordion_panel("Kidney Panel", icon = icon("droplet"),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXSCR", "Creatinine (mg/dL)", 0.9, 0.3, 10, 0.01),
                                         numericInput("LBXSBU", "BUN (mg/dL)", 14, 2, 100)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("UACR", "UACR (mg/g)", 15, 0, 5000),
                                         numericInput("LBXSAL", "Albumin (g/dL)", 4.2, 1, 6, 0.1)
                          ),
                          numericInput("LBXHGB", "Hemoglobin (g/dL)", 14, 5, 20, 0.1)
          ),
          
          accordion_panel("Lipids & Liver", icon = icon("flask"),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXTC", "Total Cholesterol", 180, 50, 500),
                                         numericInput("LBDLDL", "LDL", 100, 20, 400)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXTR", "Triglycerides", 120, 20, 1000),
                                         numericInput("LBDHDD", "HDL", 50, 10, 150)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXSATSI", "ALT (U/L)", 20, 1, 500),
                                         numericInput("LBXSASSI", "AST (U/L)", 22, 1, 500)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXSAPSI", "ALP (U/L)", 70, 10, 500),
                                         numericInput("LBXSTB", "Bilirubin", 0.7, 0.1, 10, 0.1)
                          )
          ),
          
          accordion_panel("Blood & Others", icon = icon("vial"),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("bmi", "BMI (kg/m2)", 25, 10, 70, 0.1),
                                         numericInput("BMXWAIST", "Waist (cm)", 90, 50, 200)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("BPXPLS", "Pulse", 72, 30, 200),
                                         numericInput("LBXWBCSI", "WBC", 7, 1, 30, 0.1)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXRDW", "RDW (%)", 13, 10, 30, 0.1),
                                         numericInput("LBXMCVSI", "MCV (fL)", 90, 50, 150)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXNEPCT", "Neutrophil (%)", 60, 0, 100),
                                         numericInput("LBXLYPCT", "Lymphocyte (%)", 30, 0, 100)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXMOPCT", "Monocyte (%)", 7, 0, 30),
                                         numericInput("LBXGLU", "Glucose", 95, 30, 500)
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         numericInput("LBXIN", "Insulin", 10, 1, 200),
                                         numericInput("LBXSUA", "Uric Acid", 5, 1, 15)
                          )
          )
        ),
        
        br(),
        actionButton("run", "Calculate Risk",
                     class = "btn-primary btn-lg w-100",
                     icon = icon("calculator")),
        br(),
        
        div(class = "text-muted small",
            icon("info-circle"),
            " Results are estimates based on NHANES 1999-2018.",
            " Not a substitute for clinical judgment."
        )
      ),
      
      uiOutput("main_ui")
    )
  ),
  
  # ============ TAB 2: About ============
  nav_panel(
    "About",
    icon = icon("circle-info"),
    card(
      card_header(h3("About CKM-CRA")),
      card_body(
        h5("What is CRA?"),
        p("CRA (CKM Regression Accessibility) is a 5-dimension score that",
          "quantifies how many risk factors are well-controlled enough to",
          "consider treatment de-escalation."),
        h5("What does each dimension mean?"),
        tags$ul(
          tags$li(tags$b("Glycemia:"), " HbA1c <7.5% or no diabetes"),
          tags$li(tags$b("BP:"), " SBP <130 and DBP <80 mmHg"),
          tags$li(tags$b("Kidney:"), " eGFR \u2265 60 mL/min/1.73m\u00B2"),
          tags$li(tags$b("Albuminuria:"), " UACR <300 mg/g"),
          tags$li(tags$b("Obesity:"), " BMI <30 kg/m\u00B2")
        ),
        h5("What is organ age?"),
        p("Organ-specific biological age is predicted from clinical biomarkers",
          "using machine learning. A positive gap means the organ is aging",
          "faster than expected; negative means slower."),
        h5("What is the risk stratification?"),
        p("Based on 5-year absolute mortality risk:"),
        tags$ul(
          tags$li(tags$span(style = "color:#2E7D32;font-weight:bold;", "<5%"), " Low"),
          tags$li(tags$span(style = "color:#F9A825;font-weight:bold;", "5-10%"), " Moderate"),
          tags$li(tags$span(style = "color:#EF6C00;font-weight:bold;", "10-20%"), " High"),
          tags$li(tags$span(style = "color:#C62828;font-weight:bold;", "\u226520%"), " Very High")
        ),
        hr(),
        p(class = "text-muted",
          "Reference: CKM Regression Accessibility and Mortality in US Adults",
          "with Advanced Cardiometabolic-Kidney Disease.")
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a(
      href   = "https://github.com/zhuzhengyang517/ckmCRA",
      target = "_blank",
      icon("github"), " Source Code"
    )
  )
)

# ============================================
# Server
# ============================================
server <- function(input, output, session) {
  
  # ---- Collect patient data ----
  patient_data <- eventReactive(input$run, {
    data.frame(
      age = input$age, sex = as.numeric(input$sex),
      race = as.numeric(input$race), educ = as.numeric(input$educ),
      pir_cat = as.numeric(input$pir_cat), dm = as.numeric(input$dm),
      BPXSY1 = input$BPXSY1, BPXDI1 = input$BPXDI1,
      LBXGH = input$LBXGH, EGFR = input$EGFR,
      LBXSCR = input$LBXSCR, LBXSBU = input$LBXSBU,
      UACR = input$UACR, LBXSAL = input$LBXSAL, LBXHGB = input$LBXHGB,
      LBXTC = input$LBXTC, LBDLDL = input$LBDLDL,
      LBXTR = input$LBXTR, LBDHDD = input$LBDHDD,
      LBXSATSI = input$LBXSATSI, LBXSASSI = input$LBXSASSI,
      LBXSAPSI = input$LBXSAPSI, LBXSTB = input$LBXSTB,
      bmi = input$bmi, BMXWAIST = input$BMXWAIST,
      BPXPLS = input$BPXPLS, LBXWBCSI = input$LBXWBCSI,
      LBXRDW = input$LBXRDW, LBXMCVSI = input$LBXMCVSI,
      LBXNEPCT = input$LBXNEPCT, LBXLYPCT = input$LBXLYPCT,
      LBXMOPCT = input$LBXMOPCT, LBXGLU = input$LBXGLU,
      LBXIN = input$LBXIN, LBXSUA = input$LBXSUA
    )
  })
  
  # ---- Run pipeline ----
  result <- eventReactive(input$run, {
    req(patient_data())
    tryCatch(
      run_ckm_cra(patient_data(), verbose = FALSE),
      error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
  })
  
  # ---- Main UI ----
  output$main_ui <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(
        card(
          card_header(h3(icon("hand-wave"), " Welcome")),
          card_body(
            h5("How to use this calculator"),
            tags$ol(
              tags$li("Fill in patient information on the left."),
              tags$li("Click ", tags$b("Calculate Risk"), "."),
              tags$li("Review CRA score, organ ages, and mortality risk."),
              tags$li("Explore which biomarkers drive each organ age."),
              tags$li("Download the result as CSV.")
            ),
            hr(),
            p(tags$b("Interpretation at a glance:")),
            tags$ul(
              tags$li(tags$span(style = "color:#2E7D32;font-weight:bold;",
                                "CRA \u2265 4:"), " patient may be eligible for de-escalation"),
              tags$li(tags$span(style = "color:#C62828;font-weight:bold;",
                                "CRA < 3:"), " continue or intensify treatment"),
              tags$li(tags$b("Positive gap:"), " organ aging faster than expected"),
              tags$li(tags$b("Negative gap:"), " organ aging slower than expected")
            )
          )
        )
      )
    }
    r <- result()
    if (is.null(r)) return(NULL)
    
    cra_info  <- interpret_cra(r$CRA)
    risk_info <- interpret_risk(r$risk_category)
    
    tagList(
      # ==================== TOP: 3 summary cards ====================
      layout_columns(col_widths = c(4, 4, 4),
                     
                     card(
                       class = "border-0 shadow-sm",
                       card_header(
                         class = "bg-primary text-white",
                         icon("clipboard-check"), " CRA Score"
                       ),
                       card_body(
                         div(class = "text-center",
                             h1(sprintf("%d / 5", r$CRA),
                                style = sprintf("color: %s; font-weight: 700;", cra_info$color)),
                             h5(tags$span(icon(cra_info$icon), cra_info$label),
                                style = sprintf("color: %s;", cra_info$color)),
                             br(),
                             p(cra_info$text, style = "font-weight: 600;"),
                             p(class = "text-muted small", cra_info$detail)
                         )
                       )
                     ),
                     
                     card(
                       class = "border-0 shadow-sm",
                       card_header(
                         class = "bg-primary text-white",
                         icon("chart-line"), " Mortality Risk"
                       ),
                       card_body(
                         div(class = "text-center",
                             h2(r$risk_category,
                                style = sprintf("color: %s; font-weight: 700;", risk_info$color)),
                             br(),
                             h4(sprintf("%.1f%%", r$risk_5y * 100), style = "margin: 0;"),
                             p("5-year risk", class = "text-muted small"),
                             h4(sprintf("%.1f%%", r$risk_10y * 100), style = "margin: 0;"),
                             p("10-year risk", class = "text-muted small"),
                             br(),
                             p(risk_info$text, style = "font-weight: 600;"),
                             p(class = "text-muted small", risk_info$detail)
                         )
                       )
                     ),
                     
                     card(
                       class = "border-0 shadow-sm",
                       card_header(
                         class = "bg-primary text-white",
                         icon("shield-heart"), " Hazard Ratios"
                       ),
                       card_body(
                         div(class = "text-center",
                             p("Relative to a 60-year-old reference patient:",
                               class = "text-muted small"),
                             br(),
                             h4(sprintf("%.2f", r$HR_all_cause), style = "margin: 0;"),
                             p("All-cause death", class = "text-muted small"),
                             h4(sprintf("%.2f", r$sHR_cvd), style = "margin: 0;"),
                             p("CVD death", class = "text-muted small"),
                             h4(sprintf("%.2f", r$sHR_non_cvd), style = "margin: 0;"),
                             p("Non-CVD death", class = "text-muted small"),
                             br(),
                             p(class = "text-muted small",
                               "HR < 1 = lower risk, HR > 1 = higher risk")
                         )
                       )
                     )
      ),
      
      # ==================== MIDDLE: Organ ages + CRA dimensions ====================
      layout_columns(col_widths = c(7, 5),
                     
                     card(
                       card_header(icon("person-cane"), " Organ Biological Ages"),
                       card_body(
                         plotOutput("plot_organ_age", height = "380px"),
                         hr(),
                         p(class = "text-muted small",
                           "Bars show predicted biological age of each organ.",
                           "Ages above the dashed line (chronological age) indicate accelerated aging.")
                       )
                     ),
                     
                     card(
                       card_header(icon("ruler-horizontal"), " CRA Dimension Detail"),
                       card_body(
                         plotOutput("plot_cra_detail", height = "380px"),
                         hr(),
                         p(class = "text-muted small",
                           "Each bar shows how far the value is from the CRA threshold.",
                           "Longer bars = further from threshold (safer if green).")
                       )
                     )
      ),
      
      # ==================== ATTRIBUTION ====================
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-primary text-white",
          icon("magnifying-glass-chart"),
          " What Drives Each Organ Age?"
        ),
        card_body(
          layout_columns(col_widths = c(4, 8),
                         selectInput("attr_organ", "Select organ:",
                                     c("Heart" = "Heart",
                                       "Kidney" = "Kidney",
                                       "Metabolic" = "MetabInf"),
                                     width = "100%"),
                         div(class = "text-muted small",
                             style = "padding-top: 30px;",
                             icon("circle-info"),
                             " Bars to the right = older; left = younger.")
          ),
          plotOutput("plot_attribution", height = "400px"),
          hr(),
          uiOutput("attribution_advice")
        )
      ),
      
      # ==================== BOTTOM: Gap table + interpretation ====================
      layout_columns(col_widths = c(7, 5),
                     
                     card(
                       card_header(icon("arrows-up-down"), " Organ Age Gaps"),
                       card_body(
                         tableOutput("tbl_organ"),
                         hr(),
                         p(class = "text-muted small",
                           icon("circle-info"),
                           " Positive gap = organ aging faster than expected.",
                           " Negative gap = organ aging slower than expected.")
                       )
                     ),
                     
                     card(
                       card_header(icon("stethoscope"), " Clinical Interpretation"),
                       card_body(
                         uiOutput("interpretation_ui"),
                         hr(),
                         downloadButton("dl_csv", "Download Full Result (CSV)",
                                        class = "btn-primary w-100",
                                        icon = icon("download"))
                       )
                     )
      )
    )
  })
  
  # ==================== Organ age bar chart ====================
  output$plot_organ_age <- renderPlot({
    r <- result()
    df <- data.frame(
      Organ = factor(c("Heart", "Kidney", "Metabolic"),
                     levels = c("Heart", "Kidney", "Metabolic")),
      Age   = c(r$HeartAge_ML, r$KidneyAge_ML, r$MetabAge_ML)
    )
    
    ggplot(df, aes(x = Organ, y = Age, fill = Organ)) +
      geom_col(width = 0.6, color = "grey30", alpha = 0.85) +
      geom_hline(yintercept = r$age, linetype = "dashed",
                 color = "#C62828", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.1f", Age)),
                vjust = -0.5, size = 4.5, fontface = "bold") +
      annotate("text", x = 3.4, y = r$age, label = paste0("Actual: ", r$age),
               color = "#C62828", hjust = 1, vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = c("Heart" = "#1565C0",
                                   "Kidney" = "#F9A825",
                                   "Metabolic" = "#C62828")) +
      labs(x = "", y = "Biological age (years)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank())
  })
  
  # ==================== CRA Dimension Detail ====================
  output$plot_cra_detail <- renderPlot({
    r <- result()
    
    # Margin to threshold (positive = OK, negative = not OK)
    margins <- c(
      "Glycemia"    = (7.5 - r$LBXGH) / 7.5,
      "BP"          = min((130 - r$BPXSY1) / 130,
                          (80  - r$BPXDI1) / 80),
      "Kidney"      = (r$EGFR - 60) / 60,
      "Albuminuria" = (300 - r$UACR) / 300,
      "Obesity"     = (30  - r$bmi)  / 30
    )
    
    value_labels <- c(
      sprintf("HbA1c %.1f%%", r$LBXGH),
      sprintf("%.0f/%.0f mmHg", r$BPXSY1, r$BPXDI1),
      sprintf("eGFR %.0f", r$EGFR),
      sprintf("UACR %.0f", r$UACR),
      sprintf("BMI %.1f", r$bmi)
    )
    
    threshold_labels <- c(
      "HbA1c <7.5%",
      "SBP<130 / DBP<80",
      "eGFR \u2265 60",
      "UACR <300",
      "BMI <30"
    )
    
    df <- data.frame(
      dim       = factor(c("Glycemia", "BP", "Kidney",
                           "Albuminuria", "Obesity"),
                         levels = rev(c("Glycemia", "BP", "Kidney",
                                        "Albuminuria", "Obesity"))),
      margin    = as.numeric(margins),
      value     = value_labels,
      threshold = threshold_labels
    )
    
    df$margin_plot <- pmax(pmin(df$margin, 1), -1)
    df$fill_color  <- ifelse(df$margin > 0, "#2E7D32", "#C62828")
    df$status_icon <- ifelse(df$margin > 0, "\u2713", "\u2717")
    df$label_x <- ifelse(df$margin >= 0, df$margin_plot + 0.03,
                         df$margin_plot - 0.03)
    df$label_hj <- ifelse(df$margin >= 0, 0, 1)
    
    ggplot(df, aes(x = margin_plot, y = dim)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "grey40", linewidth = 0.7) +
      geom_col(aes(fill = fill_color),
               width = 0.6, alpha = 0.9, color = "grey30") +
      geom_text(aes(x = label_x, label = value, hjust = label_hj),
                size = 3.8, fontface = "bold", color = "grey20") +
      geom_text(aes(x = 0.02, label = threshold),
                hjust = 0, vjust = 2.2,
                size = 2.8, color = "grey50") +
      geom_text(aes(x = 1.12, label = status_icon),
                size = 6, fontface = "bold",
                color = df$fill_color) +
      scale_fill_identity() +
      scale_x_continuous(
        limits = c(-1.05, 1.25),
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = function(x) ifelse(x == 0, "at threshold",
                                    sprintf("%+.0f%%", x * 100))
      ) +
      labs(
        x = "Margin to threshold (% of threshold)",
        y = "",
        title = "CRA Dimension Detail",
        subtitle = "Longer bars = further from threshold; green = OK, red = not OK"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", size = 14,
                                     color = "#1565C0"),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, fontface = "bold")
      )
  })
  
  # ==================== Attribution plot ====================
  output$plot_attribution <- renderPlot({
    req(result(), input$attr_organ)
    r <- result()
    df <- explain_organ_age(r, organ = input$attr_organ)
    df$label <- label_vars(df$variable)
    df <- df[df$contribution != 0, ]
    
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 6,
                        label = "No significant driver detected."))
    }
    
    df <- head(df, 10)
    df$label <- factor(df$label, levels = rev(df$label))
    df$fill_color <- ifelse(df$contribution > 0, "#C62828", "#2E7D32")
    
    x_max <- max(abs(df$contribution)) * 1.4
    if (x_max < 1) x_max <- 1
    
    ggplot(df, aes(x = contribution, y = label)) +
      geom_col(aes(fill = fill_color), width = 0.65,
               color = "grey30", alpha = 0.9) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "grey40", linewidth = 0.6) +
      geom_text(aes(label = sprintf("%+.1f yr", contribution),
                    hjust = ifelse(contribution >= 0, -0.15, 1.15)),
                size = 4, fontface = "bold", color = "grey20") +
      scale_fill_identity() +
      scale_x_continuous(
        limits = c(-x_max, x_max),
        labels = function(x) sprintf("%+.0f", x)
      ) +
      labs(
        x = "Contribution to organ age (years)",
        y = "",
        title = sprintf("Top drivers of %s age",
                        switch(input$attr_organ,
                               "Heart"    = "Heart",
                               "Kidney"   = "Kidney",
                               "MetabInf" = "Metabolic")),
        subtitle = "Bars to the right = older; to the left = younger"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", size = 14,
                                     color = "#1565C0"),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold")
      )
  })
  
  # ==================== Attribution advice ====================
  output$attribution_advice <- renderUI({
    req(result(), input$attr_organ)
    r <- result()
    df <- explain_organ_age(r, organ = input$attr_organ)
    df$label <- label_vars(df$variable)
    df <- df[df$contribution != 0, ]
    
    positive <- df[df$contribution > 0, ]
    negative <- df[df$contribution < 0, ]
    
    pos_top <- if (nrow(positive) > 0)
      positive[which.max(positive$contribution), ] else NULL
    neg_top <- if (nrow(negative) > 0)
      negative[which.min(negative$contribution), ] else NULL
    
    organ_label <- switch(input$attr_organ,
                          "Heart"    = "heart",
                          "Kidney"   = "kidney",
                          "MetabInf" = "metabolic"
    )
    
    tagList(
      if (!is.null(pos_top)) {
        div(
          class = "alert",
          style = "background-color: #C6282815; border-left: 4px solid #C62828;",
          h6(icon("triangle-exclamation"),
             sprintf(" Needs attention: %s", pos_top$label),
             style = "color: #C62828; font-weight: bold; margin-bottom: 8px;"),
          p(sprintf("Your %s contributes %+.1f years to your %s age.",
                    pos_top$label, pos_top$contribution, organ_label)),
          p(sprintf("Optimizing this may reduce organ age by up to %.1f years.",
                    abs(pos_top$contribution)),
            style = "font-style: italic; color: grey30;")
        )
      },
      if (!is.null(neg_top)) {
        div(
          class = "alert",
          style = "background-color: #2E7D3215; border-left: 4px solid #2E7D32;",
          h6(icon("thumbs-up"),
             sprintf(" Keep it up: %s", neg_top$label),
             style = "color: #2E7D32; font-weight: bold; margin-bottom: 8px;"),
          p(sprintf("Your %s contributes %+.1f years to your %s age.",
                    neg_top$label, neg_top$contribution, organ_label)),
          p(sprintf("This is protecting your %s. Maintain current habits.",
                    organ_label),
            style = "font-style: italic; color: grey30;")
        )
      },
      div(
        class = "text-muted small",
        icon("circle-info"),
        " Attribution is based on single-variable perturbation. ",
        "Effects reflect deviation from the reference population, not causation."
      )
    )
  })
  
  # ==================== Gap table ====================
  output$tbl_organ <- renderTable({
    r <- result()
    gap_data <- data.frame(
      Organ = c("Heart", "Kidney", "Metabolic"),
      Age   = round(c(r$HeartAge_ML, r$KidneyAge_ML, r$MetabAge_ML), 1),
      Gap   = round(c(r$HeartAgeGap_ML, r$KidneyAgeGap_ML, r$MetabAgeGap_ML), 1)
    )
    gap_data$Status <- sapply(gap_data$Gap, function(g) {
      if (g > 5) "Accelerated"
      else if (g > 0) "Older"
      else if (g > -5) "Younger"
      else "Protective"
    })
    gap_data
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  # ==================== Clinical interpretation ====================
  output$interpretation_ui <- renderUI({
    r <- result()
    cra_info  <- interpret_cra(r$CRA)
    risk_info <- interpret_risk(r$risk_category)
    
    gaps <- c(Heart     = r$HeartAgeGap_ML,
              Kidney    = r$KidneyAgeGap_ML,
              Metabolic = r$MetabAgeGap_ML)
    worst_organ <- names(which.max(gaps))
    worst_gap   <- max(gaps)
    
    tagList(
      h6(icon("check-circle"), " Summary"),
      p(sprintf("This %d-year-old patient has a CRA score of %d/5 (%s).",
                r$age, r$CRA, cra_info$label)),
      p(sprintf("Estimated 5-year mortality risk is %.1f%% (%s).",
                r$risk_5y * 100, as.character(r$risk_category))),
      hr(),
      h6(icon("triangle-exclamation"), " Key Findings"),
      tags$ul(
        tags$li(sprintf("Most affected organ: %s (gap = %+.1f years).",
                        worst_organ, worst_gap)),
        tags$li(sprintf("CRA dimensions achieved: %d / 5.", r$CRA))
      ),
      hr(),
      h6(icon("notes-medical"), " Suggested Actions"),
      if (r$CRA >= 4) {
        tags$ul(
          tags$li("Consider de-escalation of treatment."),
          tags$li("Schedule routine follow-up.")
        )
      } else if (r$CRA == 3) {
        tags$ul(
          tags$li("Target the remaining uncontrolled dimension(s)."),
          tags$li("Reassess after 3-6 months.")
        )
      } else {
        tags$ul(
          tags$li("Continue current treatment intensity."),
          tags$li("Consider intensifying management of uncontrolled factors.")
        )
      },
      if (worst_gap > 5) {
        tagList(
          hr(),
          div(class = "alert alert-warning",
              icon("triangle-exclamation"),
              sprintf(" The %s shows accelerated aging (+%.1f years).",
                      worst_organ, worst_gap),
              " Close monitoring of this organ is recommended.")
        )
      }
    )
  })
  
  # ==================== Download ====================
  output$dl_csv <- downloadHandler(
    filename = function() paste0("ckm_cra_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(result(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)