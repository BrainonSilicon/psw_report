library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(glue)
library(pander)
library(ascii)

# ------------------------------
# Set up
# ------------------------------
out_root   <- "outputs_sandbox7"
out_tables <- file.path(out_root, "tables")
out_plots  <- file.path(out_root, "plots")
out_diag   <- file.path(out_root, "diagnostics")
dir.create(out_root,   showWarnings = FALSE, recursive = TRUE)
dir.create(out_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(out_plots,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_diag,   showWarnings = FALSE, recursive = TRUE)

oecd_country_codes <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX","MEX","NLD",
  "NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

indicators_4 <- c(
  "Adverse effects of medical treatment",
  "Maternal mortality (Deaths per 100 000 live births)",
  "Neonatal disorders (Deaths per 100 000 live births (standardised rate))",
  "Treatable mortality (Deaths per 100 000 population (standardised rate))"
)

# All four are "lower is better" → remem to flip sign before standardising
direction_basic <- tibble(
  indicator    = indicators_4,
  high_is_good = FALSE
)

# Colours
col_others  <- "#599ad3"
col_uk      <- "#f9a65a"
col_iceland <- "#DE7862FF"
col_luxem <- "#b97150"

uk_name     <- "United Kingdom"
uk_code     <- "GBR"
ice_nm  <- "Iceland"
ice_code <- "ISL"
luxem_nm  <- "Luxembourg"
luxem_code <- "LUX"

# Minimal global theme
theme_set(
  theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.title.y = element_blank()
    )
)


# ------------------------------
# Load data
# ------------------------------
input_data <- read_csv("data/ranking_data_9.csv") %>%
  filter(country_code %in% oecd_country_codes, indicator %in% indicators_4)
(unique(input_data$indicator))
(unique(input_data$country_code))

missing_inds <- setdiff(indicators_4, unique(input_data$indicator))
if (length(missing_inds)) {
  warning("These indicators in indicators_4 are not present in the data:\n  - ",
          paste(missing_inds, collapse = "\n  - "))
}

required_cols <- c("indicator","country_code","year","value")
missing_cols  <- setdiff(required_cols, names(input_data))
if (length(missing_cols)) {
  stop("Missing required columns in input_data.csv: ", paste(missing_cols, collapse = ", "))
}

dups <- input_data %>%
  count(indicator, country_code, year, name = "n_rows") %>%
  filter(n_rows > 1) %>%
  arrange(desc(n_rows), indicator, country_code, year)

if (nrow(dups) > 0) {
  message("⚠ Found duplicate rows per (indicator, country_code, year). Averaging within key and continuing.")
  write_csv(dups, file.path(out_diag, "duplicates_found.csv"))
  data1 <- input_data %>%
    group_by(indicator, country_code, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
} else {
  cat("No duplicate (indicator, country_code, year) keys found.\n")
  data1 <- input_data %>% select(indicator, country_code, year, value)
}

cleaned_data <- input_data %>%
  filter(country_code %in% oecd_country_codes,
         !is.na(country_code), !is.na(indicator), !is.na(year), !is.na(value)) %>%
  group_by(country_code, indicator, year) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

country_lookup <- input_data %>%
  filter(!is.na(country), country != "") %>%
  group_by(country_code) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(country_code, country_name = country)

glimpse(cleaned_data)
glimpse(input_data)

# ------------------------------
# Funtcions set up
# ------------------------------

pick_last3 <- function(df, exclude_zeros = FALSE,
                       zero_exceptions = c("NOR","DNK","IRL")) {
  # df: columns indicator, country_code, year, value
  df %>%
    group_by(indicator, country_code) %>%
    arrange(desc(year), .by_group = TRUE) %>%
    {
      if (exclude_zeros) {
        filter(., value != 0 | country_code %in% zero_exceptions)
      } else .
    } %>%
    slice_head(n = 3) %>%
    mutate(.rank_within = row_number()) %>%
    ungroup()
}

years_used_table <- function(win_df, tag) {
  # win_df has up to 3 rows per (indicator, country_code)
  wide <- win_df %>%
    arrange(indicator, country_code, desc(year)) %>%
    mutate(y = as.integer(year)) %>%
    group_by(indicator, country_code) %>%
    summarise(
      y1 = nth(y, 1),
      y2 = nth(y, 2),
      y3 = nth(y, 3),
      n_years_used = n(),
      years_used = paste(sort(unique(y), decreasing = TRUE), collapse = ", "),
      .groups = "drop"
    )
  out_csv <- file.path(out_tables, paste0("years_used_", tag, ".csv"))
  write_csv(wide, out_csv)

  cat(glue("\nYears-used table ({tag}) — preview:\n"))
  inds <- unique(wide$indicator)
  for (ind in inds) {
    cat(glue("\n[{ind}] — (first 20 rows)\n"))
    sub <- wide %>% filter(indicator == ind) %>% select(-indicator)
    pander::pander(head(sub, 20), style = "rmarkdown")
    print(ascii::ascii(head(sub, 20)), type = "plain")
  }
  invisible(wide)
}


# ------------------------------
# Window, z-scores, composite, plots
# ------------------------------

# Use the deduped dataset as base // now using input_data as ranking_data_8/9 has already been pre-processed
base_df <- input_data %>%
  select(country_code, indicator, year, value) %>%
  filter(!is.na(country_code), !is.na(indicator), !is.na(year), !is.na(value))

base_df <- base_df %>%
  mutate(indicator = factor(indicator, levels = indicators_4))

# --- FIX: keep `source` in base_df and sanity-check required columns ---
# base_df <- input_data %>%
#   filter(country_code %in% oecd_country_codes,
#          indicator %in% indicators_4) %>%
#   select(country_code, indicator, year, value, source) %>%
#   filter(!is.na(country_code), !is.na(indicator), !is.na(year), !is.na(value), !is.na(source))

# # normalise source labels just in case (whitespace/case)
# base_df <- base_df %>%
#   mutate(source = trimws(toupper(source))) %>%
#   mutate(source = dplyr::case_when(
#     source %in% c("OECD") ~ "OECD",
#     source %in% c("IHME") ~ "IHME",
#     TRUE ~ source
#   ))

# # assert required columns exist
# req_cols <- c("country_code","indicator","year","value","source")
# stopifnot(all(req_cols %in% names(base_df)))

# # quick peek
# message("Rows in base_df: ", nrow(base_df))
# print(table(base_df$source, useNA = "ifany"))


# "last n years" window per country × indicator
build_window <- function(df, n_years = 3, drop_zeros = FALSE,
                         zero_exceptions = c("NOR","DNK","IRL")) {

  df %>%
    arrange(country_code, indicator, desc(year)) %>%
    group_by(country_code, indicator) %>%
    # take last N non-missing rows first
    filter(!is.na(value)) %>%
    { if (drop_zeros) filter(., value != 0 | country_code %in% zero_exceptions) else . } %>%
    slice_head(n = n_years) %>%
    summarise(
      years_used   = paste(sort(unique(year)), collapse = ", "),
      n_years_used = dplyr::n(),
      avg_value    = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# indicator z-scores (flip sign when high_is_good == FALSE)
compute_indicator_z <- function(win_df, directions_tbl = direction_basic) {
  win_df %>%
    left_join(directions_tbl, by = "indicator") %>%
    mutate(value_oriented = if_else(high_is_good, avg_value, -avg_value)) %>%
    group_by(indicator) %>%
    mutate(
      ind_mean = mean(value_oriented, na.rm = TRUE),
      ind_sd   = sd(value_oriented,   na.rm = TRUE),
      z        = if_else(is.na(ind_sd) | ind_sd == 0, 0, (value_oriented - ind_mean) / ind_sd)
    ) %>%
    ungroup()
}

aggregate_composite <- function(ztab) {
  ztab %>%
    group_by(country_code) %>%
    summarise(
      indicators_used = sum(!is.na(z)),
      score           = mean(z, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number())
}

map_colour <- function(code) {
  if (code == uk_code) return(col_uk)
  if (code == ice_code)   return(col_iceland)
  if (code == luxem_code)   return(col_luxem)
  (col_others)
}

plot_bars <- function(df, value_col, title_text, subtitle_text = NULL, 
                      file_stub, caption_text = NULL, 
                      width = 10.8, height = 10.5) {

  title_wrapped    <- stringr::str_wrap(title_text,    width = 60)
  subtitle_wrapped <- if (!is.null(subtitle_text)) stringr::str_wrap(subtitle_text, width = 95) else NULL

  df <- df %>%
    dplyr::left_join(country_lookup, by = "country_code") %>%
    dplyr::mutate(
      label = dplyr::coalesce(country_name, country_code),   # <- use country_name
      fill  = vapply(country_code, map_colour, character(1))) %>%
    dplyr::arrange(.data[[value_col]]) %>%
    dplyr::mutate(label = factor(label, levels = label))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]], y = label, fill = fill)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      title = title_wrapped, 
      subtitle = subtitle_wrapped, 
      x = NULL, 
      y = NULL,
      caption = caption_text) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title   = ggplot2::element_text(face = "bold", size = 16, lineheight = 1.05,
                                           margin = ggplot2::margin(b = 8)),
      plot.subtitle= ggplot2::element_text(size = 11, colour = "grey30",
                                           margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 9, colour = "grey40"),
      axis.text.y  = ggplot2::element_text(size = 10),
      # more space so long titles/labels don’t clip
      plot.margin  = ggplot2::margin(t = 18, r = 70, b = 12, l = 12)
    ) +
    # tweak this 
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
    ggplot2::coord_cartesian(clip = "off")

  dir.create(file.path(out_plots, dirname(file_stub)), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(out_plots, paste0(file_stub, ".png")), p, width = 9, height = 10, dpi = 220)
  invisible(p)
}

plot_indicator_panels <- function(df, value_col, title_prefix, file_suffix, subtitle_text = NULL) {
  inds <- unique(df$indicator)
  for (ind in inds) {
    dd <- df %>% filter(indicator == ind)
    if (nrow(dd) == 0) next

    title_here <- title2(title_prefix, ind)
    stub_here  <- paste0(gsub("[^A-Za-z0-9]+", "_", ind), "_", file_suffix)
    plot_bars(
      dd %>% dplyr::select(country_code, !!rlang::sym(value_col)),
      value_col     = value_col,
      title_text    = title_here,
      subtitle_text = subtitle_text,
      file_stub     = stub_here
    )
  }
}

# test this out to try the title wrapping
title2 <- function(prefix, indicator) paste0(prefix, ":\n", indicator)

win_all    <- build_window(base_df, n_years = 3, drop_zeros = FALSE)
win_nozero <- build_window(base_df, n_years = 3, drop_zeros = TRUE,
                           zero_exceptions = c("NOR","DNK","IRL"))

# ---- Diagnostics: years used (long + wide), and “<3 years available” flags v important for Iceland checks ----
diag_and_save_window <- function(win_df, tag) {
  # Long table (per country × indicator)
  write_csv(win_df, file.path(out_tables, paste0("years_used_", tag, "_long.csv")))

  # Wide string of years per indicator
  years_wide <- win_df %>%
    select(country_code, indicator, years_used) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = years_used)

  write_csv(years_wide, file.path(out_tables, paste0("years_used_", tag, "_wide.csv")))

  # Console preview
  cat("\n▶ Years used (", tag, ") — first 20 rows (wide):\n", sep = "")
  pander::pander(head(years_wide, 20), style = "rmarkdown")

  # Countries with < 3 years in the window (informational, not an error)
  fewer_than_3 <- win_df %>%
    filter(n_years_used < 3) %>%
    arrange(indicator, desc(n_years_used))

  write_csv(fewer_than_3, file.path(out_tables, paste0("countries_with_lt3_years_", tag, ".csv")))
  if (nrow(fewer_than_3)) {
    cat("\n⚠ Countries with fewer than 3 years in window (", tag, "):\n", sep = "")
    pander::pander(head(fewer_than_3, 30), style = "rmarkdown")
  } else {
    cat("\nAll country×indicator pairs have 3 years in window (", tag, ").\n", sep = "")
  }
}

diag_and_save_window(win_all,    "last3_all")
diag_and_save_window(win_nozero, "last3_nozero")

z_all    <- compute_indicator_z(win_all,    direction_basic)
z_nozero <- compute_indicator_z(win_nozero, direction_basic)

cmp_all    <- aggregate_composite(z_all)
cmp_nozero <- aggregate_composite(z_nozero)

write_csv(z_all,    file.path(out_tables, "indicator_z_last3_all_v7_v1.csv"))
write_csv(z_nozero, file.path(out_tables, "indicator_z_last3_nozero_v7_v1.csv"))
write_csv(cmp_all,    file.path(out_tables, "composite_last3_all_v7_v1.csv"))
write_csv(cmp_nozero, file.path(out_tables, "composite_last3_nozero_v7_v1.csv"))


# --------------------------------------------------------------
# indicator-level plots both (raw & z)
# --------------------------------------------------------------
# Raw = window averages; Z = standardised scores

plot_indicator_panels(win_all, value_col = "avg_value",
                      title_prefix = "Windowed raw mean (last 3 years)",
                      subtitle_text = "Per country: mean of the most recent ≤3 reported years with non-missing values for this indicator (including any recorded 0s). Values are on the original scale; if fewer than 3 years exist, the mean uses available years; countries with no data in the window are omitted.",
                      file_suffix  = "last3_all_raw_v7_v1")
plot_indicator_panels(z_all, value_col = "z",
                      title_prefix = "Windowed z-score (last 3 years)",
                      subtitle_text = "Per country: mean of the most recent ≤3 reported years (including any recorded 0s), oriented so higher = better when lower values are desirable, then z-scored across the 38 OECD countries within this indicator; fewer than 3 years is allowed (uses available years); countries with no window data are omitted.", 
                      file_suffix  = "last3_all_z_v7_v1")
plot_indicator_panels(win_nozero, value_col = "avg_value",
                      title_prefix = "Windowed raw mean (last 3 years) — zeros dropped (NOR/DNK/IRL exempt)",
                      subtitle_text = "Per country: mean of the most recent ≤3 reported years after treating 0 values as missing due to suspected miscoding of absent data (exceptions: Norway, Denmark, and Ireland retain 0s as their 0 values fell within an acceptable range calculated using the surrounding years' values (2 years either side)). Values are on the original scale; if fewer than 3 usable years remain, the mean uses available years; countries with no usable data in the window are omitted.", 
                      file_suffix  = "last3_nozero_raw_v7_v1")
plot_indicator_panels(z_nozero,   value_col = "z",
                      title_prefix = "Windowed z-score (last 3 years) — zeros dropped (NOR/DNK/IRL exempt)",
                      subtitle_text = "Per country: mean of the most recent ≤3 reported years after treating 0s as missing because they may represent unreported data (Norway, Denmark, and Ireland retain 0s), then oriented so higher = better where lower is desirable, and finally z-scored across the 38 OECD countries within this indicator; fewer than 3 usable years is allowed; countries with none are omitted.", 
                      file_suffix  = "last3_nozero_z_v7_v1")

# --------------------------------------------------------------
# Composite ranking plots 
# --------------------------------------------------------------

plot_bars(cmp_all,    value_col = "score",
          title_text = "Composite ranking — last 3 years with raw data zeros kept in",
          subtitle_text = "Composite score per country computed as the mean of indicator-specific z-scores. For each indicator: average the most recent ≤3 reported years (including any 0s), z-score across the 38 OECD countries, then average across the 4 indicators; indicators with no data for a country are skipped; fewer than 3 years per indicator uses available years.",
          file_stub  = "composite_last3_all_v7_v1_coloured")

plot_bars(cmp_nozero, value_col = "score",
          title_text = "Composite ranking — last 3 years (countries with suspicious zeros dropped (NOR/DNK/IRL exempt))",
          subtitle_text = "Composite score per country computed as the mean of indicator-specific z-scores after addressing suspected 0 miscoding. For each indicator: average the most recent ≤3 reported years while treating 0s as missing (Norway, Denmark, and Ireland retain 0s), orient so higher = better where lower is desirable, z-score across the 38 OECD countries, then average across the 4 indicators; indicators lacking usable data for a country are excluded from its composite",
          file_stub  = "composite_last3_nozero_v7_v1_coloured")

# Comment and uncomment these lines depending on what countries your want coloured in on the plots
col_iceland_old <- col_iceland
col_luxem_old   <- col_luxem

col_iceland <- col_others
col_luxem   <- col_others

plot_bars(cmp_all,    value_col = "score",
          title_text = "Composite ranking — last 3 years with raw data zeros kept in",
          subtitle_text = "Composite score per country computed as the mean of indicator-specific z-scores. For each indicator: average the most recent ≤3 reported years (including any 0s), z-score across the 38 OECD countries, then average across the 4 indicators; indicators with no data for a country are skipped; fewer than 3 years per indicator uses available years.",
          file_stub  = "composite_last3_all_v7_v1_uk")

plot_bars(cmp_nozero, value_col = "score",
          title_text = "Composite ranking — last 3 years (countries with suspicious zeros dropped (NOR/DNK/IRL exempt))",
          subtitle_text = "Composite score per country computed as the mean of indicator-specific z-scores after addressing suspected 0 miscoding. For each indicator: average the most recent ≤3 reported years while treating 0s as missing (Norway, Denmark, and Ireland retain 0s), orient so higher = better where lower is desirable, z-score across the 38 OECD countries, then average across the 4 indicators; indicators lacking usable data for a country are excluded from its composite",
          file_stub  = "composite_last3_nozero_v7_v1_uk")

# Comment and uncomment these lines to restore original highlight colours
col_iceland <- col_iceland_old
col_luxem   <- col_luxem_old


# --------------------------------------------------------------
# Maternal-only ranking (raw & z) + with Lux/ISL removed
# --------------------------------------------------------------
ind_maternal <- "Maternal mortality (Deaths per 100 000 live births)"

mater_all_raw <- win_all    %>% filter(indicator == ind_maternal)
mater_all_z   <- z_all      %>% filter(indicator == ind_maternal)

mater_nozero_raw <- win_nozero %>% filter(indicator == ind_maternal)
mater_nozero_z   <- z_nozero   %>% filter(indicator == ind_maternal)

# Base plots
plot_bars(mater_all_raw %>% select(country_code, avg_value),
          value_col = "avg_value",
          title_text = "Maternal mortality — raw mean (last 3 years)",
          subtitle_text = "Per country: mean maternal mortality over the most recent ≤3 reported years (deaths per 100,000 live births; 0s included if present). Values are on the original scale; if fewer than 3 years exist, the mean uses available years; countries without data in the window are omitted.",
          file_stub  = "maternal_last3_all_raw_v7_v1")

plot_bars(mater_all_z   %>% select(country_code, z),
          value_col = "z",
          title_text = "Maternal mortality — z-score (last 3 years)",
          subtitle_text = "Per country: mean maternal mortality over the most recent ≤3 reported years (0s included if present), then oriented so higher = better (lower mortality is desirable) and z-scored across the 38 OECD countries; fewer than 3 years is allowed; countries without window data are omitted.", 
          file_stub  = "maternal_last3_all_z_v7_v1")

# With Luxembourg & Iceland removed
rm_codes <- c("LUX","ISL")

plot_bars(mater_all_raw %>% filter(!country_code %in% rm_codes) %>% select(country_code, avg_value),
          value_col = "avg_value",
          title_text = "Maternal mortality — raw mean (last 3 years) — LUX/ISL removed",
          subtitle_text = "Per country (excluding Luxembourg and Iceland): mean maternal mortality over the most recent ≤3 reported years (deaths per 100,000 live births; 0s included if present). Values are on the original scale; if fewer than 3 years exist, the mean uses available years; countries without window data are omitted.", 
          file_stub  = "maternal_last3_all_raw_no_LUX_ISL_v7_v1")

plot_bars(mater_all_z %>% filter(!country_code %in% rm_codes) %>% select(country_code, z),
          value_col = "z",
          title_text = "Maternal mortality — z-score (last 3 years) — LUX/ISL removed",
          subtitle_text = "Per country (excluding Luxembourg and Iceland): mean maternal mortality over the most recent ≤3 reported years (0s included if present), then z-scored across the remaining OECD countries; fewer than 3 years is allowed; countries without window data are omitted.", 
          file_stub  = "maternal_last3_all_z_no_LUX_ISL_v7_v1")

# Composite without LUX/ISL 
plot_bars(cmp_all %>% filter(!country_code %in% rm_codes),
          value_col = "score",
          title_text = "Composite ranking — last 3 years (raw zeros kept) — LUX/ISL removed",
          subtitle_text = "Per country (excluding Luxembourg and Iceland): composite score computed as the mean of indicator-specific z-scores. For each indicator: average the most recent ≤3 reported years (including any 0s), orient so higher = better where lower is desirable, z-score across the remaining OECD countries, then average across the 4 indicators; indicators with no data for a country are skipped.", 
          file_stub  = "composite_last3_all_no_LUX_ISL_v7_v1")


cmp_dir <- file.path(out_plots, "composite_multiplot")
dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)

plot_bars_nosave <- function(df, value_col, title_text, subtitle_text = NULL) {
  title_wrapped    <- stringr::str_wrap(title_text,    width = 60)
  subtitle_wrapped <- if (!is.null(subtitle_text)) stringr::str_wrap(subtitle_text, width = 95) else NULL

  pdat <- df %>%
    dplyr::left_join(country_lookup, by = "country_code") %>%
    dplyr::mutate(
      label = dplyr::coalesce(country_name, country_code),
      fill  = vapply(country_code, map_colour, character(1))
    ) %>%
    dplyr::arrange(.data[[value_col]]) %>%
    dplyr::mutate(label = factor(label, levels = label))

  ggplot(pdat, aes(x = .data[[value_col]], y = label, fill = fill)) +
    geom_col(width = 0.8) +
    scale_fill_identity() +
    labs(
      title    = title_wrapped,
      subtitle = subtitle_wrapped,
      x = NULL, y = NULL
    ) +
    theme(
      plot.title.position = "plot",
      plot.title   = element_text(face = "bold", size = 16, lineheight = 1.05,
                                  margin = margin(b = 8)),
      plot.subtitle= element_text(size = 11, colour = "grey30",
                                  margin = margin(b = 8)),
      axis.text.y  = element_text(size = 10),
      plot.margin  = margin(t = 18, r = 70, b = 12, l = 12),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(clip = "off")
}

xlims_cmp <- range(c(cmp_all$score, cmp_nozero$score), na.rm = TRUE)

p_cmp_all <- plot_bars_nosave(
  cmp_all, value_col = "score",
  title_text = "Composite ranking — last 3 years (zeros kept in)",
  subtitle_text = "Composite = mean of indicator z-scores. For each indicator: average the most recent ≤3 reported years (including 0s), z-score across the 38 OECD countries, then average across the 4 indicators."
) + scale_x_continuous(limits = xlims_cmp, expand = expansion(mult = c(0.02, 0.18)))

p_cmp_noz <- plot_bars_nosave(
  cmp_nozero, value_col = "score",
  title_text = "Composite ranking — last 3 years (suspicious zeros dropped; NOR/DNK/IRL kept)",
  subtitle_text = "Composite = mean of indicator z-scores. For each indicator: average the most recent ≤3 reported years while treating 0s as missing (Norway, Denmark, and Ireland retain 0s), then z-score across the 38 OECD countries and average across the 4 indicators."
) + scale_x_continuous(limits = xlims_cmp, expand = expansion(mult = c(0.02, 0.18)))

stacked <- p_cmp_all / p_cmp_noz +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Composite Rankings — Comparison",
    subtitle = "Top: zeros kept in • Bottom: suspicious zeros dropped (NOR/DNK/IRL kept)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, colour = "grey30")
    )
  )

ggsave(file.path(cmp_dir, "composite_compare_stacked_v2.png"),
       stacked, width = 11, height = 18, dpi = 220)

side_by_side <- p_cmp_all | p_cmp_noz +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Composite Rankings — Comparison",
    subtitle = "Left: zeros kept in • Right: suspicious zeros dropped (NOR/DNK/IRL kept)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, colour = "grey30")
    )
  )

ggsave(file.path(cmp_dir, "composite_compare_side_by_side_v2.png"),
       side_by_side, width = 18, height = 10, dpi = 220)


# --------------------------------------------------------------
# Composite comparison using mirror plots with ONE central label column and same x-scale
# --------------------------------------------------------------

cmp_dir <- file.path(out_plots, "composite_multiplot")
dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)

mk_labeled <- function(df) {
  df %>%
    left_join(country_lookup, by = "country_code") %>%
    mutate(label = dplyr::coalesce(country_name, country_code),
           fill  = vapply(country_code, map_colour, character(1)))
}
cmp_all_l <- mk_labeled(cmp_all)
cmp_noz_l <- mk_labeled(cmp_nozero)

alpha_levels <- union(cmp_all_l$label, cmp_noz_l$label) %>% unique() %>% sort()
y_order      <- rev(alpha_levels)  # ggplot draws bottom→top; reversing puts Australia at TOP

cmp_all_l <- tibble(label = alpha_levels) %>%
  left_join(cmp_all_l %>% select(label, country_code, score, fill), by = "label")
cmp_noz_l <- tibble(label = alpha_levels) %>%
  left_join(cmp_noz_l %>% select(label, country_code, score, fill), by = "label")

cmp_all_l$label <- factor(cmp_all_l$label, levels = alpha_levels)
cmp_noz_l$label <- factor(cmp_noz_l$label, levels = alpha_levels)

rng     <- range(c(cmp_all_l$score, cmp_noz_l$score), na.rm = TRUE)
max_abs <- max(abs(rng))
xlims   <- c(-max_abs, max_abs)

span      <- diff(xlims)
label_pad <- span * 0.015
xlims_lab <- c(xlims[1] - label_pad * 1.2, xlims[2] + label_pad * 1.2)

cmp_all_l <- cmp_all_l %>%
  mutate(rank_all = dplyr::dense_rank(dplyr::desc(score)))
cmp_noz_l <- cmp_noz_l %>%
  mutate(rank_noz = dplyr::dense_rank(dplyr::desc(score)))

panel_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title   = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    axis.title   = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),                 # middle panel shows labels
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border     = element_blank(),             # no per-panel box
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
    panel.grid.minor.x = element_line(colour = "grey94", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin  = margin(t = 6, r = 4, b = 6, l = 4)
  )

ttl_all <- "Zeros kept"
ttl_noz <- "Zeros removed (NOR/DNK/IRL kept)"

p_left <- ggplot(cmp_all_l, aes(x = score, y = label, fill = fill)) +
  geom_col(width = 0.85, na.rm = TRUE) +
  scale_fill_identity() +
  scale_y_discrete(limits = y_order) +  # FORCE Australia at top
  scale_x_continuous(limits = xlims_lab,
                     breaks = scales::pretty_breaks(5),
                     expand = expansion(mult = c(0.005, 0.02))) +
  geom_text(aes(x = score + if_else(score >= 0, label_pad, -label_pad),
                label = rank_all),
            size = 3.2, colour = "grey20", fontface = "bold", na.rm = TRUE) +
  labs(title = ttl_all) +
  panel_theme +
  geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey65") +
  coord_cartesian(clip = "off")

p_mid <- ggplot(tibble(label = factor(y_order, levels = y_order)), aes(x = 0, y = label)) +
  geom_text(aes(label = label), size = 3.6, lineheight = 0.96) +
  scale_y_discrete(limits = y_order) +
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  theme_void(base_size = 12) +
  theme(plot.margin = margin(t = 10, r = 1, b = 6, l = 1))

p_right <- ggplot(cmp_noz_l, aes(x = score, y = label, fill = fill)) +
  geom_col(width = 0.85, na.rm = TRUE) +
  scale_fill_identity() +
  scale_y_discrete(limits = y_order) +
  scale_x_continuous(limits = xlims_lab,
                     breaks = scales::pretty_breaks(5),
                     expand = expansion(mult = c(0.005, 0.02))) +
  geom_text(aes(x = score + if_else(score >= 0, label_pad, -label_pad),
                label = rank_noz),
            size = 3.2, colour = "grey20", fontface = "bold", na.rm = TRUE) +
  labs(title = ttl_noz) +
  panel_theme +
  geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey65") +
  coord_cartesian(clip = "off")

mirror_v2 <- p_left | p_mid | p_right
mirror_v2 <- mirror_v2 +
  plot_layout(widths = c(1.6, 0.35, 1.6)) +
  plot_annotation(
    title    = "OECD composite rankings (z-scores), last 3-year window",
    subtitle = "Left: zeros kept · Center: countries (A–Z) · Right: zeros removed (NOR/DNK/IRL kept)",
    theme = theme(
      plot.title   = element_text(face = "bold", size = 16, margin = margin(b = 3)),
      plot.subtitle= element_text(size = 10.5, colour = "grey35", margin = margin(b = 6)),
      plot.background = element_rect(fill = "white", colour = "grey60", linewidth = 0.7)  # single outline
    )
  )

ggsave(file.path(cmp_dir, "composite_compare_mirror_v11.png"),
       mirror_v2, width = 15, height = 15, dpi = 220)

# Sanity check
dropped_by_indicator <- win_all %>%
  select(indicator, country_code) %>%
  anti_join(win_nozero %>% select(indicator, country_code),
            by = c("indicator","country_code")) %>%
  arrange(indicator, country_code)

cat("\nCountries dropped in zeros-dropped variant (by indicator):\n")
print(dropped_by_indicator, n = 200)

# Set up the pretty console tables (w. pander + ascii)

pretty_console_table <- function(df, caption = NULL, digits = 3) {
  pander::panderOptions("table.split.table", Inf)
  pander::panderOptions("table.style", "rmarkdown")
  pander::panderOptions("round", digits)
  pander::panderOptions("keep.trailing.zeros", TRUE)
  pander::panderOptions("missing", "—")

  if (!is.null(caption)) cat("\n")
  pander::pander(df, caption = caption)
}

country_label_safe <- function(code) {
  nm <- tryCatch(
    country_lookup$country_name[match(code, country_lookup$country_code)],
    error = function(e) NA_character_
  )
  ifelse(!is.na(nm) & nzchar(nm), nm, code)
}

ordinal_suffix <- function(n) {
  n <- as.integer(n)
  out <- rep(NA_character_, length(n))
  ok  <- !is.na(n)
  if (!any(ok)) return(out)
  nn <- n[ok]
  special <- nn %% 100L %in% 11:13
  last_digit <- nn %% 10L
  base_suf <- c("th","st","nd","rd","th","th","th","th","th","th")[last_digit + 1L]
  suf <- ifelse(special, "th", base_suf)
  out[ok] <- paste0(nn, suf)
  out
}

# composite: show ranks for one or more countries
ranks_in_composite_multi <- function(comp_table, country_codes, caption = NULL, digits = 3) {
  tab <- comp_table %>%
    dplyr::mutate(
      Country      = vapply(country_code, country_label_safe, character(1)),
      Rank         = rank,
      `Rank (ord)` = ordinal_suffix(rank),
      `z-score`    = score
    ) %>%
    dplyr::select(Country, country_code, Rank, `Rank (ord)`, `z-score`) %>%
    dplyr::arrange(Rank)

  wanted <- tibble::tibble(country_code = country_codes)
  out <- wanted %>% dplyr::left_join(tab, by = "country_code")

  out$Country <- ifelse(out$country_code == uk_code,
                        paste0("**", out$Country, "**"),
                        out$Country)

  if (is.null(caption)) {
    caption <- "Composite ranking — selected countries"
  }
  pretty_console_table(out, caption = caption, digits = digits)
  invisible(out)
}

# indicator-specific: show ranks for multiple countries within one indicator
ranks_in_indicator_multi <- function(z_table, indicator_name, country_codes,
                                     caption = NULL, digits = 3) {
  # z_table must have: indicator, country_code, z (from compute_indicator_z())
  z_sub <- z_table %>%
    dplyr::filter(indicator == indicator_name) %>%
    dplyr::arrange(dplyr::desc(z)) %>%
    dplyr::mutate(
      Rank         = dplyr::row_number(),
      Country      = vapply(country_code, country_label_safe, character(1)),
      `Rank (ord)` = ordinal_suffix(Rank),
      `z-score`    = z
    ) %>%
    dplyr::select(Country, country_code, Rank, `Rank (ord)`, `z-score`)

  wanted <- tibble::tibble(country_code = country_codes)
  out <- wanted %>% dplyr::left_join(z_sub, by = "country_code")

  out$Country <- ifelse(out$country_code == uk_code,
                        paste0("**", out$Country, "**"),
                        out$Country)

  if (is.null(caption)) {
    caption <- paste0("Indicator ranking — ", indicator_name, " — selected countries")
  }
  pretty_console_table(out, caption = caption, digits = digits)
  invisible(out)
}

# 6) Convenience: show the *top N* table for composite (nice for slides)
top_n_composite_table <- function(comp_table, n = 10, caption = NULL, digits = 3) {
  tab <- comp_table %>%
    dplyr::arrange(desc(score)) %>%
    dplyr::mutate(
      Rank         = dplyr::row_number(),
      Country      = vapply(country_code, country_label_safe, character(1)),
      `Rank (ord)` = ordinal_suffix(Rank),
      `z-score`    = score
    ) %>%
    dplyr::select(Rank, `Rank (ord)`, Country, country_code, `z-score`) %>%
    dplyr::arrange(Rank) %>%
    dplyr::slice(1:n)

  tab$Country <- ifelse(tab$country_code == uk_code,
                        paste0("**", tab$Country, "**"),
                        tab$Country)

  if (is.null(caption)) caption <- paste0("Composite ranking — Top ", n)
  pretty_console_table(tab, caption = caption, digits = digits)
  invisible(tab)
}


# Composite: specific countries
ranks_in_composite_multi(cmp_all, c("GBR","ISL","LUX","IRL"),
                         caption = "Composite ranking (zeros kept) — Selected countries")

# Composite (zeros dropped variant)
ranks_in_composite_multi(cmp_nozero, c("GBR","ISL","LUX","IRL"),
                         caption = "Composite ranking (zeros dropped; NOR/DNK/IRL exempt) — Selected countries")

# Indicator: maternal mortality for a few countries
ind_maternal <- "Maternal mortality (Deaths per 100 000 live births)"
ranks_in_indicator_multi(z_all, ind_maternal, c("GBR","ISL","LUX","IRL"),
                         caption = "Maternal mortality — z-score ranks (zeros kept) — Selected countries")

# Top 10 composite (nice slide)
top_n_composite_table(cmp_all, n = 10)

# --------------------------------------------------------------
# Source-aware windows - trying to replicate Roberto's 2023 ranking  =====
# --------------------------------------------------------------

library(patchwork) 

subdir <- "sb 2023 report data window replic"
plots_dir <- file.path(out_plots, subdir)
tabs_dir  <- file.path(out_tables, subdir)
diag_dir  <- file.path(out_diag,  subdir)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tabs_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(diag_dir,  recursive = TRUE, showWarnings = FALSE)

# new palettes
col_others  <- "#003049"
col_uk      <- "#d62828"
col_iceland <- "#f77f00"
col_luxem   <- "#fcbf49"


filter_by_source_rule <- function(df,
                                  mode = c("single","last3"),
                                  zeros_out = FALSE,
                                  ihme_single_year = 2019,
                                  oecd_single_year_zeros_in = 2022,
                                  oecd_single_year_zeros_out = 2022,
                                  ihme_last3_max = 2019,
                                  oecd_last3_max = 2022,
                                  zero_exceptions = c("NOR","DNK","IRL")) {
  mode <- match.arg(mode)

  df0 <- if (zeros_out) {
    df %>% filter(value != 0 | country_code %in% zero_exceptions)
  } else df

  ihme <- df0 %>% filter(source == "IHME")
  oecd <- df0 %>% filter(source == "OECD")

  if (mode == "single") {
    oecd_target <- if (zeros_out) oecd_single_year_zeros_out else oecd_single_year_zeros_in
    ihme <- ihme %>% filter(year == ihme_single_year)
    oecd <- oecd %>% filter(year == oecd_target)

    # avg duplicates within (indicator,country,year) (just in case)
    out <- bind_rows(ihme, oecd) %>%
      group_by(indicator, country_code, year) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    # create window summary columns
    out <- out %>%
      group_by(country_code, indicator) %>%
      summarise(
        years_used   = paste(sort(unique(year)), collapse = ", "),
        n_years_used = n(),
        avg_value    = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  } else { # last3
    ihme <- ihme %>% filter(year <= ihme_last3_max)
    oecd <- oecd %>% filter(year <= oecd_last3_max)

    out <- bind_rows(ihme, oecd) %>%
      arrange(country_code, indicator, desc(year)) %>%
      group_by(country_code, indicator) %>%
      slice_head(n = 3) %>%
      summarise(
        years_used   = paste(sort(unique(year)), collapse = ", "),
        n_years_used = n(),
        avg_value    = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  out
}

# go from window -> z -> composite (+ save diagnostics)
diag_save <- function(win_df, tag) {
  write_csv(win_df, file.path(tabs_dir, paste0("years_used_", tag, "_long.csv")))
  fewer3 <- win_df %>% filter(n_years_used < 3)
  write_csv(fewer3, file.path(tabs_dir, paste0("countries_lt3_", tag, ".csv")))
}

to_composite <- function(win_df, tag) {
  diag_save(win_df, tag)
  ztab <- compute_indicator_z(win_df, direction_basic)
  write_csv(ztab, file.path(tabs_dir, paste0("indicator_z_", tag, ".csv")))
  comp <- aggregate_composite(ztab)
  write_csv(comp, file.path(tabs_dir, paste0("composite_", tag, ".csv")))
  list(z = ztab, comp = comp)
}

save_indicator_panels_one_png <- function(df, value_col, title_prefix, file_stub, subtitle_text = NULL) {
  make_one <- function(ind) {
    dd <- df %>% filter(indicator == ind)
    if (nrow(dd) == 0) return(NULL)
    title_here <- title2(title_prefix, ind)
    # reuse plot_bars but return the ggplot object
    dd2 <- dd %>% select(country_code, !!rlang::sym(value_col))
    title_wrapped <- stringr::str_wrap(title_here, 60)
    subtitle_wrapped <- if (!is.null(subtitle_text)) stringr::str_wrap(subtitle_text, 95) else NULL

    dd2 <- dd2 %>%
      left_join(country_lookup, by = "country_code") %>%
      mutate(label = coalesce(country_name, country_code),
             fill  = vapply(country_code, map_colour, character(1))) %>%
      arrange(.data[[value_col]]) %>%
      mutate(label = factor(label, levels = label))

    ggplot(dd2, aes(x = .data[[value_col]], y = label, fill = fill)) +
      geom_col(width = 0.8) +
      scale_fill_identity() +
      labs(title = title_wrapped, subtitle = subtitle_wrapped, x = NULL, y = NULL) +
      theme_bw(base_size = 10) +
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 9, colour = "grey30")) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      coord_cartesian(clip = "off")
  }

  plots <- lapply(indicators_4, make_one)
  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (length(plots) == 0) return(invisible(NULL))

  lay <- wrap_plots(plots, ncol = 2)
  ggsave(file.path(plots_dir, paste0(file_stub, ".png")), lay, width = 12, height = 10, dpi = 220)
  invisible(lay)
}

  
# the 4 base datasets for P1–P4

# p1: zeros in, single-year (IHME 2019; OECD 2021)
win_p1 <- filter_by_source_rule(base_df, mode = "single", zeros_out = FALSE)
res_p1 <- to_composite(win_p1, "P1_single_zerosIN_ihme2019_oecd2021")

# P2: zeros out, single-year (IHME 2019; OECD 2022)
win_p2 <- filter_by_source_rule(base_df, mode = "single", zeros_out = TRUE)
res_p2 <- to_composite(win_p2, "P2_single_zerosOUT_ihme2019_oecd2022")

# p3: zeros in, last-3 (IHME ≤2019; OECD ≤2022)
win_p3 <- filter_by_source_rule(base_df, mode = "last3", zeros_out = FALSE)
res_p3 <- to_composite(win_p3, "P3_last3_zerosIN_ihmeLE2019_oecdLE2022")

# p4: zeros out, last-3 (IHME ≤2019; OECD ≤2022)
win_p4 <- filter_by_source_rule(base_df, mode = "last3", zeros_out = TRUE)
res_p4 <- to_composite(win_p4, "P4_last3_zerosOUT_ihmeLE2019_oecdLE2022")

# handling ISL/LUX under zeros-in, last-3 

# rmv ISL/LUX from plotting only (analysis incl them)
res_p5 <- res_p3  # same analysis as P3
cmp_p5_plotonly <- res_p5$comp %>% filter(!country_code %in% c("ISL","LUX"))

# rmv ISL/LUX from analysis and plotting
z_p6 <- res_p3$z %>% filter(!country_code %in% c("ISL","LUX"))
cmp_p6 <- aggregate_composite(z_p6)
write_csv(z_p6,   file.path(tabs_dir, "indicator_z_P6_last3_zerosIN_exclISL_LUX.csv"))
write_csv(cmp_p6, file.path(tabs_dir, "composite_P6_last3_zerosIN_exclISL_LUX.csv"))

# composite plots

plot_bars(res_p1$comp, value_col = "score",
  title_text = "P1 • Composite — zeros kept • IHME 2019 • OECD 2022",
  subtitle_text = "Single-year by source. Z-scores within indicator across countries available under this rule; then averaged across the 4 indicators.",
  file_stub  = file.path(subdir, "P1_composite_single_zerosIN_ihme2019_oecd2022"))

plot_bars(res_p2$comp, value_col = "score",
  title_text = "P2 • Composite — zeros removed (NOR/DNK/IRL exempt) • IHME 2019 • OECD 2022",
  subtitle_text = "Single-year by source with zeros treated as missing (whitelist kept). Z-scored within indicator; averaged across indicators.",
  file_stub  = file.path(subdir, "P2_composite_single_zerosOUT_ihme2019_oecd2022"))

plot_bars(res_p3$comp, value_col = "score",
  title_text = "P3 • Composite — zeros kept • last 3 yrs (IHME ≤2019; OECD ≤2022)",
  subtitle_text = "Window uses the most recent ≤3 reported years up to the source-specific cut-off.",
  file_stub  = file.path(subdir, "P3_composite_last3_zerosIN"))

plot_bars(res_p4$comp, value_col = "score",
  title_text = "P4 • Composite — zeros removed (NOR/DNK/IRL exempt) • last 3 yrs (IHME ≤2019; OECD ≤2022)",
  subtitle_text = "Zeros treated as missing before selecting the last-3-year window.",
  file_stub  = file.path(subdir, "P4_composite_last3_zerosOUT"))

plot_bars(cmp_p5_plotonly, value_col = "score",
  title_text = "P5 • Composite — zeros kept • last 3 yrs • ISL/LUX hidden from plot",
  subtitle_text = "Analysis includes Iceland and Luxembourg; they are omitted from the chart only.",
  file_stub  = file.path(subdir, "P5_composite_last3_zerosIN_hideISL_LUX"))

plot_bars(cmp_p6, value_col = "score",
  title_text = "P6 • Composite — zeros kept • last 3 yrs • ISL/LUX excluded from analysis",
  subtitle_text = "Iceland and Luxembourg are excluded prior to z-scoring, changing the reference distribution.",
  file_stub  = file.path(subdir, "P6_composite_last3_zerosIN_exclISL_LUX"))

save_indicator_panels_one_png(res_p3$z, value_col = "z",
  title_prefix = "Indicator z-scores — P3 (zeros kept, last-3)",
  file_stub = "P3_indicator_panels_z_onePNG",
  subtitle_text = "Z-scored within indicator across countries available under P3.")

save_indicator_panels_one_png(res_p4$z, value_col = "z",
  title_prefix = "Indicator z-scores — P4 (zeros removed, last-3)",
  file_stub = "P4_indicator_panels_z_onePNG",
  subtitle_text = "Zeros treated as missing (NOR/DNK/IRL exempt) before windowing; z-scored within indicator.")


# -----------------------------
# Coverage heatmaps for each indicator
# -----------------------------

plot_indicator_heatmap <- function(ind_name,
                                   zeros_out = FALSE,
                                   zero_exceptions = c("NOR","DNK","IRL"),
                                   custom_name = NULL) {
  stopifnot(ind_name %in% indicators_4)

  df <- input_data %>%
    filter(indicator == ind_name,
           country_code %in% oecd_country_codes) %>%
    select(country_code, country, year, value, source)

  if (zeros_out) {
    df <- df %>% filter(value != 0 | country_code %in% zero_exceptions)
  }

  # ORDER THE COUNTRIES!!! TODO: fix
  cov_tab <- df %>%
    group_by(country_code) %>%
    summarise(n_years = sum(!is.na(value)), .groups = "drop") %>%
    arrange(desc(n_years))

  # Use country name if avail
  df <- df %>%
    left_join(country_lookup, by = "country_code") %>%
    mutate(
      Country = ifelse(!is.na(country_name) & nzchar(country_name),
                       country_name, country_code),
      Country = factor(Country, levels = Country[match(cov_tab$country_code, country_code)])
    )

  # set title here
  title_here <- paste0(ind_name, if (zeros_out) " (zeros removed)" else " (zeros kept)")

  p <- ggplot(df, aes(x = year, y = Country, fill = value)) +
    geom_raster(interpolate = FALSE) + # geom_tile(width = 0.95, height = 0.95, na.rm = TRUE) +
    scale_fill_gradient(name = "Value", low = "#08306B", high = "#9ECAE1",
                        na.value = "grey90",
                        limits = c(min(df$value, na.rm=TRUE), max(df$value, na.rm=TRUE))) +
    scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    labs(title = title_here, x = "Year", y = "Country") +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 16)
    )

  heatmap_dir <- file.path(out_plots, "indicator_coverage_plots")
  dir.create(heatmap_dir, recursive = TRUE, showWarnings = FALSE)

  default_name <- paste0(
    gsub("[^A-Za-z0-9]+", "_", ind_name),
    ifelse(zeros_out, "_zerosOUT", "_zerosIN")
  )
  file_stub <- if (!is.null(custom_name)) custom_name else default_name

  ggsave(file.path(heatmap_dir, paste0(file_stub, "_v10.png")),
         p, width = 12, height = 8, dpi = 220)

  invisible(p)
}

plot_indicator_heatmap(
  ind_name = "Neonatal disorders (Deaths per 100 000 live births (standardised rate))",
  zeros_out = FALSE  # zeros kept
)

plot_indicator_heatmap(
  ind_name = "Neonatal disorders (Deaths per 100 000 live births (standardised rate))",
  zeros_out = TRUE,  # zeros removed
  custom_name = "neonatal_disorders_heatmap_zeros_removed"
)


# -----------------------------
# Coverage heatmaps: indicators × (years and/or countries) 
# -----------------------------

# Subfolder for these plots
coverage_dir <- file.path(out_plots, "indicator_coverage_plots_v2")
dir.create(coverage_dir, recursive = TRUE, showWarnings = FALSE)

zero_exceptions <- c("NOR","DNK","IRL")

# Helper: return a filtered, tidy base for coverage counting
coverage_base <- function(zeros_out = FALSE) {
  df <- input_data %>%
    filter(country_code %in% oecd_country_codes,
           indicator %in% indicators_4) %>%
    select(indicator, country_code, year, value) %>%
    # standardise indicator factor order
    mutate(indicator = factor(indicator, levels = indicators_4))

  if (zeros_out) {
    df <- df %>% filter(value != 0 | country_code %in% zero_exceptions)
  }
  # Drop true missing values for counting coverage
  df <- df %>% filter(!is.na(value), !is.na(year))
  df
}

# Generic heatmap plotter
plot_heatmap_counts <- function(dat, xvar, yvar, fillvar, title, filename,
                                x_breaks = scales::pretty_breaks(10)) {
  p <- ggplot(dat, aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[fillvar]])) +
    geom_raster(interpolate = FALSE) + # geom_tile(width = 0.95, height = 0.95) +
    scale_fill_gradient(name = "Count", low = "#9ECAE1", high = "#08306B") +
    labs(title = title, x = NULL, y = NULL) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # If x is numeric (years), add nice breaks
  if (is.numeric(dat[[xvar]])) {
    p <- p + scale_x_continuous(breaks = x_breaks)
  }

  ggsave(file.path(coverage_dir, filename), p, width = 12, height = 8, dpi = 220)
  invisible(p)
}

# -----------------------------
# Plot 1: indicators × years — count of countries (zeros kept)
# -----------------------------
df1 <- coverage_base(zeros_out = FALSE)

years_all <- sort(unique(df1$year))
df1_grid <- tidyr::expand_grid(indicator = levels(df1$indicator), year = years_all) %>%
  left_join(
    df1 %>% distinct(indicator, year, country_code) %>%
      count(indicator, year, name = "n_countries"),
    by = c("indicator","year")
  ) %>%
  mutate(n_countries = dplyr::coalesce(n_countries, 0L))

plot_heatmap_counts(
  dat = df1_grid, xvar = "year", yvar = "indicator", fillvar = "n_countries",
  title = "Coverage: Countries reporting per Indicator × Year (zeros kept)",
  filename = "P1_indicators_by_year_countriesCount_zerosIN.png"
)

# -----------------------------
# Plot 2: indicators × years — count of countries (zeros taken out; NOR/DNK/IRL exempt)
# -----------------------------
df2 <- coverage_base(zeros_out = TRUE)

years_all2 <- sort(unique(df2$year))
df2_grid <- tidyr::expand_grid(indicator = levels(df2$indicator), year = years_all2) %>%
  left_join(
    df2 %>% distinct(indicator, year, country_code) %>%
      count(indicator, year, name = "n_countries"),
    by = c("indicator","year")
  ) %>%
  mutate(n_countries = dplyr::coalesce(n_countries, 0L))

plot_heatmap_counts(
  dat = df2_grid, xvar = "year", yvar = "indicator", fillvar = "n_countries",
  title = "Coverage: Countries reporting per Indicator × Year (zeros removed; NOR/DNK/IRL kept)",
  filename = "P2_indicators_by_year_countriesCount_zerosOUT.png"
)

# -----------------------------
# Plot 3: indicators × countries — count of years (zeros kept)
# -----------------------------
df3 <- coverage_base(zeros_out = FALSE) %>%
  left_join(country_lookup, by = "country_code") %>%
  mutate(country_label = dplyr::coalesce(country_name, country_code))

# count years per indicator × country
df3_counts <- df3 %>%
  distinct(indicator, country_label, year) %>%
  count(indicator, country_label, name = "n_years")

# order countries by total coverage (more years → earlier)
country_order3 <- df3_counts %>% group_by(country_label) %>% summarise(n = sum(n_years), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(country_label)

df3_counts <- df3_counts %>%
  mutate(country_label = factor(country_label, levels = country_order3))

plot_heatmap_counts(
  dat = df3_counts, xvar = "country_label", yvar = "indicator", fillvar = "n_years",
  title = "Coverage: Years reported per Indicator × Country (zeros kept)",
  filename = "P3_indicators_by_country_yearsCount_zerosIN.png"
)

# -----------------------------
# Plot 4: indicators × countries — count of years (zeros taken out; NOR/DNK/IRL exempt)
# -----------------------------
df4 <- coverage_base(zeros_out = TRUE) %>%
  left_join(country_lookup, by = "country_code") %>%
  mutate(country_label = dplyr::coalesce(country_name, country_code))

df4_counts <- df4 %>%
  distinct(indicator, country_label, year) %>%
  count(indicator, country_label, name = "n_years")

country_order4 <- df4_counts %>% group_by(country_label) %>% summarise(n = sum(n_years), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(country_label)

df4_counts <- df4_counts %>%
  mutate(country_label = factor(country_label, levels = country_order4))

plot_heatmap_counts(
  dat = df4_counts, xvar = "country_label", yvar = "indicator", fillvar = "n_years",
  title = "Coverage: Years reported per Indicator × Country (zeros removed; NOR/DNK/IRL kept)",
  filename = "P4_indicators_by_country_yearsCount_zerosOUT.png"
)

print(out_plots)

# -----------------------------
# Quad heatmap
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(stringr); library(patchwork)
})

quad_dir <- file.path(out_plots, "indicator_coverage_plots_v3")
dir.create(quad_dir, recursive = TRUE, showWarnings = FALSE)

shorten_indicator <- function(x) {
  x1 <- sub("\\s*\\(.*$", "", x)
  x2 <- sub("(?i)\\s+per\\s+.*$", "", x1, perl = TRUE)
  x3 <- sub("(?i)\\s+percentage.*$", "", x2, perl = TRUE)
  stringr::str_squish(x3)
}
wrap_indicator <- function(x, width = 30) stringr::str_wrap(shorten_indicator(x), width = width)
wrap_title     <- function(x, width = 60) stringr::str_wrap(x, width = width)

zero_exceptions <- c("NOR","DNK","IRL")
MIN_YEAR <- 2000

coverage_base <- function(zeros_out = FALSE) {
  df <- input_data %>%
    filter(country_code %in% oecd_country_codes,
           indicator %in% indicators_4,
           !is.na(value), !is.na(year),
           year >= MIN_YEAR) %>%
    select(indicator, country_code, year, value) %>%
    mutate(indicator = factor(indicator, levels = indicators_4))
  if (zeros_out) df <- df %>% filter(value != 0 | country_code %in% zero_exceptions)
  df
}

df_in  <- coverage_base(FALSE)
df_out <- coverage_base(TRUE)

# indicators × years — count countries 
years_all <- sort(unique(c(df_in$year, df_out$year)))

p1_dat <- tidyr::expand_grid(indicator = levels(df_in$indicator), year = years_all) %>%
  left_join(df_in  %>% distinct(indicator, year, country_code) %>%
              count(indicator, year, name = "count"), by = c("indicator","year")) %>%
  mutate(count = coalesce(count, 0L),
         indicator_lab = wrap_indicator(as.character(indicator)))

p2_dat <- tidyr::expand_grid(indicator = levels(df_out$indicator), year = years_all) %>%
  left_join(df_out %>% distinct(indicator, year, country_code) %>%
              count(indicator, year, name = "count"), by = c("indicator","year")) %>%
  mutate(count = coalesce(count, 0L),
         indicator_lab = wrap_indicator(as.character(indicator)))

n_countries_obs <- max(
  dplyr::n_distinct(df_in$country_code),
  dplyr::n_distinct(df_out$country_code)
)

# indicators × countries — count years 
get_country_label <- function(code) {
  if (exists("country_lookup")) {
    nm <- country_lookup$country_name[match(code, country_lookup$country_code)]
    ifelse(!is.na(nm) & nzchar(nm), nm, code)
  } else code
}

p3_counts <- df_in %>%
  mutate(country_label = get_country_label(country_code)) %>%
  distinct(indicator, country_label, year) %>%
  count(indicator, country_label, name = "count") %>%
  mutate(indicator_lab = wrap_indicator(indicator))

p4_counts <- df_out %>%
  mutate(country_label = get_country_label(country_code)) %>%
  distinct(indicator, country_label, year) %>%
  count(indicator, country_label, name = "count") %>%
  mutate(indicator_lab = wrap_indicator(indicator))

# ORDER COUNTRIES ALPHABETICALLY
alpha_countries <- sort(unique(c(p3_counts$country_label, p4_counts$country_label)))
p3_counts$country_label <- factor(p3_counts$country_label, levels = alpha_countries)
p4_counts$country_label <- factor(p4_counts$country_label, levels = alpha_countries)

n_years_obs <- max(
  dplyr::n_distinct(df_in$year),
  dplyr::n_distinct(df_out$year)
)

# TODO: change the whitespace
x_exp <- expansion(mult = c(0.01, 0.01))
y_exp <- expansion(mult = c(0.01, 0.01))

# make sure that title is not clipped 
base_theme <- theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 8)),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

scale_fill_countries <- scale_fill_gradient(name = "# countries", low = "#9ECAE1", high = "#08306B",
                                            limits = c(0, n_countries_obs))

scale_fill_years <- scale_fill_gradient(name = "# years", low = "#9ECAE1", high = "#08306B",
                                            limits = c(0, n_years_obs))

ttl_p1 <- wrap_title("Key Indicator Data Coverage by Year: Zero Values Kept")
sub_p1 <- wrap_title("Count of OECD countries reporting per indicator-year (years ≥ 2000). Zeros are treated as real values. Sources: IHME & OECD; limited to the 38 OECD countries.")
ttl_p2 <- wrap_title("Key Indicator Data Coverage by Year: Zero Values Removed")
sub_p2 <- wrap_title("Count of OECD countries reporting per indicator-year (years ≥ 2000). Zeros are treated as missing except for Norway, Denmark, and Ireland. Sources: IHME & OECD; limited to the 38 OECD countries.")
ttl_p3 <- wrap_title("Key Indicator Data Coverage by Country: Years Reported (Zeros Kept)")
sub_p3 <- wrap_title("Number of years with any reported value for each indicator and OECD country (years ≥ 2000). Zeros are treated as real values. Sources: IHME & OECD; 38 OECD countries. Countries ordered alphabetically.")
ttl_p4 <- wrap_title("Key Indicator Data Coverage by Country: Years Reported (Zeros Removed)")
sub_p4 <- wrap_title("Number of years with any reported value for each indicator and OECD country (years ≥ 2000). Zeros are treated as missing except for Norway, Denmark, and Ireland. Countries ordered alphabetically. Sources: IHME & OECD; 38 OECD countries.")

make_p1 <- function() ggplot(p1_dat, aes(x = year, y = indicator_lab, fill = count)) +
  geom_raster(interpolate = FALSE) + # geom_tile(width = 0.98, height = 0.98, colour = NA) +
  scale_fill_countries +
  scale_x_continuous(expand = x_exp, breaks = scales::pretty_breaks(10)) +
  scale_y_discrete(expand = y_exp) +
  labs(title = ttl_p1, subtitle = sub_p1, x = "Year", y = "Indicator") +
  base_theme

make_p2 <- function() ggplot(p2_dat, aes(x = year, y = indicator_lab, fill = count)) +
  geom_raster(interpolate = FALSE) + # geom_tile(width = 0.98, height = 0.98, colour = NA) +
  scale_fill_countries +
  scale_x_continuous(expand = x_exp, breaks = scales::pretty_breaks(10)) +
  scale_y_discrete(expand = y_exp) +
  labs(title = ttl_p2, subtitle = sub_p2, x = "Year", y = "Indicator") +
  base_theme

make_p3 <- function() ggplot(p3_counts, aes(x = country_label, y = indicator_lab, fill = count)) +
  geom_raster(interpolate = FALSE) + # geom_tile(width = 0.98, height = 0.98, colour = NA) +
  scale_fill_years +
  scale_x_discrete(expand = x_exp) +
  scale_y_discrete(expand = y_exp) +
  labs(title = ttl_p3, subtitle = sub_p3, x = "Country", y = "Indicator") +
  base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

make_p4 <- function() ggplot(p4_counts, aes(x = country_label, y = indicator_lab, fill = count)) +
  geom_raster(interpolate = FALSE) + geom_tile(width = 0.98, height = 0.98, colour = NA) +
  scale_fill_years +
  scale_x_discrete(expand = x_exp) +
  scale_y_discrete(expand = y_exp) +
  labs(title = ttl_p4, subtitle = sub_p4, x = "Country", y = "Indicator") +
  base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Build plots
p1 <- make_p1()
p2 <- make_p2()
p3 <- make_p3()
p4 <- make_p4()

# Column-wise legend
left_col  <- (p1 / p2) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.justification = "center", legend.box.just = "center")

right_col <- (p3 / p4) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.justification = "center", legend.box.just = "center")

quad <- left_col | right_col

ggsave(file.path(quad_dir, "coverage_quadrant_v8.png"),
       quad, width = 18, height = 16, dpi = 220)

print(quad_dir)
