library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# change file paths as needed
iso_csv    <- "./iso3_country_mapping_wide.csv"
input_csv  <- "./IHME/raw/all_maternal_disorders_deaths.csv"
country_name_col <- "location_name" 
output_dir <- "./IHME/processed"

norm_key <- function(x) str_squish(str_to_lower(as.character(x)))

iso_map_raw <- readr::read_csv(iso_csv, show_col_types = FALSE) %>%
  transmute(country_name = as.character(country_name),
            iso3c = as.character(iso3c))

iso_map <- iso_map_raw %>%
  mutate(country_key = norm_key(country_name)) %>%
  distinct(country_name, iso3c, country_key)

conflicts <- iso_map %>%
  count(country_key, iso3c) %>%
  count(country_key, name = "n_codes") %>%
  filter(n_codes > 1)

iso_map_key <- iso_map_raw %>%
  mutate(country_key = norm_key(country_name)) %>%
  filter(country_key != "", !is.na(country_key)) %>%
  group_by(country_key) %>%
  summarise(iso3c = first(iso3c), .groups = "drop")

df <- readr::read_csv(input_csv, show_col_types = FALSE)
cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
glimpse(df)

stopifnot(country_name_col %in% names(df))
required_base <- c("cause_id", "cause_name", "year", "val")
missing_base  <- setdiff(required_base, names(df))
if (length(missing_base)) {
  stop("Missing required column(s): ",
       paste(missing_base, collapse = ", "),
       "\nPlease verify the source file or adjust your column mapping.")
}
head(unique(df[[country_name_col]]), 20)

df_out <- df %>%
  mutate(.country_key = norm_key(.data[[country_name_col]])) %>%
  left_join(iso_map_key, by = c(".country_key" = "country_key")) %>%
  select(-.country_key) %>%
  relocate(iso3c, .after = all_of(country_name_col))

cat("\nAFTER JOIN — PREVIEW\n")
print(head(df_out, 10))
cat("Rows after join:", nrow(df_out), "| Cols:", ncol(df_out), "\n")

unmatched <- df_out %>%
  filter(is.na(iso3c)) %>%
  distinct(.data[[country_name_col]]) %>%
  arrange(.data[[country_name_col]])
names(unmatched) <- country_name_col

if (nrow(unmatched) == 0) {
  cat("All country names matched to ISO3 codes.\n")
} else {
  cat("⚠️ Unmatched country names:", nrow(unmatched), "\n")
  print(head(unmatched, 50))
  out_unmatched <- file.path(dirname(output_dir),
                             paste0(tools::file_path_sans_ext(basename(input_csv_path)),
                                    "_unmatched.csv"))
  readr::write_csv(unmatched, out_unmatched)
  cat("Saved unmatched list to:", out_unmatched, "\n")
}

# Cleaning steps for IHME data (Global Burden of Disease)
required_for_final <- c("cause_id", "cause_name", "year", "val", "iso3c", country_name_col)
missing_final <- setdiff(required_for_final, names(df_out))
if (length(missing_final)) {
  stop("Columns missing before final schema: ",
       paste(missing_final, collapse = ", "),
       "\nPlease check earlier sections.")
}

final_df <- df_out %>%
  transmute(
    indicator_code  = .data[["cause_id"]],
    indicator       = .data[["cause_name"]],
    parent_location = NA_character_, 
    country_code    = .data[["iso3c"]],
    country         = .data[[country_name_col]],
    period          = .data[["year"]],
    period_type     = NA_character_,
    ranged_year_end = .data[["year"]],
    value           = .data[["val"]],
    date_updated    = NA_character_
  )

cat("\nFINAL DATA — glimpse():\n")
glimpse(final_df)
cat("Rows in final_df:", nrow(final_df), "\n")

cat("\nSanity check (NAs):\n")
cat(" country_code:", sum(is.na(final_df$country_code)), "\n")
cat(" country     :", sum(is.na(final_df$country)), "\n")
cat(" indicator_code:", sum(is.na(final_df$indicator_code)), "\n")
cat(" period      :", sum(is.na(final_df$period)), "\n")
cat(" value       :", sum(is.na(final_df$value)), "\n")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
out_clean <- file.path(
  output_dir,
  paste0(tools::file_path_sans_ext(basename(input_csv)), "_cleaned.csv")
)
write_csv(final_df, out_clean)
cat("\nSaved cleaned file to:", out_clean, "\n")


out_clean <- file.path(output_dir,
                       paste0(tools::file_path_sans_ext(basename(input_csv_path)),
                              "_cleaned.csv"))
write_csv(df_out, out_clean)
