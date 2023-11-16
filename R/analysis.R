library(tidyverse)
library(readxl)

read_languages <- function(sheet) {
  state_data <- read_excel(
    "data/acs_lang.xls",
    sheet = sheet,
    skip = 5,
    col_names = c(
      "language",
      "n_speakers",
      "moe_n_speakers",
      "n_non_native_speakers",
      "moe_n_non_native"
    ),
    na = c("", "(X)", "(D)", "(B)", "--")
  ) |>
    mutate(language = str_remove(language, "\\.*"))

  end_of_data <- which(state_data$language == "Uncodable")

  state_data |>
    slice(seq_len(end_of_data)) |>
    filter(
      language %in% c(
        "Latvian",
        "Sahaptian",
        "French",
        "Aleut",
        "Pennsylvania Dutch",
        "Armenian",
        "Tagalog",
        "Mikasuki"
      )
    )
}

read_languages("Alabama")
read_languages("Alaska")

sheets <- excel_sheets("data/acs_lang.xls")[-1]

languages <- sheets |>
  map(read_languages)

languages |>
  list_rbind() |>
  mutate(state = sheets)

names(languages) <- sheets
names(languages)

languages_df <- languages |>
  list_rbind(names_to = "state")

languages_df |>
  group_by(language) |>
  summarize(n_speakers = sum(n_speakers, na.rm = TRUE))

