word_count <- function(input) {
  lower_input               <- tolower(input)
  numeric_list              <- utf8ToInt(lower_input)
  filtered_list             <- Filter(function(x) x %in% c(32, 48:57, 65:90, 97:122), numeric_list)
  filtered_string           <- intToUtf8(filtered_list)
  splitted_words            <- strsplit(filtered_string, "[ \t\n]")[[1]]
  filtered_splitted_words   <- Filter(function(w) nchar(w) > 0, splitted_words)
  freq                      <- table(filtered_splitted_words)

  as.list(freq)
}
