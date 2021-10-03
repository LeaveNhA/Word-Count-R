combine <- function(fns){
  function(args)
    Reduce(function(fn, acc) fn(acc), rev(fns), args, right = TRUE)
}

word_count <- function(input) {
  combine(c(tolower,
            utf8ToInt,
            function(numeric_list) Filter(function(x) x %in% c(32, 48:57, 65:90, 97:122), numeric_list),
            intToUtf8,
            function(filtered_string) strsplit(filtered_string, "[ \t\n]")[[1]],
            function(splitted_words) Filter(function(w) nchar(w) > 0, splitted_words),
            table,
            as.list))(input)
}
