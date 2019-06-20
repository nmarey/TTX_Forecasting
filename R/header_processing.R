# Header Pre Processing
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



header_processing <- function(df){

  headers <- names(df)


# change .. to _ft_ -------------------------------------------------------

  pattern_ft <- "\\d[[:punct:]][[:punct:]]"
  headers[grepl(pattern_ft,headers)] <- stringr::str_replace(
    headers[grepl(pattern_ft, headers)], "[[:punct:]][[:punct:]]", "_ft_")

  rm(pattern_ft)

# replace . with _ when it represents a space -----------------------------
  pattern_space <- "[^0-9]\\.[^0-9]"
  patern_space_binary <- grepl(pattern_space, headers)
  headers[patern_space_binary] <- stringr::str_replace(
    headers[patern_space_binary], "\\.", "_")


  patern_space_binary <- grepl(pattern_space, headers)
  headers[patern_space_binary] <- stringr::str_replace(
    headers[patern_space_binary], "\\.", "_")


  patern_space_binary <- grepl(pattern_space, headers)
  headers[patern_space_binary] <- stringr::str_replace(
    headers[patern_space_binary], "\\.", "_")


  rm(patern_space_binary, pattern_space)

# replace . with space for "numeric dat char" -----------------------------

  pattern_numeric_dot_char <- "\\d\\.\\D"
  pattern_numeric_dot_char_binary <- grepl(pattern_numeric_dot_char, headers)
  headers[pattern_numeric_dot_char_binary] <- stringr::str_replace(
    headers[pattern_numeric_dot_char_binary], "\\.", "_")

  rm(pattern_numeric_dot_char, pattern_numeric_dot_char_binary)

# remove . at end of name -------------------------------------------------
  pattern_dot_end <- "\\.$"
  pattern_dot_end_binary <- grepl(pattern_dot_end, headers)
  headers[pattern_dot_end_binary] <- stringr::str_remove(
    headers[pattern_dot_end_binary], pattern_dot_end)

  rm(pattern_dot_end, pattern_dot_end_binary)


# replace names -----------------------------------------------------------
  colnames(df) <- headers

}
