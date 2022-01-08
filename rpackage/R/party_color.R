#' Get the party color for a given party abbrivation
#'
#' @param x a party abbrivation
party_color <- function(x){
  checkmate::assert_string(x)
  # Taken from: https://sv.wikipedia.org/wiki/Mall:Partif%C3%A4rg
  pc <- list("S" = "#EE2020",
             "M" = "#1B49DD",
             "C" = "#009933",
             "L" = "#6BB7EC",
             "KD" = "#231977",
             "MP" = "#83CF39",
             "V" = "#AF0000",
             "SD" = "#DDDD00",
             # Germany
             "CDU/CSU" = "#000000",
             "SPD" = "#EB001F",
             "GRUNE" = "#64A12D",
             "FDP" = "#FFED00",
             "LINKE" = "#BE3075",
             "AfD" = "#009EE0",
             # Spain
             "PP" = "#1D84CE",
             "PSOE" = "#EF1C27",
             "UP" = "#7B4977",
             "ERC" = "#FFB232",
             "VOX" = "#63BE21",
             "Cs" = "#EB6109",
             "CC" = "#FFD700"
             )
  ifelse(x %in% names(pc), pc[[x]], "darkgrey")
}
