library(tikzDevice)


round_floats_in_text <- function(text) {
  # Define the pattern to match (float1, float2) with optional spaces
  pattern <- "\\(\\s*(-?\\d+\\.\\d+)\\s*,\\s*(-?\\d+\\.\\d+)\\s*\\)"
  
  # Find matches
  matches <- gregexpr(pattern, text, perl = TRUE)
  
  # Extract matches and process them
  regmatches(text, matches) <- lapply(regmatches(text, matches), function(m) {
    sapply(m, function(match) {
      # Extract numbers from the matched string
      numbers <- as.numeric(unlist(regmatches(match, gregexpr("-?\\d+\\.\\d+", match))))
      
      # Round the numbers
      rounded_numbers <- round(numbers)
      
      # Replace with rounded values
      sprintf("(%d, %d)", rounded_numbers[1], rounded_numbers[2])
    })
  })
  
  return(text)
}

# Example usage:
text <- "Some values are ( 12.7 ,45.3) and also (-3.6,  7.9 )."
new_text <- round_floats_in_text(text)
print(new_text)

#For some reason, Rstudio needs to know the time zone...
options(tz="CA")

options(tikzSanitizeCharacters = c('%','$','}','{','^', '_'))
options(tikzReplacementCharacters = c('\\%','\\$','\\}','\\{',
                                      '\\^{}', '\\textunderscore'))

file <- "visualizations/score_clusters_stats.tex"

save_tex <- function(g, file, width = 5.5, height = 4.5, reduce_power=5) {
  tikz(file = file, width = width, height = height, sanitize = TRUE, standAlone = TRUE)
  print(g)
  dev.off()
  # remove all \r from the file
  txt <- readr::read_file(file)
  
  txt <- stringr::str_replace_all(txt, "\n\r", "")
  txt <- stringr::str_replace_all(txt, "\n\n", "\n")
  
  txt <- round_floats_in_text(txt)
  
  lines <- stringr::str_split_1(string = txt, pattern = ";")
  ulines <- unique(lines)
  for (ul in ulines) {
    to_remove <- which(lines == ul)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }
  
  txt <- paste(lines, collapse = ";")
  
  readr::write_file(txt, file)
  
  lines <- readLines(con=file)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines,perl=F))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines,perl=F))]
  # detect duplicated \definecolor lines
  #ucolors <- unique(lines[grepl("\\\\definecolor\\{.*\\}\\{.*\\}\\{.*\\}", lines,perl=F)])
  #for (uc in ucolors){
  #  to_remove <- which(lines==uc)[-1]
  #  if (length(to_remove) > 0) {
  #    lines <- lines[-to_remove]
  #  }
  #}
  upath <- unique(lines[grepl("\\path\\[", lines,perl=F)])
  for (up in upath){
    to_remove <- which(lines==up)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }
  # remove duplicated \definecolor lines (keep only first)
  lines <- lines[lines!=""]
  lines <- lines[lines != lag(lines, 1)]
  if (reduce_power != 0){
    if (reduce_power < 1){
      reduce_power <- ceiling(1/reduce_power)
      flt <- rep(c(rep(F, reduce_power), T), length(lines)/reduce_power)[1:length(lines)]
    } else {
      flt <- rep(c(F,rep(T, reduce_power)), length(lines)/reduce_power)[1:length(lines)]
    }
    rnd <- as.logical(1-grepl("\\path\\[.*\\] \\(.*\\) circle \\(.*\\);", lines,perl=F)*flt) & as.logical(1-grepl("\\t\\(.*\\) --", lines,perl=F)*flt)
    lines <- lines[rnd]
  }
  lines <- lines[which(lines == "\\begin{tikzpicture}[x=1pt,y=1pt]"):which(lines == "\\end{tikzpicture}")]
  writeLines(lines, con=file)
}
