# It read text files into a vector of words or columns
read_txt <- function(file, mode = c("line", "word"), keep_punctuation = FALSE) {
  # Use tryCatch to catch errors from match.arg
  tryCatch({
    mode <- match.arg(mode, c("line", "word"))
  }, error = function(e) {
    stop("Invalid mode. Choose either 'line' or 'word'.")
  })

  # Read the contents of the file and suppress warnings like end line incomplete
  temp <- suppressWarnings(readLines(file))

  # Concatenate all lines into a single string with new line characters
  text <- paste(temp, collapse = "\n")

  # Breaks the text file into lines or words
  if (mode == "line") {
    if (keep_punctuation) {
      # Split text based on ., ?, or new line while preserving punctuation
      lines <- unlist(strsplit(text, "(?<=[.!?])\\s*|\\n+", perl = TRUE))
    } else {
      # Split text based on ., ?, or new line and remove punctuation
      lines <- unlist(strsplit(text, "[.!?]\\s*|\\n+"))
    }
    lines <- trimws(lines)  # Trim white spaces from each line
    lines <- lines[lines != ""]  # Remove empty strings
    return(lines)
  } else if (mode == "word") {
    if (keep_punctuation) {
      # Split text into words, keeping punctuation as separate elements
      words <- unlist(strsplit(text, "(?<=[^\\w'])|(?=[^\\w'])", perl = TRUE))
    } else {
      # Split text into words, removing punctuation but keeping contractions
      words <- unlist(strsplit(text, "[?[:space:]\n.,()!-]", perl = FALSE))
      words <- words[words != ""]
    }
    words <- trimws(words)  # Trim white spaces from each word
    words <- words[words != ""]  # Remove empty strings
    return(words)
  }
}
