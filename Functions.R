# Character count
char_count <- function (filename = this_filename()) {
  text_to_count_output <- wordcountaddin:::text_to_count(filename)
  word_count_output <- wordcountaddin::text_stats_fn_(text_to_count_output)
  word_count_output$n_char_tot_korp
}

# Remove output index
## Make sure to keep the default for normal processing.
default_output_hook <- knitr::knit_hooks$get("output")

## Output hooks handle normal R console output.
knitr::knit_hooks$set(output = function(x, options) {
  comment <- knitr::opts_current$get("comment")
    
  if(is.na(comment))
    comment <- ""
  
  can_null <- grepl(
      paste0(comment, "\\s*\\[\\d?\\]"),
      x, perl = TRUE
    )
  do_null <- isTRUE(knitr::opts_current$get("null_prefix"))
    
  if(can_null && do_null) {
      # By default R print output aligns at the right brace.
      align_index <- regexpr("\\]", x)[1] - 1
      # Two cases: start or newline
      re <- paste0("^.{", align_index, "}\\]")
      rep <- comment
      x <- gsub(re, rep,  x)
      re <- paste0("\\\n.{", align_index, "}\\]")
      rep <- paste0("\n", comment)
      x <- gsub(re, rep,  x)
    }
    default_output_hook(x, options)
  }
)