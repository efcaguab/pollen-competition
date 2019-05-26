render_pdf <- function(rmd_file, output_file, clean_md = FALSE, clean_tex = FALSE, quiet = TRUE, params = NULL){

  output_tex <- paste0(tools::file_path_sans_ext(rmd_file), ".tex")
  try({
    rmarkdown::render(rmd_file,
                      output_file = output_tex,
                      output_dir = dirname(rmd_file),
                      intermediates_dir = dirname(rmd_file),
                      clean = clean_md,
                      quiet = quiet,
                      params = params)
  })

  compile_pdf(output_tex,
              output_file,
              clean = clean_tex)
}

# Use tinytext to compile the file
compile_pdf <- function(file_in, file_out, clean) {

  tinytex.output_dir <- getOption("tinytex.output_dir")
  tinytex.engine_args <- getOption("tinytex.engine_args")
  on.exit({
    options(tinytex.output_dir = tinytex.output_dir,
            tinytex.engine_args = tinytex.engine_args)
  })

  options(tinytex.output_dir = "paper",
          tinytex.engine_args = "'--output-directory=paper'")

  tinytex::latexmk(file_in,
                   pdf_file = file_out,
                   clean = clean)
}

# download bibliography from a remote file
get_bibliography <- function(filename_in, filename_out, bib_retrieved){
  message("Retrieving bibliography - ", bib_retrieved)
  readLines(filename_in) %>%
    writeLines(filename_out)

}
