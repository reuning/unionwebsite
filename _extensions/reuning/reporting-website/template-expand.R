if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
    ## If previewing/rending one page
    quit()
}

if (!require(yaml)) {
    simpleError("Please install the yaml package")
}



template_files <- list.files(pattern = "*-template.qmd",
                             recursive = TRUE,
                             ignore.case = TRUE,  full.names = TRUE)

if (length(template_files) == 0) {
    stop("No template files found, skipping processing")
}
yml_files <- gsub("qmd$", "yml", template_files)



for (ii in seq_along(template_files)){

    template <- readLines(template_files[ii])
    base_path <- dirname(yml_files[ii])
    all_vars <- read_yaml(yml_files[ii])

    if ("data" %in% names(all_vars)) {
        ## Loading data if is set
        eval(parse(text = all_vars$data))
    }

    items <- all_vars$items
    dir_out <- file.path(base_path, names(items)[[jj]])
    if(!dir.exists(dir_out)) dir.create(dir_out)

    for (jj in seq_along(items)){
        qmd_out <- template

        if ("data" %in% names(items[[jj]])) {
            eval(parse(text = paste0("tmp_df <- ", items[[jj]]$data$subset)))
            file_out <- file.path(dir_out, items[[jj]]$data$filename)
            write.csv(tmp_df, file_out, row.names = FALSE)

            qmd_out <- gsub("\\{\\{< template data_filename >\\}\\}",
                             paste0("\"", items[[jj]]$data$filename, "\""),
                             qmd_out)

        }
        vars <- items[[jj]]
        vars$data <- NULL
        for (kk in seq_along(vars)) {
            qmd_out <- gsub(paste0("\\{\\{< template ",
                                    names(vars)[kk], " >\\}\\}"),
                             vars[[kk]], qmd_out)
        }


        file_out <- file.path(dir_out, "index.qmd")
        writeLines(qmd_out, file_out)
    }

}