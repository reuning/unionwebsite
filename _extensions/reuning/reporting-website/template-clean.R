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

if("keep-files" %in% names(yml_files)){
    if(yml_files$keep_files %in% c("TRUE", "true", "True")) {
        stop("Not data deleting files")
    }
}

for (ii in seq_along(template_files)){

    base_path <- dirname(yml_files[ii])
    all_vars <- read_yaml(yml_files[ii])

    items <- all_vars$items
    for (jj in seq_along(items)){

        if ("data" %in% names(items[[jj]])) {
            file_out <- file.path(base_path, items[[jj]]$data$filename)
            file.remove(file_out)
        }

    }
}
