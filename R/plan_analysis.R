#' @title Get the analysis plan
#' @description Generate the analysis pipeline within the `drake` framework.
#' @return `drake` workflow for the analysis
#' @export
get_plan <- function(){
  drake_plan(
    raw_data = readxl::read_excel(file_in("analysis/data/other-iris.xlsx")),
    ready_data = dplyr::mutate(raw_data, Species = forcats::fct_inorder(Species)),
    hist = create_plot(ready_data),
    fit = lm(Sepal.Width ~ Petal.Width + Species, ready_data),
    report = rmarkdown::render(
      knitr_in("analysis/templates/report.Rmd"),
      output_file = file_out("analysis/manuscript/report.html"),
      output_dir = "analysis/manuscript",
      quiet = TRUE
    ),
    strings_in_dots = "literals"
  )
}
