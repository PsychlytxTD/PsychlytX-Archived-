desc_mds <- list.files("descriptions", pattern = ".md")

for (i in desc_mds) {
  rmarkdown::pandoc_convert(
    i, to = "latex", from = "markdown", output = sub("md$", "tex", i),
    wd = "descriptions"
  )
}
