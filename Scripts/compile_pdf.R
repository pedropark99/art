library(tidyverse)
library(quarto)
library(fs)

if (fs::file_exists("temp_entire_book.qmd")) {
  fs::file_delete("temp_entire_book.qmd")
}
if (fs::file_exists("temp_entire_book.typ")) {
  fs::file_delete("temp_entire_book.typ")
}

typst_yaml <- read_file("typst_yaml.yaml")

chapters <- fs::dir_ls("Chapters", glob = "*.qmd")
chapters <- c("index.qmd", chapters)
file <- c(typst_yaml, map_chr(chapters, read_file)) |>
  paste0(collapse = "\n")


file <- str_split(file, "\n") |>
  unlist()

figure_paths <- tibble(
    line_index = seq_along(file),
    image_text = str_extract(file, "^(!\\[(.*))", group = 1),
    image_path = str_extract_all(image_text, "(\\([- ./_a-zA-Z0-9]+\\))") |>
      map_chr(function(x) tail(x, n = 1)) |>
      str_replace_all("^\\(|\\)$", "")
  ) |>
  filter(!is.na(image_text))

figure_paths <- figure_paths %>% 
  mutate(
    image_text = str_replace(image_text, "[.]/[.][.]/", "./")
  )


for(i in seq_along(figure_paths$image_text)) {
  line_index <- figure_paths$line_index[i]
  image_text <- figure_paths$image_text[i]
  
  file[line_index] <- image_text
}



file <- paste0(file, collapse = "\n")
temp_path <- "temp_entire_book.qmd"
write_file(file, temp_path)



system2("quarto", args = c("render", '"temp_entire_book.qmd"', "--to", "typst"))


if (fs::file_exists("temp_entire_book.qmd")) {
  fs::file_delete("temp_entire_book.qmd")
}
if (fs::file_exists("temp_entire_book.typ")) {
  fs::file_delete("temp_entire_book.typ")
}



