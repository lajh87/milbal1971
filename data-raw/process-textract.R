df <- readr::read_csv("data-raw/Textract_Vol_071(1)_The Military Balance.csv")


df |> 
  janitor::clean_names() |>
  dplyr::filter(page_number == "'1") |>
  dplyr::filter(stringr::str_detect(layout , "Title")) |>
  dplyr::select(layout, text, file) 

file_title <- c("Notes", "04597227108459804",
  "PREFACE", "04597227108459805",
  "Abbreviation", "04597227108459806",
  "The United States and the Soviet Union", "04597227108459807",
  "The European balance", "04597227108459808",
  "The Middle East and the Mediterranean", "04597227108459809",
  "Sub-Saharan Africa", "04597227108459810", 
  "Asia and Australasia", "04597227108459811",
  "Tables", "04597227108459812",
  "Appendix: The military balance between Nato and the Warsaw pact", "04597227108459812") |>
  matrix(ncol = 2, byrow = TRUE) |>
  dplyr::as_tibble(.name_repair = "minimal")  |>
  rlang::set_names(c("title", "file")) |>
  dplyr::mutate(file = file.path(file, "layout.csv")) |>
  dplyr::mutate(title = tolower(stringr::str_replace_all(title, "\\s", "_"))) |>
  dplyr::mutate(title = paste0(stringr::str_remove_all(title, ":"), ".qmd")) |> 
  dplyr::mutate(id = 1:dplyr::n()) |>
  dplyr::mutate(id = ifelse(nchar(id) == 1, paste0("0", id), id)) |>
  dplyr::mutate(title = paste(id, title, sep = "-"))

for (i in seq_along(file_title$title)){
  
  df |>
    janitor::clean_names() |> 
    dplyr::filter(file == file_title$file[i]) |>
    dplyr::filter(page_number != "'1") |>
    dplyr::mutate(text = stringr::str_remove_all(text, "^'")) |>
    dplyr::mutate(text = dplyr::case_when(
      stringr::str_detect(layout, "Title") ~ paste("#", text),
      stringr::str_detect(layout, "Section header") ~ paste("##", text),
      TRUE ~ text
    )) |> 
    
    dplyr::mutate(text= ifelse(is.na(text), "", text)) |>
    dplyr::summarise(text= paste(text, collapse = "\n")) |>
    dplyr::pull() |>
    write(file_title$title[i])
  
}


