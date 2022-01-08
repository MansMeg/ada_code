
german_house_list <- list("Allensbach",
                          "Forsa",
                          "Forsch\u2019gr.Wahlen",
                          "GMS",
                          "Infratestdimap",
                          "INSA",
                          "Kantar(Emnid)",
                          "Yougov")

german_houses <- tibble::tibble(country = "Germany",
                               house_name = unlist(german_house_list),
                               house = unlist(german_house_list))


swedish_house_list <- list(c("Sentio", "Sentio Research"),
                           c("Novus", "Novus  (1)", "Novus  (2)", "Novus (1)", "Novus (2)"),
                           c("Sifo"),
                           c("Demoskop"),
                           c("Skop"),
                           c("SCB"),
                           c("YouGov"),
                           c("Inizio"),
                           c("Ipsos", "Ipsos (Synovate)", "Synovate"),
                           c("United Minds"))

tibble_list <- list()
for(i in seq_along(swedish_house_list)){
  tibble_list[[i]] <- tibble::tibble(country = "Sweden",
                                    house_name = unlist(swedish_house_list[[i]]),
                                    house = unlist(swedish_house_list[[i]][1]))
}
swedish_houses <- do.call(rbind, tibble_list)


spanish_house_list <- list(c("Celeste-Tel"),
                   c("CIS", "CIS (Gravitas)", "CIS (JM&A)", "CIS (Kiko Llaneras)", "CIS (SocioM\u00E9trica)"),
                   c("GAD3"),
                   c("GESOP"),
                   c("Invymark"),
                   c("JM&A"),
                   c("Metroscopia"),
                   c("NC Report"),
                   c("Opina"),
                   c("Obradoiro de Sociolox\u00EDa"),
                   c("Sigma Dos"),
                   "Simple L\u00F3gica",
                   c("SocioM\u00E9trica"),
                   "TNS Demoscopia")
tibble_list <- list()
for(i in seq_along(spanish_house_list)){
  tibble_list[[i]] <- tibble::tibble(country = "Spain",
                                    house_name = unlist(spanish_house_list[[i]]),
                                    house = unlist(spanish_house_list[[i]][1]))
}
spanish_houses <- do.call(rbind, tibble_list)


#"\u00E9" "é"
#"\u00F3" "ó"
#"\u00ED" "í"
#"\u2019" "’"
#as.u_char_seq("’", "")

house_map <- rbind(german_houses, swedish_houses, spanish_houses)

usethis::use_data(house_map, overwrite = TRUE)

