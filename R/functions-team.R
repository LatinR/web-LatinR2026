

generate_card <- function(person, class_group = "card-header-team") {
  bslib::card(
    full_screen = FALSE,
    bslib::card_header(person$name_complete[1], htmltools::HTML(person$icons[1]), class = class_group),
    bslib::card_body(htmltools::HTML(person$titles),
    )
  )
}

get_info_committe <- function(){
  # Run code below when we need to update
  url_sheets_comite <- "https://docs.google.com/spreadsheets/d/10TaBeco18m8d6FQOGJAIMxUjn10OJGQY_cSPdbERxsI/edit?resourcekey=&gid=616118855#gid=616118855"
  
  sheets_comite_raw <-
     googlesheets4::read_sheet(url_sheets_comite, "Respuestas de formulario 1")
  
  sheets_comite_raw |>
     readr::write_rds(here::here("sobre/equipo/comite.rds"))
  
  sheets_comite_raw <- readr::read_rds(here::here("sobre/equipo/comite.rds"))
  
  sheets_comite <- sheets_comite_raw |> 
    janitor::clean_names() |> 
    dplyr::arrange(nombre) |> 
    dplyr::rename(
      name_complete = nombre,
      titles = afiliacion_institucional,
      site = sitio_web,
      github = git_hub,
      linkedin = linked_in
    ) |> 
    dplyr::select(-marca_temporal) |> 
    dplyr::mutate(
      titles = dplyr::if_else(
        is.na(titles), "", titles
      ),
      titles = stringr::str_replace_all(titles, "\n|\\n", " | ")
    ) |> 
    generate_icons()
  
  sheets_comite
}

update_info_team <- function(){
    # Run code below when we need to update
  url_sheets_team <- "https://docs.google.com/spreadsheets/d/13AKRAYxg6Q1FFyscCuY6WKEG0DmG9IRu7MpTK236H7M/edit?resourcekey=&gid=1301120377#gid=1301120377"

  sheets_team_raw <-
    googlesheets4::read_sheet(url_sheets_team)

  sheets_team_raw |>
    readr::write_rds(here::here("sobre/equipo/team.rds"))
  
  #sheets_team_raw
}

get_info_team <- function(){

  sheets_team_raw <- readr::read_rds(here::here("sobre/equipo/team.rds"))

  sheets_team <- sheets_team_raw |> 
    janitor::clean_names() |> 
    dplyr::rename(
      name_complete = 2,
      titles = 3,
      site = sitio_web,
      github = git_hub,
      linkedin = linked_in,
    ) |> 
    dplyr::arrange(name_complete) |> 
    dplyr::select(-marca_temporal) |>
    # dplyr::mutate(
    #   linkedin = fix_social_url(linkedin, "https://linkedin.com/in/"),
    #   github = fix_social_url(github, "https://github.com/"),
    #   twitter =  fix_social_url(twitter, "https://twitter.com/")
    # ) |> 
    tidyr::drop_na(name_complete) |> 
    dplyr::mutate(
      category = dplyr::case_when(
       name_complete %in% c("Natalia da Silva", "Riva Quiroga", "Yanina Bellini Saibene") ~ "chairs",
       name_complete %in% c("Olga Usuaga", "Elena Patiño") ~ "chairs_locales",
        TRUE ~ "organizacion"
      )
    ) |> 
    dplyr::mutate(
      titles = dplyr::if_else(
        is.na(titles), "", titles
      ),
      titles = stringr::str_replace_all(titles, "\n|\\n", " | ")
    )
  
  sheets_team_icons <- sheets_team |> 
    generate_icons()
  
  sheets_team_icons 
}


fix_social_url <- function(var, url_base) {
  dplyr::case_when(
    stringr::str_starts(var, "http") ~ var,
    is.na(var) ~ NA_character_,
    TRUE ~ paste0(url_base, var)
  )
}



generate_icons <- function(tab){
  tab_with_icons <- tab |>
    dplyr::mutate(
      site = stringr::str_trim(site),
      icon_github = dplyr::case_when(
        !is.na(github) ~ glue::glue(
          '<a href="{github}" target="_blank"><i class="fab fa-github"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
      
      icon_twitter = dplyr::case_when(
        !is.na(twitter) ~ glue::glue(
          '<a href="{twitter}" target="_blank"><i class="fab fa-twitter"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
      
      icon_linkedin = dplyr::case_when(
        !is.na(linkedin) ~ glue::glue(
          '<a href="{linkedin}" target="_blank"><i class="fab fa-linkedin"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
      
      icon_mastodon = dplyr::case_when(
        !is.na(mastodon) ~ glue::glue(
          '<a href="{mastodon}" target="_blank"><i class="fab fa-mastodon"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
 
      icon_bluesky = dplyr::case_when(
        !is.na(blue_sky) ~ glue::glue(
          '<a href="{blue_sky}" target="_blank"><i class="fab fa-bluesky"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
           
      icon_site = dplyr::case_when(
        !is.na(site) ~ glue::glue(
          '<a href="{site}" target="_blank"><i class="fas fa-home"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
      
      icon_orcid = dplyr::case_when(
        !is.na(orcid) ~ glue::glue(
          '<a href="{orcid}" target="_blank"><i class="fab fa-orcid"></i></a>  '
        ),
        TRUE ~ ""
      ) ,
        
      icons = glue::glue("{icon_site}{icon_orcid}{icon_github}{icon_linkedin}{icon_bluesky}{icon_mastodon}{icon_twitter}")
    )
  
  tab_with_icons
}
