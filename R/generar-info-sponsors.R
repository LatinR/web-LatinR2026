library(tibble)
library(yaml)
title_mapping <- list(
    "es" = list(
        "partner" = "Partner",
        "sponsor" = "Patrocinadores",
        "community" = "Comunidades Amigas",
        "supporter" = "Nos Apoyan"
    ),
    "en" = list(
        "partner" = "Partner",
        "sponsor" = "Sponsors",
        "community" = "Partner Communities",
        "supporter" = "Supporters"
    ),
    "pt" = list(
        "partner" = "Partner",
        "sponsor" = "Patrocinadores",
        "community" = "Comunidades parceiras",
        "supporter" = "Apoiadores"
    )
)


get_title_by_language <- function(type, lang = "es") {
    if (type %in% names(title_mapping[[lang]])) {
        return(title_mapping[[lang]][[type]])
    } else {
        return(type)
    }
}


generar_info_sponsors <- function() {
    sponsors <- yaml.load_file(here::here("sobre/apoyo/sponsors.yml"))


    tibble::tibble(
        name = sapply(sponsors, function(x) x$name),
        href = sapply(sponsors, function(x) x$href),
        img = sapply(sponsors, function(x) x$thumbnail),
        type = sapply(sponsors, function(x) x$type)
    )
}


generar_lista_sponsors <- function(info_sponsors, lang = "es") {
    html_output <- ""

    unique_types <- unique(info_sponsors$type)

    unique_types <- factor(unique_types, levels = c("partner", "sponsor", "supporter", "community"), ordered = TRUE)
    unique_types <- unique_types[order(unique_types)]

    for (sponsor_type in unique_types) {
        title <- get_title_by_language(sponsor_type, lang)
        html_output <- paste0(html_output, "<h2>", title, "</h2><div class='sponsorsFooter'><ul>")

        sponsors_of_type <- info_sponsors[info_sponsors$type == sponsor_type, ]

        for (i in 1:nrow(sponsors_of_type)) {
            html_output <- paste0(
                html_output,
                "<li><a href='", sponsors_of_type$href[i],
                "' target='_blank' class='sblogos__link'><img class='sblogos__img ",
                sponsor_type, "' src='", sponsors_of_type$img[i],
                "' alt='", sponsors_of_type$name[i], " logo' /></a></li>"
            )
        }

        html_output <- paste0(html_output, "</ul></div>")
    }

    return(html_output)
}
