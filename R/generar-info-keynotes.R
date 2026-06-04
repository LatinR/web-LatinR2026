generar_info_keynotes <- function() {
  tibble::tibble(
    name = "Nic Crane",
    href = "https://niccrane.com",
    img = "img/posts/nic-crane.jpg",
    info_es = "Nic Crane es mantenedora de software de código abierto, consultora independiente de R y formadora. Es miembro del Comité de Gestión del Proyecto Apache Arrow y una de las personas que mantiene el paquete Arrow para R, con lo que contribuye al ecosistema general de Arrow y a su adopción en los flujos de trabajo de ciencia de datos. Nic colabora con el paquete ellmer para R e imparte clases y escribe sobre el uso de modelos de lenguaje a gran escala (LLM) en R (https://niccrane.com).",
    info_en = "Nic Crane is an open-source maintainer and independent R consultant and educator. They are a member of the Apache Arrow Project Management Committee and one of the maintainers of the Arrow R package, contributing to the broader Arrow ecosystem and its adoption across data science workflows. Nic is a contributor to the ellmer R package and teaches and writes about using LLMs in R (https://niccrane.com/).",
    info_pt = "Nic Crane é mantenedora de software de código aberto, consultora independente de R e educadora. É membro do Comitê de Gestão do Projeto Apache Arrow e uma das mantenedoras do pacote Arrow para R, contribuindo para o ecossistema geral do Arrow e sua adoção em fluxos de trabalho de ciência de dados. Nic colabora com o pacote ellmer para R e ensina e escreve sobre o uso de LLMs em R (https://niccrane.com/)."
  )
}

generar_card_keynote <- function(tab, col_info = "info_es"){
  bslib::card(
    full_screen = FALSE,
    bslib::card_header(tab$name, class = "card-header-keynote"),
    bslib::card_image(
      file = here::here(tab$img),
      href = tab$href,
      border_radius = "all"
    ),
    bslib::card_body(shiny::markdown(tab[[col_info]]), ),
  )
}


generar_cards_keynotes <-
  function(info_keynotes,
           col_information = "info_es",
           width_col = 0.5) {
    cards <- lapply(seq_len(nrow(info_keynotes)), function(i) {
      generar_card_keynote(info_keynotes[i, ], col_info = col_information)
    })
    do.call(bslib::layout_column_wrap, c(
      list(
        class = "cards-display",
        width = width_col,
        gap = "0.5cm",
        height = 300
      ),
      cards
    ))
  }
