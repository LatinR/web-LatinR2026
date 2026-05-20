generar_info_keynotes <- function() {
  tibble::tibble(
    name = "Nic Crane",
    href = "https://niccrane.com",
    img = "img/posts/nic-crane.jpg",
    info_es = "Nic Crane es mantenedora de software de código abierto, consultores independiente de R y formadores. Es miembro del Comité de Gestión del Proyecto Apache Arrow y una de las personas que mantiene el paquete Arrow para R, con lo que contribuye al ecosistema general de Arrow y a su adopción en los flujos de trabajo de ciencia de datos. Nic colabora con el paquete ellmer para R e imparte clases y escribe sobre el uso de modelos de lenguaje a gran escala (LLM) en R (https://niccrane.com).",
    info_en = "Nic Crane is an open-source maintainer and independent R consultant and educator. They are a member of the Apache Arrow Project Management Committee and one of the maintainers of the Arrow R package, contributing to the broader Arrow ecosystem and its adoption across data science workflows. Nic is a contributor to the ellmer R package and teaches and writes about using LLMs in R (https://niccrane.com/).",
    info_pt = "Nic Crane é "
  ) |>
     tibble::add_row(
       name = "Edgar Ruiz",
       href = "https://edgarruiz.netlify.app",
       img = "img/posts/edgar-ruiz.jpg",
       info_es = "Edgar Ruiz es Solutions Engineer en Posit (anteriormente RStudio). Es un experto en el uso de R con bases de datos y computación distribuida; es el autor principal del paquete {sparklyr}, que permite usar Apache Spark desde R, y colaborador del paquete {dbplyr}. Es un comunicador entusiasta y referente de la comunidad latinoamericana de R.",
       info_en = "Edgar Ruiz is a Solutions Engineer at Posit (formerly RStudio). He is an expert in using R with databases and distributed computing; he is the lead author of the {sparklyr} package, which enables using Apache Spark from R, and a contributor to {dbplyr}. He is an enthusiastic communicator and a reference in the Latin American R community.",
       info_pt = "Edgar Ruiz é Solutions Engineer na Posit (anteriormente RStudio). Ele é especialista no uso de R com bancos de dados e computação distribuída; é o autor principal do pacote {sparklyr}, que permite usar o Apache Spark a partir do R, e colaborador do pacote {dbplyr}. É um comunicador entusiasmado e referência na comunidade latino-americana de R."
     ) |>
     tibble::add_row(
       name = "TBD",
       href = "#",
       img = "img/posts/latinr-skeleton.png",
       info_es = "Próximamente anunciaremos el tercer conferencista invitado de LatinR 2026. ¡Estén atentos!",
       info_en = "We will soon announce the third keynote speaker at LatinR 2026. Stay tuned!",
       info_pt = "Em breve anunciaremos o terceiro palestrante convidado do LatinR 2026. Fiquem atentos!"
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
    bslib::layout_column_wrap(
      class = "cards-display",
      width = width_col,
      gap = "0.5cm",
      height = 300,
      generar_card_keynote(info_keynotes[1,], col_info = col_information),
      generar_card_keynote(info_keynotes[2,],  col_info = col_information),
      generar_card_keynote(info_keynotes[3,],  col_info = col_information),
      # generar_card_keynote(info_keynotes[4,],  col_info = col_information)
    )
  }
