generar_info_keynotes <- function() {
  tibble::tibble(
    name = "Nic Crane",
    href = "https://niccrane.com",
    img = "img/posts/nic-crane.png",
    info_es = "Nic Crane mantiene software de código abierto, realiza consultorías de R y también tiene experiencia como docente. Es parte del Comité de Gestión del Proyecto Apache Arrow y una de las personas que mantiene el paquete Arrow para R, con lo que contribuye al ecosistema general de Arrow y a su adopción en los flujos de trabajo de ciencia de datos. Nic colabora con el paquete ellmer para R e imparte clases y escribe sobre el uso de modelos de lenguaje a gran escala (LLM) en R.",
    info_en = "Nic Crane is an open-source maintainer and independent R consultant and educator. They are a member of the Apache Arrow Project Management Committee and one of the maintainers of the Arrow R package, contributing to the broader Arrow ecosystem and its adoption across data science workflows. Nic is a contributor to the ellmer R package and teaches and writes about using LLMs in R (https://niccrane.com/).",
    info_pt = "Nic Crane trabalha com manutenção de software de código aberto, consultoria em R e também possui experiência em ensino. Integra o Comitê de Gestão do Projeto Apache Arrow e faz parte da equipe responsável pela manutenção do pacote Arrow para R, contribuindo para o ecossistema Arrow como um todo e para sua adoção em fluxos de trabalho de ciência de dados. Nic colabora com o pacote ellmer para R, além de ministrar cursos e escrever sobre o uso de modelos de linguagem de grande porte (LLMs) em R."
  ) |>
    tibble::add_row(
      name = "Emil Hvitfeldt",
      href = "https://emilhvitfeldt.com",
      img = "img/posts/emil-hvitfeldt.png",
      info_es = "Emil Hvitfeldt es Ingeniero de Software en Posit, donde trabaja en el ecosistema tidymodels. Es autor y mantenedor de varios paquetes de R, entre ellos textrecipes, paletteer y hardhat, y coautor del libro Supervised Machine Learning for Text Analysis in R.",
      info_en = "Emil Hvitfeldt is a Software Engineer at Posit, where he works on the tidymodels ecosystem. He is the author and maintainer of several R packages including textrecipes, paletteer, and hardhat, and co-author of the book Supervised Machine Learning for Text Analysis in R.",
      info_pt = "Emil Hvitfeldt é Engenheiro de Software na Posit, onde trabalha no ecossistema tidymodels. É autor e mantenedor de diversos pacotes do R, incluindo textrecipes, paletteer e hardhat, além de coautor do livro Supervised Machine Learning for Text Analysis in R."
    ) #|> 
    # tibble::add_row(
    #   name = "Stephanie Zimmer",
    #   href = "https://github.com/szimmer",
    #   img = "img/posts/StephanieZ.jpg",
    #   info_es = "",
    #   info_pt = "",
    #   info_en = ""
    # ) |> 
    # tibble::add_row(
    # name = "Equipo TRACE-LAC",
    #  href = "https://medicina.javeriana.edu.co/trace-lac",
    #   img = "img/posts/logo-tracelac.png",
    #   info_es = "",
    #   info_pt = "",
    #   info_en = ""
    # )
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
      # generar_card_keynote(info_keynotes[3,],  col_info = col_information),
      # generar_card_keynote(info_keynotes[4,],  col_info = col_information)
    )
  }
