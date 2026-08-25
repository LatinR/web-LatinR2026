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
      info_es = "Emil Hvitfeldt es ingeniero de software en Posit y forma parte del equipo de tidymodels, que trabaja para mejorar las capacidades de modelado en R. Mantiene varios paquetes relacionados con el modelado, el análisis de texto y las paletas de colores. También intenta convertir *slidecrafting* en un verbo respetable. Es coautor, junto con Julia Silge, del libro *Supervised Machine Learning for Text Analysis in R*, y actualmente trabaja en el libro *Feature Engineering A-Z*.",
      info_en = "Emil Hvitfeldt is a software engineer at Posit and part of the tidymodels team’s effort to improve R’s modeling capabilities. He maintains several packages within the realms of modeling, text analysis, and color palettes. Trying to make slidecrafting a well respecting verb. He co-authored the book Supervised Machine Learning for Text Analysis in R with Julia Silge. Working on book Feature Engineering A-Z.",
      info_pt = "Emil Hvitfeldt é engenheiro de software na Posit e faz parte da equipe do tidymodels, que trabalha para aprimorar as capacidades de modelagem em R. Ele mantém vários pacotes nas áreas de modelagem, análise de texto e paletas de cores. Também está tentando transformar *slidecrafting* em um verbo respeitável. É coautor, junto com Julia Silge, do livro *Supervised Machine Learning for Text Analysis in R* e atualmente está trabalhando no livro *Feature Engineering A-Z*."
    ) |>
    tibble::add_row(
      name = "Mauricio Gómez Ardila",
      href = "https://www.linkedin.com/in/mauricio-gomez-ardila/",
      img = "img/posts/mauricio-gomez-ardila.png",
      info_es = "Mauricio Gómez Ardila es especialista de actuaría en Seguros Sura y docente de cátedra en la Facultad de Ingeniería de la Universidad de Antioquia, Colombia. Combina las matemáticas aplicadas y el aprendizaje estadístico para el análisis y solución de problemas financieros y de gestión de riesgos. Su experiencia se centra en el desarrollo de modelos de tarifación de seguros y optimización de reservas técnicas, integrando métodos actuariales con analítica avanzada.",
      info_en = "Mauricio Gómez Ardila is an actuarial specialist at Seguros Sura and a lecturer at the Faculty of Engineering of the Universidad de Antioquia, Colombia. He combines applied mathematics and statistical learning to analyze and solve financial and risk management problems. His expertise focuses on the development of insurance pricing models and optimization of technical reserves, integrating actuarial methods with advanced analytics.",
      info_pt = "Mauricio Gómez Ardila é especialista em atuária na Seguros Sura e professor na Faculdade de Engenharia da Universidade de Antioquia, Colômbia. Combina matemática aplicada e aprendizado estatístico para analisar e solucionar problemas financeiros e de gestão de riscos. Sua experiência concentra-se no desenvolvimento de modelos de precificação de seguros e otimização de reservas técnicas, integrando métodos atuariais com análise avançada de dados."
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
    do.call(
      bslib::layout_column_wrap,
      c(
        list(
          class = "cards-display",
          width = width_col,
          gap = "0.5cm",
          height = 300
        ),
        cards
      )
    )
  }
