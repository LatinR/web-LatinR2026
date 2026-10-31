# Arma el programa (resumido y detallado) a partir de los .rds que genera data-raw/actualizar-programa.R
#
# - programa-sesiones.rds: un bloque del cronograma por fila (dia, inicio, fin, sala, id_sesion, sesion).
#   Si la sala está vacía, el bloque es para todos (breaks, keynotes, etc.).
#   Si una fila tiene id_sesion pero no tiene texto en `sesion`, es la continuación de la
#   sesión con ese mismo id (por ejemplo, la segunda parte de un tutorial después del break).
#   Cuando todas las salas continúan después de un break, las dos partes se muestran como un
#   solo bloque (ver unir_bloques_con_break()).
# - programa-agrupacion.rds: qué presentación va en cada sesión y en qué orden.
# - programa-presentaciones.rds y programa-autores.rds: título, resumen y autores de cada presentación.

textos_programa <- list(
  es = list(
    resumen = "Ver resumen",
    ver_mas = "Ver más",
    pagina_tutoriales = "../tutoriales/index.html",
    continuacion = "Continuación"
  ),
  pt = list(
    resumen = "Ver resumo",
    ver_mas = "Ver mais",
    pagina_tutoriales = "../tutoriales/workshops-pt.html",
    continuacion = "Continuação"
  ),
  en = list(
    resumen = "Show abstract",
    ver_mas = "Read more",
    pagina_tutoriales = "../tutoriales/workshops.html",
    continuacion = "Continued"
  )
)

# Ancla de la descripción de cada tutorial en la página de tutoriales
# (cronograma/tutoriales/). Las claves son los id_sesion de la hoja sesiones-web.
anclas_tutoriales <- c(
  T1 = "taller-ellmer",
  T2 = "taller-revealjs",
  T3 = "taller-quarto-gha",
  T4 = "taller-shiny-ellmer",
  T5 = "taller-tidymodels",
  T6 = "taller-paquete-r"
)

# Título de la pestaña de cada día. Los nombres coinciden con la columna `dia` de la hoja.
dias_programa <- list(
  es = c(
    "miércoles" = "Miércoles 11 de noviembre",
    "jueves" = "Jueves 12 de noviembre",
    "viernes" = "Viernes 13 de noviembre"
  ),
  pt = c(
    "miércoles" = "Quarta-feira, 11 de novembro",
    "jueves" = "Quinta-feira, 12 de novembro",
    "viernes" = "Sexta-feira, 13 de novembro"
  ),
  en = c(
    "miércoles" = "Wednesday, November 11",
    "jueves" = "Thursday, November 12",
    "viernes" = "Friday, November 13"
  )
)

cargar_programa <- function() {
  leer <- function(archivo) readr::read_rds(here::here("cronograma/programa", archivo))

  list(
    sesiones = leer("programa-sesiones.rds"),
    agrupacion = leer("programa-agrupacion.rds"),
    presentaciones = leer("programa-presentaciones.rds"),
    autores = leer("programa-autores.rds")
  )
}

# Convierte una hora "H:MM" en minutos desde la medianoche, para ordenar y comparar.
hora_a_minutos <- function(hora) {
  partes <- stringr::str_match(hora, "(\\d{1,2}):(\\d{2})")
  as.numeric(partes[, 2]) * 60 + as.numeric(partes[, 3])
}

formatear_horario <- function(inicio, fin) {
  glue::glue("{inicio}–{fin}")
}

# Completa los enlaces que vienen sin "https://" o como nombre de usuario.
# Descarta los valores que no se pueden convertir en un enlace (correos, texto con espacios).
normalizar_enlace <- function(valor, red) {
  valor <- stringr::str_trim(valor)

  if (is.na(valor) || valor == "" || stringr::str_detect(valor, "\\s")) {
    return(NA_character_)
  }
  if (stringr::str_detect(valor, "^https?://")) {
    return(valor)
  }
  if (red == "mastodon" && stringr::str_detect(valor, "^@?[^@/]+@[^@/]+$")) {
    partes <- stringr::str_split(stringr::str_remove(valor, "^@"), "@")[[1]]
    return(glue::glue("https://{partes[2]}/@{partes[1]}"))
  }
  if (stringr::str_detect(valor, "^[^/]+@[^/]+\\.[a-z]+$")) {
    return(NA_character_)
  }
  if (!stringr::str_detect(valor, "\\.")) {
    usuario <- stringr::str_remove(valor, "^@")
    base <- c(
      github = "https://github.com/",
      twitter = "https://x.com/",
      linkedin = "https://www.linkedin.com/in/"
    )
    return(if (red %in% names(base)) paste0(base[[red]], usuario) else NA_character_)
  }
  if (red == "bluesky" && !stringr::str_detect(valor, "/")) {
    return(paste0("https://bsky.app/profile/", stringr::str_remove(valor, "^@")))
  }
  paste0("https://", valor)
}

iconos_redes <- c(
  sitio_web = "bi-globe",
  github = "bi-github",
  linkedin = "bi-linkedin",
  mastodon = "bi-mastodon",
  bluesky = "bi-bluesky",
  twitter = "bi-twitter-x",
  enlace = "bi-link-45deg"
)

# "<strong>Nombre</strong>" seguido de un ícono por cada red que la persona haya dejado.
formatear_autor <- function(autor) {
  iconos <- purrr::imap_chr(iconos_redes, function(icono, red) {
    enlace <- normalizar_enlace(autor[[red]], red)
    if (is.na(enlace)) {
      return("")
    }
    glue::glue(
      ' <a href="{htmltools::htmlEscape(enlace, attribute = TRUE)}" target="_blank" ',
      'aria-label="{red}"><i class="bi {icono}"></i></a>'
    )
  })

  paste0(
    '<span class="charla-autor"><strong>', htmltools::htmlEscape(autor$nombre), '</strong>',
    paste(iconos, collapse = ""), '</span>'
  )
}

generar_div_charla <- function(charla, autores, textos) {
  titulo <- dplyr::coalesce(charla$titulo, charla$titulo_web)
  autores_charla <- autores |>
    dplyr::filter(id_presentacion == charla$id_presentacion)

  html <- '<div class="charla">\n'
  html <- paste0(html, '  <div class="charla-titulo">', htmltools::htmlEscape(titulo), '</div>\n')

  if (nrow(autores_charla) > 0) {
    lista_autores <- purrr::map_chr(seq_len(nrow(autores_charla)), \(i) formatear_autor(autores_charla[i, ]))
    html <- paste0(html, '  <div class="charla-autores">', paste(lista_autores, collapse = "\n"), '</div>\n')
  }

  if (!is.na(charla$resumen) && charla$resumen != "") {
    parrafos <- stringr::str_split(charla$resumen, "\n{2,}")[[1]]
    resumen_html <- paste0('<p>', htmltools::htmlEscape(parrafos), '</p>', collapse = "\n")
    html <- paste0(
      html,
      '  <details class="charla-resumen">\n',
      '    <summary>', textos$resumen, '</summary>\n',
      '    ', resumen_html, '\n',
      '  </details>\n'
    )
  }

  paste0(html, '</div>\n')
}

es_continuacion <- function(sesion) {
  !is.na(sesion$id_sesion) & sesion$id_sesion != "" & (is.na(sesion$sesion) | sesion$sesion == "")
}

# Una sesión: horario, título y las charlas asignadas (si las tiene).
# Las continuaciones solo muestran un aviso, sin repetir el título ni las charlas.
generar_div_sesion <- function(sesion, programa, textos, mostrar_horario = TRUE) {
  if (es_continuacion(sesion)) {
    return(paste0(
      '<div class="sesion sesion-continuacion">\n',
      '  <div class="sesion-titulo">', textos$continuacion, '</div>\n',
      '</div>\n'
    ))
  }

  charlas <- programa$agrupacion |>
    dplyr::filter(id_sesion == sesion$id_sesion) |>
    dplyr::arrange(as.numeric(orden)) |>
    dplyr::left_join(programa$presentaciones, by = "id_presentacion")

  html_charlas <- purrr::map_chr(
    seq_len(nrow(charlas)),
    \(i) generar_div_charla(charlas[i, ], programa$autores, textos)
  ) |>
    paste(collapse = "")

  html_horario <- if (mostrar_horario) {
    paste0('  <div class="sesion-horario">', formatear_horario(sesion$inicio, sesion$fin), '</div>\n')
  } else {
    ""
  }

  html_enlace <- if (isTRUE(sesion$id_sesion %in% names(anclas_tutoriales))) {
    paste0(
      '  <a class="sesion-enlace" href="', textos$pagina_tutoriales, '#',
      anclas_tutoriales[[sesion$id_sesion]], '">', textos$ver_mas, '</a>\n'
    )
  } else {
    ""
  }

  paste0(
    '<div class="sesion">\n',
    html_horario,
    '  <div class="sesion-titulo">', htmltools::htmlEscape(sesion$sesion), '</div>\n',
    html_enlace,
    html_charlas,
    '</div>\n'
  )
}

# Un bloque para todos (sin sala): una fila de ancho completo.
# Los keynotes y el panel (título que empieza con "Keynote" o "Panel") llevan una clase propia para destacarlos.
generar_div_general <- function(sesion, programa, textos) {
  clase_keynote <- if (stringr::str_detect(sesion$sesion, "^(Keynote|Panel)")) " cronograma-keynote" else ""

  paste0(
    '<div class="cronograma-row cronograma-general', clase_keynote, '">\n',
    '  <div class="horario">', formatear_horario(sesion$inicio, sesion$fin), '</div>\n',
    '  <div class="bloque">', generar_div_sesion(sesion, programa, textos, mostrar_horario = FALSE), '</div>\n',
    '</div>\n'
  )
}

# Un bloque de sesiones paralelas: una columna por sala y, dentro de cada columna,
# las sesiones de esa sala una debajo de la otra.
generar_div_paralelas <- function(sesiones_bloque, salas, programa, textos, nota = NULL) {
  inicio <- sesiones_bloque$inicio[which.min(sesiones_bloque$minuto_inicio)]
  fin <- sesiones_bloque$fin[which.max(sesiones_bloque$minuto_fin)]
  salas_bloque <- salas[salas %in% sesiones_bloque$sala]

  columnas <- purrr::map_chr(salas_bloque, function(sala_actual) {
    sesiones_sala <- sesiones_bloque |>
      dplyr::filter(sala == sala_actual) |>
      dplyr::arrange(minuto_inicio)

    # El horario de la sesión solo se muestra si es distinto al del bloque completo.
    html_sesiones <- purrr::map_chr(seq_len(nrow(sesiones_sala)), function(i) {
      sesion <- sesiones_sala[i, ]
      mismo_horario <- sesion$inicio == inicio && sesion$fin == fin
      generar_div_sesion(sesion, programa, textos, mostrar_horario = !mismo_horario)
    }) |>
      paste(collapse = "")

    paste0(
      '<div class="sala">\n',
      '  <div class="sala-nombre">', htmltools::htmlEscape(sala_actual), '</div>\n',
      html_sesiones,
      '</div>\n'
    )
  })

  paste0(
    '<div class="cronograma-row">\n',
    '  <div class="horario">', formatear_horario(inicio, fin), '</div>\n',
    '  <div class="bloque">\n',
    '    <div class="salas-paralelas" style="--n-salas: ', length(salas_bloque), ';">\n', paste(columnas, collapse = ""), '    </div>\n',
    if (!is.null(nota)) paste0('    <div class="bloque-nota">', nota, '</div>\n'),
    '  </div>\n',
    '</div>\n'
  )
}

# Une los bloques de sesiones que se interrumpen por un break. Si un bloque de salas
# va seguido de un único bloque general (por ejemplo, el coffee break) y después de un
# bloque donde todas las salas son continuaciones, los tres se convierten en un solo
# bloque: cada sesión toma el horario de término de su continuación y el break queda
# como nota dentro del bloque.
unir_bloques_con_break <- function(bloques) {
  resultado <- list()
  i <- 1

  while (i <= length(bloques)) {
    bloque <- bloques[[i]]
    se_puede_unir <- i + 2 <= length(bloques) &&
      !bloque$general[1] &&
      bloques[[i + 1]]$general[1] && nrow(bloques[[i + 1]]) == 1 &&
      !bloques[[i + 2]]$general[1] && all(es_continuacion(bloques[[i + 2]]))

    if (se_puede_unir) {
      pausa <- bloques[[i + 1]]
      continuacion <- bloques[[i + 2]]
      fin_continuacion <- continuacion$fin[match(bloque$id_sesion, continuacion$id_sesion)]

      bloque <- bloque |>
        dplyr::mutate(
          fin = dplyr::coalesce(fin_continuacion, fin),
          minuto_fin = hora_a_minutos(fin)
        )
      attr(bloque, "nota") <- paste0(
        htmltools::htmlEscape(pausa$sesion), ": ", formatear_horario(pausa$inicio, pausa$fin)
      )
      i <- i + 3
    } else {
      i <- i + 1
    }

    resultado <- c(resultado, list(bloque))
  }

  resultado
}

# Cronograma completo de un día. Las sesiones con sala que quedan entre dos bloques
# generales (breaks, keynotes, etc.) forman un bloque de sesiones paralelas.
generar_html_dia <- function(programa, dia_filtro, idioma = "es") {
  textos <- textos_programa[[idioma]]
  salas <- unique(stats::na.omit(programa$sesiones$sala))

  sesiones_dia <- programa$sesiones |>
    dplyr::filter(dia == dia_filtro) |>
    dplyr::mutate(
      minuto_inicio = hora_a_minutos(inicio),
      minuto_fin = hora_a_minutos(fin),
      general = is.na(sala) | sala == ""
    ) |>
    dplyr::arrange(minuto_inicio, !general) |>
    dplyr::mutate(bloque = cumsum(general | !dplyr::lag(!general, default = FALSE)))

  html_bloques <- sesiones_dia |>
    dplyr::group_split(bloque) |>
    unir_bloques_con_break() |>
    purrr::map_chr(function(sesiones_bloque) {
      if (sesiones_bloque$general[1]) {
        generar_div_general(sesiones_bloque, programa, textos)
      } else {
        generar_div_paralelas(sesiones_bloque, salas, programa, textos, nota = attr(sesiones_bloque, "nota"))
      }
    })

  paste0('<div class="cronograma-container">\n', paste(html_bloques, collapse = ""), '</div>')
}

# Imprime el programa de cada día (para usar en un chunk con `results: asis`).
# - Con `pestanas = TRUE` cada día es una pestaña: el chunk tiene que estar
#   dentro de un div `.panel-tabset`. Con `pestanas = FALSE` los días van uno
#   después del otro, cada uno con su título.
# - Con `detallado = FALSE` se muestran solo las sesiones, sin las charlas.
imprimir_programa <- function(idioma = "es", detallado = TRUE, pestanas = TRUE) {
  dias <- dias_programa[[idioma]]
  programa <- cargar_programa()

  if (!detallado) {
    programa$agrupacion <- programa$agrupacion[0, ]
  }

  purrr::iwalk(dias, function(titulo_dia, dia) {
    if (pestanas) {
      cat("\n\n##", titulo_dia, "\n\n")
    } else {
      cat("\n\n###", titulo_dia, "{.programa-dia}\n\n")
    }
    cat("```{=html}\n", generar_html_dia(programa, dia, idioma), "\n```\n")
  })
}
