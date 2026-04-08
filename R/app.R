#' @import shiny
#' @import bslib
#' @import ggplot2
NULL
ui <- bslib::page_sidebar(
  title = HTML("<span style='color:#F17CB0; font-size:20px;'><b>Casse-tête - Slitherlink</b><br></span>"),

  sidebar = bslib::sidebar(
    selectInput(
      "difficulte",
      HTML("<span style='color:#F17CB0; font-size:20px;'><b>Niveau de difficulté</b><br></span>"),
      choices = c("Débutant","Intermédiaire","Expert")
    ),

    actionButton("nouvelle_grille", "Nouvelle grille", class = "btn-primary"),
    actionButton("reinitialiser", "Réinitialiser la grille", class = "btn-warning"),
    br(),
    textOutput("timer")
  ),
  bslib::accordion(
    open = FALSE,  # le panneau est fermé par défaut
    bslib::accordion_panel(
      HTML("<span style='color:#F17CB0; font-size:16px;'><b>Règle du Jeu :</b><br></span>"),
      HTML("<span style='color:#F17CB0; font-size:12px;'>
        Le Slither Link se joue sur une matrice rectangulaire formée de cases carrées. Chaque case peut soit être vide, soit contenir un chiffre entre 0 et 3 inclus. Les côtés des cases sont vides au début de la partie. L'objectif est de tracer des traits le long des côtés des cases, de sorte à respecter deux règles : l'ensemble des traits doit former une unique ligne continue, une case contenant un chiffre doit avoir exactement autant de côtés avec un trait tracé que le chiffre qu'elle contient.
        </span>")
    )
  ),
  bslib::card(
    HTML("<span style='color:#F17CB0; font-size:16px;'><b>Partie de Slitherlink</b><br></span>"),
    div(
      style = "display:flex; justify-content:center;",
      plotOutput("grid_plot", width = "400px", height = "400px", click = "plot_click",dblclick = "plot_dblclick")
    )
  )
)

server <- function(input, output, session) {

  n <- 5  # taille grille

  # Valeur réactive pour stocker la grille d'indices
  grille_indices <- reactiveVal()
  boucle_solution <- reactiveVal()
  #Stock les croix
  croix <- reactiveVal(data.frame(
    x = numeric(),
    y = numeric()
  ))
  # Stocke les segments du joueur
  segments_joueur <- reactiveVal(data.frame(
    x1=numeric(),
    y1=numeric(),
    x2=numeric(),
    y2=numeric()
  ))
  victoire <- reactiveVal(FALSE)
  timer_active <- reactiveVal(FALSE)
  start_time <- reactiveVal(NULL)
  elapsed <- reactiveVal(0)
  afficher_solution <- reactiveVal(FALSE)
  modal_affichee <- reactiveVal(FALSE)

  normaliser_segment <- function(x1, y1, x2, y2) {
    if (x1 < x2 || (x1 == x2 && y1 < y2)) {
      return(paste(x1, y1, x2, y2, sep = ","))
    } else {
      return(paste(x2, y2, x1, y1, sep = ","))
    }
  }

  verifier_victoire <- function(segments_joueur_df, solution_df) {
    if (is.null(segments_joueur_df) || nrow(segments_joueur_df) == 0 ||
        is.null(solution_df) || nrow(solution_df) == 0) {
      return(FALSE)
    }

    # Normaliser les segments du joueur
    joueur_set <- apply(segments_joueur_df, 1, function(row) {
      normaliser_segment(row[1], row[2], row[3], row[4])
    })

    # Normaliser les segments de la solution
    solution_set <- apply(solution_df, 1, function(row) {
      normaliser_segment(row[1], row[2], row[3], row[4])
    })

    # Comparer les ensembles (ordre et sens ignorés)
    return(setequal(joueur_set, solution_set))
  }

  # Générer une nouvelle grille quand on clique sur le bouton
  observeEvent(input$nouvelle_grille, {
    timer_active(FALSE)
    start_time(NULL)
    elapsed(0)
    afficher_solution(FALSE)
    modal_affichee(FALSE)
    min_seg <- switch(input$difficulte,
                      "Débutant" = 10,
                      "Intermédiaire" = 12,
                      "Expert" = 16)
    boucle <- generer_boucle_slitherlink(n, min_segments = min_seg)
    slitherlink <- convertir_en_indices(boucle)

    grille_indices(slitherlink$indices)
    #Enlève les croix, les segments et la solution de l'ancienne grille
    segments_joueur(data.frame(x1=numeric(),y1=numeric(),x2=numeric(),y2=numeric()))
    croix(data.frame(x=numeric(),y=numeric()))
    afficher_solution(FALSE)

    # Stocker les segments de la solution - version simple
    segments_solution_df <- do.call(rbind, lapply(boucle$segments, function(s) {
      data.frame(
        x1 = s[1] - 1,  # convertir 1->0 pour ggplot
        y1 = n - (s[2]-1),  # inverser y pour que 0 = bas
        x2 = s[3] - 1,
        y2 = n - (s[4]-1)
      )
    }))
    boucle_solution(segments_solution_df)
  })
  #Récupère la position exact du click
  observeEvent(input$plot_click, {

    x <- input$plot_click$x
    y <- input$plot_click$y

    # Arrondir vers le noeud le plus proche
    gx <- round(x)
    gy <- round(y)

    segs <- segments_joueur()
    croix_df <- croix()

    # déterminer si horizontal ou vertical
    if (abs(x - gx) > abs(y - gy)) {

      # segment horizontal
      x1 <- floor(x)
      x2 <- x1 + 1
      y1 <- gy
      y2 <- gy

    } else {

      # segment vertical
      y1 <- floor(y)
      y2 <- y1 + 1
      x1 <- gx
      x2 <- gx

    }

    new_seg <- data.frame(x1=x1,y1=y1,x2=x2,y2=y2)

    cx <- (x1 + x2)/2
    cy <- (y1 + y2)/2

    # supprimer croix si elle existe
    croix_df <- croix_df[!(
      abs(croix_df$x - cx) < 0.2 &
        abs(croix_df$y - cy) < 0.2
    ),]

    croix(croix_df)

    # vérifier si le segment existe déjà
    existe <- any(
      segs$x1 == x1 &
        segs$y1 == y1 &
        segs$x2 == x2 &
        segs$y2 == y2
    )
    if(!existe){
      segs <- rbind(segs,new_seg)
    }
    segments_joueur(segs)
    if (verifier_victoire(segments_joueur(), boucle_solution())) {
      if (!victoire()) {
        victoire(TRUE)
        showNotification("Bravo ! Vous avez gagné !", type = "message", duration = 10)
      }
    } else {
      victoire(FALSE)
    }
    if (!timer_active()) {
      timer_active(TRUE)
      start_time(Sys.time())
    }
  })
  #Récupére la position exact du double click
  observeEvent(input$plot_dblclick, {

    x <- input$plot_dblclick$x
    y <- input$plot_dblclick$y

    # Arrondir vers le noeud le plus proche
    gx <- round(x)
    gy <- round(y)

    segs <- segments_joueur()
    croix_df <- croix()

    # déterminer si horizontal ou vertical
    if (abs(x - gx) > abs(y - gy)) {

      # segment horizontal
      x1 <- floor(x)
      x2 <- x1 + 1
      y1 <- gy
      y2 <- gy

    } else {

      # segment vertical
      y1 <- floor(y)
      y2 <- y1 + 1
      x1 <- gx
      x2 <- gx
    }

    #Supprime le segment s'il existe

    segs <- segs[!(
      segs$x1 == x1 &
        segs$y1 == y1 &
        segs$x2 == x2 &
        segs$y2 == y2
    ),]

    segments_joueur(segs)

    cx <- (x1 + x2)/2
    cy <- (y1 + y2)/2
    # ajouter croix si elle n'existe pas
    existe <- any(
      abs(croix_df$x - cx) < 0.2 &
        abs(croix_df$y - cy) < 0.2
    )

    if(!existe){
      croix_df <- rbind(croix_df, data.frame(x=cx,y=cy))
    }

    croix(croix_df)
    if (verifier_victoire(segments_joueur(), boucle_solution())) {
      if (!victoire()) {
        victoire(TRUE)
        showNotification("Bravo ! Vous avez gagné !", type = "message", duration = 20)
      }
    } else {
      victoire(FALSE)
    }
    if (!timer_active()) {
      timer_active(TRUE)
      start_time(Sys.time())
    }
  })

  observeEvent(input$reinitialiser, {
    segments_joueur(data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric()))
    croix(data.frame(x=numeric(), y=numeric()))
    afficher_solution(FALSE)
  })
  #Affiche la proposition de solution
  observe({
    req(timer_active(), elapsed() >= 300, !victoire(), !afficher_solution(), !modal_affichee())
    modal_affichee(TRUE)
    showModal(modalDialog(
      title = "5 minutes écoulées",
      "Souhaitez-vous voir la solution ?",
      footer = tagList(
        actionButton("voir_solution", "Oui, afficher la solution"),
        modalButton("Non, continuer")
      ),
      easyClose = FALSE
    ))
  })
  observeEvent(input$voir_solution, {
    afficher_solution(TRUE)
    removeModal()
  })

  # Générer une première grille au démarrage
  observe({
    min_seg <- 8
    boucle <- generer_boucle_slitherlink(n, min_segments = min_seg)
    slitherlink <- convertir_en_indices(boucle)

    grille_indices(slitherlink$indices)

    # Stocker les segments de la solution - version simple
    segments_solution_df <- do.call(rbind, lapply(boucle$segments, function(s) {
      data.frame(
        x1 = s[1] - 1,  # convertir 1->0 pour ggplot
        y1 = n - (s[2]-1),  # inverser y pour que 0 = bas
        x2 = s[3] - 1,
        y2 = n - (s[4]-1)
      )
    }))
    boucle_solution(segments_solution_df)
  })

  observe({
    req(timer_active())
    invalidateLater(1000, session)
    isolate({
      elapsed(round(as.numeric(difftime(Sys.time(), start_time(), units = "secs"))))
    })
  })

  output$timer <- renderText({
    e <- elapsed()
    minutes <- e %/% 60
    secondes <- e %% 60
    paste("Temps :", sprintf("%02d:%02d", minutes, secondes))
  })
  output$grid_plot <- renderPlot({

    indices <- grille_indices()
    solution <- boucle_solution()

    if (is.null(indices)) {
      return(NULL)
    }
    #Créer un data frame pour les points (noeuds)
    points_df <- expand.grid(
      x = 0:n,
      y = 0:n
    )

    indices_df <- expand.grid(
      i = 1:n,
      j = 1:n
    )
    indices_df$valeur <- indices[cbind(indices_df$i, indices_df$j)]


    # Créer le ggplot
    p <- ggplot()
    if (afficher_solution() && !is.null(solution) && nrow(solution) > 0) {
      p <- p + geom_segment(
        data = solution,
        aes(x = x1, y = y1, xend = x2, yend = y2),
        linewidth = 2,
        color = "grey",
        alpha = 0.8
      )
    }
    p <- p +
      # Dessiner les segments du joueur
      geom_segment(
        data = segments_joueur(),
        aes(x = x1, y = y1, xend = x2, yend = y2),
        linewidth = 2,
        color = if (victoire())"#7ED957" else "#fba29d"
      ) +

      # Ajouter les points (noeuds)
      geom_point(data = points_df,
                 aes(x = x, y = y),
                 size = 4, color = "pink") +

      # ajoute les croix
      geom_text(
        data = croix(),
        aes(x = x, y = y),
        label = "X",
        color = "red",
        size = 6,
        fontface = "bold"
      ) +

      # Ajouter les indices
      geom_text(data = indices_df, aes(x = j - 0.5, y = n - i + 0.5,
                                       label = ifelse(is.na(valeur), "", valeur)),
                size = 8, color = "purple", fontface = "bold") +

      # Ajuster les limites et l'apparence
      scale_x_continuous(limits = c(-0.2, n + 0.2), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-0.2, n + 0.2), expand = c(0, 0)) +

      # Thème minimal
      theme_void() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        aspect.ratio = 1
      ) +

      # Coordonnées fixes
      coord_fixed()

    return(p)
  })

}
lancer_slitherlink <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

