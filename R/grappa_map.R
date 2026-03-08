#' Create an interactive map of grappa locations
#'
#' Builds an interactive world map of valid grappa drinking locations
#' using \pkg{leaflet}. Only rows with valid coordinates are shown.
#'
#' The function:
#' - loads the data with \code{grappa_data()}
#' - validates it with \code{validate_grappa_data()}
#' - stops if validation errors are found
#' - maps only rows where \code{map_valid == TRUE}
#' - allows interactive filtering by player directly on the map
#' - saves the map as an HTML file
#' - opens the HTML file automatically in the default browser
#'
#' @param cluster Logical; if TRUE, marker clustering is enabled.
#' @param warn Logical; if TRUE, warnings from validation are printed.
#' @param file Optional character string. Path of the HTML file to create.
#'   If NULL, a temporary HTML file is created.
#' @param selfcontained Logical; if TRUE, saves a self-contained HTML widget.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{map}{The leaflet map widget.}
#'   \item{file}{The path to the generated HTML file.}
#' }
#' @export
grappa_map <- function(cluster = TRUE,
                       warn = TRUE,
                       file = NULL,
                       selfcontained = TRUE) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required but not installed.", call. = FALSE)
  }

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required but not installed.", call. = FALSE)
  }

  df <- grappa_data()
  res <- validate_grappa_data(df)

  if (!res$ok) {
    stop(
      paste0(
        "grappa_map() cannot be created because the dataset contains ",
        res$summary$n_errors,
        " validation error(s)."
      ),
      call. = FALSE
    )
  }

  if (warn && res$summary$n_warnings > 0) {
    warning(
      paste0(
        "Validation completed with ",
        res$summary$n_warnings,
        " warning(s). Only map-valid rows will be displayed."
      ),
      call. = FALSE
    )
  }

  map_df <- res$data[res$data$map_valid, , drop = FALSE]

  if (nrow(map_df) == 0) {
    stop("No valid coordinates available to build the map.", call. = FALSE)
  }

  map_df$player <- as.character(map_df$player)
  map_df$player[is.na(map_df$player) | !nzchar(map_df$player)] <- "Unknown"

  icon_path <- system.file(
    "extdata/icons/grappa_glass.png",
    package = "gRappa"
  )

  if (icon_path == "") {
    stop(
      "Cannot find icon file 'inst/extdata/icons/grappa_glass.png' in the package.",
      call. = FALSE
    )
  }

  grappa_icon <- leaflet::makeIcon(
    iconUrl = icon_path,
    iconWidth = 32,
    iconHeight = 32,
    iconAnchorX = 16,
    iconAnchorY = 32,
    popupAnchorX = 0,
    popupAnchorY = -28
  )

  players <- sort(unique(map_df$player))

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  for (p in players) {
    df_p <- map_df[map_df$player == p, , drop = FALSE]

    popup_text <- vapply(seq_len(nrow(df_p)), function(i) {
      row <- df_p[i, , drop = FALSE]

      first_line <- paste(stats::na.omit(c(row$street, row$street_number)), collapse = " ")
      if (!nzchar(first_line)) {
        first_line <- NA_character_
      }

      address_parts <- c(
        first_line,
        row$city,
        row$postal_code,
        row$region,
        row$country
      )
      address_parts <- address_parts[!is.na(address_parts) & nzchar(address_parts)]

      address <- if (length(address_parts) > 0) {
        paste(address_parts, collapse = ", ")
      } else {
        "-"
      }

      grappa_label <- ifelse(is.na(row$grappa_label), "-", row$grappa_label)
      place_name <- ifelse(is.na(row$place_name), "-", row$place_name)
      date <- ifelse(is.na(row$date), "-", as.character(row$date))
      note <- ifelse(is.na(row$note), "-", row$note)

      paste0(
        "<strong>Player:</strong> ", p, "<br/>",
        "<strong>Date:</strong> ", date, "<br/>",
        "<strong>Grappa:</strong> ", grappa_label, "<br/>",
        "<strong>Place:</strong> ", place_name, "<br/>",
        "<strong>Address:</strong> ", address, "<br/>",
        "<strong>Note:</strong> ", note
      )
    }, character(1))

    m <- m |>
      leaflet::addMarkers(
        data = df_p,
        lng = ~longitude,
        lat = ~latitude,
        popup = popup_text,
        icon = grappa_icon,
        group = p,
        clusterOptions = if (cluster) leaflet::markerClusterOptions() else NULL
      )
  }

  m <- m |>
    leaflet::addLayersControl(
      overlayGroups = players,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (is.null(file)) {
    file <- tempfile(pattern = "grappa_map_", fileext = ".html")
  } else {
    file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  }

  htmlwidgets::saveWidget(
    widget = m,
    file = file,
    selfcontained = selfcontained
  )

  utils::browseURL(file)

  invisible(list(
    map = m,
    file = file
  ))
}
