# ---- Helpers: OSRM ----

# Suaviza rota OSRM removendo pontos redundantes e aplicando simplificação
# coords: matriz n x 2 (lon, lat)
# tolerancia_m: distância mínima entre pontos consecutivos (metros)
# angulo_min: ângulo mínimo de mudança de direção para manter ponto (graus)
suavizar_rota_osrm <- function(coords, tolerancia_m = 5, angulo_min = 1) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Passo 1: Remove pontos duplicados ou muito próximos
  manter <- rep(TRUE, n)
  for (i in 2:n) {
    dist <- geosphere::distHaversine(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist < tolerancia_m) {
      manter[i] <- FALSE
    }
  }
  # Sempre mantém primeiro e último
  manter[1] <- TRUE
  manter[n] <- TRUE
  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Passo 2: Remove pontos em linha reta (sem mudança de direção significativa)
  manter <- rep(FALSE, n)
  manter[1] <- TRUE  # primeiro
  manter[n] <- TRUE  # último

  for (i in 2:(n - 1)) {
    # Bearing do segmento anterior
    b1 <- geosphere::bearing(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    # Bearing do segmento seguinte
    b2 <- geosphere::bearing(
      c(coords[i, 1], coords[i, 2]),
      c(coords[i + 1, 1], coords[i + 1, 2])
    )

    # Diferença angular (considerando wrap-around)
    diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)

    # Mantém se mudança de direção > angulo_min
    if (diff_ang > angulo_min) {
      manter[i] <- TRUE
    }
  }

  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 2) return(coords)

  # Passo 3: Garante espaçamento mínimo entre pontos mantidos
  # (evita clusters de pontos muito próximos após filtragem por ângulo)
  final <- matrix(coords[1, ], nrow = 1)
  colnames(final) <- colnames(coords)

  for (i in 2:(n - 1)) {
    dist_ultimo <- geosphere::distHaversine(
      c(final[nrow(final), 1], final[nrow(final), 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist_ultimo >= tolerancia_m) {
      final <- rbind(final, coords[i, ])
    }
  }

  # Sempre inclui o último ponto
  final <- rbind(final, coords[n, ])

  final
}

# Servidores OSRM locais (ajuste portas se necessário)
OSRM_SERVERS <- list(
  car  = "http://127.0.0.1:5000",
  foot = "http://127.0.0.1:5001",
  bike = "http://127.0.0.1:5002",
  bus  = "http://127.0.0.1:5003"
)

# Mapeamento de perfis do app para perfis OSRM
OSRM_PROFILES <- list(
  car  = "driving",
  foot = "foot",
  bike = "bicycle",
  bus  = "driving"  # perfil customizado baseado em driving
)

check_osrm_server <- function(base_url, perfil = "car") {
  # tenta uma request mínima, só pra ver se responde
  osrm_profile <- OSRM_PROFILES[[perfil]] %||% "driving"
  test_url <- paste0(base_url, "/route/v1/", osrm_profile, "/0,0;0,0")
  res <- tryCatch(
    httr::GET(test_url, httr::timeout(2)),
    error = function(e) NULL
  )
  !is.null(res)
}

calcular_rota_osrm <- function(pontos, perfil = c("car", "foot", "bike", "bus")) {
  perfil <- match.arg(perfil)
  base_url <- OSRM_SERVERS[[perfil]]
  osrm_profile <- OSRM_PROFILES[[perfil]] %||% "driving"
  if (is.null(base_url)) return(NULL)

  if (!check_osrm_server(base_url, perfil)) {
    return(NULL)
  }

  coords_str <- apply(pontos[, c("lng", "lat")], 1, paste, collapse = ",")
  url <- paste0(
    base_url, "/route/v1/", osrm_profile, "/",
    paste(coords_str, collapse = ";"),
    "?overview=full&geometries=geojson"
  )

  res <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 400) return(NULL)

  js <- httr::content(res, as = "parsed", encoding = "UTF-8")
  if (is.null(js$routes) || length(js$routes) == 0) return(NULL)

  route <- js$routes[[1]]
  coords <- route$geometry$coordinates

  # Converte lista de coordenadas para matriz numérica
  n_coords <- length(coords)
  coords_mat <- matrix(0, nrow = n_coords, ncol = 2)
  for (i in seq_len(n_coords)) {
    pt <- coords[[i]]
    if (is.list(pt)) pt <- unlist(pt)
    coords_mat[i, 1] <- as.numeric(pt[1])  # lon
    coords_mat[i, 2] <- as.numeric(pt[2])  # lat
  }
  colnames(coords_mat) <- c("lon", "lat")

  list(
    coords      = coords_mat,
    distance_m  = as.numeric(route$distance),
    duration_s  = as.numeric(route$duration)
  )
}

# Recalcula um segmento de rota entre dois pontos via OSRM
# Retorna: list(coords, distance_m)
recalcular_segmento <- function(from_lat, from_lng, to_lat, to_lng,
                                perfil = c("car", "foot", "bike", "bus")) {
  perfil <- match.arg(perfil)

  pts <- data.frame(
    lat = c(from_lat, to_lat),
    lng = c(from_lng, to_lng)
  )

  rota <- calcular_rota_osrm(pts, perfil = perfil)

  if (is.null(rota) || is.null(rota$coords)) {
    # Fallback: linha reta entre os pontos
    coords <- matrix(c(from_lng, to_lng, from_lat, to_lat), ncol = 2)
    colnames(coords) <- c("lon", "lat")
    distance_m <- geosphere::distHaversine(c(from_lng, from_lat), c(to_lng, to_lat))

    return(list(
      coords = coords,
      distance_m = distance_m
    ))
  }

  # Aplica suavizacao para reduzir pontos redundantes
  coords_smooth <- suavizar_rota_osrm(rota$coords, tolerancia_m = 5, angulo_min = 2)

  list(
    coords = coords_smooth,
    distance_m = rota$distance_m
  )
}

# Recalcula timestamps para uma rota editada mantendo velocidade media original
# coords: matriz n x 2 (lon, lat)
# start_time_utc: POSIXct do inicio da rota
# avg_speed_mps: velocidade media em metros por segundo
# Retorna: list(timestamps_utc, end_time_utc, total_distance_m, total_duration_s)
recalcular_tempos_rota <- function(coords, start_time_utc, avg_speed_mps) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 2) stop("Rota deve ter pelo menos 2 pontos")

  # Valida velocidade media (minimo 1 m/s = 3.6 km/h, maximo 55.5 m/s = 200 km/h)
  if (is.null(avg_speed_mps) || is.na(avg_speed_mps) || avg_speed_mps <= 0) {
    avg_speed_mps <- 8.33  # 30 km/h padrao
  }
  if (avg_speed_mps > 55.5) avg_speed_mps <- 55.5

  # Calcula distancia acumulada ao longo da rota
  dist_acum <- numeric(n)
  dist_acum[1] <- 0
  for (i in 2:n) {
    p1 <- c(coords[i - 1, 1], coords[i - 1, 2])
    p2 <- c(coords[i, 1], coords[i, 2])
    dist_acum[i] <- dist_acum[i - 1] + geosphere::distHaversine(p1, p2)
  }
  total_distance <- dist_acum[n]

  # Se distancia total for 0, assume distancia minima
  if (total_distance <= 0) total_distance <- 1

  # Calcula duracao baseado na velocidade media
  total_duration_s <- total_distance / avg_speed_mps

  # Duracao minima de 60 segundos
  if (total_duration_s < 60) total_duration_s <- 60

  # Distribui timestamps proporcionalmente a distancia
  timestamps <- start_time_utc + (dist_acum / total_distance) * total_duration_s
  end_time <- start_time_utc + total_duration_s

  list(
    timestamps_utc = timestamps,
    end_time_utc = end_time,
    total_distance_m = total_distance,
    total_duration_s = total_duration_s
  )
}

# Encontra o segmento mais proximo de um ponto clicado
# Retorna: indice do segmento ou NULL
find_closest_segment <- function(click_lat, click_lng, nodes, segments) {
  if (length(segments) == 0) return(NULL)

  # Cria lookup de nos por ID
  node_lookup <- stats::setNames(
    lapply(nodes, function(n) c(lng = n$lng, lat = n$lat)),
    vapply(nodes, `[[`, "", "id")
  )

  min_dist <- Inf
  closest_idx <- NULL

  for (i in seq_along(segments)) {
    seg <- segments[[i]]

    if (!is.null(seg$coords) && nrow(seg$coords) >= 2) {
      # Verifica distancia a cada sub-segmento da polyline
      for (j in 1:(nrow(seg$coords) - 1)) {
        dist <- point_to_segment_distance(
          click_lng, click_lat,
          seg$coords[j, 1], seg$coords[j, 2],
          seg$coords[j + 1, 1], seg$coords[j + 1, 2]
        )
        if (dist < min_dist) {
          min_dist <- dist
          closest_idx <- i
        }
      }
    } else {
      # Fallback: linha reta entre nos
      from <- node_lookup[[seg$from_node]]
      to <- node_lookup[[seg$to_node]]
      if (!is.null(from) && !is.null(to)) {
        dist <- point_to_segment_distance(
          click_lng, click_lat,
          from["lng"], from["lat"],
          to["lng"], to["lat"]
        )
        if (dist < min_dist) {
          min_dist <- dist
          closest_idx <- i
        }
      }
    }
  }

  closest_idx
}

# Distancia de um ponto a um segmento de linha (em metros)
point_to_segment_distance <- function(px, py, x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1

  if (dx == 0 && dy == 0) {
    # Segmento e um ponto
    return(geosphere::distHaversine(c(px, py), c(x1, y1)))
  }

  # Projeta ponto no segmento
  t <- max(0, min(1, ((px - x1) * dx + (py - y1) * dy) / (dx * dx + dy * dy)))

  proj_x <- x1 + t * dx
  proj_y <- y1 + t * dy

  geosphere::distHaversine(c(px, py), c(proj_x, proj_y))
}

# Extrai coordenadas de uma lista de samples (compatível LocoKit2)
extract_coords_from_samples <- function(samples) {
  n <- length(samples)
  coords <- matrix(0, nrow = n, ncol = 2)
  colnames(coords) <- c("lon", "lat")

  for (i in seq_len(n)) {
    coords[i, 1] <- sample_lon(samples[[i]])
    coords[i, 2] <- sample_lat(samples[[i]])
  }

  coords
}
