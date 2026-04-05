# ---- Helpers: Google Maps Directions API (transit) ----

# API key lida de variável de ambiente (nunca hardcoded)
GOOGLE_MAPS_API_KEY <- Sys.getenv("GOOGLE_MAPS_API_KEY")

# Perfis de transit que usam Google ao invés de OSRM
GOOGLE_TRANSIT_MODES <- c("metro", "train", "tram")

# Mapeamento de perfil do app para transit_mode da Google API
GOOGLE_TRANSIT_MODE_MAP <- list(
  metro = "subway",
  train = "train",
  tram   = "tram"
)

# Verificar se a API key está configurada e funcional
check_google_api <- function() {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (is.null(key) || !nzchar(key)) return(FALSE)

  # Chamada mínima para validar a key
  url <- paste0(
    "https://maps.googleapis.com/maps/api/directions/json",
    "?origin=0,0&destination=0,0&mode=transit&key=", key
  )
  res <- tryCatch(httr::GET(url, httr::timeout(5)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 500) return(FALSE)

  # Se retornar 200 (mesmo sem rota), a key é válida
  # REQUEST_DENIED indica key inválida
  js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(js)) return(FALSE)
  if (identical(js$status, "REQUEST_DENIED")) return(FALSE)

  TRUE
}

# Decodificar encoded polyline do Google Maps
# Retorna matrix n x 2 (lon, lat)
decode_google_polyline <- function(encoded) {
  if (!nzchar(encoded)) return(matrix(numeric(0), ncol = 2))

  chars <- utf8ToInt(encoded)
  n <- length(chars)
  lat <- 0
  lng <- 0
  coords <- list()
  i <- 1


  while (i <= n) {
    # Decodificar latitude
    shift <- 0
    result <- 0
    repeat {
      b <- chars[i] - 63
      i <- i + 1
      result <- bitwOr(result, bitwAnd(b, 31) * (2^shift))
      shift <- shift + 5
      if (b < 32) break
    }
    dlat <- if (bitwAnd(result, 1) != 0) -bitwShiftR(result + 1, 1) else bitwShiftR(result, 1)
    lat <- lat + dlat

    # Decodificar longitude
    shift <- 0
    result <- 0
    repeat {
      b <- chars[i] - 63
      i <- i + 1
      result <- bitwOr(result, bitwAnd(b, 31) * (2^shift))
      shift <- shift + 5
      if (b < 32) break
    }
    dlng <- if (bitwAnd(result, 1) != 0) -bitwShiftR(result + 1, 1) else bitwShiftR(result, 1)
    lng <- lng + dlng

    coords[[length(coords) + 1]] <- c(lng / 1e5, lat / 1e5)
  }

  mat <- do.call(rbind, coords)
  colnames(mat) <- c("lon", "lat")
  mat
}

# Mapeamento: transit_mode da API -> vehicle.type esperados na resposta
# A API retorna vehicle.type em MAIÚSCULAS (RAIL, SUBWAY, TRAM, BUS, etc.)
GOOGLE_VEHICLE_TYPES_ACEITOS <- list(
  subway = c("SUBWAY", "METRO_RAIL"),
  train  = c("RAIL", "HEAVY_RAIL", "COMMUTER_TRAIN", "HIGH_SPEED_TRAIN",
             "LONG_DISTANCE_TRAIN"),
  tram   = c("TRAM", "LIGHT_RAIL")
)

# Extrair os vehicle.type de todos os steps TRANSIT de uma rota
extrair_vehicle_types <- function(route) {
  types <- character(0)
  for (leg in route$legs) {
    for (step in leg$steps) {
      if (identical(step$travel_mode, "TRANSIT")) {
        vt <- step$transit_details$line$vehicle$type
        if (!is.null(vt)) types <- c(types, vt)
      }
    }
  }
  types
}

# Calcular rota via Google Maps Directions API com mode=transit
# pontos: data.frame com colunas lat e lng (mínimo 2 linhas)
# transit_mode: "subway", "train" ou "tram"
# Retorna: list(coords = matrix(lon, lat), distance_m, duration_s) ou NULL
calcular_rota_google_transit <- function(pontos, transit_mode = c("subway", "train", "tram")) {
  transit_mode <- match.arg(transit_mode)

  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (is.null(key) || !nzchar(key)) {
    message("[Google API] GOOGLE_MAPS_API_KEY não configurada.")
    return(NULL)
  }

  if (nrow(pontos) < 2) {
    message("[Google API] Necessários pelo menos 2 pontos.")
    return(NULL)
  }

  origin <- paste0(pontos$lat[1], ",", pontos$lng[1])
  destination <- paste0(pontos$lat[nrow(pontos)], ",", pontos$lng[nrow(pontos)])

  params <- list(
    origin       = origin,
    destination  = destination,
    mode         = "transit",
    transit_mode = transit_mode,
    alternatives = "true",
    key          = key
  )

  # Waypoints intermediários (se houver mais de 2 pontos)
  if (nrow(pontos) > 2) {
    wps <- vapply(2:(nrow(pontos) - 1), function(i) {
      paste0(pontos$lat[i], ",", pontos$lng[i])
    }, character(1))
    params$waypoints <- paste(wps, collapse = "|")
  }

  url <- "https://maps.googleapis.com/maps/api/directions/json"
  res <- tryCatch(
    httr::GET(url, query = params, httr::timeout(30)),
    error = function(e) {
      message("[Google API] Erro na requisição: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(res) || res$status_code >= 400) {
    message("[Google API] Requisição falhou (status: ", res$status_code %||% "NULL", ")")
    return(NULL)
  }

  js <- tryCatch(
    httr::content(res, as = "parsed", encoding = "UTF-8"),
    error = function(e) {
      message("[Google API] Erro ao parsear resposta: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(js) || !identical(js$status, "OK")) {
    msg <- js$status %||% "desconhecido"
    detail <- js$error_message %||% ""
    message("[Google API] Status: ", msg, " ", detail)
    return(NULL)
  }

  if (length(js$routes) == 0) {
    message("[Google API] Nenhuma rota encontrada.")
    return(NULL)
  }

  # Filtrar rotas que realmente usem o modo de transporte solicitado
  tipos_aceitos <- GOOGLE_VEHICLE_TYPES_ACEITOS[[transit_mode]]
  route <- NULL

  for (r in js$routes) {
    vts <- extrair_vehicle_types(r)
    if (any(vts %in% tipos_aceitos)) {
      route <- r
      break
    }
  }

  if (is.null(route)) {
    modos_encontrados <- unique(unlist(lapply(js$routes, extrair_vehicle_types)))
    message(
      "[Google API] Nenhuma rota encontrada usando ", transit_mode,
      ". Modos encontrados: ", paste(modos_encontrados, collapse = ", ")
    )
    return(NULL)
  }

  # Extrair apenas os steps TRANSIT do modo solicitado (sem trechos a pé)
  coords_list <- list()
  total_distance <- 0
  total_duration <- 0

  for (leg in route$legs) {
    for (step in leg$steps) {
      if (!identical(step$travel_mode, "TRANSIT")) next
      vt <- step$transit_details$line$vehicle$type
      if (is.null(vt) || !(vt %in% tipos_aceitos)) next

      # Decodificar polyline do step individual
      enc <- step$polyline$points
      if (!is.null(enc) && nzchar(enc)) {
        step_coords <- decode_google_polyline(enc)
        if (nrow(step_coords) >= 2) {
          coords_list[[length(coords_list) + 1]] <- step_coords
        }
      }
      total_distance <- total_distance + (step$distance$value %||% 0)
      total_duration <- total_duration + (step$duration$value %||% 0)
    }
  }

  if (length(coords_list) == 0) {
    message("[Google API] Nenhum step TRANSIT do modo solicitado possui polyline.")
    return(NULL)
  }

  # Concatenar coordenadas de todos os steps de transit aceitos
  coords_mat <- do.call(rbind, coords_list)
  colnames(coords_mat) <- c("lon", "lat")

  if (nrow(coords_mat) < 2) {
    message("[Google API] Polyline do transit com menos de 2 pontos.")
    return(NULL)
  }

  list(
    coords     = coords_mat,
    distance_m = as.numeric(total_distance),
    duration_s = as.numeric(total_duration)
  )
}
