###############################################################################
# Multiplex Network Generator
# Generates edge lists for multiplex interstate networks from:
#   Layer 1: Shared IGO membership (peacesciencer / CoW)
#   Layer 2: Bilateral trade (peacesciencer / CoW)
#   Layer 3: Alliances by type (peacesciencer / ATOP)
#   Layer 4: Contiguity (peacesciencer / CoW)
#   Layer 5: Preferential Trade Agreements (DESTA)
#   Layer 6: Defense Cooperation Agreements (DCAD / Kinne 2020)
#
# Author:  Jared Edgerton
# Usage:   Source this file, then call generate_multiplex_network()
###############################################################################

# --- Package dependencies ---------------------------------------------------

required_packages <- c(
  "peacesciencer",
  "tidyverse",
  "haven",       # for reading .dta files (DESTA, DCAD)
  "readxl",      # for reading .xlsx files if needed
  "countrycode"  # for standardizing country codes across datasets
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(peacesciencer)
library(tidyverse)
library(haven)
library(countrycode)

###############################################################################
# LAYER 1 — Shared IGO Membership
###############################################################################

#' Build the shared-IGO-membership edge list.
#'
#' Uses peacesciencer's add_igos(), which returns `dyadigos`: the count of
#' IGOs in which both states hold full membership in a given year.
#'
#' @param base_dy A dyad-year tibble created by create_dyadyears().
#' @return A tibble with columns: ccode1, ccode2, year, dyadigos.
build_igo_edgelist <- function(base_dy) {
  message("Building IGO shared-membership layer...")
  base_dy %>%
    add_igos() %>%
    select(ccode1, ccode2, year, dyadigos) %>%
    filter(!is.na(dyadigos), dyadigos > 0)
}

###############################################################################
# LAYER 2 — Bilateral Trade
###############################################################################

#' Build the trade edge list.
#'
#' Uses peacesciencer's add_cow_trade(), which returns:
#'   flow1       — imports of ccode1 from ccode2 (millions current USD)
#'   flow2       — imports of ccode2 from ccode1 (millions current USD)
#'   smoothflow1 — smoothed version of flow1
#'   smoothflow2 — smoothed version of flow2
#'
#' NOTE: The dyadic trade data must be downloaded first via
#'   peacesciencer::download_extdata()
#'
#' @param base_dy A dyad-year tibble created by create_dyadyears().
#' @return A tibble with columns: ccode1, ccode2, year,
#'         flow1, flow2, smoothflow1, smoothflow2, total_trade.
build_trade_edgelist <- function(base_dy) {
  message("Building bilateral trade layer...")
  base_dy %>%
    add_cow_trade() %>%
    mutate(total_trade = flow1 + flow2) %>%
    select(ccode1, ccode2, year, flow1, flow2,
           smoothflow1, smoothflow2, total_trade) %>%
    filter(!is.na(total_trade), total_trade > 0)
}

###############################################################################
# LAYER 3 — Alliances (ATOP, by type)
###############################################################################

#' Build alliance edge lists broken down by obligation type.
#'
#' Uses peacesciencer's add_atop_alliance(), which returns binary indicators:
#'   atop_defense  — defense pact
#'   atop_offense  — offense pact
#'   atop_neutral  — neutrality pact
#'   atop_nonagg   — non-aggression pact
#'   atop_consul   — consultation pact
#'
#' Obligations are NOT mutually exclusive; a single treaty can carry several.
#'
#' @param base_dy A dyad-year tibble created by create_dyadyears().
#' @return A named list of tibbles, one per alliance type, plus an "any"
#'         element for any alliance of any kind.
build_alliance_edgelist <- function(base_dy) {
  message("Building alliance layers (ATOP)...")
  alliance_df <- base_dy %>%
    add_atop_alliance() %>%
    select(ccode1, ccode2, year,
           atop_alliance, atop_defense, atop_offense,
           atop_neutral, atop_nonagg, atop_consul)

  alliance_types <- c("atop_defense", "atop_offense", "atop_neutral",
                       "atop_nonagg", "atop_consul")

  out <- list(
    any_alliance = alliance_df %>% filter(atop_alliance == 1)
  )

  for (atype in alliance_types) {
    out[[atype]] <- alliance_df %>% filter(.data[[atype]] == 1)
  }

  out
}

###############################################################################
# LAYER 4 — Contiguity
###############################################################################

#' Build the contiguity edge list.
#'
#' Uses peacesciencer's add_contiguity(), which returns `conttype`:
#'   1 = land contiguity
#'   2 = separated by <= 12 mi water
#'   3 = separated by <= 24 mi water
#'   4 = separated by <= 150 mi water
#'   5 = separated by <= 400 mi water
#'   0 = not contiguous
#'
#' @param base_dy A dyad-year tibble created by create_dyadyears().
#' @return A tibble with columns: ccode1, ccode2, year, conttype.
build_contiguity_edgelist <- function(base_dy) {
  message("Building contiguity layer...")
  base_dy %>%
    add_contiguity() %>%
    select(ccode1, ccode2, year, conttype) %>%
    filter(conttype > 0)
}

###############################################################################
# LAYER 5 — Preferential Trade Agreements (DESTA)
###############################################################################

#' Build the PTA edge list from the DESTA dataset.
#'
#' DESTA (Design of Trade Agreements) provides comprehensive data on PTAs
#' signed since 1945. The dyadic version has country-pair-year observations.
#'
#' Download: https://www.designoftradeagreements.org/downloads/
#'
#' The DESTA dyadic dataset typically includes:
#'   - country1, country2 (ISO3 or country names)
#'   - year (entry into force or signature year)
#'   - base_treaty (treaty identifier)
#'   - typememb: 1 = PTA, 2 = FTA, 3 = CU, 4 = EIA, 5 = PSA, 6 = Framework
#'
#' The function expects the DESTA *dyadic* dataset. If you have the list
#' dataset, you will need to expand it to dyad-year observations first.
#'
#' @param desta_path Path to the DESTA dyadic dataset (.dta, .csv, or .xlsx).
#' @param year_range Optional numeric vector c(start, end) to subset years.
#' @return A tibble with columns: ccode1, ccode2, year, base_treaty, typememb,
#'         plus the original DESTA country identifiers.
build_pta_edgelist <- function(desta_path, year_range = NULL) {
  message("Building PTA layer from DESTA...")

  ext <- tools::file_ext(desta_path)
  desta <- switch(ext,
    "dta" = haven::read_dta(desta_path),
    "csv" = readr::read_csv(desta_path, show_col_types = FALSE),
    "xlsx" = readxl::read_excel(desta_path),
    stop("Unsupported file format: ", ext)
  )

  # --- Identify country columns --------------------------------------------
  # DESTA dyadic data may use iso1/iso2, country1/country2, or ccode1/ccode2.
  # We attempt to detect the naming convention and harmonize to COW codes.
  desta <- desta %>% rename_with(tolower)

  # Detect and standardize country identifiers
  if (all(c("iso1", "iso2") %in% names(desta))) {
    desta <- desta %>%
      mutate(
        ccode1 = countrycode(iso1, "iso3c", "cown", warn = FALSE),
        ccode2 = countrycode(iso2, "iso3c", "cown", warn = FALSE)
      )
  } else if (all(c("country1", "country2") %in% names(desta))) {
    desta <- desta %>%
      mutate(
        ccode1 = countrycode(country1, "country.name", "cown", warn = FALSE),
        ccode2 = countrycode(country2, "country.name", "cown", warn = FALSE)
      )
  } else if (!all(c("ccode1", "ccode2") %in% names(desta))) {
    warning(
      "Could not detect country identifiers in DESTA data. ",
      "Expected iso1/iso2, country1/country2, or ccode1/ccode2. ",
      "Returning raw data — you will need to add COW codes manually."
    )
    if (!is.null(year_range)) {
      desta <- desta %>% filter(year >= year_range[1], year <= year_range[2])
    }
    return(desta)
  }

  # Ensure undirected ordering: ccode1 < ccode2
  desta <- desta %>%
    mutate(
      lo = pmin(ccode1, ccode2, na.rm = TRUE),
      hi = pmax(ccode1, ccode2, na.rm = TRUE),
      ccode1 = lo,
      ccode2 = hi
    ) %>%
    select(-lo, -hi)

  if (!is.null(year_range)) {
    desta <- desta %>% filter(year >= year_range[1], year <= year_range[2])
  }

  desta <- desta %>%
    filter(!is.na(ccode1), !is.na(ccode2)) %>%
    distinct(ccode1, ccode2, year, .keep_all = TRUE)

  desta
}

###############################################################################
# LAYER 6 — Defense Cooperation Agreements (DCAD)
###############################################################################

#' Build the DCA edge list from the DCAD dataset (Kinne 2020).
#'
#' The DCAD covers all independent countries for 1980-2010 and includes
#' 1,872 unique bilateral defense cooperation agreements.
#'
#' Download: https://www.brandonkinne.com/dcad
#'           or https://correlatesofwar.org/data-sets/defense-cooperation-agreement-dataset/
#'
#' The DCAD dyadic file contains binary indicators for whether a country-pair
#' has an active DCA in a given year. Six indicator variants exist:
#'   dcaGeneralV1 / dcaGeneralV2  — general agreements (strict / broad)
#'   dcaSectorV1  / dcaSectorV2   — sector agreements  (strict / broad)
#'   dcaAnyV1     / dcaAnyV2      — any agreement      (strict / broad)
#'
#' @param dcad_path Path to the DCAD dataset (.dta, .csv, or .xlsx).
#'        This should be the dyad-year file, not the treaty-level file.
#' @param year_range Optional numeric vector c(start, end) to subset years.
#' @param dca_indicator Which DCAD indicator to use for filtering active DCAs.
#'        Default "dcaAnyV1" (any agreement, high confidence). Set to NULL
#'        to return all rows without filtering.
#' @return A tibble with columns: ccode1, ccode2, year, plus DCAD indicators.
build_dca_edgelist <- function(dcad_path, year_range = NULL,
                                dca_indicator = "dcaAnyV1") {
  message("Building DCA layer from DCAD (Kinne 2020)...")

  ext <- tools::file_ext(dcad_path)
  dcad <- switch(ext,
    "dta" = haven::read_dta(dcad_path),
    "csv" = readr::read_csv(dcad_path, show_col_types = FALSE),
    "xlsx" = readxl::read_excel(dcad_path),
    stop("Unsupported file format: ", ext)
  )

  dcad <- dcad %>% rename_with(tolower)

  # DCAD dyadic data typically uses ccode1/ccode2 (COW codes) already.
  # If not, attempt conversion.
  if (!all(c("ccode1", "ccode2") %in% names(dcad))) {
    # Try statea/stateb or other naming conventions
    if (all(c("statea", "stateb") %in% names(dcad))) {
      dcad <- dcad %>% rename(ccode1 = statea, ccode2 = stateb)
    } else if (all(c("cow1", "cow2") %in% names(dcad))) {
      dcad <- dcad %>% rename(ccode1 = cow1, ccode2 = cow2)
    } else {
      warning(
        "Could not detect COW code columns in DCAD data. ",
        "Expected ccode1/ccode2, statea/stateb, or cow1/cow2. ",
        "Returning raw data — you will need to map identifiers manually."
      )
      if (!is.null(year_range)) {
        dcad <- dcad %>% filter(year >= year_range[1], year <= year_range[2])
      }
      return(dcad)
    }
  }

  # Ensure undirected ordering
  dcad <- dcad %>%
    mutate(
      lo = pmin(ccode1, ccode2, na.rm = TRUE),
      hi = pmax(ccode1, ccode2, na.rm = TRUE),
      ccode1 = lo,
      ccode2 = hi
    ) %>%
    select(-lo, -hi)

  if (!is.null(year_range)) {
    dcad <- dcad %>% filter(year >= year_range[1], year <= year_range[2])
  }

  # Filter to active DCAs using the specified indicator
  if (!is.null(dca_indicator)) {
    # The DCAD column names are case-sensitive in the original data;
    # we lowered them above, so match case-insensitively
    indicator_lc <- tolower(dca_indicator)
    if (indicator_lc %in% names(dcad)) {
      dcad <- dcad %>% filter(.data[[indicator_lc]] == 1)
    } else {
      # Fallback: detect any column starting with "dca"
      dca_cols <- names(dcad)[str_detect(names(dcad), "^dca")]
      if (length(dca_cols) > 0) {
        message(sprintf("  Indicator '%s' not found. Using '%s' instead.",
                        dca_indicator, dca_cols[1]))
        dcad <- dcad %>% filter(.data[[dca_cols[1]]] == 1)
      } else {
        message("  No DCA indicator column found. Returning all rows.")
      }
    }
  }

  dcad <- dcad %>%
    filter(!is.na(ccode1), !is.na(ccode2)) %>%
    distinct(ccode1, ccode2, year, .keep_all = TRUE)

  dcad
}

###############################################################################
# MAIN GENERATOR — Assemble the Multiplex Network
###############################################################################

#' Generate a multiplex interstate network.
#'
#' Builds edge lists for up to six relational layers and returns them in a
#' single named list. The peacesciencer layers (IGO, trade, alliance,
#' contiguity) are always generated. The PTA and DCA layers are generated
#' only when file paths are supplied.
#'
#' @param year_range Numeric vector c(start, end). Default c(1980, 2010).
#' @param directed   Logical. If TRUE, returns directed dyad-years where
#'                   (A, B) != (B, A). Default FALSE (undirected).
#' @param system     Character. "cow" (default) or "gw" for state system.
#' @param desta_path Optional. Path to the DESTA dyadic dataset for PTA layer.
#' @param dcad_path  Optional. Path to the DCAD dyad-year dataset for DCA layer.
#'
#' @return A named list with elements:
#'   \item{igo}{Shared IGO membership edge list}
#'   \item{trade}{Bilateral trade edge list}
#'   \item{alliance}{Named list of alliance edge lists by type}
#'   \item{contiguity}{Contiguity edge list}
#'   \item{pta}{PTA edge list (NULL if desta_path not provided)}
#'   \item{dca}{DCA edge list (NULL if dcad_path not provided)}
#'   \item{meta}{List of metadata: year_range, directed, system, timestamp}
#'
#' @examples
#' # Minimal run with just peacesciencer layers:
#' mplex <- generate_multiplex_network(year_range = c(1990, 2000))
#'
#' # With external PTA and DCA data:
#' mplex <- generate_multiplex_network(
#'   year_range = c(1980, 2010),
#'   desta_path = "data/desta_dyadic.dta",
#'   dcad_path  = "data/dcad_dyadic.csv"
#' )
#'
#' # Access individual layers:
#' mplex$igo
#' mplex$trade
#' mplex$alliance$atop_defense
#' mplex$contiguity
#' mplex$pta
#' mplex$dca
generate_multiplex_network <- function(
    year_range = c(1980, 2010),
    directed   = FALSE,
    system     = "cow",
    desta_path = NULL,
    dcad_path  = NULL
) {
  message("=== Multiplex Network Generator ===")
  message(sprintf("Year range: %d-%d | Directed: %s | System: %s",
                  year_range[1], year_range[2], directed, system))

  # --- Step 0: Ensure external peacesciencer data is available -------------
  # The CoW dyadic trade data is too large for CRAN and lives remotely.
  # download_extdata() is idempotent — it skips files that already exist.
  message("\nEnsuring peacesciencer external data is available...")
  peacesciencer::download_extdata()

  # --- Step 1: Build the base dyad-year scaffold --------------------------
  message("\nCreating base dyad-year data...")
  base_dy <- create_dyadyears(
    system      = system,
    directed    = directed,
    subset_years = seq(year_range[1], year_range[2])
  )
  message(sprintf("  Base scaffold: %s dyad-years", format(nrow(base_dy), big.mark = ",")))

  # --- Step 2: peacesciencer layers ----------------------------------------
  igo_el         <- build_igo_edgelist(base_dy)
  trade_el       <- build_trade_edgelist(base_dy)
  alliance_el    <- build_alliance_edgelist(base_dy)
  contiguity_el  <- build_contiguity_edgelist(base_dy)

  # --- Step 3: External layers (PTA and DCA) --------------------------------
  pta_el <- NULL
  if (!is.null(desta_path)) {
    pta_el <- build_pta_edgelist(desta_path, year_range)
  } else {
    message("\nSkipping PTA layer — no desta_path provided.")
    message("  Download DESTA: https://www.designoftradeagreements.org/downloads/")
  }

  dca_el <- NULL
  if (!is.null(dcad_path)) {
    dca_el <- build_dca_edgelist(dcad_path, year_range)
  } else {
    message("\nSkipping DCA layer — no dcad_path provided.")
    message("  Download DCAD: https://www.brandonkinne.com/dcad")
    message("  Or from CoW:   https://correlatesofwar.org/data-sets/defense-cooperation-agreement-dataset/")
  }

  # --- Step 4: Assemble and report -----------------------------------------
  multiplex <- list(
    igo        = igo_el,
    trade      = trade_el,
    alliance   = alliance_el,
    contiguity = contiguity_el,
    pta        = pta_el,
    dca        = dca_el,
    meta       = list(
      year_range = year_range,
      directed   = directed,
      system     = system,
      generated  = Sys.time()
    )
  )

  message("\n=== Multiplex Network Summary ===")
  message(sprintf("  IGO edges:           %s", format(nrow(igo_el), big.mark = ",")))
  message(sprintf("  Trade edges:         %s", format(nrow(trade_el), big.mark = ",")))
  message(sprintf("  Alliance (any):      %s", format(nrow(alliance_el$any_alliance), big.mark = ",")))
  message(sprintf("    - Defense:         %s", format(nrow(alliance_el$atop_defense), big.mark = ",")))
  message(sprintf("    - Offense:         %s", format(nrow(alliance_el$atop_offense), big.mark = ",")))
  message(sprintf("    - Neutrality:      %s", format(nrow(alliance_el$atop_neutral), big.mark = ",")))
  message(sprintf("    - Non-aggression:  %s", format(nrow(alliance_el$atop_nonagg), big.mark = ",")))
  message(sprintf("    - Consultation:    %s", format(nrow(alliance_el$atop_consul), big.mark = ",")))
  message(sprintf("  Contiguity edges:    %s", format(nrow(contiguity_el), big.mark = ",")))
  if (!is.null(pta_el)) {
    message(sprintf("  PTA edges:           %s", format(nrow(pta_el), big.mark = ",")))
  }
  if (!is.null(dca_el)) {
    message(sprintf("  DCA edges:           %s", format(nrow(dca_el), big.mark = ",")))
  }
  message("=================================")

  multiplex
}

###############################################################################
# UTILITY — Convert to long-format stacked edge list
###############################################################################

#' Stack all layers into a single long-format edge list.
#'
#' Useful for software that expects a single edge list with a "layer" column
#' (e.g., muxViz, multinet, or custom GNN pipelines).
#'
#' @param mplex A multiplex network list from generate_multiplex_network().
#' @return A tibble with columns: ccode1, ccode2, year, layer, weight.
stack_multiplex <- function(mplex) {
  layers <- list()

  # IGO: weight = count of shared memberships
  if (!is.null(mplex$igo) && nrow(mplex$igo) > 0) {
    layers$igo <- mplex$igo %>%
      transmute(ccode1, ccode2, year, layer = "igo", weight = as.numeric(dyadigos))
  }

  # Trade: weight = total bilateral trade
  if (!is.null(mplex$trade) && nrow(mplex$trade) > 0) {
    layers$trade <- mplex$trade %>%
      transmute(ccode1, ccode2, year, layer = "trade", weight = total_trade)
  }

  # Alliances: one row per alliance type active in the dyad-year
  alliance_types <- c("atop_defense", "atop_offense", "atop_neutral",
                       "atop_nonagg", "atop_consul")
  for (atype in alliance_types) {
    if (!is.null(mplex$alliance[[atype]]) && nrow(mplex$alliance[[atype]]) > 0) {
      layers[[atype]] <- mplex$alliance[[atype]] %>%
        transmute(ccode1, ccode2, year, layer = atype, weight = 1)
    }
  }

  # Contiguity: weight = inverted conttype so land=5, 12mi=4, etc.
  if (!is.null(mplex$contiguity) && nrow(mplex$contiguity) > 0) {
    layers$contiguity <- mplex$contiguity %>%
      transmute(ccode1, ccode2, year, layer = "contiguity",
                weight = 6 - conttype)
  }

  # PTA: binary
  if (!is.null(mplex$pta) && nrow(mplex$pta) > 0) {
    layers$pta <- mplex$pta %>%
      transmute(ccode1, ccode2, year, layer = "pta", weight = 1)
  }

  # DCA: binary
  if (!is.null(mplex$dca) && nrow(mplex$dca) > 0) {
    layers$dca <- mplex$dca %>%
      transmute(ccode1, ccode2, year, layer = "dca", weight = 1)
  }

  bind_rows(layers)
}

###############################################################################
# UTILITY — Export layers as separate CSVs
###############################################################################

#' Write each layer to its own CSV file inside a directory.
#'
#' @param mplex  A multiplex network list from generate_multiplex_network().
#' @param outdir Path to the output directory (created if it doesn't exist).
#' @param prefix Optional filename prefix (default "multiplex").
export_multiplex_csv <- function(mplex, outdir = "multiplex_output",
                                  prefix = "multiplex") {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  write_layer <- function(df, name) {
    if (!is.null(df) && nrow(df) > 0) {
      path <- file.path(outdir, sprintf("%s_%s.csv", prefix, name))
      readr::write_csv(df, path)
      message(sprintf("  Wrote %s", path))
    }
  }

  write_layer(mplex$igo, "igo")
  write_layer(mplex$trade, "trade")
  write_layer(mplex$contiguity, "contiguity")

  # Alliance sub-layers
  for (nm in names(mplex$alliance)) {
    write_layer(mplex$alliance[[nm]], nm)
  }

  write_layer(mplex$pta, "pta")
  write_layer(mplex$dca, "dca")

  # Also write the stacked version
  stacked <- stack_multiplex(mplex)
  path <- file.path(outdir, sprintf("%s_stacked.csv", prefix))
  readr::write_csv(stacked, path)
  message(sprintf("  Wrote %s (stacked, %s rows)",
                  path, format(nrow(stacked), big.mark = ",")))
}

###############################################################################
# QUICK-START EXAMPLE
###############################################################################

if (FALSE) {
  # --- Option A: peacesciencer layers only ----------------------------------
  mplex <- generate_multiplex_network(
    year_range = c(1990, 2000),
    directed   = FALSE,
    system     = "cow"
  )

  # --- Option B: Full multiplex with PTA and DCA ----------------------------
  # 1. Download DESTA dyadic data from:
  #    https://www.designoftradeagreements.org/downloads/
  # 2. Download DCAD dyad-year data from:
  #    https://www.brandonkinne.com/dcad
  #    or https://correlatesofwar.org/data-sets/defense-cooperation-agreement-dataset/
  mplex <- generate_multiplex_network(
    year_range = c(1980, 2010),
    directed   = FALSE,
    desta_path = "path/to/desta_dyadic.dta",
    dcad_path  = "path/to/dcad_dyadic.dta"
  )

  # --- Inspect layers -------------------------------------------------------
  mplex$igo
  mplex$trade
  mplex$alliance$atop_defense
  mplex$alliance$atop_offense
  mplex$contiguity
  mplex$pta
  mplex$dca

  # --- Stack into one long edge list ----------------------------------------
  stacked <- stack_multiplex(mplex)
  stacked

  # --- Export to CSV --------------------------------------------------------
  export_multiplex_csv(mplex, outdir = "output/multiplex")
}
