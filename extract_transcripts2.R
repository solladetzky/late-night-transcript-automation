# ============================================================================
# Archive.org TV News Transcript Extractor (R) — v3 Batch Mode
# ============================================================================
# Extracts closed-caption transcripts from archive.org TV archive items.
# Processes in batches so you can run 100 at a time on your schedule.
#
# Usage (Terminal):
#   cd ~/Desktop/Research
#
#   # First 100 episodes
#   Rscript extract_transcripts.R "Jimmy Kimmel Live! - jk_episodes.csv" jimmy_kimmel_transcripts 1 100
#
#   # Next 100
#   Rscript extract_transcripts.R "Jimmy Kimmel Live! - jk_episodes.csv" jimmy_kimmel_transcripts 101 200
#
#   # Next 100
#   Rscript extract_transcripts.R "Jimmy Kimmel Live! - jk_episodes.csv" jimmy_kimmel_transcripts 201 300
#
#   # ... and so on. Or do the rest all at once:
#   Rscript extract_transcripts.R "Jimmy Kimmel Live! - jk_episodes.csv" jimmy_kimmel_transcripts 301 9999
#
#   # Already-downloaded files are always skipped, so you can also just re-run
#   # the full range and it will only grab what's missing:
#   Rscript extract_transcripts.R "Jimmy Kimmel Live! - jk_episodes.csv" jimmy_kimmel_transcripts 1 9999
#
# Requirements:
#   install.packages(c("httr2", "rvest", "readr", "stringr"))
# ============================================================================

library(httr2)
library(rvest)
library(readr)
library(stringr)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
RATE_LIMIT_SECONDS <- 1.5
MAX_RETRIES        <- 3
TIMEOUT_SECONDS    <- 45

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

extract_identifier <- function(url) {
  if (is.na(url) || trimws(url) == "") return(NA_character_)
  match <- str_match(url, "/details/([^/]+)")
  if (is.na(match[1, 2])) return(NA_character_)
  return(match[1, 2])
}


fetch_with_retry <- function(url, retries = MAX_RETRIES) {
  for (attempt in seq_len(retries)) {
    Sys.sleep(RATE_LIMIT_SECONDS)
    tryCatch({
      resp <- request(url) |>
        req_timeout(TIMEOUT_SECONDS) |>
        req_headers("User-Agent" = "Mozilla/5.0 (research transcript extractor)") |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()

      status <- resp_status(resp)
      if (status == 200) return(resp)
      if (status == 429) {
        wait <- 2^(attempt + 2)
        message(sprintf("    Rate limited. Waiting %ds...", wait))
        Sys.sleep(wait)
      } else if (status == 404) {
        message("    404 Not Found")
        return(NULL)
      } else {
        message(sprintf("    HTTP %d", status))
      }
    }, error = function(e) {
      message(sprintf("    Request error (attempt %d/%d): %s", attempt, retries, e$message))
      Sys.sleep(2^attempt)
    })
  }
  return(NULL)
}


parse_srt <- function(text) {
  lines <- str_split(text, "\n")[[1]]
  lines <- trimws(lines)
  keep <- vapply(lines, function(line) {
    if (line == "") return(FALSE)
    if (str_detect(line, "^\\d+$")) return(FALSE)
    if (str_detect(line, "^\\d{2}:\\d{2}:\\d{2}")) return(FALSE)
    return(TRUE)
  }, logical(1))
  lines <- lines[keep]
  lines <- str_replace_all(lines, "<[^>]+>", "")
  lines <- lines[lines != ""]
  paste(lines, collapse = "\n")
}


get_transcript_from_page <- function(identifier) {
  # --- Strategy 1: Scrape the details page for embedded CC text ---
  url <- sprintf("https://archive.org/details/%s", identifier)
  resp <- fetch_with_retry(url)

  if (!is.null(resp)) {
    html_text_raw <- tryCatch(resp_body_string(resp), error = function(e) NA_character_)

    if (!is.na(html_text_raw) && nchar(html_text_raw) > 100) {
      page <- tryCatch(read_html(html_text_raw), error = function(e) NULL)

      if (!is.null(page)) {
        selectors <- c(
          "#TextTranscriptScrollerContent",
          ".cc-text",
          "[class*='transcript']",
          "[class*='caption']",
          "[id*='transcript']",
          "[id*='caption']",
          "pre"
        )

        for (sel in selectors) {
          nodes <- tryCatch(html_elements(page, sel), error = function(e) list())
          if (length(nodes) > 0) {
            text <- html_text2(nodes[[1]])
            if (nchar(trimws(text)) > 100) {
              message("    Found via page selector: ", sel)
              return(trimws(text))
            }
          }
        }

        body_text <- tryCatch(html_text2(page), error = function(e) "")
        lines <- str_split(body_text, "\n")[[1]]
        cc_lines <- lines[str_detect(lines, "^\\s*>>")]
        if (length(cc_lines) > 5) {
          message("    Found via >> speaker markers")
          return(paste(trimws(cc_lines), collapse = "\n"))
        }
      }
    }
  }

  # --- Strategy 2: Try direct SRT download ---
  for (suffix in c(".cc5.srt", ".cc1.srt")) {
    srt_url <- sprintf("https://archive.org/download/%s/%s%s", identifier, identifier, suffix)
    message(sprintf("    Trying SRT: %s%s", identifier, suffix))
    srt_resp <- fetch_with_retry(srt_url)
    if (!is.null(srt_resp)) {
      srt_text <- tryCatch(resp_body_string(srt_resp), error = function(e) NA_character_)
      if (!is.na(srt_text) && nchar(trimws(srt_text)) > 50) {
        message(sprintf("    Got SRT (%d chars)", nchar(srt_text)))
        return(parse_srt(srt_text))
      }
    }
  }

  return(NA_character_)
}


sanitize_filename <- function(name) {
  str_replace_all(name, '[<>:"/\\\\|?*]', '_')
}


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

process_csv <- function(input_csv, output_dir = "jimmy_kimmel_transcripts",
                        start_row = 1, end_row = Inf) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  episodes <- read_csv(input_csv, show_col_types = FALSE)
  total <- nrow(episodes)

  # Clamp end_row to actual number of rows
  end_row <- min(end_row, total)
  start_row <- max(start_row, 1)

  message(sprintf("CSV has %d rows total", total))
  message(sprintf("Processing rows %d to %d (%d episodes)", start_row, end_row, end_row - start_row + 1))
  message(sprintf("Output directory: %s", output_dir))
  message(strrep("=", 60))

  success <- 0; failed <- 0; skipped <- 0

  for (i in start_row:end_row) {
    url         <- as.character(episodes$archive_video[i])
    air_date    <- as.character(episodes$original_air_date[i])
    season      <- as.character(episodes$season[i])
    episode_num <- as.character(episodes$episode_number[i])
    details     <- as.character(episodes$episode_details[i])

    identifier <- extract_identifier(url)

    if (is.na(identifier)) {
      message(sprintf("[%d/%d] SKIP - No URL", i, total))
      skipped <- skipped + 1
      next
    }

    safe_id <- sanitize_filename(identifier)
    out_filename <- paste0(safe_id, ".txt")
    out_path <- file.path(output_dir, out_filename)

    if (file.exists(out_path) && file.info(out_path)$size > 0) {
      message(sprintf("[%d/%d] EXISTS - %s", i, total, identifier))
      success <- success + 1
      next
    }

    message(sprintf("[%d/%d] Processing: %s (%s)", i, total, identifier, air_date))

    transcript_text <- tryCatch(
      get_transcript_from_page(identifier),
      error = function(e) {
        message(sprintf("    ERROR: %s", e$message))
        NA_character_
      }
    )

    if (!is.na(transcript_text) && nchar(trimws(transcript_text)) > 50) {
      header <- sprintf(
        paste0(
          "# Transcript: %s\n",
          "# Air Date: %s\n",
          "# Season: %s, Episode: %s\n",
          "# Details: %s\n",
          "# Source: %s\n",
          "%s\n\n"
        ),
        identifier, air_date, season, episode_num, details, url, strrep("=", 60)
      )
      writeLines(paste0(header, trimws(transcript_text)), out_path)

      success <- success + 1
      message(sprintf("    SAVED: %s (%d chars)", out_filename, nchar(transcript_text)))
    } else {
      failed <- failed + 1
      message(sprintf("    FAILED: No transcript found"))
    }
  }

  # Update summary CSV (append-friendly)
  summary_path <- file.path(output_dir, "_extraction_summary.csv")
  message(strrep("=", 60))
  message(sprintf("Batch %d-%d complete!", start_row, end_row))
  message(sprintf("  Successful:  %d", success))
  message(sprintf("  Failed:      %d", failed))
  message(sprintf("  Skipped:     %d", skipped))

  # Count total .txt files so far
  all_txt <- list.files(output_dir, pattern = "\\.txt$")
  message(sprintf("  Total transcripts so far: %d files", length(all_txt)))
  message("")
  if (end_row < total) {
    message(sprintf("Next batch: Rscript extract_transcripts.R \"%s\" %s %d %d",
                    input_csv, output_dir, end_row + 1, min(end_row + 100, total)))
  } else {
    message("All rows processed!")
    # Zip everything
    txt_files <- list.files(output_dir, pattern = "\\.txt$", full.names = TRUE)
    if (length(txt_files) > 0) {
      zip_path <- paste0(output_dir, ".zip")
      zip(zip_path, files = txt_files, flags = "-j")
      message(sprintf("  Zip file: %s (%d files)", zip_path, length(txt_files)))
    }
  }
}


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript extract_transcripts.R <csv> [output_dir] [start_row] [end_row]\n\n")
  cat("Examples:\n")
  cat('  Rscript extract_transcripts.R "episodes.csv" jimmy_kimmel_transcripts 1 100\n')
  cat('  Rscript extract_transcripts.R "episodes.csv" jimmy_kimmel_transcripts 101 200\n')
  quit(status = 1)
}

input_csv  <- args[1]
output_dir <- ifelse(length(args) >= 2, args[2], "jimmy_kimmel_transcripts")
start_row  <- ifelse(length(args) >= 3, as.integer(args[3]), 1)
end_row    <- ifelse(length(args) >= 4, as.integer(args[4]), Inf)

process_csv(input_csv, output_dir, start_row, end_row)
