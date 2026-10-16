# late-night-transcript-automation

## Project Overview

Developed an R-based workflow as part of a research assistantship at UC Santa Barbara to automate the collection of archival television transcripts from the Internet Archive.

The project involved compiling episode data for five late-night comedy shows spanning more than a decade, producing five spreadsheets containing thousands of entries each.

## Purpose

The goal was to create organized, usable datasets for future academic research examining keyword usage and language patterns across late-night television programs.

## Technical Approach

- Retrieved closed-caption transcripts from Internet Archive video links.

- Processed episodes in batches to manage large datasets.

- Cleaned and organized transcript text.

- Implemented retry logic, rate limiting, and duplicate checks.

- Organized transcripts with corresponding episode information.

## Research Applications

The resulting datasets support future keyword analysis and comparisons of language patterns across television programs and time periods.

## Project Files

- **extract_transcripts.R** — Main R script for retrieving, cleaning, and organizing closed-caption transcripts from Internet Archive video links.
- **extract_transcripts2.R** — Alternate extraction script adapted to a different CSV input format.
- **archive.R** — Test script for running transcript extraction on a smaller batch of episodes.
