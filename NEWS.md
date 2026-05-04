# bibliometrix (development version)

## New Features
- **`completeMetadata()`**: new function to complete missing bibliographic metadata via DOI lookup against **OpenAlex** and **Crossref**. Existing values are never overwritten. Provenance is recorded both as a per-row `M$ENRICH` column (compact tags like `AB:OA; TC:OA; CR:CR`) and as a long-format `attr(M, "enrichment")` data frame `(SR, field, source, timestamp)`.
    - Two-pass orchestration: OpenAlex runs first (broader coverage of AB/CR/C1/TC), Crossref then fills residual gaps. OpenAlex is auto-skipped when the source DB is OpenAlex (`M$DB[1] == "OPENALEX"`).
    - TC handling matches `missingData()` semantics: `TC = 0` is treated as vacant **only when the entire column sums to zero** (the same collection-level rule used by the audit table), so Scopus exports with all-zero citation counts get refreshed via OpenAlex while collections with mixed legitimate zeros are left untouched.
    - Honest per-source field coverage matrix: `ID` (Keywords Plus) and `WC` (WoS categories) are always skipped (WoS-proprietary); `TC` requires the OpenAlex pass; `DE` is off by default for both sources.
- **Biblioshiny**: new `Complete` and `Undo` buttons in the Missing Data audit modal. The Complete sub-modal pre-selects fields with status Poor/Critical/Completely missing and reads the polite-pool email and OpenAlex API key from the Settings panel (no per-modal credential typing).
- **Biblioshiny**: new before/after comparison modal opened after each enrichment, showing per-tag Before %, After %, Δ %, and the number of cells filled by OpenAlex vs Crossref, plus an Undo path that restores the pre-enrichment collection.

## Changes:
- **Biblioshiny**: Minor fix in table layout for numeric columns

## Bug Fixes and Improvements
- **Biblioshiny - Metadata completion result**: fixed the Before/After modal showing identical percentages. The "after" snapshot is now read from the value returned by `completeMetadata()` rather than from the reactive table, which was stale because its modal was already closed.
- **Biblioshiny - Metadata completion result**: the Delta column is now rendered as a coloured badge (blue ▼ for improvement, red ▲ for regression) so the gain from each enrichment pass is immediately visible. A small icon prefix prevents `renderBibliobox` from auto-detecting the column as numeric and silently stripping the HTML.
- **Biblioshiny - Synonym / Stopword pop-ups**: the modal used to render very narrow because `popUpGeneric()` defaulted to `size = "40%"`, which is not a valid `show_alert()` enum and was silently dropped. Switched to `size = "l"` and added `white-space: nowrap` to the term column so single words like "bibliometrics" or "entrepreneurship" no longer break mid-word.
- **Biblioshiny - Unified Text Editing layout**: the Stop Words / Synonyms boxes in Most Frequent Words, WordCloud, Treemap, Word Dynamics and Trend Topics now use the same panel layout (orange "Stop Words" panel + green "Synonyms" panel with icons) as the Co-occurrence Network menu. Existing input ids are preserved, no server-side change required.

# bibliometrix V.5.3.0 (Release date: 2026-04-10)

## Major Changes
- **New Table Rendering Engine**: Replaced `DT` (DataTables) with a custom `htmlBoxFormat` engine for data visualization.
    - **Improved UI Stability**: Fixed a long-standing glitch where column names would occasionally wrap or misalign in high-density dashboards.
    - **Enhanced Performance**: Optimized client-side rendering using a lightweight vanilla JavaScript implementation for sorting, filtering, and pagination.
    - **Uniform Design**: Introduced a new "Bibliobox" style that ensures consistent aesthetics across all analysis modules.
    - **Better Data Handling**: Improved DOI and URL handling in tables; the filtering and sorting logic now correctly ignores underlying HTML tags (like links) to focus on visible text.

## Performance and User Experience Improvements
- **Asynchronous Processing**: All Biblioshiny analyses now run asynchronously, keeping the UI responsive during long-running operations.
- **Async AI Integration**: AI API calls (Gemini and OpenRouter) are now non-blocking with proper timeout handling and exponential backoff on retries.
- **AI Summary Button**: Added AI-powered document summaries in Most Global/Local Cited Documents tables.
- **Function Optimization**: Optimized heavy functions including `rpys()`, `cocMatrix()`, and table rendering for better performance.
- **Power-off Button**: Added a power-off button in Biblioshiny to properly shut down the application.
- **Max Rows Control**: Added `max.rows` argument in `biblioshiny()` to control the maximum number of rows displayed in tables.

## New Features
- Add PRISMA Flow Diagram module to Biblioshiny
- Added OpenAlex API key settings panel and warning banner for unauthenticated access.
- Added email setting for polite use of the OpenAlex AI features.
- Improved cited reference (CR) download from the OpenAlex API.

## Technical Improvements
- Added `renderBibliobox` server-side wrapper to simplify the migration from `renderDT` and provide global error handling via `tryCatch`.
- Implemented responsive horizontal scrolling for wide datasets while maintaining fixed headers for better readability.
- Refined pagination logic to handle large datasets more gracefully with dynamic page indicators.
- Implemented future/promises for non-blocking API calls with `req_timeout(120)` to HTTP pipelines.
- Cleaned up async AI pipeline, removing unused `geminiGenerate()` and `geminiPromptImage()` functions.

## Bug Fixes
- #591 [Solved] – Synced the country dictionary with the latest ISO 3166-1 standard.
- Fixed reference matching algorithm.
- Fixed plot resize issues.
- Fixed thematic evolution display issues.
- Fixed chromium compatibility issues on Ubuntu server.
- Improved author name formatting.
- Fixed Biblioshiny crash and empty HTML downloads when clicking download/screenshot buttons before running the analysis. All export buttons are now disabled until results are available.
- Fixed OpenAlex API error 400 when searching by Author or Concept fields. Entity-based fields now use a two-step ID resolution approach compatible with the current OpenAlex API.


# bibliometrix V.5.2.1 (Release date: 2025-12-11)

Changes:

* Removed old geemini models 1.5 and 2.0 from biblioAI
* Solved issue with internet connection checking in MS Windows systems

# bibliometrix V.5.2.0 (Release date: 2025-11-04)

Features:

* Added Author Bio Card (new function AuthorBio())
* Added new plot layout in plotThematicEvolution()
* Added automatic identification of continuous flows in plotThematicEvolution()
* Added seed argument to networkPlot(), ThematicMap(), and ThematicEvolution() functions
* Improved OpenAlex data conversion process
* Enhanced Community Repulsion algorithm in networkPlot(), ThematicMap(), and ThematicEvolution() functions
* Added Citation Matching algorithm (new function applyCitationMatching())
* Added Life Cycle Analysis (new function lifeCycle())
* Biblioshiny enhancements:
    - Added AI-powered article summaries
    - Added Author Profile feature (using OpenAlex data)
    - Added API Search and Download menu for OpenAlex and PubMed
    - Removed API support for Dimensions.ai
    - Added Content Analysis menu (using contentanalysis library)
    - Improved parameter layout across all analyses
    - Added seed parameter in Settings
    - Added Life Cycle Analysis menu

Changes:

#564 [Solved] - bibliometrix now supports the new Scopus CSV format (both for data import and local citation analyses)


# bibliometrix V.5.1.1 (Release date: 2025-09-02)

Features:
* Biblioshiny – Filters: Added the ability to upload a journal ranking list and filter publications based on selected rankings.

Changes:
* Biblioshiny: Introduced automatic checks and updates for required packages, such as curl (version ≥ 6.3.0).


# bibliometrix v.5.1.0 (Release date: 2025-07-15)

Features:
* rpys(): 
    - Introduced two options for the median window: centered and backward.
    - Implemented an algorithm to detect citation sequences and influential references ("Hot Papers", "Constant Performers", "Life Cycles", and "Sleeping Beauties").
    - These results are now also included in Biblioshiny outputs.
* Biblioshiny:
    - Added animated diachronic networks to Conceptual and Social Structure analyses.
    - Completely redesigned the Filters panel using a J-AU-DOC framework, now supporting a broader range of filters (e.g., Countries, Journals, Citations).
* Biblio AI: 
    - Enhanced prompt templates.
    - Integrated new Gemini models (2.5 Flash and Flash-lite).
    
Changes:
* Fixed an issue in mergeDbSources(): the function now preserves the cited references field when merging files from a single database.
* convert2df() now automatically converts ISO2 country codes to full country names for OpenAlex data.
    

# bibliometrix v.5.0.1 

Features:
* Introduced mergeKeywords(), a new function that combines DE and ID keywords into a single field named KW_Merged.

Changes:
* Resolved various issues in biblioshiny().
* Updated the following functions to work with the new KW_Merged field:
  - tableTag()
  - cocMatrix()
  - biblioNetwork()
  - conceptualStructure()
  - thematicMap()
  - thematicEvolution()
  - threeFieldPlot()


# bibliometrix v.5.0.0

Features:
* Biblioshiny 5.0 now includes Biblio AI – a powerful AI assistant to support your science mapping analyses.
Changes:
* Resolved multiple issues in biblioshiny().
