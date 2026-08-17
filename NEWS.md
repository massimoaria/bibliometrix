# bibliometrix (development version)

## New Features
- **Biblioshiny — OpenAlex search: exact match (stemming off)**: the OpenAlex query builder gained an **Exact match** checkbox. By default OpenAlex applies stemming to search filters, so *bone* also retrieves *bones* and *surgery* also retrieves *surgeries*, which can inflate the result count well beyond what the user intended. When the box is checked, Biblioshiny queries the `.exact` variant of the search filter (`title.search.exact`, `abstract.search.exact`, `title_and_abstract.search.exact`, `default.search.exact`), the API counterpart of the *Enable stemming* switch on the OpenAlex website. Quoted phrases, parentheses and the AND/OR/NOT operators work in both modes, and `Author`/`Concept` rows are unaffected since they resolve to entity IDs.
- **Biblio AI — Gemini 3.5 Flash Lite**: added `gemini-3.5-flash-lite` to the model list. It is Google's recommended replacement for Gemini 2.5 Flash — higher quality at the same price and a faster throughput. `gemini_ai()` sends it with `thinkingConfig$thinkingLevel = "minimal"`, which is what keeps 2.5-Flash-class latency, and caps its output at the model's 65536-token ceiling. Verified against both a free-tier and a paid API key, so it is listed under **Free tier** (free-tier models also work with paid keys).
- **OpenAlex CSV import — new web-export format**: `convert2df(dbsource = "openalex")` now reads the new OpenAlex *Export to CSV* layout (human-readable headers such as `Work ID`, `Title`, `Author`, `Source`, `Author IDs`, `Institution`) in addition to the legacy dotted-path column names (`id`, `authorships.author.display_name`, ...). The paper identifier was renamed by OpenAlex from `id` to `Work ID`.
- **OpenAlex CSV — field validation & fault tolerance**: `csvOA2df()` now validates the available metadata. It stops with a clear, actionable message when a **required** field is missing (`Work ID`, `Title`, `Author`, `Source`, `Year`) and warns about missing **recommended** fields, while the import still proceeds. In Biblioshiny these are surfaced as a dialog / notification, so an incomplete OpenAlex CSV no longer crashes the app.
- **`completeMetadata()` — OpenAlex enrichment by Work ID**: records are now looked up on OpenAlex by their **Work ID (`id_oa`)** when the DOI is absent (typical of minimal OpenAlex CSV exports), instead of being skipped. The OpenAlex pass is no longer disabled for OpenAlex-sourced collections, so fields omitted from a partial CSV export (abstract, affiliations, references, ...) can be downloaded from the OpenAlex API.

## Bug Fixes and Improvements
- **Biblioshiny — the Gemini API key check no longer depends on a single model**: `setGeminiAPI()` validated the key by sending a `generateContent` request to a hard-coded `gemini-2.5-flash`. When Google stopped serving the 2.5 family to newly created API keys, that probe started returning *"HTTP 404: This model models/gemini-2.5-flash is no longer available to new users"*, so **every new user was told their key had been refused — whatever model they had selected in Settings**, and on a paid key too. The key is now validated against the model **catalogue** (`ListModels`), which is model-agnostic and cannot be broken by a model retirement; a transport failure is reported as a connection error instead of blaming the key. If the model selected in Settings is not in the catalogue of that specific key, the success message says so, instead of letting the user discover it as an HTTP 404 at the first analysis.
- **Biblioshiny — default Gemini model updated**: fresh installations defaulted to `gemini-2.5-flash-lite`, which recently created API keys cannot use. The default (in the model selector, in `loadGeminiModel()` and in the Content Analysis fallback) is now `gemini-3.5-flash-lite`. The two Gemini 2.5 entries remain selectable, for keys that still have access to them, under a **Legacy** group that states the limitation.
- **Biblioshiny — plot export no longer requires pandoc**: saving a Three-Field Plot (and any other network/plotly export) aborted with *"Saving a widget with `selfcontained = TRUE` requires pandoc"* on installations without pandoc, since `htmlwidgets::saveWidget()`/`visSave()` route self-contained output through `rmarkdown::pandoc_self_contained_html()`. These pages are temporary scratch files, screenshotted locally and deleted immediately, so they are now written with `selfcontained = FALSE`: the export works with no pandoc installed and the resulting PNG is unchanged. Affected: `plotlySankey2png()`, `plot2png(type = "vis")` and `plot2pngGemini()`, which also cleans up after itself now.
- **Biblioshiny — the Gemini API key status was never displayed**: the Settings panel declares `uiOutput("apiStatus")`, but the server assigned `output$status <- renderText(...)` from inside `renderUI()` — an output id that does not exist in the UI — and returned no UI at all. Every message was silently discarded, so a user pasting an invalid key saw nothing happen. The status is now a reactive value rendered by a single output, colour-coded for validating/success/error.
- **Biblioshiny — actionable message for "AQ." Gemini keys**: Google AI Studio has started issuing some accounts OAuth-style tokens prefixed with `AQ.` instead of classic `AIzaSy...` API keys. They are rejected by the `generativelanguage.googleapis.com` REST endpoint Biblio AI calls (HTTP 401, *"Expected OAuth 2 access token"*), so they can never work. `setGeminiAPI()` now reports the real error returned by Google and, for `AQ.` keys, explains how to obtain a usable key. Empty/short keys are also rejected before spending a network round trip, and the validated key is trimmed before being saved, so a key pasted with a trailing newline no longer breaks the next launch.
- **Package functions now work without `library(bibliometrix)` (#629)**: internal calls to `data()` did not specify `package = "bibliometrix"`, so the package datasets (`bibtag`, `countries`, `stopwords`, `logo`) were only found when the package was attached. Calling e.g. `bibliometrix::convert2df(..., dbsource = "generic", format = "bibtex")` in a fresh session raised *"data set 'bibtag' not found"* followed by *"undefined columns selected"*. All 16 affected call sites are now package-qualified, so `bibliometrix::` usage works without attaching the package. Affected functions: `bib2df()`, `dimensions2df()`, `csvOA2df()`, `metaTagExtraction()`, `biblioAnalysis()`, `collabByRegionPlot()`, `termExtraction()`, `bradford()`, `rpys()`, `histPlot()`, `conceptualStructure()`, `fieldByYear()`, `couplingMap()`, `thematicMap()`, `authorProdOverTime()`.
- **`metaTagExtraction()` — empty `RP`/`C1` strings treated as missing**: a `Correspondence Address` column present but empty in a Scopus CSV yielded `RP == ""` (not `NA`), which wiped out all affiliations in `AU_UN()` (crashing `biblioAnalysis()` with *"invalid argument to unary operator"*) and silently overwrote valid `C1` data in `AU1_CO()` (losing the corresponding-author country). Empty or whitespace-only `RP`/`C1` values are now normalized to `NA` on entry, restoring the intended fallback on `C1`.
- **OpenAlex CSV import no longer crashes on sparse collections**: missing optional columns are now created as `NA_character_` (a logical `NA` broke `strsplit()` in `biblioAnalysis()`), and the `DT` (document type) column is always present (defaults to `ARTICLE`, since the web CSV export does not include the work type).
- **Biblioshiny — Missing Data audit**: `wcTable()` now treats an all-`NA` Science Categories (`WC`) column as missing, fixing a *"missing value where TRUE/FALSE needed"* crash on import of OpenAlex collections without subject categories.
- **Biblioshiny — Metadata completion modal**: eligibility now counts records with a **DOI or an OpenAlex Work ID**, and OpenAlex enrichment is offered for OpenAlex collections (previously deselected by default). Labels and help text updated accordingly.
- **Biblioshiny — Import Info & References**: added a dedicated **OpenAlex** section documenting the minimum and recommended metadata to select when exporting, and the need to enrich the collection via the OpenAlex API to obtain cited references.
- **`missingData()` no longer fails on collections without a `TC` column**: the collection-level rule that flags an all-zero citation column as fully missing assigned `missing_counts["TC"]` unconditionally. Since `sum(as.numeric(NULL))` is `0`, the branch fired even when the data frame had no `TC` column at all, appending a spurious element to the counts vector and aborting with *"arguments imply differing number of rows"*. The assignment is now guarded on the column being present. `completeMetadata()`, which calls `missingData()` on entry, was affected by the same crash.

## Technical Improvements
- **Test suite realigned with the OpenAlex Work ID refactoring**: the `completeMetadata()` unit tests still built the old `by_doi` enrichment payload (now `by_key`, since lookups may be keyed by DOI *or* Work ID) and stubbed `.enrich_from_openalex()` without its new `key_type` argument, so `R CMD check` failed on all three platforms. The OpenAlex tests now cover the current contract — lookup by `id_oa` with `key_type = "id"` — and stub the client so they run offline instead of issuing a real API request.
- **CI**: `actions/checkout` bumped from v4 to v5; v4 targets the Node.js 20 runtime, deprecated on GitHub Actions runners.

# bibliometrix 5.4.1

## Bug Fixes and Improvements
- **Minor fixes** in Biblioshiny UI


# bibliometrix 5.4.0

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
- **Biblioshiny — ggplot downloads and Excel report exports now work on R 4.6 macOS without XQuartz**: on hosts where R reports `capabilities("cairo") == TRUE` but the cairo dynamic library cannot actually be loaded (typical on macOS R ≥ 4.6 when XQuartz is not installed), `safe_png_device()` used to return a function that crashed at draw time. Symptoms: the *Download* button on any ggplot panel produced an empty response (Chrome saved a stub `<id>.html` "file not available"); *Add to Report* produced a sheet with no image inside the Excel file. The selector now **probes** each candidate device (ragg → quartz on macOS → default `png()` → cairo) by writing a 2×2 in test PNG, picks the first one that actually flushes a non-empty file, and caches the result for the session. Users who hit the bug get the working device automatically; no `install.packages('ragg')` step required.
- **Biblioshiny — Report builder no longer crashes on missing/unrecognised plots**: clicking *Add to Report* on a tab whose plot had not yet finished rendering (or whose plot object was of an unsupported class) used to abort the Shiny session with `Error in insertImage: File does not exist`, because `addGgplotsWb()` skipped every writer branch and then handed a non-existent path to `openxlsx::insertImage`. The function now (a) skips NULL plots with a warning, (b) wraps each writer (`safe_ggsave`, `igraph2PNG`, dendrogram device) in `tryCatch`, (c) verifies the PNG was actually written before calling `insertImage`. `addScreenWb()` was hardened in the same way for screenshot-style entries. Users who hit the bug now see a console warning identifying the offending sheet/plot index and the report continues building from the next chart.
- **Biblioshiny - Metadata completion result**: fixed the Before/After modal showing identical percentages. The "after" snapshot is now read from the value returned by `completeMetadata()` rather than from the reactive table, which was stale because its modal was already closed.
- **Biblioshiny - Metadata completion result**: the Delta column is now rendered as a coloured badge (blue ▼ for improvement, red ▲ for regression) so the gain from each enrichment pass is immediately visible. A small icon prefix prevents `renderBibliobox` from auto-detecting the column as numeric and silently stripping the HTML.
- **Biblioshiny - Synonym / Stopword pop-ups**: the modal used to render very narrow because `popUpGeneric()` defaulted to `size = "40%"`, which is not a valid `show_alert()` enum and was silently dropped. Switched to `size = "l"` and added `white-space: nowrap` to the term column so single words like "bibliometrics" or "entrepreneurship" no longer break mid-word.
- **Biblioshiny - Unified Text Editing layout**: the Stop Words / Synonyms boxes in Most Frequent Words, WordCloud, Treemap, Word Dynamics and Trend Topics now use the same panel layout (orange "Stop Words" panel + green "Synonyms" panel with icons) as the Co-occurrence Network menu. Existing input ids are preserved, no server-side change required.
- **Biblio AI model selection**:
  - Add open Gemma 4 models (gemma-4-31b-it, gemma-4-26b-a4b-it) via the Google Gemini API. gemini_ai() now handles their bare model id (no "gemini-" prefix), drops the unsupported `seed` field and caps output tokens for Gemma models.
  - Parse "thought" parts for thinking models so the answer text is returned instead of the reasoning trace.
  - Retry transient 5xx server errors (500/502/504), not only 429/503.
  - Group the model dropdown into "Free tier" / "Paid API key required".
  - Default to Gemini 2.5 Flash Lite on first launch.
- **Co-occurrence Network**:
  - New "Documents" tab: fuzzy assignment of documents to network clusters, reusing clusterAssignment() (the same engine as the Thematic Map).
  - Include the document-to-cluster table in the Excel report.
  - Feed the three most central documents per cluster to Biblio AI through the shared doc2clust() helper (same approach as the Thematic Map).
- clusterAssignment(): coerce TC/PY to numeric to avoid an error on collections where TC is imported as character (affects Thematic Map too).

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
