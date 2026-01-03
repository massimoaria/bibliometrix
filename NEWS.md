# biblioshiny (development version)

## Major Changes
- **New Table Rendering Engine**: Replaced `DT` (DataTables) with a custom `htmlBoxFormat` engine for data visualization.
    - **Improved UI Stability**: Fixed a long-standing glitch where column names would occasionally wrap or misalign in high-density dashboards.
    - **Enhanced Performance**: Optimized client-side rendering using a lightweight vanilla JavaScript implementation for sorting, filtering, and pagination.
    - **Uniform Design**: Introduced a new "Bibliobox" style that ensures consistent aesthetics across all analysis modules.
    - **Better Data Handling**: Improved DOI and URL handling in tables; the filtering and sorting logic now correctly ignores underlying HTML tags (like links) to focus on visible text.

## Technical Improvements
- Added `renderBibliobox` server-side wrapper to simplify the migration from `renderDT` and provide global error handling via `tryCatch`.
- Implemented responsive horizontal scrolling for wide datasets while maintaining fixed headers for better readability.
- Refined pagination logic to handle large datasets more gracefully with dynamic page indicators.


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
