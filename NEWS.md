# bibliometrix (development version)

Features:
* Added Author Bio Card
* Biblioshiny: Added Article AI-Summary

Changes:
* #564 [Solved] - Now bibliometrix supports new scopus CSV format (both in importing and local citation analyses)


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
