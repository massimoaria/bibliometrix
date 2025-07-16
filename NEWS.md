# bibliometrix (development version)

Features:

Changes:
* Biblioshiny - Added check and automatic update for imported packages: curl package (>= 6.3.0)


# bibliometrix v.5.1.0 (Release date: 2025-07-15)

Features:
* rpys(): 
    - Added the possibility to choose between two different median window approaches (centered or backward)
    - Added the algorithm to identify citation sequences and influencial references ("Hot papers", "Constant Performers", "Life Cycles", and "Sleeping Beauty")
    - Added these results also in Biblioshiny outputs.
* Biblioshiny:
    - Added Animated Diachronic Networks in Conceptual and Social Structures.
    - The Filters menu has been completely redesigned. It now follows a J-AU-DOC approach and includes many more filters (such as Countries, Journals, Citations).
* Biblio AI: 
    - Improved prompt lists
    - Added new Gemini models (2.5 Flash and Flash-lite)
    
Changes:
* Solved issue in mergeDbSources(). Now when merging files from a single db, the function preserves the cited reference field
* Added automatic conversion of country ISO2 to country names for OpenAlex data in convert2df()
    

# bibliometrix v.5.0.1 

Features:
* Added a new function mergeKeywords() to merge DE and ID keywords in a new column 'KW_Merged'

Changes:
* Solved several issues in biblioshiny()
* The following functions have been modified to work with merged keywords (field 'KW_Merged'): 
  - tableTag()
  - cocMatrix()
  - biblioNetwork()
  - conceptualStructure()
  - thematicMap()
  - thematicEvolution()
  - threeFieldPlot()


# bibliometrix v.5.0.0

Features:
* Biblioshiny 5.0 now includes Biblio AI – a powerful AI assistant for your science mapping analyses.

Changes:
* Solved several issues in biblioshiny()
