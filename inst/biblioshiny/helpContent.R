helpContent <- function(){
  biblioAI <- 
    "<body>
    <div class='container'>
    <h3>🧠 Biblio AI: AI-Powered Bibliometric Analysis</h3>
    
    <p>Starting from version <strong>5.0</strong>, <em>Biblioshiny</em> introduces <strong>Biblio AI</strong>, a new suite of features powered by Google's <strong>Gemini</strong> models. This integration allows users to receive <em>automatic interpretations</em>, <em>critical insights</em>, and <em>narrative summaries</em> of their bibliometric results – directly within the platform.</p>
    
    <p><strong>Note:</strong> Biblio AI requires a <strong>Chrome-based browser</strong> (such as Google Chrome or Microsoft Edge) installed on your computer to work correctly.</p>
    
    <h4>✨ What does Biblio AI do?</h4>
    <p>Biblio AI enhances the core analytical modules of <em>Biblioshiny</em> by providing contextual, AI-generated commentary on several results, such as:</p>
    <ul>
      <li><strong>Overview:</strong> High-level summaries of key bibliometric indicators and collection features.</li>
      <li><strong>Three-Field Plot:</strong> Interpretation of the connections among sources, authors, and keywords.</li>
      <li><strong>Authors' Production over Time:</strong> Insights on temporal dynamics and productivity patterns of key authors.</li>
      <li><strong>Corresponding Author's Countries Collaboration:</strong> Discussion of international scientific collaboration patterns.</li>
      <li><strong>Most Local Cited Documents:</strong> Evaluation of the most influential documents within the dataset.</li>
      <li><strong>Reference Publication Year Spectroscopy:</strong> Identification and interpretation of historical citation peaks.</li>
      <li><strong>Trend Topics:</strong> Explanation of thematic evolution and detection of emerging research trends.</li>
      <li><strong>Knowledge Structures:</strong> Analysis of conceptual maps and networks such as co-citation and co-word analysis.</li>
      <li><strong>Country Collaboration World Map:</strong> AI-assisted reading of global co-authorship and geographical patterns.</li>
    </ul>
    <p>In each of these sections, users can activate the <strong>Biblio AI</strong> panel to access dynamic text explanations, perfect for use in scientific writing, presentations, or reporting.</p>
    
    <h4>🔧 How to enable Biblio AI?</h4>
    <p>To enable Biblio AI, follow these simple steps:</p>
    <ol>
      <li><strong>Register</strong> at <a href='https://makersuite.google.com/' target='_blank'>Google AI Studio</a> (free access available).</li>
      <li><strong>Generate an API Key</strong> enabled for Gemini model access (Free Tier supported).</li>
      <li><strong>Enter your API Key</strong> in the <em>Settings</em> section of <em>Biblioshiny</em>.</li>
    </ol>
    <p>The interface will guide you through the secure and local setup. Your API key is used only on your device to interact with the AI model.</p>
    
    <h4>🎯 Why use Biblio AI?</h4>
    <ul>
      <li>Reduces time spent interpreting complex outputs.</li>
      <li>Supports scientific writing and research reporting.</li>
      <li>Helps users better understand bibliometric patterns and dynamics.</li>
      <li>Delivers explanations in natural language, accessible to both experts and newcomers.</li>
    </ul>
    </div>
    </body>"
  
  info <- 
    "<body>
    <div class='container'>
    <h3>📚 Supported Bibliographic Databases and Suggested File Formats</h3>
    
    <p><strong>Biblioshiny</strong> imports and analyzes collections exported from the following bibliographic databases:</p>
    <ul>
      <li><a href='https://www.webofscience.com' target='_blank'>Web of Science</a></li>
      <li><a href='https://www.scopus.com' target='_blank'>Scopus</a></li>
      <li><a href='https://www.openalex.org' target='_blank'>OpenAlex</a></li>
      <li><a href='https://www.dimensions.ai' target='_blank'>Dimensions</a></li>
      <li><a href='https://www.lens.org' target='_blank'>The Lens</a></li>
      <li><a href='https://pubmed.ncbi.nlm.nih.gov' target='_blank'>PubMed</a></li>
      <li><a href='https://www.cochranelibrary.com' target='_blank'>Cochrane Library</a></li>
    </ul>
    
    <p><strong>Web of Science</strong>, <strong>Scopus</strong>, and <strong>OpenAlex</strong> allow users to export the complete set of metadata, making it possible to perform all analyses implemented in <strong>Biblioshiny</strong>.</p>
    
    <p>Some other databases, such as <strong>Dimensions</strong>, <strong>PubMed</strong>, and <strong>Cochrane Library</strong>, provide only a limited set of metadata. This may impose restrictions on the range of analyses that can be conducted using those datasets.</p>
    
    <p>The following table (not included here) reports, for each supported database:</p>
    <ul>
      <li>The <strong>file formats supported</strong> by the export interface</li>
      <li>The <strong>types of metadata</strong> contained in each export option</li>
      <li>The <strong>suggested file format</strong> to use with <strong>Biblioshiny</strong></li>
    </ul>
    </div>
    </body>"
  
  publications <-
    "<body>
    <div class='container'>
    <h3>📖 Main Authors' References (Bibliometrics)</h3>
    
    <ul>
      <li><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></li>
      
      <li><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>Comparative science mapping: a novel conceptual structure analysis with metadata.</i> <strong>Scientometrics</strong>. <a href='https://doi.org/10.1007/s11192-024-05161-6' target='_blank'>https://doi.org/10.1007/s11192-024-05161-6</a></li>

      <li><strong>Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2023).</strong> <i>openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex.</i> <strong>R Journal</strong>, 15(4). <a href='https://doi.org/10.32614/rj-2023-089' target='_blank'>https://doi.org/10.32614/rj-2023-089</a></li>

      <li><strong>Aria, M., Misuraca, M., & Spano, M. (2020).</strong> <i>Mapping the evolution of social research and data science on 30 years of Social Indicators Research.</i> <strong>Social Indicators Research</strong>. <a href='https://doi.org/10.1007/s11205-020-02281-3' target='_blank'>https://doi.org/10.1007/s11205-020-02281-3</a></li>

      <li><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2022).</strong> <i>Thematic Analysis as a New Culturomic Tool: The Social Media Coverage on COVID-19 Pandemic in Italy.</i> <strong>Sustainability</strong>, 14(6), 3643. <a href='https://doi.org/10.3390/su14063643' target='_blank'>https://doi.org/10.3390/su14063643</a></li>

      <li><strong>Aria, M., Alterisio, A., Scandurra, A., Pinelli, C., & D'Aniello, B. (2021).</strong> <i>The scholar's best friend: research trends in dog cognitive and behavioural studies.</i> <strong>Animal Cognition</strong>. <a href='https://doi.org/10.1007/s10071-020-01448-2' target='_blank'>https://doi.org/10.1007/s10071-020-01448-2</a></li>
      
      <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2016).</strong> <i>Foundations and trends in performance management: A twenty-five years bibliometric analysis in business and public administration domains.</i> <strong>Scientometrics</strong>. <a href='https://doi.org/10.1007/s11192-016-1948-8' target='_blank'>https://doi.org/10.1007/s11192-016-1948-8</a></li>
        
      <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2015).</strong> <i>Twenty years of research on performance management in business and public administration domains.</i> Presented at CARME 2015. <a href='https://www.bibliometrix.org/documents/2015Carme_cuccurulloetal.pdf' target='_blank'>Link</a></li>
          
      <li><strong>Sarto, F., Cuccurullo, C., & Aria, M. (2014).</strong> <i>Exploring healthcare governance literature: systematic review and paths for future research.</i> <strong>Mecosan</strong>. <a href='https://www.francoangeli.it/Riviste/Scheda_Rivista.aspx?IDarticolo=52780&lingua=en' target='_blank'>Link</a></li>
            
      <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2013).</strong> <i>Twenty years of research on performance management in business and public administration domains.</i> <strong>Academy of Management Proceedings</strong>, Vol. 2013, No. 1, p. 14270. <a href='https://doi.org/10.5465/AMBPP.2013.14270abstract' target='_blank'>https://doi.org/10.5465/AMBPP.2013.14270abstract</a></li>
              
      <li><strong>Belfiore, A., Salatino, A., & Osborne, F. (2022).</strong> <i>Characterising Research Areas in the field of AI.</i> <strong>arXiv</strong> preprint. <a href='https://doi.org/10.48550/arXiv.2205.13471' target='_blank'>https://doi.org/10.48550/arXiv.2205.13471</a></li>
                
      <li><strong>Belfiore, A., Cuccurullo, C., & Aria, M. (2022).</strong> <i>IoT in healthcare: A scientometric analysis.</i> <strong>Technological Forecasting and Social Change</strong>, 184, 122001. <a href='https://doi.org/10.1016/j.techfore.2022.122001' target='_blank'>https://doi.org/10.1016/j.techfore.2022.122001</a></li>
                  
      <li><strong>D'Aniello, L., Spano, M., Cuccurullo, C., & Aria, M. (2022).</strong> <i>Academic Health Centers' configurations, scientific productivity, and impact: insights from the Italian setting.</i> <strong>Health Policy</strong>. <a href='https://doi.org/10.1016/j.healthpol.2022.09.007' target='_blank'>https://doi.org/10.1016/j.healthpol.2022.09.007</a></li>

      <li><strong>Belfiore, A., Scaletti, A., Lavorato, D., & Cuccurullo, C. (2022).</strong> <i>The long process by which HTA became a paradigm: A longitudinal conceptual structure analysis.</i> <strong>Health Policy</strong>. <a href='https://doi.org/10.1016/j.healthpol.2022.12.006' target='_blank'>https://doi.org/10.1016/j.healthpol.2022.12.006</a></li>
    </ul>
    </div>
    </body>"
  
  filters <- 
    "<body>
    <div class='container'>
    <h3>🔍 Filters Information</h3>
    
    <p>This section allows users to refine the document collection by applying multiple filters based on metadata fields available in the dataset. Below is a description of each filter and its usage:</p>
    
    <h4>1. General</h4>
    <ul>
      <li><strong>Document Type:</strong> Filters documents by type (e.g., Article, Book Chapter, Proceedings Paper). The selected document types are shown automatically; to remove one, simply click on it.</li>
      <li><strong>Language:</strong> Filters documents based on the language of publication.</li>
      <li><strong>Publication Year:</strong> Select a range of years using the slider to filter documents published within a specific time interval.</li>
      <li><strong>Subject Category:</strong> If available, this filter enables selection by subject category (e.g., Business, Education, Engineering). This option appears only if the source database (e.g., Scopus) provides this metadata. If the information is not available, the filter is automatically hidden.</li>
    </ul>
    
    <h4>2. Journal (J)</h4>
    <ul>
      <li><strong>Upload a List of Journals:</strong> Allows users to upload a custom list of journal titles to restrict the collection accordingly. Accepted file formats: <code>.csv</code>, <code>.txt</code>, and <code>.xlsx</code>. Journal titles must be listed in the first column of the file.</li>
      <li><strong>Upload a Journal Ranking List:</strong> Enables users to filter journals based on a predefined ranking system (e.g., Q1–Q4). The uploaded file must be in <code>.csv</code> or <code>.xlsx</code> format and include <strong>two columns with headers</strong>: the first for the journal titles, and the second for the corresponding ranking category.</li>
      <li><strong>Source by Bradford Law Zones:</strong> Enables filtering of journals based on Bradford's Law.</li>
    </ul>
    
    <h4>3. Author's Country (AU)</h4>
    <ul>
      <li><strong>Region and Country:</strong> Enables filtering by geographic region or specific country of the authors. Regions can be quickly selected using predefined buttons (e.g., Europe, Asia), while countries can be selected individually from the list.</li>
    </ul>
    
    <h4>4. Documents (DOC)</h4>
    <ul>
      <li><strong>Total Citations:</strong> Filters documents based on the total number of citations received.</li>
      <li><strong>Total Citations per Year:</strong> Filters documents by the average number of citations received per year since publication.</li>
    </ul>
    
    <h4>📝 Additional Notes</h4>
    <ul>
      <li>Filters with dual-column boxes (e.g., Subject Category, Country) are automatically pre-populated with values representing the structure of the current collection. These selections can be modified by clicking on any item in the right-hand column to remove it.</li>
      <li>Use the <strong>Apply</strong> button to activate the selected filters and update the collection.</li>
      <li>Use the <strong>Reset</strong> button to clear all filters and restore the original dataset.</li>
    </ul>
    </div>
    </body>"
  
  authorProfile <- 
    "<body>
    <div class='container'>
    <h3>👤 Author Profile Overview</h3>
    
    <p>The Author Profile page provides a <strong>dual-perspective bibliometric overview</strong> of each author included in the collection:</p>
    
    <h4>🔹 Global Profile</h4>
    <p>The <strong>Global Profile</strong> presents the author's complete scientific output, based on metadata retrieved from <a href='https://openalex.org' target='_blank'>OpenAlex</a> via the <code>openalexR</code> R package. This profile includes <em>all publications authored by the researcher</em>, regardless of whether they are part of the current collection.</p>

    <p><strong>Main features of the Global Profile include:</strong></p>
    <ul>
      <li>Total Publications and Citations</li>
      <li>H-Index and i10-Index</li>
      <li>2-Year Mean Citation Rate</li>
      <li>Publication Trends over the last 10 years</li>
      <li>Main Research Topics extracted from OpenAlex concepts</li>
    </ul>

    <p><strong>Data Source:</strong> OpenAlex API (via <code>openalexR</code>)<br>
    <strong>Unique Identifier:</strong> OpenAlex Author ID (e.g., <code>A5014455237</code>)</p>
    
    <h4>🔸 Local Profile</h4>
    <p>The <strong>Local Profile</strong> focuses exclusively on the subset of the author's publications that are included in the <em>user-defined collection</em> currently under analysis in the project.</p>
    
    <p><strong>Main features of the Local Profile include:</strong></p>
    <ul>
      <li>Number of Publications, Total Citations, and Local H-Index</li>
      <li>Average Citations per Work</li>
      <li>Recent Activity: Number of publications in the last 5 years</li>
      <li>Publication Trends (based only on local data)</li>
      <li>Main Keywords derived from the local collection</li>
      <li>List of Publications with full metadata (title, year, journal, DOI, citations)</li>
    </ul>
    
    <p>This local profile helps contextualize the author's role and impact <strong>within the specific research topic or dataset</strong> under investigation.</p>
    
    <h4>🔄 Interpretation and Use</h4>
    <p>The <strong>Global Profile</strong> offers a broad, external view of the author's overall scholarly influence, while the <strong>Local Profile</strong> highlights their specific relevance <em>within the current study</em>.</p>

    <p>This dual visualization is particularly useful for:</p>
    <ul>
      <li>Identifying influential researchers in the topic area</li>
      <li>Comparing local vs. global impact</li>
      <li>Evaluating thematic alignment of authors with the collection's focus</li>
    </ul>
    
    <h4>📚 References</h4>
    <p><strong>Priem, J. et al. (2022).</strong> <i>OpenAlex: A fully-open index of scholarly works, authors, venues, institutions, and concepts.</i> Retrieved from <a href='https://openalex.org' target='_blank'>https://openalex.org</a></p>
      
    <p><strong>Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2024).</strong> <i>openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex.</i> <strong>R Journal</strong>, 15(4), 167–180. <a href='https://doi.org/10.32614/RJ-2023-089' target='_blank'>https://doi.org/10.32614/RJ-2023-089</a></p>
        
    <p><strong>Aria, M. et al. (2023).</strong> <i>openalexR: An R package for programmatic access to OpenAlex metadata.</i> <strong>CRAN</strong>. Retrieved from <a href='https://cran.r-project.org/package=openalexR' target='_blank'>https://cran.r-project.org/package=openalexR</a></p>
        
    <p><strong>Hirsch, J.E. (2005).</strong> <i>An index to quantify an individual's scientific research output.</i> <strong>Proceedings of the National Academy of Sciences</strong>, 102(46), 16569–16572. <a href='https://doi.org/10.1073/pnas.0507655102' target='_blank'>https://doi.org/10.1073/pnas.0507655102</a></p>
    </div>
    </body>"
  
  referenceMatching <- 
    "<body>
    <div class='container'>
    <h3>🔗 Reference Matching: Algorithm and Usage</h3>
    
    <p>The <strong>Reference Matching</strong> module implements an advanced algorithm to identify and link cited references within a bibliometric collection to the actual documents present in the dataset. This process enables accurate <em>citation network analysis</em>, <em>co-citation studies</em>, and identification of <strong>highly-cited works within the collection</strong>.</p>
    
    <h4>🔬 Algorithm Overview</h4>
    <p>The reference matching algorithm follows a multi-step procedure designed to maximize accuracy while handling noisy and incomplete bibliographic data:</p>
    
    <ol>
      <li><strong>Reference Extraction:</strong> Cited references are parsed from the reference list (CR field) of each document in the collection. Each reference string is decomposed into structured components: first author surname, publication year, journal/source, volume, page, and DOI (when available).</li>
      
      <li><strong>Data Normalization:</strong> Both references and documents undergo extensive normalization to reduce variability:
        <ul>
          <li>Author names are standardized (e.g., removing accents, abbreviations, and middle initials)</li>
          <li>Journal titles are normalized using abbreviation lookup tables and string similarity methods</li>
          <li>Years, volumes, and page numbers are cleaned and formatted uniformly</li>
        </ul>
      </li>
      
      <li><strong>Blocking Strategy:</strong> To improve computational efficiency, references are grouped into blocks based on <strong>first author surname</strong> and <strong>publication year</strong>. Only references and documents within the same block are compared, reducing the search space significantly.</li>
      
      <li><strong>Similarity Computation:</strong> For each reference-document pair within a block, a <em>matching score</em> is calculated using a weighted combination of similarity measures:
        <ul>
          <li><strong>DOI matching</strong> (if available): exact match = 100% confidence</li>
          <li><strong>First author similarity:</strong> string distance (Jaro-Winkler or Levenshtein)</li>
          <li><strong>Year match:</strong> exact or within ±1 year tolerance</li>
          <li><strong>Journal/source similarity:</strong> string distance between normalized titles</li>
          <li><strong>Volume and page matching:</strong> exact or fuzzy comparison</li>
        </ul>
      </li>
      
      <li><strong>Threshold-Based Assignment:</strong> A reference is matched to a document if the combined similarity score exceeds a predefined threshold (typically 0.85–0.95). The threshold can be adjusted by the user to balance precision and recall.</li>
      
      <li><strong>Ambiguity Resolution:</strong> In cases where a reference matches multiple documents, the algorithm selects the candidate with the highest similarity score. If scores are nearly identical, the match is flagged for manual review.</li>
    </ol>
    
    <h4>💡 Usage in Biblioshiny</h4>
    <p>To perform reference matching in <strong>Biblioshiny</strong>, follow these steps:</p>
    
    <ol>
      <li><strong>Load your bibliographic collection:</strong> Ensure that your dataset includes the <code>CR</code> (Cited References) field, which is available in full exports from Web of Science, Scopus, and OpenAlex.</li>
      
      <li><strong>Navigate to the Reference Matching module:</strong> Access the module from the <em>Analysis</em> menu or the <em>Citation Network</em> section.</li>
      
      <li><strong>Configure matching parameters:</strong>
        <ul>
          <li><strong>Similarity threshold:</strong> Adjust the matching threshold to control precision (higher values = stricter matching, fewer false positives).</li>
          <li><strong>Normalization options:</strong> Enable or disable specific normalization rules (e.g., journal abbreviation matching, fuzzy year tolerance).</li>
        </ul>
      </li>
      
      <li><strong>Run the algorithm:</strong> Click <strong>Start Matching</strong> to initiate the process. Depending on the collection size, this may take several minutes.</li>
      
      <li><strong>Review results:</strong> The output includes:
        <ul>
          <li>A summary table of matched and unmatched references</li>
          <li>A list of ambiguous matches for manual inspection</li>
          <li>Network visualization options (e.g., co-citation network, historiograph)</li>
        </ul>
      </li>
      
      <li><strong>Export matched data:</strong> The matched citation network can be exported for further analysis in external tools (e.g., Gephi, Pajek) or used directly in Biblioshiny for advanced network analysis.</li>
    </ol>
    
    <h4>⚙️ Key Parameters and Options</h4>
    <ul>
      <li><strong>Matching Threshold:</strong> Minimum similarity score (0–1) required for a match. Default: 0.90. Lower values increase recall but may introduce false positives.</li>
      <li><strong>Fuzzy Year Matching:</strong> Allows matches within ±1 year (useful for handling publication date discrepancies). Default: enabled.</li>
      <li><strong>DOI Priority:</strong> When a DOI is available, it overrides other matching criteria. Default: enabled.</li>
      <li><strong>Manual Review Mode:</strong> Flags ambiguous matches (score between 0.85–0.90) for user verification. Default: disabled.</li>
    </ul>
    
    <h4>📊 Applications</h4>
    <p>Reference matching is essential for several bibliometric analyses:</p>
    <ul>
      <li><strong>Co-citation analysis:</strong> Identify documents frequently cited together, revealing intellectual structure.</li>
      <li><strong>Historiograph:</strong> Trace the historical development of research topics through citation linkages.</li>
      <li><strong>Most Cited Local Documents:</strong> Rank documents by the number of times they are cited <em>within the collection</em>.</li>
      <li><strong>Citation networks:</strong> Construct directed citation graphs for network-based metrics (PageRank, betweenness centrality).</li>
    </ul>
    
    <h4>⚠️ Important Notes</h4>
    <ul>
      <li>Reference matching quality depends heavily on the completeness and accuracy of the <code>CR</code> field in the original data export.</li>
      <li>Incomplete or poorly formatted references (e.g., missing author names, incorrect years) may result in lower matching rates.</li>
      <li>For very large collections (>10,000 documents), consider using subsets or increasing the matching threshold to improve performance.</li>
      <li>Always verify ambiguous matches manually, especially for high-stakes analyses.</li>
    </ul>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    
    <p><strong>Garfield, E. (1979).</strong> <i>Citation indexing: Its theory and application in science, technology, and humanities.</i> New York: Wiley.</p>
    
    <p><strong>Small, H. (1973).</strong> <i>Co-citation in the scientific literature: A new measure of the relationship between two documents.</i> <strong>Journal of the American Society for Information Science</strong>, 24(4), 265–269. <a href='https://doi.org/10.1002/asi.4630240406' target='_blank'>https://doi.org/10.1002/asi.4630240406</a></p>
    </div>
    </body>"
  
  importOrLoad <- 
    "<body>
    <div class='container'>
    <h3>📥 Import or Load: Building Your Bibliometric Collection</h3>
    
    <p>The <strong>Import or Load</strong> module is the starting point for any bibliometric analysis in <strong>Biblioshiny</strong>. This section allows users to build their bibliographic collection by either <em>importing raw files</em> from supported databases or <em>loading pre-processed bibliometrix files</em> saved in previous sessions.</p>
    
    <h4>📂 Three Import Options</h4>
    <p><strong>Biblioshiny</strong> offers three flexible ways to create or load a bibliographic collection:</p>
    
    <h4>1. Import Raw File(s)</h4>
    <p>Import bibliographic data directly from supported databases in their <strong>native export formats</strong>.</p>
    
    <p><strong>Supported Databases:</strong></p>
    <ul>
      <li><strong>Web of Science</strong> (.txt, .bib format)</li>
      <li><strong>Scopus</strong> (.bib, .csv format)</li>
      <li><strong>OpenAlex</strong> (via API integration or pre-downloaded files)</li>
      <li><strong>Dimensions</strong> (.csv, .xlsx format)</li>
      <li><strong>Lens</strong> (.csv format)</li>
      <li><strong>PubMed</strong> (.txt format)</li>
      <li><strong>Cochrane Library</strong> (.txt format)</li>
    </ul>
    
    <p><strong>Import Process:</strong></p>
    <ol>
      <li>Click <strong>Browse</strong> to select one or more raw export files from your computer</li>
      <li>Biblioshiny automatically detects the database format and parses the metadata</li>
      <li>The system converts the raw data into a standardized <code>bibliometrix</code> data frame</li>
      <li>A <strong>Conversion Results</strong> summary displays the number of documents successfully imported</li>
      <li>View a preview table showing key metadata fields (DOI, Authors, Title, Journal, etc.)</li>
    </ol>
    
    <p><strong>Important Notes:</strong></p>
    <ul>
      <li>Files from different databases can be merged later using the <strong>Merge Collections</strong> module</li>
      <li>For best results, export the <strong>full record with cited references</strong> from the source database</li>
      <li>Some databases (e.g., Web of Science, Scopus) have export limits—download data in batches if necessary</li>
      <li>Always check the <strong>file format requirements</strong> in the <a href='#'>Info section</a> before exporting from databases</li>
    </ul>
    
    <h4>2. Load Bibliometrix File(s)</h4>
    <p>Resume work on a previously processed collection by loading <strong>.rds</strong> or <strong>.xlsx</strong> files generated by Biblioshiny or the <code>bibliometrix</code> R package.</p>
    
    <p><strong>Use Cases:</strong></p>
    <ul>
      <li>Continue analysis from a previous session</li>
      <li>Load collections pre-processed using the <code>bibliometrix</code> R package</li>
      <li>Share standardized datasets with collaborators</li>
      <li>Work with large collections that have already undergone data cleaning and filtering</li>
    </ul>
    
    <p><strong>Supported Formats:</strong></p>
    <ul>
      <li><strong>.rds</strong>: R Data Serialization format (preserves full metadata and structure)</li>
      <li><strong>.xlsx</strong>: Excel format (compatible with bibliometrix exports)</li>
    </ul>
    
    <h4>3. Use a Sample Collection</h4>
    <p>Perfect for <strong>testing</strong> and <strong>learning</strong> Biblioshiny's features without preparing your own data.</p>
    <ul>
      <li>Select from pre-loaded example datasets covering various research domains</li>
      <li>Ideal for exploring the platform's analytical capabilities</li>
      <li>No file upload required—start analyzing immediately</li>
    </ul>
    
    <h4>🔍 Post-Import Features</h4>
    <p>After successfully importing or loading a collection, you can:</p>
    <ul>
      <li><strong>View Collection Metadata:</strong> Preview document details in a sortable, filterable table</li>
      <li><strong>Add Brief Description:</strong> Write a custom description of your collection for documentation purposes</li>
      <li><strong>Export Collection:</strong> Save your processed collection as <code>.rds</code>, <code>.xlsx</code>, or <code>.csv</code> for backup or sharing</li>
      <li><strong>Start Analysis:</strong> Click the blue <strong>Start</strong> button to proceed to filtering and analysis modules</li>
    </ul>
    
    <h4>💾 Exporting Collections</h4>
    <p>Once your collection is loaded, you can export it in multiple formats:</p>
    <ul>
      <li><strong>.rdata</strong>: Recommended for preserving all metadata and R-specific structures</li>
      <li><strong>.xlsx</strong>: Excel-compatible format for sharing with non-R users</li>
    </ul>
    
    <h4>⚠️ Best Practices</h4>
    <ul>
      <li><strong>Always save your processed collections</strong> after importing raw files to avoid re-conversion</li>
      <li><strong>Use descriptive filenames</strong> when exporting (e.g., <code>management_wos_1990-2020.rdata</code>)</li>
      <li><strong>Check conversion results carefully</strong>—some database exports may have formatting issues that require manual correction</li>
      <li><strong>For large collections (>5,000 documents)</strong>, consider applying filters early to improve performance</li>
    </ul>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    </div>
    </body>"
  
  api <- 
    "<body>
    <div class='container'>
    <h3>🌐 API: Gathering Data from Bibliographic Databases</h3>
    
    <p>The <strong>API</strong> module enables users to retrieve bibliographic data directly from supported databases using their <strong>Application Programming Interfaces (APIs)</strong>. This feature eliminates the need for manual file downloads and allows for <em>dynamic, reproducible data collection</em> based on custom search queries.</p>
    
    <br>
    
    <h4>🔌 Supported APIs</h4>
    <p><strong>Biblioshiny</strong> currently integrates with the following bibliographic database APIs:</p>
    <ul>
      <li><strong>PubMed</strong>: Access to biomedical and life sciences literature via the NCBI Entrez API</li>
      <li><strong>Dimensions</strong>: Extensive research publication data (requires institutional access or API key)</li>
    </ul>
    
    <p>Each database API has specific <strong>query capabilities</strong>, <strong>rate limits</strong>, and <strong>metadata coverage</strong>. Biblioshiny automatically handles API authentication, pagination, and data formatting.</p>
    
    <br>
    
    <h4>🔧 How to Use the API Module</h4>
    <p>The API workflow consists of three main steps:</p>
    
    <h4>Step 1: Configure API Query</h4>
    <p>Define your search parameters using the interactive query builder:</p>
    
    <ul>
      <li><strong>Select Database:</strong> Choose the target database from the dropdown menu (e.g., PubMed, OpenAlex)</li>
      <li><strong>Search Terms:</strong> Enter keywords, phrases, or Boolean queries following the database-specific syntax:
        <ul>
          <li><strong>PubMed:</strong> Supports MeSH terms, field tags (e.g., <code>[Title/Abstract]</code>), Boolean operators (<code>AND</code>, <code>OR</code>, <code>NOT</code>)</li>
          <li><strong>Dimensions:</strong> Full-text search across titles, abstracts, and author names</li>
        </ul>
      </li>
      <li><strong>Start Year / End Year:</strong> Define the temporal range for your query (e.g., 1990–2025)</li>
      <li><strong>Additional Filters:</strong> Some APIs allow filtering by document type, language, or publication venue</li>
    </ul>
    
    <p><strong>Example PubMed Query:</strong></p>
    <code>
    Search Terms: bibliometrics[Title/Abstract] AND scientometrics[Title/Abstract]<br>
    Start Year: 2010<br>
    End Year: 2025
    </code>
    
    <h4>Step 2: Test and Validate Query</h4>
    <p>Before downloading the full dataset, it's essential to validate your query:</p>
    
    <ol>
      <li>Click <strong>Try the Query</strong> to send a test request to the API</li>
      <li>The <strong>Query Translation</strong> panel shows the exact API call being executed</li>
      <li>The <strong>Documents Returned</strong> section displays the number of results matching your query (e.g., 'PubMed API returns 898 documents')</li>
      <li>If the result count is too high or too low, refine your search terms and test again</li>
    </ol>
    
    <p><strong>Important:</strong> Some APIs have result limits (e.g., PubMed allows up to 10,000 records per query). If your query exceeds the limit, consider narrowing the time range or adding more specific keywords.</p>
    
    <h4>Step 3: Download Data</h4>
    <p>Once satisfied with the query validation:</p>
    
    <ol>
      <li>Adjust the <strong>Total Documents to Download</strong> slider (range: 1 to the maximum returned by the query)</li>
      <li>Click <strong>2. Download</strong> to retrieve the bibliographic metadata</li>
      <li>The API will fetch records in batches, respecting rate limits automatically</li>
      <li>Progress is displayed in real-time (e.g., 'Fetching records 1-500...')</li>
      <li>Downloaded data is automatically converted into the <code>bibliometrix</code> format</li>
    </ol>
    
    <h4>Step 4: Export and Analyze</h4>
    <p>After successful data retrieval:</p>
    <ul>
      <li>The collection is ready for immediate analysis in Biblioshiny</li>
      <li>Use the <strong>Export a Bibliometrix File</strong> option to save the data as <code>.rds</code>, <code>.xlsx</code>, or <code>.csv</code></li>
      <li>Navigate to the <strong>Filters</strong> or <strong>Overview</strong> modules to begin your bibliometric analysis</li>
    </ul>
    
    <br>
    
    <h4>🔑 API Authentication</h4>
    <p>Some databases require <strong>API keys</strong> or <strong>institutional access</strong>:</p>
    
    <ul>
      <li><strong>PubMed:</strong> No API key required for basic usage. However, registering for an <a href='https://www.ncbi.nlm.nih.gov/account/' target='_blank'>NCBI API key</a> increases rate limits (from 3 to 10 requests/second)</li>
      <li><strong>Dimensions:</strong> Requires a valid API key or institutional subscription. Contact <a href='https://www.dimensions.ai/' target='_blank'>Digital Science</a> for access</li>
    </ul>
    
    <p>To configure API keys in Biblioshiny, navigate to <strong>Settings</strong> and enter your credentials in the <strong>API Configuration</strong> section.</p>
    
    <h4>📊 Advantages of API-Based Data Collection</h4>
    <ul>
      <li><strong>Reproducibility:</strong> Queries can be saved and re-executed to update datasets with new publications</li>
      <li><strong>Efficiency:</strong> No manual file downloads or conversions—data is ready for analysis immediately</li>
      <li><strong>Flexibility:</strong> Combine data from multiple APIs using the <strong>Merge Collections</strong> module</li>
      <li><strong>Up-to-Date Data:</strong> Retrieve the latest publications without waiting for database export files</li>
      <li><strong>Large-Scale Retrieval:</strong> Automate collection of thousands of records with a single query</li>
    </ul>
    
    <br>
    
    <h4>⚠️ Important Considerations</h4>
    <ul>
      <li><strong>Rate Limits:</strong> Respect API rate limits to avoid temporary bans. Biblioshiny automatically throttles requests, but excessive use may still trigger restrictions</li>
      <li><strong>Metadata Completeness:</strong> API-retrieved data may have fewer metadata fields than manual exports (e.g., PubMed lacks detailed affiliation data)</li>
      <li><strong>Query Syntax:</strong> Each database has unique query syntax—consult the <a href='#'>Info section</a> for database-specific guidelines</li>
      <li><strong>Network Stability:</strong> Large queries (>5,000 records) require a stable internet connection—consider splitting into smaller batches if interrupted</li>
    </ul>
    
    <br>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2023).</strong> <i>openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex.</i> <strong>R Journal</strong>, 15(4). <a href='https://doi.org/10.32614/rj-2023-089' target='_blank'>https://doi.org/10.32614/rj-2023-089</a></p>
    
    <p><strong>NCBI Resource Coordinators. (2018).</strong> <i>Database resources of the National Center for Biotechnology Information.</i> <strong>Nucleic Acids Research</strong>, 46(D1), D8–D13. <a href='https://doi.org/10.1093/nar/gkx1095' target='_blank'>https://doi.org/10.1093/nar/gkx1095</a></p>
    </div>
    </body>"
  
  mergeCollections <- 
    "<body>
    <div class='container'>
    <h3>🔀 Merge Collections: Combining Data from Multiple Sources</h3>
    
    <p>The <strong>Merge Collections</strong> module allows users to combine bibliographic datasets from different databases (Web of Science, Scopus, OpenAlex, PubMed, etc.) into a single unified collection. This functionality is essential for <strong>comprehensive literature reviews</strong>, <strong>cross-database validation</strong>, and maximizing <strong>metadata coverage</strong> by leveraging the strengths of multiple sources.</p>
    
    <h4>🎯 Why Merge Collections?</h4>
    <ul>
      <li><strong>Broader Coverage:</strong> Different databases index different journals and document types—merging increases the comprehensiveness of your dataset</li>
      <li><strong>Complementary Metadata:</strong> Scopus may provide detailed affiliation data, while Web of Science offers comprehensive citation links—combining them enriches your analysis</li>
      <li><strong>Validation:</strong> Cross-referencing records from multiple sources improves data quality and identifies discrepancies</li>
      <li><strong>Deduplication:</strong> Automatically removes duplicate records that appear in multiple databases</li>
    </ul>
    
    <h4>🔧 How to Merge Collections</h4>
    <p>The merge process in <strong>Biblioshiny</strong> is straightforward:</p>
    
    <ol>
      <li><strong>Navigate to Merge Collections:</strong> Select <strong>Data > Merge Collections</strong> from the main menu</li>
      <li><strong>Select Collection Files:</strong> Click <strong>Browse</strong> and select two or more bibliometrix files to merge:
        <ul>
          <li>Supported formats: <code>.rdata</code>, <code>.xlsx</code></li>
          <li>Files can originate from different databases (e.g., <code>wos_collection.rdata</code> + <code>scopus_collection.xlsx</code>)</li>
          <li>Files must be valid bibliometrix data frames (created via Import or Load, or R package)</li>
        </ul>
      </li>
      <li><strong>Configure Merge Options:</strong>
        <ul>
          <li><strong>Remove Duplicates:</strong> Enable (recommended) to automatically detect and remove duplicate records</li>
          <li><strong>Verbose Output:</strong> Enable to display detailed information about the merge process and duplicates removed</li>
        </ul>
      </li>
      <li><strong>Click Start:</strong> The merge algorithm combines the collections, standardizes metadata fields, and removes duplicates</li>
      <li><strong>Review Results:</strong> A summary displays the total number of documents and how many duplicates were removed</li>
      <li><strong>Export Merged Collection:</strong> Save the unified dataset for future analysis</li>
    </ol>
    
    <h4>🔬 Merge Algorithm Overview</h4>
    <p>The merge process follows a sophisticated multi-stage algorithm implemented by the <code>mergeDbSources()</code> function:</p>
    
    <h4>Stage 1: Database Identification and Ordering</h4>
    <ul>
      <li>Each collection is tagged with its source database (<code>DB</code> field: ISI, SCOPUS, OPENALEX, LENS, DIMENSIONS, PUBMED, COCHRANE)</li>
      <li>Collections are ordered by database priority to preserve the most reliable metadata when conflicts arise</li>
      <li>Order: Web of Science (ISI) > Scopus > OpenAlex > Lens > Dimensions > PubMed > Cochrane</li>
    </ul>
    
    <h4>Stage 2: Field Alignment</h4>
    <ul>
      <li>Common metadata fields are identified and aligned across databases (e.g., <code>TI</code> = Title, <code>AU</code> = Authors, <code>DI</code> = DOI)</li>
      <li>Database-specific fields are preserved when possible</li>
      <li>Missing fields in one database are filled from another when duplicates are detected</li>
      <li>A unified <code>KW_Merged</code> field is created by combining keywords from all sources</li>
    </ul>
    
    <h4>Stage 3: Duplicate Detection</h4>
    <p>Duplicates are identified using a <strong>two-step matching strategy</strong>:</p>
    
    <p><strong>Step 1: DOI-Based Matching</strong></p>
    <ul>
      <li>Documents with identical DOIs are flagged as duplicates</li>
      <li>This is the most reliable method, as DOIs are unique identifiers</li>
      <li>Empty or missing DOIs (<code>''</code> or <code>NA</code>) are ignored to avoid false positives</li>
      <li>Only the first occurrence is retained; subsequent matches are removed</li>
    </ul>
    
    <p><strong>Step 2: Title-Year Matching</strong></p>
    <ul>
      <li>For records without DOIs, duplicates are detected using <strong>normalized titles</strong> and <strong>publication years</strong></li>
      <li><strong>Title Normalization:</strong>
        <ul>
          <li>Remove all punctuation and special characters</li>
          <li>Convert to lowercase</li>
          <li>Remove extra whitespace</li>
          <li>Example: 'Science Mapping: A Review' → 'science mapping a review'</li>
        </ul>
      </li>
      <li><strong>Matching Criterion:</strong> Two documents are duplicates if they have:
        <ul>
          <li>Identical normalized titles <strong>AND</strong></li>
          <li>Identical publication years (<code>PY</code>)</li>
        </ul>
      </li>
      <li>This method captures ~95% of duplicates but may miss records with minor title variations</li>
    </ul>
    
    <h4>Stage 4: Author Name Standardization</h4>
    <p>When merging collections from multiple databases, author name formats are standardized:</p>
    <ul>
      <li>Format: <code>LASTNAME INITIALS</code> (e.g., 'Aria M; Cuccurullo C')</li>
      <li>Commas in author names are removed to ensure consistency</li>
      <li>Middle initials are condensed to single letters</li>
      <li>This standardization improves author-based analyses (e.g., collaboration networks, productivity rankings)</li>
    </ul>
    
    <h4>Stage 5: Metadata Integration</h4>
    <ul>
      <li>The <code>DB_Original</code> field stores each document's source database</li>
      <li>The <code>DB</code> field is set to 'ISI' (Web of Science format) for compatibility with downstream analyses</li>
      <li>Cited references (<code>CR</code>) are preserved but stored in <code>CR_raw</code> to allow re-processing later</li>
      <li>A unique <code>SR</code> (Short Reference) identifier is generated for each document</li>
    </ul>
    
    <h4>📊 Merge Statistics and Validation</h4>
    <p>After merging, the system provides detailed statistics:</p>
    <ul>
      <li><strong>Total Documents Before Merge:</strong> Sum of all input collections</li>
      <li><strong>Duplicates Removed:</strong> Number of records eliminated (broken down by DOI matches and title-year matches)</li>
      <li><strong>Total Documents After Merge:</strong> Final collection size</li>
      <li><strong>Coverage by Database:</strong> Proportion of documents from each source (visible in <code>DB_Original</code> field)</li>
    </ul>
    
    <p><strong>Example Output:</strong></p>
    <code>
    Merging 3 collections:<br>
    - WoS: 1,500 documents<br>
    - Scopus: 1,800 documents<br>
    - OpenAlex: 2,000 documents<br>
    Total: 5,300 documents<br>
    <br>
    Removing duplicates...<br>
    - 450 duplicates removed by DOI<br>
    - 320 duplicates removed by title-year match<br>
    <br>
    Final collection: 4,530 documents
    </code>
    
    <h4>📌 Best Practices</h4>
    <ul>
      <li><strong>Always enable duplicate removal</strong> unless you have a specific reason to retain duplicates</li>
      <li><strong>Prioritize Web of Science or Scopus</strong> as the primary source—these databases generally have the most complete metadata</li>
      <li><strong>Use OpenAlex to supplement coverage</strong> for open-access publications or gray literature</li>
      <li><strong>Validate merge results</strong> by checking the distribution of <code>DB_Original</code> values—extreme imbalances may indicate incomplete data from one source</li>
      <li><strong>Save merged collections immediately</strong> to avoid re-processing</li>
    </ul>
    
    <h4>⚠️ Important Considerations</h4>
    <ul>
      <li><strong>Citation Data:</strong> Merged collections reset the <code>CR</code> (Cited References) field—you'll need to run <strong>Reference Matching</strong> again after merging</li>
      <li><strong>Field Coverage:</strong> Some databases provide richer metadata than others—merging doesn't 'fill in' missing fields unless duplicates are detected</li>
      <li><strong>Large Collections:</strong> Merging collections >10,000 documents may take several minutes—be patient and avoid interrupting the process</li>
      <li><strong>Database-Specific Analyses:</strong> Some analyses are database-specific—merged collections may lose this granularity</li>
    </ul>
    
    <h4>🔍 Example Use Cases</h4>
    <ul>
      <li><strong>Systematic Literature Review:</strong> Combine Web of Science, Scopus, and PubMed to ensure no relevant publications are missed</li>
      <li><strong>Open Science Research:</strong> Merge OpenAlex with traditional databases to include preprints and institutional repositories</li>
      <li><strong>Validation Study:</strong> Compare overlap between databases to assess index coverage and bias</li>
      <li><strong>Longitudinal Analysis:</strong> Merge historical Web of Science data with recent OpenAlex records to extend temporal coverage</li>
    </ul>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    
    <p><strong>Visser, M., van Eck, N. J., & Waltman, L. (2021).</strong> <i>Large-scale comparison of bibliographic data sources: Scopus, Web of Science, Dimensions, Crossref, and Microsoft Academic.</i> <strong>Quantitative Science Studies</strong>, 2(1), 20–41. <a href='https://doi.org/10.1162/qss_a_00112' target='_blank'>https://doi.org/10.1162/qss_a_00112</a></p>
    
    <p><strong>Martín-Martín, A., Thelwall, M., Orduna-Malea, E., & Delgado López-Cózar, E. (2021).</strong> <i>Google Scholar, Microsoft Academic, Scopus, Dimensions, Web of Science, and OpenCitations' COCI: a multidisciplinary comparison of coverage via citations.</i> <strong>Scientometrics</strong>, 126(1), 871–906. <a href='https://doi.org/10.1007/s11192-020-03690-4' target='_blank'>https://doi.org/10.1007/s11192-020-03690-4</a></p>
    </div>
    </body>"
  
  mainInformation <- 
    "<body>
  <div class='container'>
  <h3>📊 Main Information: Overview of Your Bibliometric Collection</h3>
  
  <p>The <strong>Main Information</strong> page provides a comprehensive, at-a-glance summary of the key bibliometric indicators for your collection. This dashboard-style interface displays <strong>12 core metrics</strong> organized into visual cards, allowing you to quickly assess the scope, composition, and characteristics of your dataset.</p>
  
  <p>This section is the ideal starting point for understanding your collection before diving into more detailed analyses. It answers fundamental questions such as: <em>How large is my dataset? What is the temporal coverage? How collaborative is the research? How impactful are the documents?</em></p>
  
    <br>
    
  <h4>📈 Core Metrics Explained</h4>
  <p>The Main Information dashboard displays the following indicators:</p>
  
  <h4>1. Timespan</h4>
  <ul>
    <li><strong>Definition:</strong> The temporal range covered by the collection, from the earliest to the most recent publication year.</li>
    <li><strong>Example:</strong> <code>1985-2020</code> indicates documents published between 1985 and 2020.</li>
    <li><strong>Interpretation:</strong> A wider timespan enables longitudinal trend analysis and historical perspectives. Collections spanning decades are suitable for studying research evolution and paradigm shifts.</li>
  </ul>
  
  <h4>2. Sources</h4>
  <ul>
    <li><strong>Definition:</strong> The total number of distinct publication venues (journals, conferences, books) represented in the collection.</li>
    <li><strong>Interpretation:</strong> A higher number of sources suggests a <em>multidisciplinary</em> or <em>dispersed research field</em>, while a lower number indicates concentration in a few core journals. This metric is useful for identifying dominant publication venues via Bradford's Law analysis.</li>
  </ul>
  
  <h4>3. Documents</h4>
  <ul>
    <li><strong>Definition:</strong> The total number of bibliographic records (articles, reviews, proceedings, etc.) in the collection.</li>
    <li><strong>Interpretation:</strong> This is the fundamental sample size for all subsequent analyses. Larger collections (>1,000 documents) provide more robust insights, especially for network and clustering analyses.</li>
  </ul>
  
  <h4>4. Annual Growth Rate</h4>
  <ul>
    <li><strong>Definition:</strong> The average percentage increase in the number of publications per year over the collection's timespan.</li>
    <li><strong>Formula:</strong> Compound Annual Growth Rate (CAGR) calculated as: <code>[(N_final / N_initial)^(1/years) - 1] × 100</code></li>
    <li><strong>Interpretation:</strong> A positive growth rate indicates an <em>expanding research field</em>, while negative or near-zero values suggest maturity or decline. High growth rates (>10%) often signal <strong>emerging topics</strong> attracting increasing scholarly attention.</li>
  </ul>
  
  <h4>5. Authors</h4>
  <ul>
    <li><strong>Definition:</strong> The total number of unique authors who contributed to the documents in the collection.</li>
    <li><strong>Interpretation:</strong> This metric reflects the size of the research community. A high author-to-document ratio suggests <em>collaborative research</em>, while a low ratio may indicate a field dominated by a few prolific researchers.</li>
  </ul>
  
  <h4>6. Authors of Single-Authored Docs</h4>
  <ul>
    <li><strong>Definition:</strong> The number of authors who published at least one single-authored document in the collection.</li>
    <li><strong>Interpretation:</strong> Single-authored papers are more common in humanities and theoretical disciplines. A low proportion suggests <em>high collaboration intensity</em>, typical of experimental sciences and interdisciplinary fields.</li>
  </ul>
  
  <h4>7. International Co-Authorship</h4>
  <ul>
    <li><strong>Definition:</strong> The percentage of documents authored by researchers from multiple countries.</li>
    <li><strong>Interpretation:</strong> High international collaboration (>30%) indicates <em>global research networks</em> and is often associated with higher citation impact. This metric is a proxy for research globalization and cross-border knowledge exchange.</li>
  </ul>
  
  <h4>8. Co-Authors per Document</h4>
  <ul>
    <li><strong>Definition:</strong> The average number of authors per document in the collection.</li>
    <li><strong>Interpretation:</strong> Values typically range from 2 (social sciences, humanities) to 5+ (biomedical sciences, physics). Increasing values over time reflect the trend toward <strong>team science</strong> and large-scale collaborative projects.</li>
  </ul>
  
  <h4>9. Author's Keywords (DE)</h4>
  <ul>
    <li><strong>Definition:</strong> The total number of unique keywords provided by authors (<code>DE</code> = <em>Descriptors</em>) across all documents.</li>
    <li><strong>Interpretation:</strong> A rich keyword set (>1,000 unique terms) enables robust thematic analysis and topic modeling. The diversity of keywords reflects the <em>conceptual breadth</em> of the research field.</li>
  </ul>
  
  <h4>10. References</h4>
  <ul>
    <li><strong>Definition:</strong> The total number of cited references listed in the bibliographies of all documents in the collection.</li>
    <li><strong>Interpretation:</strong> This metric is essential for citation-based analyses (co-citation, bibliographic coupling, reference publication year spectroscopy). Larger reference pools enable more comprehensive intellectual structure mapping.</li>
  </ul>
  
  <h4>11. Document Average Age</h4>
  <ul>
    <li><strong>Definition:</strong> The average number of years elapsed since publication, calculated relative to the current year.</li>
    <li><strong>Formula:</strong> <code>Current Year - Mean(Publication Years)</code></li>
    <li><strong>Interpretation:</strong> Lower values (<5 years) indicate a focus on <em>recent research</em>, while higher values suggest inclusion of foundational or historical literature. This metric helps assess whether the collection is <strong>contemporary</strong> or <strong>retrospective</strong>.</li>
  </ul>
  
  <h4>12. Average Citations per Document</h4>
  <ul>
    <li><strong>Definition:</strong> The mean number of citations received by documents in the collection (based on database citation counts).</li>
    <li><strong>Interpretation:</strong> Higher values indicate <em>high-impact research</em>. Average citation rates vary widely by discipline (e.g., biomedical sciences >20, social sciences ~10). This metric is influenced by document age, field norms, and database coverage.</li>
  </ul>
  
    <br>
    
  <h4>🧠 Biblio AI Integration</h4>
  <p>If <strong>Biblio AI</strong> is enabled, you can click the <strong>Biblio AI</strong> tab to receive an <em>automated narrative summary</em> of these indicators. The AI-generated text provides contextualized interpretations, highlights notable patterns, and offers insights suitable for inclusion in research reports or presentations.</p>
  
  <p><strong>Example AI-generated insights:</strong></p>
  <ul>
    <li>'The collection exhibits a strong annual growth rate of 14.05%, suggesting an emerging and rapidly expanding research domain.'</li>
    <li>'With 36.41% international co-authorship, the field demonstrates moderate global collaboration, indicating opportunities for further cross-border partnerships.'</li>
    <li>'The average of 37.12 citations per document reflects high scholarly impact, placing this collection above typical citation rates for the social sciences.'</li>
  </ul>
  
    <br>
    
  <h4>📋 Viewing Options</h4>
  <p>The Main Information page offers three viewing modes via tabs at the top:</p>
  <ul>
    <li><strong>Plot:</strong> Visual card-based dashboard (default view) with color-coded metrics</li>
    <li><strong>Table:</strong> Tabular representation of all indicators for easy export to reports</li>
    <li><strong>Biblio AI:</strong> AI-generated narrative summary and interpretation (requires Gemini API key)</li>
  </ul>
  
    <br>
    
  <h4>💡 How to Use Main Information</h4>
  <p>This section is designed for multiple purposes:</p>
  <ul>
    <li><strong>Initial Data Assessment:</strong> Quickly validate that your collection has been imported correctly and contains the expected number of documents and metadata fields.</li>
    <li><strong>Research Reporting:</strong> Extract summary statistics for the 'Methods' or 'Data' section of a systematic review or bibliometric study.</li>
    <li><strong>Comparative Analysis:</strong> Compare indicators across different datasets (e.g., two time periods, competing research streams) to identify differences in growth, collaboration, or impact.</li>
    <li><strong>Presentation Material:</strong> Export the dashboard or AI-generated text for use in slides, posters, or grant proposals.</li>
  </ul>
  
    <br>
    
  <h4>📌 Best Practices</h4>
  <ul>
    <li><strong>Always review Main Information first</strong> before proceeding to advanced analyses—it helps identify potential data quality issues (e.g., missing years, incomplete author data).</li>
    <li><strong>Compare with field benchmarks:</strong> Contextualize your indicators by comparing them with known norms for your discipline (e.g., citation rates, collaboration patterns).</li>
    <li><strong>Document your collection:</strong> Use the 'Brief Description' text box (visible in the Import/Load section) to record search queries, inclusion criteria, and data sources for reproducibility.</li>
    <li><strong>Export summary statistics:</strong> Save the table view as a reference for your research documentation or supplementary materials.</li>
  </ul>
  
    <br>
    
  <h4>⚠️ Important Considerations</h4>
  <ul>
    <li><strong>Database Bias:</strong> Indicators reflect the coverage and indexing policies of the source database(s). Web of Science and Scopus have different journal lists, which affects metrics like citation counts and international co-authorship.</li>
    <li><strong>Citation Lag:</strong> Recent documents (<2 years old) typically have lower citation counts due to insufficient time for accumulation. Average citations per document may be biased downward if your collection includes many recent papers.</li>
    <li><strong>Incomplete Metadata:</strong> Some databases (e.g., PubMed, Dimensions) provide limited metadata, which may result in missing or incomplete values for certain indicators (e.g., author affiliations for international co-authorship calculation).</li>
    <li><strong>Growth Rate Sensitivity:</strong> Annual growth rate calculations are sensitive to the start and end years of the collection. Unusual spikes or drops in specific years can distort the overall trend.</li>
  </ul>
  
    <br>
    
  <h4>🔍 Next Steps</h4>
  <p>After reviewing the Main Information dashboard, proceed to more detailed analyses:</p>
  <ul>
    <li><strong>Filters:</strong> Refine your collection by applying metadata filters (e.g., document type, time range, subject category)</li>
    <li><strong>Sources:</strong> Identify the most productive journals and analyze publication patterns</li>
    <li><strong>Authors:</strong> Examine author productivity, collaboration networks, and impact metrics</li>
    <li><strong>Conceptual Structure:</strong> Explore thematic evolution and topic clustering via keyword co-occurrence and thematic maps</li>
    <li><strong>Intellectual Structure:</strong> Investigate citation networks through co-citation analysis and historiography</li>
  </ul>
  
    <br>
    
  <h4>📚 References</h4>
  <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
  
  <p><strong>Zupic, I., & Čater, T. (2015).</strong> <i>Bibliometric methods in management and organization.</i> <strong>Organizational Research Methods</strong>, 18(3), 429–472. <a href='https://doi.org/10.1177/1094428114562629' target='_blank'>https://doi.org/10.1177/1094428114562629</a></p>
  </div>
  </body>"
  
  lifeCycle <- 
    "<body>
  <div class='container'>
  <h3>📈 Life Cycle of Scientific Production: Modeling Research Topic Evolution</h3>
  
  <p>The <strong>Life Cycle of Scientific Production</strong> module implements a <strong>logistic growth model</strong> to analyze the temporal dynamics of research topics. This approach, grounded in the theory of <em>scientific paradigms</em> and <em>innovation diffusion</em>, allows researchers to identify the current developmental stage of a field, predict future trends, and estimate when a topic will reach maturity or saturation.</p>
  
  <p>By fitting a logistic curve to the annual publication counts in your collection, this analysis reveals whether a research area is in its <strong>emergence phase</strong>, <strong>rapid growth phase</strong>, <strong>maturity phase</strong>, or <strong>decline phase</strong>.</p>
  
  <br>
  
  <h4>📐 The Logistic Growth Model</h4>
  <p>The life cycle analysis is based on the <strong>logistic growth function</strong>, which models how the cumulative number of publications evolves over time:</p>
  
  <p><strong>Formula:</strong></p>
  <code>
  P(t) = K / (1 + exp(-b(t - t₀)))
  </code>
  
  <p>Where:</p>
  <ul>
    <li><strong>P(t)</strong>: Cumulative number of publications at time <code>t</code></li>
    <li><strong>K</strong>: Saturation level (maximum total publications the topic will produce)</li>
    <li><strong>b</strong>: Growth rate parameter (determines the steepness of the curve)</li>
    <li><strong>t₀</strong>: Inflection point (time when growth rate is highest)</li>
  </ul>
  
  <p>The <strong>annual publication rate</strong> is derived as the first derivative of P(t), producing a bell-shaped curve that peaks at the inflection point and gradually declines as the topic approaches saturation.</p>
  
  <br>
  
  <h4>🔬 Model Overview: Key Parameters</h4>
  <p>The <strong>Model Overview</strong> section displays four fundamental indicators derived from the fitted logistic model:</p>
  
  <h4>1. Saturation (K)</h4>
  <ul>
    <li><strong>Definition:</strong> The estimated maximum total number of publications that will ever be produced on this research topic.</li>
    <li><strong>Interpretation:</strong> 
      <ul>
        <li>High K values (>5,000) indicate a <em>broad, impactful research domain</em> with sustained long-term interest.</li>
        <li>Low K values (<1,000) suggest a <em>niche topic</em> with limited scope or a specialized subtopic within a larger field.</li>
        <li>The current cumulative total as a percentage of K reveals how close the topic is to exhaustion.</li>
      </ul>
    </li>
    <li><strong>Example:</strong> K = 8,980 publications suggests the topic will produce approximately 8,980 total documents before reaching saturation.</li>
  </ul>
  
  <h4>2. Peak Year (T<sub>m</sub>)</h4>
  <ul>
    <li><strong>Definition:</strong> The year when annual publication output is predicted to reach its maximum.</li>
    <li><strong>Interpretation:</strong>
      <ul>
        <li>If the peak year is in the <strong>future</strong>, the topic is still in a <em>growth phase</em> and attracting increasing attention.</li>
        <li>If the peak year is in the <strong>past</strong>, the topic has entered a <em>maturity or decline phase</em>, with decreasing annual output.</li>
        <li>If the peak year is <strong>near the present</strong>, the topic is at the <em>zenith of its popularity</em>.</li>
      </ul>
    </li>
    <li><strong>Example:</strong> Peak Year = 2029 indicates the topic will reach maximum annual productivity in 2029, suggesting it is currently in an accelerating growth phase.</li>
  </ul>
  
  <h4>3. Peak Annual</h4>
  <ul>
    <li><strong>Definition:</strong> The maximum number of publications per year predicted to occur at the Peak Year.</li>
    <li><strong>Interpretation:</strong> This metric reflects the <em>intensity of research activity</em> at the topic's peak. Higher values indicate greater scholarly attention and resource allocation.</li>
    <li><strong>Example:</strong> Peak Annual = 592 pubs/year means the topic will generate approximately 592 publications annually at its zenith.</li>
  </ul>
  
  <h4>4. Growth Duration (Δ<sub>t</sub>)</h4>
  <ul>
    <li><strong>Definition:</strong> The estimated time span (in years) from the topic's emergence (10% of K) to near-saturation (90% of K).</li>
    <li><strong>Interpretation:</strong>
      <ul>
        <li><strong>Short duration (<10 years):</strong> Rapid maturation, typical of <em>hot topics</em>, technological innovations, or crisis-driven research (e.g., COVID-19 studies).</li>
        <li><strong>Medium duration (10-20 years):</strong> Typical of <em>mainstream research domains</em> with sustained but gradual growth.</li>
        <li><strong>Long duration (>20 years):</strong> Slow-developing fields, foundational topics, or interdisciplinary areas requiring extensive infrastructure.</li>
      </ul>
    </li>
    <li><strong>Example:</strong> Growth Duration = 16.7 years suggests the topic will take approximately 17 years to mature from its early stage to near-saturation.</li>
  </ul>
  
  <br>
  
  <h4>✅ Model Fit Quality</h4>
  <p>The <strong>Model Fit Quality</strong> section assesses how well the logistic curve fits the observed publication data using four statistical metrics:</p>
  
  <h4>1. R² (Coefficient of Determination)</h4>
  <ul>
    <li><strong>Range:</strong> 0 to 1 (higher is better)</li>
    <li><strong>Interpretation:</strong> Proportion of variance in publication counts explained by the model.
      <ul>
        <li><strong>R² > 0.90:</strong> Excellent fit—the logistic model accurately captures the publication trend.</li>
        <li><strong>0.70 < R² < 0.90:</strong> Good fit—the model is reasonable but may not capture all nuances (e.g., fluctuations due to external events).</li>
        <li><strong>R² < 0.70:</strong> Poor fit—the logistic model may not be appropriate for this dataset (non-logistic growth pattern, data quality issues).</li>
      </ul>
    </li>
    <li><strong>Example:</strong> R² = 0.953 indicates an excellent fit, with 95.3% of publication variance explained by the model.</li>
  </ul>
  
  <h4>2. RMSE (Root Mean Squared Error)</h4>
  <ul>
    <li><strong>Definition:</strong> Average deviation between observed and predicted annual publications.</li>
    <li><strong>Interpretation:</strong> Lower values indicate better fit. RMSE should be interpreted relative to the scale of annual publications (e.g., RMSE = 10 is negligible for topics with 500+ annual pubs, but significant for topics with <50 pubs/year).</li>
  </ul>
  
  <h4>3. AIC (Akaike Information Criterion)</h4>
  <ul>
    <li><strong>Purpose:</strong> Balances model fit against complexity (penalizes overfitting).</li>
    <li><strong>Interpretation:</strong> Lower AIC values indicate a better model. AIC is most useful for <em>comparing alternative models</em> rather than assessing absolute fit quality.</li>
  </ul>
  
  <h4>4. BIC (Bayesian Information Criterion)</h4>
  <ul>
    <li><strong>Purpose:</strong> Similar to AIC but applies a stronger penalty for model complexity.</li>
    <li><strong>Interpretation:</strong> Lower BIC values indicate better models. BIC is more conservative than AIC and favors simpler models.</li>
  </ul>
  
  <p><strong>Overall Assessment:</strong> Biblioshiny automatically classifies model fit as <strong>Excellent</strong>, <strong>Good</strong>, or <strong>Poor</strong> based primarily on R² values. An 'Excellent' fit (R² > 0.90) validates the use of logistic growth assumptions for forecasting.</p>
  
  <br>
  
  <h4>📍 Current Status</h4>
  <p>This section provides a snapshot of the topic's present state relative to its life cycle trajectory:</p>
  
  <ul>
    <li><strong>Last Observed Year:</strong> The most recent year with publication data in your collection.</li>
    <li><strong>Annual Publications:</strong> The number of publications in the last observed year.</li>
    <li><strong>Cumulative Total:</strong> The total number of publications from the collection's start to the last observed year.</li>
    <li><strong>Progress to Saturation:</strong> The percentage of K (saturation level) already reached. 
      <ul>
        <li><strong>0-30%:</strong> Emergence or early growth phase.</li>
        <li><strong>30-70%:</strong> Rapid growth phase (the topic is 'hot').</li>
        <li><strong>70-90%:</strong> Late growth phase, approaching maturity.</li>
        <li><strong>>90%:</strong> Maturity or decline phase, nearing exhaustion.</li>
      </ul>
    </li>
  </ul>
  
  <p><strong>Example Interpretation:</strong> If Progress to Saturation = 10.0%, the topic is in the <strong>rapid growth phase</strong>, with 90% of its publication potential still ahead. This signals a <em>promising emerging field</em> attracting increasing scholarly attention.</p>
  
  <br>
  
  <h4>🏁 Milestone Years</h4>
  <p>The <strong>Milestone Years</strong> section predicts when the topic will reach specific saturation thresholds:</p>
  
  <ul>
    <li><strong>10% of K:</strong> Emergence milestone—marks the topic's transition from niche to recognized research area.</li>
    <li><strong>50% of K (Midpoint):</strong> The inflection point where growth rate is highest. This coincides with the Peak Year (T<sub>m</sub>).</li>
    <li><strong>90% of K:</strong> Maturity milestone—indicates the topic is approaching saturation, with declining annual growth.</li>
    <li><strong>99% of K:</strong> Near-complete saturation—the topic has exhausted most of its research potential.</li>
  </ul>
  
  <p><strong>Example:</strong></p>
  <code>
  10% of K: 2021.0<br>
  50% of K: 2029.3 (+9 years)<br>
  90% of K: 2037.6 (+18 years)<br>
  99% of K: 2046.7 (+27 years)
  </code>
  
  <p>This indicates the topic emerged around 2021, will peak in 2029, and approach saturation by 2038, with a full life cycle spanning approximately 25 years.</p>
  
  <p>The system also classifies the topic's current phase (e.g., <strong>'rapid growth phase'</strong> if between 10-50% of K) to aid interpretation.</p>
  
  <br>
  
  <h4>🚀 Forecast</h4>
  <p>The <strong>Forecast</strong> section projects future publication output based on the fitted logistic model:</p>
  
  <ul>
    <li><strong>Forecast Period:</strong> The time range for predictions (typically 5-50 years into the future).</li>
    <li><strong>Projection for 2025:</strong> Estimated cumulative total publications by 2025 (includes annual projection in parentheses).</li>
    <li><strong>Projection for 2030:</strong> Estimated cumulative total publications by 2030 (includes annual projection in parentheses).</li>
  </ul>
  
  <p><strong>Example:</strong></p>
  <code>
  Projection for 2025: 2183 cumulative (436 annual)<br>
  Projection for 2030: 4898 cumulative (587 annual)
  </code>
  
  <p>This suggests the topic will grow from ~900 publications (current) to over 4,800 by 2030, with annual output peaking around 587 publications per year.</p>
  
  <p><strong>Important:</strong> Forecasts assume the logistic model remains valid (no disruptive events, paradigm shifts, or external shocks). Long-term forecasts (>10 years) should be interpreted with caution.</p>
  
  <br>
  
  <h4>📊 Visualizations</h4>
  <p>The <strong>Plot</strong> tab provides two complementary graphs:</p>
  
  <h4>1. Life Cycle - Annual Publications</h4>
  <ul>
    <li><strong>Blue solid line:</strong> Logistic fit to observed data</li>
    <li><strong>Blue dashed line:</strong> Forecasted annual publications</li>
    <li><strong>Blue dots:</strong> Observed annual publications from your collection</li>
    <li><strong>Red dashed vertical line:</strong> Peak Year (T<sub>m</sub>)</li>
  </ul>
  
  <p><strong>Interpretation:</strong> This bell-shaped curve shows how publication activity rises, peaks, and eventually declines. The shape reveals the topic's maturity:</p>
  <ul>
    <li><strong>Steep ascent, pre-peak:</strong> Emerging or rapidly growing topic.</li>
    <li><strong>Near or at peak:</strong> Mature topic at maximum attention.</li>
    <li><strong>Descending curve, post-peak:</strong> Declining topic losing relevance.</li>
  </ul>
  
  <h4>2. Cumulative Growth Curve</h4>
  <ul>
    <li><strong>Green solid line:</strong> Logistic fit to observed cumulative data</li>
    <li><strong>Green dashed line:</strong> Forecasted cumulative publications</li>
    <li><strong>Green dots:</strong> Observed cumulative publications</li>
    <li><strong>Horizontal dashed lines:</strong> Saturation thresholds (50%, 90%)</li>
  </ul>
  
  <p><strong>Interpretation:</strong> This S-shaped curve illustrates the topic's total knowledge accumulation over time. The curve's position and steepness reveal:</p>
  <ul>
    <li><strong>Lower left (shallow slope):</strong> Emergence phase with slow initial growth.</li>
    <li><strong>Middle (steep slope):</strong> Rapid growth phase with exponential accumulation.</li>
    <li><strong>Upper right (flattening):</strong> Maturity phase approaching saturation asymptote (K).</li>
  </ul>
  
  <br>
  
  <h4>🧠 Biblio AI Integration</h4>
  <p>The <strong>Biblio AI</strong> tab allows you to generate AI-powered narrative interpretations of the life cycle analysis. Key features include:</p>
  
  <ul>
    <li><strong>Customizable Prompts:</strong> Edit the default prompt to add context-specific details (e.g., research domain, database source, filter criteria).</li>
    <li><strong>Graph-Based Analysis:</strong> Biblio AI analyzes the visualizations to identify trends, anomalies, and key transition points.</li>
    <li><strong>Automatic Interpretation:</strong> Generates text suitable for research reports, explaining model parameters, growth phases, and forecasts in natural language.</li>
  </ul>
  
  <p><strong>Example Prompt Enhancement:</strong></p>
  <code>
  The analysis was performed on a collection downloaded from WOS focusing on machine learning applications in healthcare from 1990-2020.
  </code>
  
  <p>This contextual information helps Biblio AI produce more accurate and domain-relevant interpretations.</p>
  
  <h4>💡 Use Cases</h4>
  <ul>
    <li><strong>Identifying Emerging Topics:</strong> Detect rapidly growing fields in their early stages (10-30% of K) for strategic research investment.</li>
    <li><strong>Timing Research Entry:</strong> Avoid entering saturated fields (>90% of K) where novelty is harder to achieve.</li>
    <li><strong>Forecasting Resource Needs:</strong> Predict future publication volumes to plan journal submissions, conferences, or funding opportunities.</li>
    <li><strong>Comparative Life Cycle Analysis:</strong> Run the analysis on multiple subtopics to identify which are growing vs. declining.</li>
    <li><strong>Paradigm Shift Detection:</strong> Poor model fit (R² < 0.70) may signal non-logistic patterns caused by disruptive innovations or paradigm shifts.</li>
  </ul>
  
  <br>
  
  <h4>📌 Best Practices</h4>
  <ul>
    <li><strong>Ensure sufficient data:</strong> Logistic models require at least 10-15 years of publication data for reliable fitting. Collections with <10 years may produce unstable forecasts.</li>
    <li><strong>Check model fit:</strong> Always review R² and visual fit before interpreting forecasts. Poor fits (R² < 0.70) indicate the logistic model may not be appropriate.</li>
    <li><strong>Consider external events:</strong> The model assumes smooth, uninterrupted growth. Real-world shocks (e.g., pandemics, funding cuts, technological breakthroughs) can invalidate long-term forecasts.</li>
    <li><strong>Use relative comparisons:</strong> Life cycle parameters (K, Peak Year) are most informative when comparing multiple topics or time periods within the same field.</li>
    <li><strong>Validate forecasts periodically:</strong> Re-run the analysis with updated data every 2-3 years to recalibrate predictions.</li>
  </ul>
  
  <br>
  
  <h4>⚠️ Important Considerations</h4>
  <ul>
    <li><strong>Database Coverage:</strong> The model reflects only publications indexed in your source database(s). Incomplete coverage (e.g., missing journals, preprints) can distort saturation estimates.</li>
    <li><strong>Definition Drift:</strong> Topic boundaries may shift over time (e.g., 'artificial intelligence' in 1990 vs. 2020), affecting the validity of K estimates.</li>
    <li><strong>Multiple Life Cycles:</strong> Some broad topics exhibit <em>multiple overlapping life cycles</em> as subtopics emerge and decline independently. In such cases, aggregate logistic fits may be misleading.</li>
    <li><strong>Self-Fulfilling Prophecies:</strong> Publishing forecasts may influence researcher behavior (e.g., avoiding 'saturated' topics), potentially altering actual trajectories.</li>
    <li><strong>Model Limitations:</strong> The logistic model assumes a single saturation point and smooth growth. Topics experiencing <em>resurgence</em> (e.g., due to new technologies) may not fit this pattern.</li>
  </ul>
  
  <br>
  
  <h4>🔍 Interpreting Fit Quality Issues</h4>
  <p>If your model shows poor fit (R² < 0.70), consider these potential causes:</p>
  <ul>
    <li><strong>Insufficient Data:</strong> Too few years or highly irregular publication patterns.</li>
    <li><strong>Non-Logistic Growth:</strong> The topic may exhibit exponential, linear, or cyclic growth rather than logistic.</li>
    <li><strong>Recent Disruptions:</strong> External shocks (e.g., COVID-19 boosting health research) create anomalies that deviate from smooth curves.</li>
    <li><strong>Topic Too Broad:</strong> Aggregating multiple subtopics with different life cycles can obscure individual patterns.</li>
    <li><strong>Data Quality Issues:</strong> Missing years, database indexing changes, or inconsistent metadata.</li>
  </ul>
  
  <p><strong>Solution:</strong> Try narrowing your collection (e.g., focusing on a specific subtopic or time range) or exploring alternative growth models.</p>
  
  <br>
  
  <h4>📚 References</h4>
  <p><strong>Aria, M., Misuraca, M., & Spano, M. (2020).</strong> <i>Mapping the evolution of social research and data science on 30 years of Social Indicators Research.</i> <strong>Social Indicators Research</strong>, 149, 803–831. <a href='https://doi.org/10.1007/s11205-020-02281-3' target='_blank'>https://doi.org/10.1007/s11205-020-02281-3</a></p>
  
  <p><strong>Bettencourt, L. M., Kaiser, D. I., & Kaur, J. (2009).</strong> <i>Scientific discovery and topological transitions in collaboration networks.</i> <strong>Journal of Informetrics</strong>, 3(3), 210–221. <a href='https://doi.org/10.1016/j.joi.2009.03.001' target='_blank'>https://doi.org/10.1016/j.joi.2009.03.001</a></p>
  
  <p><strong>Rogers, E. M. (2003).</strong> <i>Diffusion of Innovations</i> (5th ed.). New York: Free Press.</p>
  
  <p><strong>Small, H., & Upham, S. P. (2009).</strong> <i>Citation structure of an emerging research area on the verge of application.</i> <strong>Scientometrics</strong>, 79(2), 365–375. <a href='https://doi.org/10.1007/s11192-009-0424-0' target='_blank'>https://doi.org/10.1007/s11192-009-0424-0</a></p>
  
  <p><strong>Wang, Q. (2018).</strong> <i>A bibliometric model for identifying emerging research topics.</i> <strong>Journal of the Association for Information Science and Technology</strong>, 69(2), 290–304. <a href='https://doi.org/10.1002/asi.23930' target='_blank'>https://doi.org/10.1002/asi.23930</a></p>
  </div>
  </body>"
  
  return(list(
    biblioAI = biblioAI, 
    info = info, 
    publications = publications, 
    filters = filters, 
    authorProfile = authorProfile,
    referenceMatching = referenceMatching,
    importOrLoad = importOrLoad,
    api = api,
    mergeCollections = mergeCollections,
    mainInformation = mainInformation,
    lifeCycle = lifeCycle
  ))
}