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
  
  return(list(
    biblioAI = biblioAI, 
    info = info, 
    publications = publications, 
    filters = filters, 
    authorProfile = authorProfile,
    referenceMatching = referenceMatching
  ))
}