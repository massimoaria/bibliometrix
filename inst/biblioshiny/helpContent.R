helpContent <- function(){
  biblioAI <- 
    "<h3>🧠 Biblio AI: AI-Powered Bibliometric Analysis</h3>
  
  <p>Starting from version <strong>5.0</strong>, <em>Biblioshiny</em> introduces <strong>Biblio AI</strong>, a new suite of features powered by Google’s <strong>Gemini</strong> models. This integration allows users to receive <em>automatic interpretations</em>, <em>critical insights</em>, and <em>narrative summaries</em> of their bibliometric results — directly within the platform.</p>
  <br>
  <p><strong>Note:</strong> Biblio AI requires a <strong>Chrome-based browser</strong> (such as Google Chrome or Microsoft Edge) installed on your computer to work correctly.</p>
  
  <br>
  <h4>✨ What does Biblio AI do?</h4>
  <p>Biblio AI enhances the core analytical modules of <em>Biblioshiny</em> by providing contextual, AI-generated commentary on several results, as:</p>
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
  <br>
  <h4>🔧 How to enable Biblio AI?</h4>
  <p>To enable Biblio AI, follow these simple steps:</p>
  <ol>
  <li><strong>Register</strong> at <a href='https://makersuite.google.com/' target='_blank'>Google AI Studio</a> (free access available).</li>
    <li><strong>Generate an API Key</strong> enabled for Gemini model access (Free Tier supported).</li>
    <li><strong>Enter your API Key</strong> in the <em>Settings</em> section of <em>biblioshiny</em>.</li>
    </ol>
    <p>The interface will guide you through the secure and local setup. Your API key is used only on your device to interact with the AI model.</p>
    <br>
    <h4>🎯 Why use Biblio AI?</h4>
    <ul>
    <li>Reduces time spent interpreting complex outputs.</li>
    <li>Supports scientific writing and research reporting.</li>
    <li>Helps users better understand bibliometric patterns and dynamics.</li>
    <li>Delivers explanations in natural language, accessible to both experts and newcomers.</li>
    </ul>
"
    
  info <- 
    "<h3><strong>Supported Bibliographic Databases and Suggested File Formats</strong></h3>
  
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
                </ul>"


  
  publications <-
    "<h3><strong>Main Authors’ References (Bibliometrics)</strong></h3>
  
  <ul>
  <li><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></li>
    
    <li><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>Comparative science mapping: a novel conceptual structure analysis with metadata.</i> <strong>Scientometrics</strong>. <a href='https://doi.org/10.1007/s11192-024-05161-6' target='_blank'>https://doi.org/10.1007/s11192-024-05161-6</a></li>

  <li><strong>Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2023).</strong> <i>openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex.</i> <strong>R Journal</strong>, 15(4). <a href='https://doi.org/10.32614/rj-2023-089' target='_blank'>https://doi.org/10.32614/rj-2023-089</a></li>

  <li><strong>Aria, M., Misuraca, M., & Spano, M. (2020).</strong> <i>Mapping the evolution of social research and data science on 30 years of Social Indicators Research.</i> <strong>Social Indicators Research</strong>. <a href='https://doi.org/10.1007/s11205-020-02281-3' target='_blank'>https://doi.org/10.1007/s11205-020-02281-3</a></li>

  <li><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2022).</strong> <i>Thematic Analysis as a New Culturomic Tool: The Social Media Coverage on COVID-19 Pandemic in Italy.</i> <strong>Sustainability</strong>, 14(6), 3643. <a href='https://doi.org/10.3390/su14063643' target='_blank'>https://doi.org/10.3390/su14063643</a></li>

  <li><strong>Aria, M., Alterisio, A., Scandurra, A., Pinelli, C., & D'Aniello, B. (2021).</strong> <i>The scholar’s best friend: research trends in dog cognitive and behavioural studies.</i> <strong>Animal Cognition</strong>. <a href='https://doi.org/10.1007/s10071-020-01448-2' target='_blank'>https://doi.org/10.1007/s10071-020-01448-2</a></li>
    
    <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2016).</strong> <i>Foundations and trends in performance management: A twenty-five years bibliometric analysis in business and public administration domains.</i> <strong>Scientometrics</strong>. <a href='https://doi.org/10.1007/s11192-016-1948-8' target='_blank'>https://doi.org/10.1007/s11192-016-1948-8</a></li>
      
      <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2015).</strong> <i>Twenty years of research on performance management in business and public administration domains.</i> Presented at CARME 2015. <a href='https://www.bibliometrix.org/documents/2015Carme_cuccurulloetal.pdf' target='_blank'>Link</a></li>
        
        <li><strong>Sarto, F., Cuccurullo, C., & Aria, M. (2014).</strong> <i>Exploring healthcare governance literature: systematic review and paths for future research.</i> <strong>Mecosan</strong>. <a href='https://www.francoangeli.it/Riviste/Scheda_Rivista.aspx?IDarticolo=52780&lingua=en' target='_blank'>Link</a></li>
          
          <li><strong>Cuccurullo, C., Aria, M., & Sarto, F. (2013).</strong> <i>Twenty years of research on performance management in business and public administration domains.</i> <strong>Academy of Management Proceedings</strong>, Vol. 2013, No. 1, p. 14270. <a href='https://doi.org/10.5465/AMBPP.2013.14270abstract' target='_blank'>https://doi.org/10.5465/AMBPP.2013.14270abstract</a></li>
            
            <li><strong>Belfiore, A., Salatino, A., & Osborne, F. (2022).</strong> <i>Characterising Research Areas in the field of AI.</i> <strong>arXiv</strong> preprint. <a href='https://doi.org/10.48550/arXiv.2205.13471' target='_blank'>https://doi.org/10.48550/arXiv.2205.13471</a></li>
              
              <li><strong>Belfiore, A., Cuccurullo, C., & Aria, M. (2022).</strong> <i>IoT in healthcare: A scientometric analysis.</i> <strong>Technological Forecasting and Social Change</strong>, 184, 122001. <a href='https://doi.org/10.1016/j.techfore.2022.122001' target='_blank'>https://doi.org/10.1016/j.techfore.2022.122001</a></li>
                
                <li><strong>D'Aniello, L., Spano, M., Cuccurullo, C., & Aria, M. (2022).</strong> <i>Academic Health Centers’ configurations, scientific productivity, and impact: insights from the Italian setting.</i> <strong>Health Policy</strong>. <a href='https://doi.org/10.1016/j.healthpol.2022.09.007' target='_blank'>https://doi.org/10.1016/j.healthpol.2022.09.007</a></li>

  <li><strong>Belfiore, A., Scaletti, A., Lavorato, D., & Cuccurullo, C. (2022).</strong> <i>The long process by which HTA became a paradigm: A longitudinal conceptual structure analysis.</i> <strong>Health Policy</strong>. <a href='https://doi.org/10.1016/j.healthpol.2022.12.006' target='_blank'>https://doi.org/10.1016/j.healthpol.2022.12.006</a></li>
</ul>
"

  filters <- "
  <body>
  <div class='container'>
    <h3><strong>📂 Filters Information</strong></h3>
    
    <p>This section allows users to refine the document collection by applying multiple filters based on metadata fields available in the dataset. Below is a description of each filter and its usage:</p>
    
    <h4><strong>1. General</strong></h4>
    <ul>
    <li><strong>Document Type:</strong> Filters documents by type (e.g., Article, Book Chapter, Proceedings Paper). The selected document types are shown automatically; to remove one, simply click on it.</li>
    <li><strong>Language:</strong> Filters documents based on the language of publication.</li>
    <li><strong>Publication Year:</strong> Select a range of years using the slider to filter documents published within a specific time interval.</li>
    <li><strong>Subject Category:</strong> If available, this filter enables selection by subject category (e.g., Business, Education, Engineering). This option appears only if the source database (e.g., Scopus) provides this metadata. If the information is not available, the filter is automatically hidden.</li>
    </ul>
    
    <h4><strong>2. Journal (J)</strong></h4>
    <ul>
    <li><strong>Upload List of Journals:</strong> Allows users to upload a custom list of journal titles to restrict the collection accordingly. Accepted file formats: <code>.csv</code>, <code>.txt</code>, and <code>.xlsx</code>. Journal titles must be listed in the first column of the file.</li>
    <li><strong>Source by Bradford Law Zones:</strong> Enables filtering of journals based on Bradford’s Law.</li>
    </ul>
    
    <h4><strong>3. Author’s Country (AU)</strong></h4>
    <ul>
    <li><strong>Region and Country:</strong> Enables filtering by geographic region or specific country of the authors. Regions can be quickly selected using predefined buttons (e.g., Europe, Asia), while countries can be selected individually from the list.</li>
    </ul>
    
    <h4><strong>4. Documents (DOC)</strong></h4>
    <ul>
    <li><strong>Total Citations:</strong> Filters documents based on the total number of citations received.</li>
    <li><strong>Total Citations per Year:</strong> Filters documents by the average number of citations received per year since publication.</li>
    </ul>
    
    <h4><strong>📝 Additional Notes</strong></h4>
    <ul>
    <li>Filters with dual-column boxes (e.g., Subject Category, Country) are automatically pre-populated with values representing the structure of the current collection. These selections can be modified by clicking on any item in the right-hand column to remove it.</li>
    <li>Use the <strong>Apply</strong> button to activate the selected filters and update the collection.</li>
    <li>Use the <strong>Reset</strong> button to clear all filters and restore the original dataset.</li>
    </ul>
    </div>
    </body>
  "
  
  return(list(biblioAI=biblioAI, info=info, publications=publications, filters=filters))
}