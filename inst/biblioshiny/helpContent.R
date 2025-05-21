helpContent <- function(){
  biblioAI <- 
    "
  <h3>🧠 Biblio AI: AI-Powered Bibliometric Analysis</h3>
  
  <p>Starting from version <strong>5.0</strong>, <em>biblioshiny</em> introduces <strong>Biblio AI</strong>, a new suite of features powered by Google’s <strong>Gemini</strong> models. This integration allows users to receive <em>automatic interpretations</em>, <em>critical insights</em>, and <em>narrative summaries</em> of their bibliometric results — directly within the platform.</p>
  <br>
  <h4>✨ What does Biblio AI do?</h4>
  <p>Biblio AI enhances the core analytical modules of <em>biblioshiny</em> by providing contextual, AI-generated commentary on results:</p>
  <ul>
  <li><strong>Overview:</strong> Generates high-level summaries of key bibliometric indicators and collection features.</li>
  <li><strong>RPYS (Reference Publication Year Spectroscopy):</strong> Highlights and interprets historical citation peaks.</li>
  <li><strong>Trend Topics:</strong> Explains thematic evolution and emerging research trends.</li>
  <li><strong>Knowledge Structures:</strong> Offers insights into conceptual structures and co-citation/co-occurrence networks.</li>
  </ul>
  <p>In each of these sections, users can activate the <strong>Biblio AI</strong> panel to access dynamic text explanations, perfect for use in scientific writing, presentations, or reporting.</p>
  <br>
  <h4>🔧 How to activate Biblio AI?</h4>
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

  
  return(list(biblioAI=biblioAI, info=info, publications=publications))
}