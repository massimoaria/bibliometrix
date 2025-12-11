helpContent <- function() {
  ## biblioAI ----
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

  ## Info ----
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

  ## Team Publications ----
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

  ## SAAS ----
  saas <- "<body>
  <div class='container'>
    <h1>SAAS Workflow</h1>
    <p class='subtitle'>Search - Appraisal - Analysis - Synthesis</p>
      
      <svg viewBox='0 0 1240 800' xmlns='http://www.w3.org/2000/svg'>
        <defs>
        <!-- Gradients -->
        <linearGradient id='searchGrad' x1='0%' y1='0%' x2='100%' y2='100%'>
          <stop offset='0%' style='stop-color:#4A90E2;stop-opacity:1' />
            <stop offset='100%' style='stop-color:#357ABD;stop-opacity:1' />
              </linearGradient>
              <linearGradient id='appraisalGrad' x1='0%' y1='0%' x2='100%' y2='100%'>
                <stop offset='0%' style='stop-color:#66BB6A;stop-opacity:1' />
                  <stop offset='100%' style='stop-color:#4CAF50;stop-opacity:1' />
                    </linearGradient>
                    <linearGradient id='analysisGrad' x1='0%' y1='0%' x2='100%' y2='100%'>
                      <stop offset='0%' style='stop-color:#FFA726;stop-opacity:1' />
                        <stop offset='100%' style='stop-color:#FB8C00;stop-opacity:1' />
                          </linearGradient>
                          <linearGradient id='synthesisGrad' x1='0%' y1='0%' x2='100%' y2='100%'>
                            <stop offset='0%' style='stop-color:#AB47BC;stop-opacity:1' />
                              <stop offset='100%' style='stop-color:#8E24AA;stop-opacity:1' />
                                </linearGradient>
                                
                                <!-- Arrow marker -->
                                <marker id='arrowhead' markerWidth='10' markerHeight='10' refX='9' refY='3' orient='auto'>
                                  <polygon points='0 0, 10 3, 0 6' fill='#666' />
                                    </marker>
                                    </defs>
                                    
                                    <!-- Central Flow Arrows -->
                                    <path d='M 300 150 L 340 150' stroke='#666' stroke-width='3' fill='none' marker-end='url(#arrowhead)' />
                                      <path d='M 600 150 L 640 150' stroke='#666' stroke-width='3' fill='none' marker-end='url(#arrowhead)' />
                                        <path d='M 900 150 L 940 150' stroke='#666' stroke-width='3' fill='none' marker-end='url(#arrowhead)' />
                                          
                                          <!-- Circular feedback arrow from Synthesis to Search -->
                                          <path d='M 1100 200 Q 1210 400 620 650 Q 50 650 140 200' 
                                          stroke='#9C27B0' stroke-width='3' fill='none' 
                                          stroke-dasharray='8,5' marker-end='url(#arrowhead)' opacity='0.6'/>
                                            
                                            <!-- SEARCH Box -->
                                            <g id='search'>
                                            <rect x='40' y='50' width='260' height='200' rx='15' fill='url(#searchGrad)' />
                                            <circle cx='170' cy='100' r='30' fill='white' opacity='0.3'/>
                                            <path d='M 160 90 Q 170 80 180 90 L 185 95 L 190 90' stroke='white' stroke-width='3' fill='none' stroke-linecap='round'/>
                                            <text x='170' y='165' font-size='28' font-weight='bold' fill='white' text-anchor='middle'>SEARCH</text>
                                            <text x='170' y='195' font-size='14' fill='white' text-anchor='middle' opacity='0.9'>Data Collection</text>
                                            <text x='50' y='290' font-size='13' fill='#333'>• API Integration</text>
                                            <text x='50' y='310' font-size='13' fill='#333'>• Database Export</text>
                                            <text x='50' y='330' font-size='13' fill='#333'>• Query Design</text>
                                            <text x='50' y='350' font-size='13' fill='#333'>• PubMed, OpenAlex</text>
                                            <text x='50' y='370' font-size='13' fill='#333'>• Web of Science</text>
                                            </g>
                                            
                                            <!-- APPRAISAL Box -->
                                            <g id='appraisal'>
                                            <rect x='340' y='50' width='260' height='200' rx='15' fill='url(#appraisalGrad)' />
                                            <circle cx='470' cy='100' r='30' fill='white' opacity='0.3'/>
                                            <path d='M 455 100 L 465 110 L 485 85' stroke='white' stroke-width='4' fill='none' stroke-linecap='round'/>
                                            <text x='470' y='165' font-size='28' font-weight='bold' fill='white' text-anchor='middle'>APPRAISAL</text>
                                            <text x='470' y='195' font-size='14' fill='white' text-anchor='middle' opacity='0.9'>Quality Assessment</text>
                                            <text x='350' y='290' font-size='13' fill='#333'>• Data Filtering</text>
                                            <text x='350' y='310' font-size='13' fill='#333'>• Quality Control</text>
                                            <text x='350' y='330' font-size='13' fill='#333'>• Duplicate Removal</text>
                                            <text x='350' y='350' font-size='13' fill='#333'>• Field Validation</text>
                                            <text x='350' y='370' font-size='13' fill='#333'>• Data Cleaning</text>
                                            </g>
                                            
                                            <!-- ANALYSIS Box -->
                                            <g id='analysis'>
                                            <rect x='640' y='50' width='260' height='200' rx='15' fill='url(#analysisGrad)' />
                                            <circle cx='770' cy='100' r='30' fill='white' opacity='0.3'/>
                                            <path d='M 755 115 L 770 90 L 785 115 M 770 90 L 770 110' stroke='white' stroke-width='3' fill='none' stroke-linecap='round'/>
                                            <text x='770' y='165' font-size='28' font-weight='bold' fill='white' text-anchor='middle'>ANALYSIS</text>
                                            <text x='770' y='195' font-size='14' fill='white' text-anchor='middle' opacity='0.9'>Deep Investigation</text>
                                            <text x='650' y='290' font-size='13' fill='#333'>• Bibliometric Analysis</text>
                                            <text x='650' y='310' font-size='13' fill='#333'>• Network Analysis</text>
                                            <text x='650' y='330' font-size='13' fill='#333'>• Citation Analysis</text>
                                            <text x='650' y='350' font-size='13' fill='#333'>• Conceptual Structure</text>
                                            <text x='650' y='370' font-size='13' fill='#333'>• Content Analysis</text>
                                            </g>
                                            
                                            <!-- SYNTHESIS Box -->
                                            <g id='synthesis'>
                                            <rect x='940' y='50' width='260' height='200' rx='15' fill='url(#synthesisGrad)' />
                                            <circle cx='1070' cy='100' r='30' fill='white' opacity='0.3'/>
                                            <text x='1070' y='108' font-size='32' fill='white' text-anchor='middle' font-weight='bold'>∑</text>
                                            <text x='1070' y='165' font-size='28' font-weight='bold' fill='white' text-anchor='middle'>SYNTHESIS</text>
                                            <text x='1070' y='195' font-size='14' fill='white' text-anchor='middle' opacity='0.9'>Results & Reporting</text>
                                            <text x='950' y='290' font-size='13' fill='#333'>• Report Generation</text>
                                            <text x='950' y='310' font-size='13' fill='#333'>• Key Insights</text>
                                            <text x='950' y='330' font-size='13' fill='#333'>• Visualization</text>
                                            <text x='950' y='350' font-size='13' fill='#333'>• AI Summary</text>
                                            <text x='950' y='370' font-size='13' fill='#333'>• Publication Output</text>
                                            </g>
                                            
                                            <!-- Bottom Tools and Features -->
                                            <g id='tools'>
                                            <rect x='40' y='420' width='1160' height='350' rx='15' fill='#f8f9fa' stroke='#dee2e6' stroke-width='2'/>
                                            <text x='620' y='455' font-size='22' font-weight='bold' fill='#495057' text-anchor='middle'>Integrated Tools & Features</text>
                                            
                                            <!-- Column 1 -->
                                            <g transform='translate(60, 480)'>
                                            <rect width='260' height='260' rx='10' fill='white' stroke='#dee2e6' stroke-width='1'/>
                                            <text x='130' y='30' font-size='16' font-weight='bold' fill='#667eea' text-anchor='middle'>Science Mapping</text>
                                            <text x='15' y='60' font-size='12' fill='#333'>✓ Co-citation Analysis</text>
                                            <text x='15' y='85' font-size='12' fill='#333'>✓ Bibliographic Coupling</text>
                                            <text x='15' y='110' font-size='12' fill='#333'>✓ Co-word Analysis</text>
                                            <text x='15' y='135' font-size='12' fill='#333'>✓ Thematic Evolution</text>
                                            <text x='15' y='160' font-size='12' fill='#333'>✓ Trend Topics</text>
                                            <text x='15' y='185' font-size='12' fill='#333'>✓ Three Fields Plot</text>
                                            <text x='15' y='210' font-size='12' fill='#333'>✓ Historiograph</text>
                                            <text x='15' y='235' font-size='12' fill='#333'>✓ Factorial Analysis</text>
                                            </g>
                                            
                                            <!-- Column 2 -->
                                            <g transform='translate(340, 480)'>
                                            <rect width='260' height='260' rx='10' fill='white' stroke='#dee2e6' stroke-width='1'/>
                                            <text x='130' y='30' font-size='16' font-weight='bold' fill='#66BB6A' text-anchor='middle'>Network Analysis</text>
                                            <text x='15' y='60' font-size='12' fill='#333'>✓ Collaboration Networks</text>
                                            <text x='15' y='85' font-size='12' fill='#333'>✓ Country Collaboration</text>
                                            <text x='15' y='110' font-size='12' fill='#333'>✓ Institution Networks</text>
                                            <text x='15' y='135' font-size='12' fill='#333'>✓ Author Networks</text>
                                            <text x='15' y='160' font-size='12' fill='#333'>✓ Citation Networks</text>
                                            <text x='15' y='185' font-size='12' fill='#333'>✓ Co-occurrence Networks</text>
                                            <text x='15' y='210' font-size='12' fill='#333'>✓ Community Detection</text>
                                            <text x='15' y='235' font-size='12' fill='#333'>✓ Centrality Measures</text>
                                            </g>
                                            
                                            <!-- Column 3 -->
                                            <g transform='translate(620, 480)'>
                                            <rect width='260' height='260' rx='10' fill='white' stroke='#dee2e6' stroke-width='1'/>
                                            <text x='130' y='30' font-size='16' font-weight='bold' fill='#FFA726' text-anchor='middle'>AI-Enhanced Analysis</text>
                                            <text x='15' y='60' font-size='12' fill='#333'>✓ Biblio AI Assistant</text>
                                            <text x='15' y='85' font-size='12' fill='#333'>✓ Document Summarization</text>
                                            <text x='15' y='110' font-size='12' fill='#333'>✓ PDF Content Analysis</text>
                                            <text x='15' y='135' font-size='12' fill='#333'>✓ Citation Context Extraction</text>
                                            <text x='15' y='160' font-size='12' fill='#333'>✓ Reference Matching</text>
                                            <text x='15' y='185' font-size='12' fill='#333'>✓ Readability Metrics</text>
                                            <text x='15' y='210' font-size='12' fill='#333'>✓ Topic Modeling</text>
                                            <text x='15' y='235' font-size='12' fill='#333'>✓ Natural Language Processing</text>
                                            </g>
                                            
                                            <!-- Column 4 -->
                                            <g transform='translate(900, 480)'>
                                            <rect width='260' height='260' rx='10' fill='white' stroke='#dee2e6' stroke-width='1'/>
                                            <text x='130' y='30' font-size='16' font-weight='bold' fill='#AB47BC' text-anchor='middle'>Visualization</text>
                                            <text x='15' y='60' font-size='12' fill='#333'>✓ Interactive Plots</text>
                                            <text x='15' y='85' font-size='12' fill='#333'>✓ Network Graphs</text>
                                            <text x='15' y='110' font-size='12' fill='#333'>✓ Thematic Maps</text>
                                            <text x='15' y='135' font-size='12' fill='#333'>✓ Word Clouds</text>
                                            <text x='15' y='160' font-size='12' fill='#333'>✓ Sankey Diagrams</text>
                                            <text x='15' y='185' font-size='12' fill='#333'>✓ Heatmaps</text>
                                            <text x='15' y='210' font-size='12' fill='#333'>✓ Time Series Plots</text>
                                            <text x='15' y='235' font-size='12' fill='#333'>✓ Export Options</text>
                                            </g>
                                            </g>
                                            
                                            <!-- Feedback Loop Label -->
                                            <text x='1110' y='300' font-size='13' fill='#9C27B0' font-style='italic'>Iterative</text>
                                            <text x='1110' y='318' font-size='13' fill='#9C27B0' font-style='italic'>Refinement</text>
                                            </svg>
                                            
                                            <div style='margin-top: 30px; padding: 20px; background: #f8f9fa; border-radius: 10px; border-left: 4px solid #667eea;'>
                                            <h3 style='color: #667eea; margin-top: 0;'>About the SAAS Workflow</h3>
                                            <p style='color: #495057; line-height: 1.6;'>
                                            The SAAS workflow represents the comprehensive process of <strong>bibliometrix</strong> and <strong>biblioshiny</strong> for conducting 
                                          scientific bibliometric analysis. Each phase is designed to ensure methodological rigor and reliable results:
                                            </p>
                                            <ul style='color: #495057; line-height: 1.8;'>
                                            <li><strong style='color: #4A90E2;'>Search:</strong> Systematic collection of bibliographic data from academic databases</li>
                                            <li><strong style='color: #66BB6A;'>Appraisal:</strong> Quality assessment and filtering of collected data</li>
                                            <li><strong style='color: #FFA726;'>Analysis:</strong> Application of advanced bibliometric techniques and AI</li>
                                            <li><strong style='color: #AB47BC;'>Synthesis:</strong> Results synthesis and scientific report generation</li>
                                            </ul>
                                            <p style='color: #6c757d; font-size: 0.9em; margin-top: 15px; font-style: italic;'>
                                            The iterative cycle allows continuous refinement of the analysis by returning to previous phases based on obtained results.
                                          </p>
                                            </div>
                                            
                                            <div style='margin-top: 20px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; text-align: center;'>
                                            <p style='color: white; margin: 0; font-size: 0.95em;'>
                                            <strong>SAAS Workflow developed by:</strong><br>
                                            Massimo Aria & Corrado Cuccurullo<br>
                                            <span style='font-size: 0.85em; opacity: 0.9;'>University of Naples Federico II, Italy</span>
                                            </p>
                                            <p style='color: white; margin: 10px 0 0 0; font-size: 0.85em; opacity: 0.9;'>
                                            Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. 
                                          <em>Journal of Informetrics</em>, 11(4), 959-975.
                                          </p>
                                            </div>
                                            </div>
                                            </body>
                                            "

  ## Filters ----
  filters <-
    "<body>
  <div class='container'>
  <h3>🔍 Filters: Refining Your Bibliometric Collection</h3>
  
  <p>The <strong>Filters</strong> module provides a comprehensive set of tools to refine and subset your bibliographic collection based on multiple metadata criteria. By applying filters, you can focus your analysis on specific document types, time periods, geographic regions, journals, or citation thresholds—enabling more targeted and meaningful bibliometric insights.</p>
  
  <p>Filters are organized into <strong>four thematic panels</strong>, each addressing different aspects of bibliographic metadata. At the top of the page, a <strong>real-time summary</strong> displays how many documents, sources, and authors remain after applying your filter selections.</p>
  
  <br>
  
  <h4>📊 Real-Time Filter Summary</h4>
  <p>Located at the top of the Filters page, this summary updates dynamically as you adjust filter settings:</p>
  <ul>
    <li><strong>Documents:</strong> Shows the number of documents currently selected (e.g., '898 of 898' means all documents are included; '450 of 898' means 450 documents match your filter criteria).</li>
    <li><strong>Sources:</strong> The number of distinct journals, books, or conferences represented in the filtered subset.</li>
    <li><strong>Authors:</strong> The total number of unique authors contributing to the filtered documents.</li>
  </ul>
  
  <p>These indicators help you assess the impact of your filters <em>before</em> applying them, ensuring your subset maintains sufficient size for robust analysis.</p>
  
  <br>
  
  <h4>1️⃣ General Filters</h4>
  <p>The <strong>General</strong> panel provides fundamental filters applicable to most bibliometric collections:</p>
  
  <h4>Document Type</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by publication type (e.g., Article, Book Chapter, Proceedings Paper, Review, Editorial, Letter, Note).</li>
    <li><strong>How to Use:</strong> 
      <ul>
        <li>By default, all document types are selected (shown in the filter box).</li>
        <li>To <strong>exclude</strong> a document type, click on its name in the filter box—it will be removed.</li>
        <li>To <strong>include</strong> a previously excluded type, click on it in the list below the filter box.</li>
      </ul>
    </li>
    <li><strong>Use Cases:</strong> 
      <ul>
        <li>Focus on <strong>peer-reviewed articles</strong> by excluding editorials, letters, and notes.</li>
        <li>Analyze <strong>conference proceedings</strong> separately from journal articles.</li>
        <li>Include only <strong>review articles</strong> for systematic literature reviews.</li>
      </ul>
    </li>
  </ul>
  
  <h4>Language</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by publication language (e.g., English, Spanish, French, German, Chinese).</li>
    <li><strong>Interaction:</strong> Similar to Document Type—click to select/deselect languages.</li>
    <li><strong>Note:</strong> Most bibliometric databases predominantly index English-language publications. Non-English documents may represent a small fraction (<5%) of typical collections.</li>
  </ul>
  
  <h4>Publication Year</h4>
  <ul>
    <li><strong>Function:</strong> Restricts the collection to documents published within a specific time range.</li>
    <li><strong>How to Use:</strong> 
      <ul>
        <li>Use the <strong>slider</strong> to adjust the start and end years.</li>
        <li>The selected range is displayed below the slider (e.g., '1985 - 2020').</li>
        <li>The histogram shows the distribution of publications across years, helping you identify periods of high activity.</li>
      </ul>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li><strong>Temporal segmentation:</strong> Analyze different decades separately (e.g., 1990-2000 vs. 2010-2020).</li>
        <li><strong>Exclude recent publications:</strong> Remove documents <2 years old to avoid citation lag bias.</li>
        <li><strong>Focus on historical literature:</strong> Study foundational works from earlier periods.</li>
      </ul>
    </li>
  </ul>
  
  <br>
  
  <h4>2️⃣ Journal (J) Filters</h4>
  <p>The <strong>Journal</strong> panel enables filtering based on publication venues, journal rankings, or Bradford's Law zones:</p>
  
  <h4>Upload a List of Journals</h4>
  <ul>
    <li><strong>Function:</strong> Restricts the collection to documents published in a user-defined list of journals.</li>
    <li><strong>How to Use:</strong>
      <ol>
        <li>Prepare a file (<code>.csv</code>, <code>.txt</code>, or <code>.xlsx</code>) with journal titles listed in the <strong>first column</strong>.</li>
        <li>Click <strong>Browse...</strong> and select your file.</li>
        <li>Only documents from journals matching the uploaded list (case-insensitive, partial matching) will be retained.</li>
      </ol>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Focus on <strong>core journals</strong> in your field (e.g., top 10 management journals).</li>
        <li>Analyze publications from <strong>open-access journals</strong> only.</li>
        <li>Exclude predatory or low-quality journals identified via external blacklists.</li>
      </ul>
    </li>
    <li><strong>Example File Format:</strong></li>
  </ul>
  <code>
  Journal of Informetrics<br>
  Scientometrics<br>
  Journal of the Association for Information Science and Technology<br>
  Research Policy
  </code>
  
  <h4>Upload a Journal Ranking List</h4>
  <ul>
    <li><strong>Function:</strong> Filters journals based on quality rankings (e.g., Q1, Q2, Q3, Q4 quartiles; A*, A, B, C grades).</li>
    <li><strong>How to Use:</strong>
      <ol>
        <li>Prepare a file (<code>.csv</code> or <code>.xlsx</code>) with <strong>two columns and headers</strong>:
          <ul>
            <li><strong>Column 1:</strong> Journal titles (must match exactly or closely)</li>
            <li><strong>Column 2:</strong> Ranking categories (e.g., Q1, Q2, A*, B)</li>
          </ul>
        </li>
        <li>Upload the file via <strong>Browse...</strong></li>
        <li>Select which ranking categories to include in your filtered collection.</li>
      </ol>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Focus on <strong>top-tier journals</strong> (e.g., Q1 only) for high-impact analysis.</li>
        <li>Compare publication patterns across journal tiers (e.g., Q1 vs. Q2-Q4).</li>
        <li>Filter by national rankings (e.g., Italian VQR, Australian ABDC, UK ABS).</li>
      </ul>
    </li>
    <li><strong>Example File Format:</strong></li>
  </ul>
  <code>
  Journal,Quartile<br>
  Journal of Informetrics,Q1<br>
  Scientometrics,Q1<br>
  Library Quarterly,Q2<br>
  Online Information Review,Q3
  </code>
  
  <h4>Source by Bradford Law Zones</h4>
  <ul>
    <li><strong>Function:</strong> Filters journals based on <strong>Bradford's Law</strong>, which divides sources into three productivity zones: <strong>Core</strong>, <strong>Zone 2</strong>, and <strong>Zone 3</strong>.</li>
    <li><strong>Theory:</strong> Bradford's Law states that:
      <ul>
        <li><strong>Core journals (Zone 1):</strong> A small number of highly productive sources publishing ~1/3 of all documents.</li>
        <li><strong>Zone 2:</strong> A moderate number of sources contributing another ~1/3.</li>
        <li><strong>Zone 3:</strong> A large number of peripheral sources producing the final ~1/3.</li>
      </ul>
    </li>
    <li><strong>How to Use:</strong>
      <ul>
        <li>Select <strong>'All Sources'</strong> to include everything (default).</li>
        <li>Select <strong>'Core'</strong> to focus on the most productive journals.</li>
        <li>Select <strong>'Zone 2'</strong> or <strong>'Zone 3'</strong> to analyze mid-tier or peripheral journals.</li>
      </ul>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Identify the <strong>core journals</strong> dominating a research field.</li>
        <li>Compare citation impact between core and peripheral sources.</li>
        <li>Exclude low-productivity journals (Zone 3) to streamline analysis.</li>
      </ul>
    </li>
  </ul>
  
  <br>
  
  <h4>3️⃣ Author's Country (AU) Filters</h4>
  <p>The <strong>Author's Country</strong> panel enables geographic filtering based on author affiliations:</p>
  
  <h4>Region</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by broad geographic regions (e.g., Africa, Asia, Europe, North America, South America, Oceania, Seven Seas, Unknown).</li>
    <li><strong>How to Use:</strong> Click on region buttons to toggle selection. Selected regions are highlighted in blue.</li>
    <li><strong>Note:</strong> 'Seven Seas' represents international waters or unclassified regions; 'Unknown' indicates missing affiliation data.</li>
  </ul>
  
  <h4>Country</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by specific author countries (e.g., USA, China, UK, Germany, Italy).</li>
    <li><strong>How to Use:</strong>
      <ul>
        <li>Use the <strong>search box</strong> to quickly find countries.</li>
        <li>Countries are displayed in two columns: <strong>left</strong> (available), <strong>right</strong> (selected).</li>
        <li>Click a country in the left column to <strong>add</strong> it; click in the right column to <strong>remove</strong>.</li>
      </ul>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Analyze <strong>national research outputs</strong> (e.g., Italian contributions to bibliometrics).</li>
        <li>Study <strong>international collaboration</strong> by including multiple countries.</li>
        <li>Compare <strong>regional trends</strong> (e.g., Europe vs. Asia vs. North America).</li>
        <li>Identify <strong>emerging research nations</strong> in a field.</li>
      </ul>
    </li>
    <li><strong>Important:</strong> Multi-country documents (with authors from different countries) are included if <em>any</em> selected country is represented among the authors.</li>
  </ul>
  
  <br>
  
  <h4>4️⃣ Documents (DOC) Filters</h4>
  <p>The <strong>Documents</strong> panel provides citation-based filters with interactive histograms:</p>
  
  <h4>Total Citations</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by their cumulative citation count (from database records).</li>
    <li><strong>How to Use:</strong>
      <ul>
        <li>Use the <strong>slider</strong> below the histogram to set minimum and maximum citation thresholds.</li>
        <li>The histogram shows the distribution of citation counts across documents, helping you identify highly-cited outliers.</li>
        <li>Example: Set minimum = 50 to include only documents with ≥50 citations.</li>
      </ul>
    </li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Focus on <strong>high-impact documents</strong> (e.g., citations >100) for influence analysis.</li>
        <li>Exclude <strong>uncited documents</strong> (citations = 0) for citation network studies.</li>
        <li>Identify the <strong>citation elite</strong> (top 1% most-cited papers).</li>
      </ul>
    </li>
  </ul>
  
  <h4>Total Citations per Year</h4>
  <ul>
    <li><strong>Function:</strong> Filters documents by their <strong>average annual citation rate</strong>, calculated as: <code>Total Citations / (Current Year - Publication Year)</code>.</li>
    <li><strong>Why Use This?</strong> Raw citation counts are biased toward older publications. Citations per year normalizes for document age, enabling fairer comparison between recent and historical works.</li>
    <li><strong>How to Use:</strong> Adjust the slider to set citation-per-year thresholds (e.g., ≥5 citations/year).</li>
    <li><strong>Use Cases:</strong>
      <ul>
        <li>Identify <strong>rapidly accumulating citations</strong> (indicators of emerging influence).</li>
        <li>Compare <strong>citation velocity</strong> across time periods.</li>
        <li>Find <strong>recent high-impact papers</strong> that haven't yet accumulated large total citation counts but show strong annual growth.</li>
      </ul>
    </li>
  </ul>
  
  <br>
  
  <h4>🎛️ Filter Workflow</h4>
  <p>Follow these steps to apply filters effectively:</p>
  
  <ol>
    <li><strong>Review the initial collection:</strong> Check the summary counts (Documents, Sources, Authors) before applying filters.</li>
    <li><strong>Select filter criteria:</strong> Adjust settings across the four panels based on your research objectives.</li>
    <li><strong>Monitor real-time updates:</strong> The summary at the top updates dynamically as you change selections, showing how many documents remain.</li>
    <li><strong>Click 'Apply':</strong> Once satisfied with your selections, click the blue <strong>Apply</strong> button to activate the filters.</li>
    <li><strong>Verify results:</strong> Check the updated summary to ensure your filters produced the expected subset size.</li>
    <li><strong>Proceed to analysis:</strong> Navigate to other modules (Overview, Sources, Authors, etc.) to analyze the filtered collection.</li>
    <li><strong>Reset if needed:</strong> Click the <strong>Reset</strong> button to clear all filters and restore the original dataset.</li>
  </ol>
  
  <br>
  
  <h4>💡 Best Practices</h4>
  <ul>
    <li><strong>Avoid over-filtering:</strong> Very small subsets (<100 documents) may not provide robust results for network or clustering analyses. Aim for at least 200-300 documents when possible.</li>
    <li><strong>Document your filters:</strong> Record which filters you applied for reproducibility and transparency in research reporting (e.g., 'Filtered to Q1 journals, 2010-2020, English-language articles only').</li>
    <li><strong>Iterative refinement:</strong> Start with broad filters and gradually narrow your criteria while monitoring the summary counts.</li>
    <li><strong>Combine filters strategically:</strong> Use multiple filter types together (e.g., specific countries + high citations + recent years) for highly targeted analyses.</li>
    <li><strong>Save filtered collections:</strong> After applying filters, export your refined collection using the <strong>Data</strong> button (top right) to preserve your work.</li>
    <li><strong>Compare filtered vs. unfiltered:</strong> Run key analyses on both the full and filtered collections to assess how filters impact results.</li>
  </ul>
  
  <br>
  
  <h4>⚠️ Important Considerations</h4>
  <ul>
    <li><strong>Citation Data Availability:</strong> Citation counts depend on database indexing. Web of Science and Scopus provide citation data; PubMed and some other databases do not. Missing citation data will result in empty histograms in the Documents panel.</li>
    <li><strong>Affiliation Data Quality:</strong> Author country filters rely on affiliation metadata, which may be incomplete or inconsistent, especially in older publications or non-WoS/Scopus databases.</li>
    <li><strong>Subject Category Coverage:</strong> Subject categories are database-specific. Scopus categories differ from Web of Science categories; merged collections may have inconsistent classification.</li>
    <li><strong>Filter Order Independence:</strong> Filters are applied simultaneously, not sequentially. The order in which you select filters does not affect the final result.</li>
    <li><strong>Bradford Zone Recalculation:</strong> Bradford's Law zones are calculated based on the <em>current</em> collection. If you merge collections or upload new data, zones may shift.</li>
  </ul>
  
  <br>
  
  <h4>🔍 Use Case Examples</h4>
  
  <p><strong>Example 1: Analyzing Top-Tier Recent Research</strong></p>
  <ul>
    <li><strong>Goal:</strong> Focus on high-impact, recent publications in core journals.</li>
    <li><strong>Filters Applied:</strong>
      <ul>
        <li>Document Type: Article, Review</li>
        <li>Publication Year: 2015-2020</li>
        <li>Source by Bradford Law Zones: Core</li>
        <li>Total Citations per Year: ≥10</li>
      </ul>
    </li>
    <li><strong>Outcome:</strong> A curated subset of influential papers from leading journals, suitable for identifying emerging research fronts.</li>
  </ul>
  
  <p><strong>Example 2: National Research Assessment</strong></p>
  <ul>
    <li><strong>Goal:</strong> Evaluate research output from Italian universities in Computer Science.</li>
    <li><strong>Filters Applied:</strong>
      <ul>
        <li>Author's Country: Italy</li>
        <li>Subject Category: Computer Science, Information Systems</li>
        <li>Document Type: Article</li>
      </ul>
    </li>
    <li><strong>Outcome:</strong> A collection focused on Italian contributions to CS, enabling analysis of national productivity, collaboration patterns, and impact.</li>
  </ul>
  
  <p><strong>Example 3: Historical Foundational Literature</strong></p>
  <ul>
    <li><strong>Goal:</strong> Study the intellectual foundations of a field by examining seminal works.</li>
    <li><strong>Filters Applied:</strong>
      <ul>
        <li>Publication Year: 1970-1990</li>
        <li>Total Citations: ≥100</li>
        <li>Document Type: Article</li>
      </ul>
    </li>
    <li><strong>Outcome:</strong> A set of highly-cited historical documents representing foundational contributions.</li>
  </ul>
  
  <br>
  
  <h4>📚 References</h4>
  <p><strong>Bradford, S. C. (1934).</strong> <i>Sources of information on specific subjects.</i> <strong>Engineering</strong>, 137, 85–86.</p>
  
  <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
  
  <p><strong>Garfield, E. (2009).</strong> <i>From the science of science to Scientometrics: visualizing the history of science with HistCite software.</i> <strong>Journal of Informetrics</strong>, 3(3), 173–179. <a href='https://doi.org/10.1016/j.joi.2009.03.009' target='_blank'>https://doi.org/10.1016/j.joi.2009.03.009</a></p>
  </div>
  </body>"

  ## Author Profile ----
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

  #
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

  #
  importOrLoad <-
    "<body>
    <div class='container'>
    <h3>📥 Import or Load: Building Your Bibliometric Collection</h3>
    
    <p>The <strong>Import or Load</strong> module is the starting point for any bibliometric analysis in <strong>Biblioshiny</strong>. This section allows users to build their bibliographic collection by either <em>importing raw files</em> from supported databases or <em>loading pre-processed bibliometrix files</em> saved in previous sessions.</p>
    
    <br>
    
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
    <p>Resume work on a previously processed collection by loading <strong>.rdata</strong> or <strong>.xlsx</strong> files generated by Biblioshiny or the <code>bibliometrix</code> R package.</p>
    
    <p><strong>Use Cases:</strong></p>
    <ul>
      <li>Continue analysis from a previous session</li>
      <li>Load collections pre-processed using the <code>bibliometrix</code> R package</li>
      <li>Share standardized datasets with collaborators</li>
      <li>Work with large collections that have already undergone data cleaning and filtering</li>
    </ul>
    
    <p><strong>Supported Formats:</strong></p>
    <ul>
      <li><strong>.rdata</strong>: R Data Serialization format (preserves full metadata and structure)</li>
      <li><strong>.xlsx</strong>: Excel format (compatible with bibliometrix exports)</li>
    </ul>
    
    <h4>3. Use a Sample Collection</h4>
    <p>Perfect for <strong>testing</strong> and <strong>learning</strong> Biblioshiny's features without preparing your own data.</p>
    <ul>
      <li>Select from pre-loaded example datasets covering various research domains</li>
      <li>Ideal for exploring the platform's analytical capabilities</li>
      <li>No file upload required—start analyzing immediately</li>
    </ul>
    
    <br>
    
    <h4>🔍 Post-Import Features</h4>
    <p>After successfully importing or loading a collection, you can:</p>
    <ul>
      <li><strong>View Collection Metadata:</strong> Preview document details in a sortable, filterable table</li>
      <li><strong>Add Brief Description:</strong> Write a custom description of your collection for documentation purposes</li>
      <li><strong>Export Collection:</strong> Save your processed collection as <code>.rdata</code>, <code>.xlsx</code>, or <code>.csv</code> for backup or sharing</li>
      <li><strong>Start Analysis:</strong> Click the blue <strong>Start</strong> button to proceed to filtering and analysis modules</li>
    </ul>
    
    <br>
    
    <h4>💾 Exporting Collections</h4>
    <p>Once your collection is loaded, you can export it in multiple formats:</p>
    <ul>
      <li><strong>.rdata</strong>: Recommended for preserving all metadata and R-specific structures</li>
      <li><strong>.xlsx</strong>: Excel-compatible format for sharing with non-R users</li>
    </ul>
    
    <br>
    
    <h4>⚠️ Best Practices</h4>
    <ul>
      <li><strong>Always save your processed collections</strong> after importing raw files to avoid re-conversion</li>
      <li><strong>Use descriptive filenames</strong> when exporting (e.g., <code>management_wos_1990-2020.rdata</code>)</li>
      <li><strong>Check conversion results carefully</strong>—some database exports may have formatting issues that require manual correction</li>
      <li><strong>For large collections (>5,000 documents)</strong>, consider applying filters early to improve performance</li>
    </ul>
    
    <br>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    </div>
    </body>"

  ## API ----
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

  ## Merge Collections ----
  mergeCollections <-
    "<body>
    <div class='container'>
    <h3>🔀 Merge Collections: Combining Data from Multiple Sources</h3>
    
    <p>The <strong>Merge Collections</strong> module allows users to combine bibliographic datasets from different databases (Web of Science, Scopus, OpenAlex, PubMed, etc.) into a single unified collection. This functionality is essential for <strong>comprehensive literature reviews</strong>, <strong>cross-database validation</strong>, and maximizing <strong>metadata coverage</strong> by leveraging the strengths of multiple sources.</p>
    
    <br>
    
    <h4>🎯 Why Merge Collections?</h4>
    <ul>
      <li><strong>Broader Coverage:</strong> Different databases index different journals and document types—merging increases the comprehensiveness of your dataset</li>
      <li><strong>Complementary Metadata:</strong> Scopus may provide detailed affiliation data, while Web of Science offers comprehensive citation links—combining them enriches your analysis</li>
      <li><strong>Validation:</strong> Cross-referencing records from multiple sources improves data quality and identifies discrepancies</li>
      <li><strong>Deduplication:</strong> Automatically removes duplicate records that appear in multiple databases</li>
    </ul>
    
    <br>
    
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
    
    <br>
    
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
    
    <p><strong>Step 3.1: DOI-Based Matching</strong></p>
    <ul>
      <li>Documents with identical DOIs are flagged as duplicates</li>
      <li>This is the most reliable method, as DOIs are unique identifiers</li>
      <li>Empty or missing DOIs (<code>''</code> or <code>NA</code>) are ignored to avoid false positives</li>
      <li>Only the first occurrence is retained; subsequent matches are removed</li>
    </ul>
    
    <p><strong>Step 3.2: Title-Year Matching</strong></p>
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
    
    <br>
    
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
    
    <br>
    
    <h4>📌 Best Practices</h4>
    <ul>
      <li><strong>Always enable duplicate removal</strong> unless you have a specific reason to retain duplicates</li>
      <li><strong>Prioritize Web of Science or Scopus</strong> as the primary source—these databases generally have the most complete metadata</li>
      <li><strong>Use OpenAlex to supplement coverage</strong> for open-access publications or gray literature</li>
      <li><strong>Validate merge results</strong> by checking the distribution of <code>DB_Original</code> values—extreme imbalances may indicate incomplete data from one source</li>
      <li><strong>Save merged collections immediately</strong> to avoid re-processing</li>
    </ul>
    
    <br>
    
    <h4>⚠️ Important Considerations</h4>
    <ul>
      <li><strong>Citation Data:</strong> Merged collections reset the <code>CR</code> (Cited References) field—you'll need to run <strong>Reference Matching</strong> again after merging</li>
      <li><strong>Field Coverage:</strong> Some databases provide richer metadata than others—merging doesn't 'fill in' missing fields unless duplicates are detected</li>
      <li><strong>Large Collections:</strong> Merging collections >10,000 documents may take several minutes—be patient and avoid interrupting the process</li>
      <li><strong>Database-Specific Analyses:</strong> Some analyses are database-specific—merged collections may lose this granularity</li>
    </ul>
    
    <br>
    
    <h4>🔍 Example Use Cases</h4>
    <ul>
      <li><strong>Systematic Literature Review:</strong> Combine Web of Science, Scopus, and PubMed to ensure no relevant publications are missed</li>
      <li><strong>Open Science Research:</strong> Merge OpenAlex with traditional databases to include preprints and institutional repositories</li>
      <li><strong>Validation Study:</strong> Compare overlap between databases to assess index coverage and bias</li>
      <li><strong>Longitudinal Analysis:</strong> Merge historical Web of Science data with recent OpenAlex records to extend temporal coverage</li>
    </ul>
    
    <br>
    
    <h4>📚 References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    
    <p><strong>Visser, M., van Eck, N. J., & Waltman, L. (2021).</strong> <i>Large-scale comparison of bibliographic data sources: Scopus, Web of Science, Dimensions, Crossref, and Microsoft Academic.</i> <strong>Quantitative Science Studies</strong>, 2(1), 20–41. <a href='https://doi.org/10.1162/qss_a_00112' target='_blank'>https://doi.org/10.1162/qss_a_00112</a></p>
    
    <p><strong>Martín-Martín, A., Thelwall, M., Orduna-Malea, E., & Delgado López-Cózar, E. (2021).</strong> <i>Google Scholar, Microsoft Academic, Scopus, Dimensions, Web of Science, and OpenCitations' COCI: a multidisciplinary comparison of coverage via citations.</i> <strong>Scientometrics</strong>, 126(1), 871–906. <a href='https://doi.org/10.1007/s11192-020-03690-4' target='_blank'>https://doi.org/10.1007/s11192-020-03690-4</a></p>
    </div>
    </body>"

  ## Main Information ----
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

  ## Life Cycle ----
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

  ## Three Field Plot ----
  threeFieldPlot <-
    "<body>
    <div class='container'>
    <h3>🔀 Three-Field Plot</h3>
    
    <p>The <strong>Three-Field Plot</strong> is an advanced visualization tool that reveals the relationships among three distinct bibliographic dimensions through an interactive <strong>Sankey diagram</strong>. This plot enables researchers to explore the complex connections between different metadata fields, making it particularly useful for understanding how research topics, authors, sources, and references are interconnected within a scientific domain.</p>
    
    <h4>🎯 Purpose and Application</h4>
    <p>The Three-Field Plot serves multiple analytical purposes:</p>
    <ul>
      <li><strong>Relationship Mapping:</strong> Visualizes how elements from three different bibliographic fields are associated with each other</li>
      <li><strong>Knowledge Flow:</strong> Tracks the flow of ideas and citations across different dimensions (e.g., from cited references through authors to keywords)</li>
      <li><strong>Thematic Connections:</strong> Identifies which keywords or topics are most strongly associated with specific authors or sources</li>
      <li><strong>Author-Topic Associations:</strong> Shows which authors are working on which topics and citing which foundational works</li>
    </ul>
    
    <h4>📊 How It Works</h4>
    <p>The visualization consists of three vertical columns representing different bibliographic fields:</p>
    <ul>
      <li><strong>Left Field:</strong> Typically represents sources (cited references, journals) or temporal information</li>
      <li><strong>Middle Field:</strong> Usually displays authors or intermediary elements that connect the other two fields</li>
      <li><strong>Right Field:</strong> Often shows keywords, topics, or other thematic elements</li>
    </ul>
    
    <p>The width of each flow (colored band) is proportional to the frequency of co-occurrence between elements. Thicker flows indicate stronger associations, while thinner ones represent weaker connections.</p>
    
    <h4>⚙️ Configuration Options</h4>
    <p>The <strong>Options</strong> panel allows you to customize the plot:</p>
    <ul>
      <li><strong>Left Field:</strong> Select from available metadata fields (e.g., Cited References, Sources, Authors' Countries)</li>
      <li><strong>Middle Field:</strong> Choose the central connecting field (e.g., Authors, Sources, Keywords)</li>
      <li><strong>Right Field:</strong> Define the destination field (e.g., Author's Keywords, Keywords Plus, Subject Categories)</li>
      <li><strong>Number of Items:</strong> Control how many top elements to display for each field (typically 10-30 items per field)</li>
    </ul>
    
    <h4>💡 Common Field Combinations</h4>
    <p>Some particularly insightful field combinations include:</p>
    <ul>
      <li><strong>References → Authors → Keywords:</strong> Shows which foundational works are cited by which authors working on which topics</li>
      <li><strong>Sources → Authors → Countries:</strong> Maps the geographical distribution of authors publishing in specific journals</li>
      <li><strong>Keywords → Authors → Cited References:</strong> Reveals the intellectual foundations of different research themes</li>
      <li><strong>Authors' Countries → Authors → Keywords:</strong> Identifies national research specializations and thematic focuses</li>
      <li><strong>Publication Year → Authors → Keywords:</strong> Tracks temporal evolution of author productivity and topic emergence</li>
    </ul>
    
    <h4>🔍 Interpretation Guidelines</h4>
    <ul>
      <li><strong>Flow Thickness:</strong> A thick flow between two elements indicates a strong association (high co-occurrence frequency)</li>
      <li><strong>Multiple Connections:</strong> Elements with many outgoing or incoming flows are central nodes in the network</li>
      <li><strong>Isolated Flows:</strong> Thin, isolated connections may represent niche specializations or emerging topics</li>
      <li><strong>Color Coding:</strong> Colors help distinguish different elements in the left field, making it easier to trace specific flows</li>
      <li><strong>Cross-field Patterns:</strong> Look for patterns where multiple elements from one field connect to the same element in another field, indicating convergence or interdisciplinarity</li>
    </ul>
    
    <h4>📌 Best Practices</h4>
    <ul>
      <li><strong>Start Simple:</strong> Begin with a small number of items (10-15 per field) to avoid visual clutter, then increase if needed</li>
      <li><strong>Logical Sequences:</strong> Arrange fields in a logical flow (e.g., past → present, source → output, context → content)</li>
      <li><strong>Interactive Exploration:</strong> Hover over flows and nodes to see exact frequencies and connections</li>
      <li><strong>Export Results:</strong> Use the plot in presentations to illustrate complex relationships in an accessible way</li>
      <li><strong>Combine with Networks:</strong> Use Three-Field Plots alongside network analyses for complementary perspectives on your data</li>
      <li><strong>Context Matters:</strong> Always interpret the plot in the context of your research question and domain knowledge</li>
    </ul>
    
    <h4>⚠️ Limitations</h4>
    <ul>
      <li><strong>Aggregation Effects:</strong> The plot shows aggregate patterns and may obscure individual document-level details</li>
      <li><strong>Top-N Selection:</strong> Only the most frequent items are displayed; rare but potentially important connections may be hidden</li>
      <li><strong>Direction Ambiguity:</strong> While flows suggest relationships, they don't always imply causal or temporal direction</li>
      <li><strong>Visual Complexity:</strong> With too many items, the plot can become difficult to interpret; reduce the number of items if necessary</li>
    </ul>
    
    <h4>🤖 Biblio AI Integration</h4>
    <p>When <strong>Biblio AI</strong> is enabled, you can generate automatic interpretations of the Three-Field Plot. The AI will:</p>
    <ul>
      <li>Identify the most important flows and connections</li>
      <li>Highlight dominant patterns and relationships</li>
      <li>Provide narrative explanations suitable for research reports and presentations</li>
      <li>Suggest potential interpretations based on the observed patterns</li>
    </ul>
    
    <h4>📚 Key References</h4>
    <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
    
    <p><strong>Chen, C. (2017).</strong> <i>Science Mapping: A Systematic Review of the Literature.</i> <strong>Journal of Data and Information Science</strong>, 2(2), 1–40. <a href='https://doi.org/10.1515/jdis-2017-0006' target='_blank'>https://doi.org/10.1515/jdis-2017-0006</a></p>
    
    </div>
    </body>"

  ## Content Analysis ----
  contentAnalysis <-
    "<body>
  <div class='container'>
  <h3>📄 Scientific Article Content Analysis</h3>
  
  <p><strong>Content Analysis</strong> is a specialized feature in <em>Biblioshiny</em> that enables researchers to perform deep, AI-enhanced analysis of individual scientific articles in PDF format. This tool goes beyond traditional bibliometric analysis by examining the full text of documents, extracting citations with their surrounding context, and revealing patterns in how research is cited and discussed within scholarly narratives.</p>
  
  <p>This menu integrates in <em>Biblioshiny</em> the functions included in the R library <strong>contentanalysis</strong> by Aria and Cuccurullo (<a href='https://cran.r-project.org/package=contentanalysis' target='_blank'>https://cran.r-project.org/package=contentanalysis</a>). The module is built on the <strong>bibliometrix</strong> ecosystem and integrates advanced text mining capabilities to support:</p>
  <ul>
    <li>Extraction and analysis of in-text citations with context windows</li>
    <li>Citation co-occurrence network analysis</li>
    <li>Readability and linguistic quality assessment</li>
    <li>Word distribution and trend analysis across document sections</li>
    <li>AI-powered document summarization through <strong>Biblio AI</strong></li>
    <li>Comprehensive reference list extraction and matching</li>
  </ul>
  
  <br>
  
  <h4>🎯 Purpose and Applications</h4>
  <p><strong>Content Analysis</strong> is particularly valuable for:</p>
  <ul>
    <li><strong>Understanding Citation Context:</strong> Examining how and where references are cited within a paper, distinguishing between peripheral mentions and substantive discussions.</li>
    <li><strong>Identifying Citation Clusters:</strong> Detecting which references are frequently cited together, revealing the conceptual structure and intellectual foundations of the research.</li>
    <li><strong>Quality Assessment:</strong> Evaluating document readability, lexical diversity, and linguistic complexity using established metrics like Flesch-Kincaid, ARI, and Gunning Fog Index.</li>
    <li><strong>Thematic Flow Analysis:</strong> Tracking how key terms and concepts are distributed across different sections of the paper (Introduction, Methods, Results, Discussion).</li>
    <li><strong>Literature Review Enhancement:</strong> Using AI-powered summarization to quickly extract key insights, research questions, methodologies, and findings from lengthy documents.</li>
    <li><strong>Citation Practice Research:</strong> Analyzing citation patterns and practices for methodological or meta-research studies.</li>
  </ul>
  
  <br>
  
  <h4>📥 Step 1: Import PDF File</h4>
  <p>The analysis begins by uploading a scientific article in PDF format. The system supports both single-column and multi-column layouts (specify the number of columns for accurate text extraction).</p>
  
  <p><strong>Citation Format Detection:</strong> The tool uses AI-enhanced extraction to identify citations in multiple formats:</p>
  <ul>
    <li><strong>Author-year format:</strong> (Smith, 2020) or Smith et al. (2015)</li>
    <li><strong>Numeric brackets:</strong> [1] or [15-17]</li>
    <li><strong>Numeric superscripts:</strong> ¹ or ²³</li>
    <li><strong>Mixed formats:</strong> The system can handle documents with inconsistent citation styles (though results may be less reliable)</li>
  </ul>
  
  <p><strong>AI-Enhanced Extraction:</strong> When enabled, this feature uses advanced AI models to improve citation detection accuracy, particularly useful for:</p>
  <ul>
    <li>PDFs with complex layouts or formatting issues</li>
    <li>Documents with non-standard citation formats</li>
    <li>Multi-column articles where citation boundaries are ambiguous</li>
    <li>Papers with extensive in-text author listings (e.g., 'Smith, Jones, Williams, and Brown, 2020')</li>
  </ul>
  
  <p><strong>Note:</strong> AI-enhanced extraction requires a configured <strong>Google Gemini API key</strong> in Settings. See the <em>Biblio AI</em> help section for setup instructions.</p>
  
  <br>
  
  <h4>⚙️ Step 2: Analysis Parameters</h4>
  <p>Users can customize the extraction and analysis through several key parameters:</p>
  
  <h5>Context Window Size (words)</h5>
  <p>Defines the number of words to extract <em>before and after</em> each citation. Default is <strong>20 words</strong>.</p>
  <ul>
    <li><strong>Smaller windows (5-10 words):</strong> Capture only immediate context, useful for identifying direct citation purposes (e.g., methodology references).</li>
    <li><strong>Medium windows (15-30 words):</strong> Balance between context richness and data volume. Suitable for most analyses.</li>
    <li><strong>Larger windows (40-50 words):</strong> Capture broader argumentative context, useful for discourse analysis or understanding how citations are integrated into narrative flow.</li>
  </ul>
  
  <h5>Max Distance for Network (characters)</h5>
  <p>Defines the maximum character distance between two citations to be considered 'co-occurring' in the network analysis. Default is <strong>800 characters</strong> (roughly 120-150 words).</p>
  <ul>
    <li><strong>Shorter distances (300-500 chars):</strong> Identify only tightly co-cited references, revealing core conceptual links.</li>
    <li><strong>Medium distances (600-1000 chars):</strong> Capture paragraph-level co-citations, showing related but distinct concepts.</li>
    <li><strong>Longer distances (1200-2000 chars):</strong> Include section-level co-citations, useful for broad thematic analysis but may create noisy networks.</li>
  </ul>
  
  <h5>Advanced Options</h5>
  <ul>
    <li><strong>Parse complex multiple citations:</strong> Attempts to separate compound citations like '(Smith 2020; Jones 2019; Williams et al. 2018)' into individual references. This improves network accuracy but may increase processing time.</li>
    <li><strong>Remove stopwords from analysis:</strong> Excludes common words (e.g., 'the', 'and', 'of') from word frequency and trend analyses, focusing on substantive terms.</li>
    <li><strong>Custom stopwords (comma-separated):</strong> Add domain-specific stopwords (e.g., 'study', 'research', 'analysis') to refine term extraction for your field.</li>
  </ul>
  
  <br>
  
  <h4>📊 Analysis Results and Tabs</h4>
  
  <br>
  
  <h5>1️⃣ Descriptive Statistics</h5>
  <p>Provides an overview of the document's structural and linguistic characteristics:</p>
  
  <p><strong>Document Metrics:</strong></p>
  <ul>
    <li><strong>Total Words:</strong> Overall word count (excluding references section if detected).</li>
    <li><strong>Citations Found:</strong> Number of unique in-text citations identified.</li>
    <li><strong>Narrative Citations:</strong> Citations that include author names in the sentence (e.g., 'As Smith (2020) demonstrated...').</li>
    <li><strong>Citation Density:</strong> Citations per 1000 words, indicating reference saturation. Typical ranges:
      <ul>
        <li>0-5: Light citation (review articles often have 10-20+)</li>
        <li>5-10: Moderate (common in empirical studies)</li>
        <li>10+: Heavy (common in systematic reviews or theoretical papers)</li>
      </ul>
    </li>
  </ul>
  
  <p><strong>Readability Indices:</strong></p>
  <ul>
    <li><strong>Flesch-Kincaid Grade:</strong> Estimates U.S. grade level required to understand the text. Values of 12-14 indicate college-level reading, while 16+ suggests graduate-level complexity.</li>
    <li><strong>Reading Ease (Flesch):</strong> 0-100 scale where higher scores indicate easier readability. Scientific papers typically score 20-40 (difficult to very difficult).</li>
    <li><strong>ARI Index (Automated Readability Index):</strong> Another grade-level estimate based on character count rather than syllables. Generally correlates with Flesch-Kincaid but may differ for technical texts.</li>
    <li><strong>Gunning Fog Index:</strong> Estimates years of formal education needed. Values above 17 indicate very complex, technical prose common in specialized research.</li>
  </ul>
  
  <p><strong>Text Statistics:</strong></p>
  <ul>
    <li><strong>Characters, Words, Sentences:</strong> Basic text volume metrics.</li>
    <li><strong>Syllables:</strong> Used in readability calculations.</li>
    <li><strong>Complex Words:</strong> Words with 3+ syllables, expressed as count and percentage. Higher percentages (>30%) indicate technical vocabulary.</li>
    <li><strong>Avg words/sentence:</strong> Sentence length. Scientific writing typically ranges from 15-25 words/sentence. Very long sentences (>30) may reduce readability.</li>
    <li><strong>Lexical Diversity:</strong> Ratio of unique words to total words. Higher diversity (>0.5) suggests varied vocabulary; lower values (<0.4) may indicate repetitive or formulaic writing.</li>
  </ul>
  
  <p><strong>N-grams Analysis:</strong> Displays the most frequent unigrams (single words), bigrams (two-word phrases), and trigrams (three-word phrases) in the document. This reveals key concepts and repeated terminology.</p>
  
  <br>
  
  <h5>2️⃣ Word Trends</h5>
  <p>Visualizes how selected terms are distributed across the document's sections. This analysis helps understand the <em>thematic flow</em> and identify where specific concepts are emphasized.</p>
  
  <p><strong>Features:</strong></p>
  <ul>
    <li><strong>Track up to 10 terms:</strong> Select from the most frequent words or enter custom terms (e.g., domain-specific keywords).</li>
    <li><strong>Segmentation options:</strong>
      <ul>
        <li><strong>Auto (use sections if available):</strong> Automatically detects standard sections (Abstract, Introduction, Methods, Results, Discussion, Conclusion) if structured.</li>
        <li><strong>Document sections:</strong> Manually defined sections based on detected headers.</li>
        <li><strong>Equal-length segments:</strong> Divides the document into uniform chunks (e.g., quartiles) regardless of logical structure.</li>
      </ul>
    </li>
    <li><strong>Visualization types:</strong>
      <ul>
        <li><strong>Line chart:</strong> Shows temporal trends for each term across segments.</li>
        <li><strong>Area chart:</strong> Emphasizes volume changes with filled areas under lines.</li>
      </ul>
    </li>
  </ul>
  
  <p><strong>Interpretation Examples:</strong></p>
  <ul>
    <li>A term peaking in the <strong>Introduction</strong> but absent in <strong>Results</strong> may indicate a concept discussed in literature review but not directly addressed in the study.</li>
    <li>Uniform distribution suggests a <strong>central theme</strong> integrated throughout the paper.</li>
    <li>Concentration in <strong>Methods</strong> indicates technical or procedural terminology.</li>
    <li>Terms appearing only in <strong>Discussion</strong> may represent emerging implications or future directions.</li>
  </ul>
  
  <p><strong>Distribution Statistics:</strong> Provides detailed frequency counts and statistical measures (mean, standard deviation, skewness) for each tracked term across segments.</p>
  
  <br>
  
  <h5>3️⃣ In-Context Citations</h5>
  <p>Displays each extracted citation with its surrounding context window, enabling <strong>qualitative citation analysis</strong>. This is one of the most powerful features for understanding <em>why</em> and <em>how</em> sources are cited.</p>
  
  <p><strong>Features:</strong></p>
  <ul>
    <li><strong>Searchable list:</strong> Filter citations by searching for specific authors, keywords, or phrases.</li>
    <li><strong>Minimum context words:</strong> Set a threshold to exclude citations with insufficient context (useful for filtering out reference-only lists or captions).</li>
    <li><strong>Grouping options:</strong>
      <ul>
        <li><strong>Auto (use sections if available):</strong> Groups citations by paper section (e.g., all Introduction citations together).</li>
        <li><strong>Document sections:</strong> Uses manually detected section headers.</li>
        <li><strong>Equal-length segments:</strong> Groups citations by position in the document (e.g., first quartile, second quartile).</li>
      </ul>
    </li>
    <li><strong>Citation type identification:</strong> The interface distinguishes between:
      <ul>
        <li><strong>Parenthetical citations:</strong> References in parentheses, often used for supporting evidence.</li>
        <li><strong>Narrative citations:</strong> Author names integrated into sentence structure, typically indicating more substantive engagement.</li>
      </ul>
    </li>
    <li><strong>Reference matching:</strong> When possible, the system attempts to match in-text citations to the corresponding full reference in the bibliography. A green indicator shows successful matches; hover to view full reference details.</li>
  </ul>
  
  <p><strong>Analytical Uses:</strong></p>
  <ul>
    <li><strong>Citation Function Analysis:</strong> Categorize citations by their role (e.g., establishing theoretical framework, justifying methodology, supporting findings, contrasting results).</li>
    <li><strong>Author Authority:</strong> Identify which authors are cited most frequently and in what contexts (e.g., some authors may be cited exclusively for methods, others for theory).</li>
    <li><strong>Hedging and Certainty:</strong> Examine the language surrounding citations to assess how authors express confidence or uncertainty (e.g., 'Smith (2020) demonstrated...' vs. 'some studies suggest... (Smith 2020)').</li>
    <li><strong>Self-Citation Patterns:</strong> Identify author self-citations and analyze whether they serve substantive or promotional purposes.</li>
  </ul>
  
  <p><strong>Export Contexts:</strong> Use the 'Export Contexts' button to download all citation contexts as a structured dataset (CSV format) for external qualitative coding or further analysis in tools like NVivo, ATLAS.ti, or custom R scripts.</p>
  
  <br>
  
  <h5>4️⃣ Network Analysis</h5>
  <p>Generates a <strong>citation co-occurrence network</strong> that visualizes which references are cited near each other in the text. This reveals the intellectual structure and conceptual clusters within the paper.</p>
  
  <p><strong>Network Construction:</strong></p>
  <ul>
    <li><strong>Nodes:</strong> Each node represents a cited reference (identified by first author and year).</li>
    <li><strong>Edges:</strong> A link is created between two references if they appear within the specified <em>Max Distance</em> parameter (default: 800 characters).</li>
    <li><strong>Node Size:</strong> Proportional to the number of times each reference is cited in the document (total citation frequency).</li>
    <li><strong>Node Color:</strong> Represents the document section where the reference appears most frequently, helping identify whether certain clusters are methodological, theoretical, or results-focused.</li>
  </ul>
  
  <p><strong>Interpretation:</strong></p>
  <ul>
    <li><strong>Densely connected clusters:</strong> Groups of tightly co-cited references indicate core conceptual or methodological frameworks that the paper builds upon.</li>
    <li><strong>Bridge references:</strong> Nodes connecting otherwise separate clusters represent interdisciplinary links or integrative studies that synthesize multiple research traditions.</li>
    <li><strong>Peripheral isolates:</strong> References cited alone without nearby co-citations may serve specific, standalone purposes (e.g., citing a statistical test or a single example).</li>
    <li><strong>Section-based coloring:</strong> If most nodes are blue (Introduction), the paper heavily relies on literature review. If red (Methods) dominates, it's methodology-focused.</li>
  </ul>
  
  <p><strong>Network Metrics:</strong> While not explicitly displayed, users can infer centrality and clustering qualitatively:
    <ul>
      <li><strong>Central nodes (many connections):</strong> Foundational references that anchor the paper's argument.</li>
      <li><strong>Betweenness (bridge position):</strong> References connecting distinct themes, suggesting synthesis or interdisciplinary work.</li>
    </ul>
  </p>
  
  <p><strong>Export Network:</strong> Download the network as a graph file (GraphML or edge list format) for advanced analysis in specialized network software like Gephi, Cytoscape, or igraph in R.</p>
  
  <p><strong>Legend:</strong> The right panel displays a legend showing the color coding for different document sections. This helps quickly identify which parts of the paper contribute most to the citation network.</p>
  
  <br>
  
  <h5>5️⃣ References</h5>
  <p>Displays the complete <strong>bibliography</strong> extracted from the document. References are automatically parsed and enriched with metadata from <strong>Crossref</strong> and <strong>OpenAlex</strong> databases when DOIs are available.</p>
  
  <p><strong>Features:</strong></p>
  <ul>
    <li><strong>Search functionality:</strong> Find specific references by author name, title, year, or DOI.</li>
    <li><strong>Source indicators:</strong> Icons show the data source:
      <ul>
        <li><strong>PDF icon:</strong> Reference extracted directly from the PDF.</li>
        <li><strong>Crossref icon:</strong> Metadata retrieved from Crossref (indicates DOI-based matching).</li>
        <li><strong>OpenAlex icon:</strong> Metadata enriched from OpenAlex (provides additional fields like citation counts, authors' affiliations).</li>
      </ul>
    </li>
    <li><strong>View Details:</strong> Click on any reference to open a detailed modal showing:
      <ul>
        <li>Full author list</li>
        <li>Publication year and journal/venue</li>
        <li>DOI (with direct link to the publisher)</li>
        <li>Abstract (if available)</li>
        <li>Citation counts and impact metrics from OpenAlex</li>
      </ul>
    </li>
    <li><strong>Export References:</strong> Download the bibliography in various formats (BibTeX, RIS, CSV) for import into reference managers like Zotero, Mendeley, or EndNote.</li>
  </ul>
  
  <p><strong>Reference Matching Quality:</strong></p>
  <ul>
    <li><strong>High confidence (green):</strong> Reference successfully matched to Crossref/OpenAlex with high certainty (exact DOI or strong title/author match).</li>
    <li><strong>Low confidence (yellow):</strong> Partial match based on fuzzy title/author similarity; manual verification recommended.</li>
    <li><strong>No match (red):</strong> Reference could not be matched to external databases. Possible reasons include:
      <ul>
        <li>Missing or incorrect DOI</li>
        <li>Pre-print or non-indexed publication</li>
        <li>Parsing errors in reference extraction</li>
        <li>Non-standard formatting in the original PDF</li>
      </ul>
    </li>
  </ul>
  
  <p><strong>Total References:</strong> The header shows the breakdown:
    <ul>
      <li><strong>Total References:</strong> All unique references found in the document.</li>
      <li><strong>From PDF:</strong> References extracted directly from the PDF (may have parsing imperfections).</li>
      <li><strong>From Crossref:</strong> References successfully matched to the Crossref database.</li>
      <li><strong>From OpenAlex:</strong> References matched to OpenAlex (often overlaps with Crossref but provides broader coverage for non-DOI works).</li>
    </ul>
  </p>
  
  <br>
  
  <h5>6️⃣ BiblioAI Summary</h5>
  <p>Uses <strong>Google Gemini</strong> AI models to generate intelligent, context-aware summaries of the analyzed document. This feature transforms raw PDF content into structured, actionable insights.</p>
  
  <p><strong>Summary Types:</strong></p>
  <ul>
    <li><strong>Short Abstract (250 words):</strong> A concise overview covering the main research question, methodology, key findings, and conclusions. Ideal for quick reference or creating study cards.</li>
    <li><strong>Narrative Abstract (500-600 words):</strong> A detailed, paragraph-form summary that places the research in context, explains the study's rationale, describes methods and results, and discusses implications. Suitable for grant applications or research summaries.</li>
    <li><strong>IMRaD Structure Summary:</strong> A structured summary organized by the traditional scientific paper format:
      <ul>
        <li><strong>Introduction:</strong> Background, research gap, and objectives</li>
        <li><strong>Methods:</strong> Study design, data sources, analytical approach</li>
        <li><strong>Results:</strong> Key findings with quantitative details where applicable</li>
        <li><strong>Discussion:</strong> Interpretation, limitations, and implications</li>
      </ul>
      This format is particularly useful for systematic reviews or meta-analyses where standardized extraction is needed.
    </li>
    <li><strong>Thematic Bibliography:</strong> Generates a thematic categorization of the paper's references, grouping cited works by conceptual topic (e.g., 'Theoretical Foundations', 'Methodological Approaches', 'Empirical Evidence', 'Critiques and Limitations'). This is invaluable for:
      <ul>
        <li>Understanding how the author organizes their intellectual framework</li>
        <li>Identifying key reference clusters for literature review purposes</li>
        <li>Discovering reading lists organized by research theme</li>
      </ul>
    </li>
    <li><strong>Research Questions & Context:</strong> Extracts and articulates:
      <ul>
        <li>The main research question(s) or hypotheses</li>
        <li>The broader research context and motivation</li>
        <li>Key theoretical or empirical gaps the study addresses</li>
        <li>The study's positioning within its field</li>
      </ul>
      Useful for understanding the strategic positioning of the research and for identifying potential research directions.
    </li>
  </ul>
  
  <p><strong>Customization:</strong></p>
  <ul>
    <li><strong>Edit Prompt:</strong> The AI summary generation is driven by an editable prompt template. Users can:
      <ul>
        <li>Add specific questions to the prompt (e.g., 'What statistical methods were used?')</li>
        <li>Specify output format preferences</li>
        <li>Include contextual information (e.g., 'This paper is from a special issue on climate change')</li>
        <li>Request focus on particular sections (e.g., 'Emphasize methodological contributions')</li>
      </ul>
    </li>
    <li><strong>Language Selection:</strong> Summaries can be generated in multiple languages (depending on Gemini model support), making international collaboration and translation easier.</li>
  </ul>
  
  <p><strong>Requirements:</strong></p>
  <ul>
    <li>A valid <strong>Google Gemini API key</strong> must be configured in <em>Settings</em>. Free tier limits allow for moderate usage (typically 15-60 requests per minute depending on the model).</li>
    <li>The 'API Key Configured' indicator must show green for the feature to work.</li>
  </ul>
  
  <p><strong>Best Practices:</strong></p>
  <ul>
    <li><strong>Verify AI outputs:</strong> While Gemini models are highly capable, always review generated summaries for accuracy, especially for technical details or numerical results.</li>
    <li><strong>Use appropriate summary types:</strong> Short abstracts are great for skimming large numbers of papers, while IMRaD summaries are better for in-depth study or quality appraisal.</li>
    <li><strong>Combine with manual review:</strong> AI summaries should complement, not replace, human reading. Use them to prioritize which papers to read in full.</li>
    <li><strong>Iterate prompts:</strong> If the initial summary misses key information, refine your prompt with more specific instructions.</li>
  </ul>
  
  <br>
  
  <h4>🧠 Integration with Biblio AI</h4>
  <p><strong>Content Analysis</strong> is fully integrated with the <strong>Biblio AI</strong> ecosystem. Throughout the analysis tabs, users can activate AI-assisted interpretation panels that:</p>
  <ul>
    <li>Explain patterns in the citation network (e.g., why certain clusters form)</li>
    <li>Interpret readability scores in the context of the target audience and field norms</li>
    <li>Suggest alternative analyses or parameters based on detected document characteristics</li>
    <li>Generate publication-ready text descriptions of figures and results</li>
  </ul>
  
  <p>These dynamic interpretations adapt to your specific document and parameters, providing contextualized guidance rather than generic explanations.</p>
  
  <br>
  
  <h4>💡 Advanced Use Cases</h4>
  
  <h5>1. Citation Context Mining for Meta-Research</h5>
  <p>Researchers studying citation practices can use Content Analysis to:</p>
  <ul>
    <li>Quantify the proportion of narrative vs. parenthetical citations across papers</li>
    <li>Analyze sentiment or evaluative language in citation contexts (requires exporting contexts for sentiment analysis)</li>
    <li>Study how often citations are backed by direct quotes vs. paraphrases</li>
    <li>Examine self-citation contexts to distinguish between necessary methodological references and gratuitous self-promotion</li>
  </ul>
  
  <h5>2. Quality and Transparency Assessment</h5>
  <p>Evaluate methodological transparency by:</p>
  <ul>
    <li>Checking if key methodological references appear in the Methods section (using In-Context Citations and section grouping)</li>
    <li>Identifying whether data sources and statistical tests are properly cited</li>
    <li>Assessing whether limitations are discussed with appropriate citations to prior critiques</li>
  </ul>
  
  <h5>3. Comparative Literature Review</h5>
  <p>Analyze multiple papers on the same topic by:</p>
  <ul>
    <li>Running Content Analysis on several key papers</li>
    <li>Comparing their citation networks to identify consensus foundational references</li>
    <li>Examining differences in thematic emphasis using Word Trends</li>
    <li>Creating a merged bibliography of all thematic bibliographies from BiblioAI summaries</li>
  </ul>
  
  <h5>4. Pedagogical Applications</h5>
  <p>Use Content Analysis to teach:</p>
  <ul>
    <li><strong>Citation Skills:</strong> Show students examples of effective narrative vs. parenthetical citations.</li>
    <li><strong>Literature Review Structure:</strong> Demonstrate how successful papers organize their conceptual frameworks using network analysis.</li>
    <li><strong>Writing Clarity:</strong> Use readability scores to illustrate the balance between technical precision and accessibility.</li>
    <li><strong>Source Integration:</strong> Highlight how professional academics weave citations into argumentative flow.</li>
  </ul>
  
  <h5>5. Editorial and Peer Review Support</h5>
  <p>Editors and reviewers can use Content Analysis to:</p>
  <ul>
    <li>Quickly assess whether a manuscript cites the appropriate foundational literature (using network analysis to check for missing clusters)</li>
    <li>Identify potential plagiarism or over-reliance on a single source (using n-gram analysis and citation frequency)</li>
    <li>Evaluate whether methods and results are properly supported by citations</li>
    <li>Generate constructive feedback on readability for papers that score poorly on standard metrics</li>
  </ul>
  
  <br>
  
  <h4>📌 Best Practices</h4>
  <ul>
    <li><strong>PDF Quality Matters:</strong> Text-based PDFs (not scanned images) produce the most accurate results. Use OCR pre-processing for image-based PDFs.</li>
    <li><strong>Check Citation Parsing:</strong> Always review the 'Citations Found' count. If it seems unusually low, try enabling AI-enhanced extraction or manually specifying the citation format.</li>
    <li><strong>Balance Context Windows:</strong> Larger windows provide richer qualitative data but increase processing time and data volume. Start with default settings (20 words) and adjust based on your analytical needs.</li>
    <li><strong>Export for Deep Dives:</strong> For complex citation function analysis or qualitative coding, export the citation contexts to CSV and work with specialized qualitative data analysis software.</li>
    <li><strong>Combine with Traditional Bibliometrics:</strong> Content Analysis is designed to complement, not replace, traditional bibliometric methods. Use it alongside tools like <em>Data → Overview</em> or <em>Conceptual Structure</em> for a complete picture.</li>
    <li><strong>Mind the Model Limits:</strong> AI-powered features (enhanced extraction, BiblioAI summaries) have rate limits and token constraints. For very long documents (>50 pages), summaries may truncate; consider analyzing sections separately.</li>
  </ul>
  
  <br>
  
  <h4>⚠️ Limitations and Considerations</h4>
  <ul>
    <li><strong>Citation Format Variability:</strong> Non-standard or inconsistent citation formats may result in incomplete extraction. Manual verification is recommended for critical analyses.</li>
    <li><strong>Automatic Section Detection:</strong> The system attempts to identify standard sections (Introduction, Methods, etc.) using heuristics. Papers with unconventional structures may be segmented incorrectly. Use 'Equal-length segments' as a fallback.</li>
    <li><strong>Language Support:</strong> While the tool supports PDFs in any language, AI-powered features (enhanced extraction, summaries) are optimized for English. Other languages may produce less accurate results.</li>
    <li><strong>Reference Matching Accuracy:</strong> Matching in-text citations to bibliography entries relies on fuzzy matching when DOIs are unavailable. Ambiguous references (e.g., 'Smith et al., 2020' when multiple Smiths exist) may match incorrectly.</li>
    <li><strong>Network Interpretation:</strong> Citation co-occurrence does not imply intellectual similarity. References may appear together for diverse reasons (e.g., contrasting studies, citing a methods paper alongside an application). Always combine network analysis with context reading.</li>
    <li><strong>AI Hallucinations:</strong> While rare, Biblio AI summaries may occasionally include plausible but incorrect details not present in the original text. Critical applications (e.g., systematic reviews) should verify AI outputs against the source document.</li>
    <li><strong>Database Coverage:</strong> Reference enrichment from Crossref/OpenAlex is limited to indexed works with DOIs. Books, preprints, gray literature, or very recent papers may not be matched.</li>
  </ul>
  
  <br>
  
  <h4>🔄 Integration with Biblioshiny Workflow</h4>
  <p><strong>Content Analysis</strong> complements other Biblioshiny modules:</p>
  <ul>
    <li><strong>Import Phase:</strong> After collecting a bibliographic dataset (e.g., from Web of Science), use Content Analysis to deeply examine a few key highly-cited papers identified in <em>Most Local Cited Documents</em>.</li>
    <li><strong>Conceptual Structure Analysis:</strong> Once you've identified thematic clusters using co-word analysis or MCA, select representative papers from each cluster and use Content Analysis to understand how those themes are discussed and cited within individual papers.</li>
    <li><strong>Intellectual Structure:</strong> After running co-citation or bibliographic coupling networks, use Content Analysis on the central nodes to see <em>why</em> they're central—are they cited together because they address the same method, theory, or empirical finding?</li>
    <li><strong>Trend Topics:</strong> When you identify an emerging trend, analyze a seminal paper from that trend to understand its intellectual roots and citation context.</li>
  </ul>
  
  <br>
  
  <h4>📚 Key References</h4>
  
  <p><strong>Main References on Bibliometrix and Content Analysis Tools:</strong></p>
  	
  <p><strong>Aria, M., & Cuccurullo, C. (2025).</strong> <i>contentanalysis: Scientific Content and Citation Analysis from PDF Documents.</i> <strong>[R package]</strong>. <a href='https://doi.org/10.32614/CRAN.package.contentanalysis' target='_blank'>https://doi.org/10.32614/CRAN.package.contentanalysis</a></p>
  
  <p><strong>Aria, M., & Cuccurullo, C. (2017).</strong> <i>bibliometrix: An R-tool for comprehensive science mapping analysis.</i> <strong>Journal of Informetrics</strong>, 11(4), 959–975. <a href='https://doi.org/10.1016/j.joi.2017.08.007' target='_blank'>https://doi.org/10.1016/j.joi.2017.08.007</a></p>
  
  <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>Comparative science mapping: a novel conceptual structure analysis with metadata.</i> <strong>Scientometrics</strong>. <a href='https://doi.org/10.1007/s11192-024-05161-6' target='_blank'>https://doi.org/10.1007/s11192-024-05161-6</a></p>
  
  <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2022).</strong> <i>Thematic Analysis as a New Culturomic Tool: The Social Media Coverage on COVID-19 Pandemic in Italy.</i> <strong>Sustainability</strong>, 14(6), 3643. <a href='https://doi.org/10.3390/su14063643' target='_blank'>https://doi.org/10.3390/su14063643</a></p>
  
  <p><strong>Aria, M., Misuraca, M., & Spano, M. (2020).</strong> <i>Mapping the evolution of social research and data science on 30 years of Social Indicators Research.</i> <strong>Social Indicators Research</strong>, 149, 803–831. <a href='https://doi.org/10.1007/s11205-020-02281-3' target='_blank'>https://doi.org/10.1007/s11205-020-02281-3</a></p>
  
  <br>
  
  <p><strong>Quantitative Content Analysis - Foundational Works:</strong></p>
  
  <p><strong>Berelson, B. (1952).</strong> <i>Content Analysis in Communication Research.</i> Glencoe, IL: Free Press.</p>
  
  <p><strong>Krippendorff, K. (2018).</strong> <i>Content Analysis: An Introduction to Its Methodology</i> (4th ed.). Thousand Oaks, CA: SAGE Publications.</p>
  
  <p><strong>Neuendorf, K. A. (2017).</strong> <i>The Content Analysis Guidebook</i> (2nd ed.). Thousand Oaks, CA: SAGE Publications.</p>
  
  <p><strong>Weber, R. P. (1990).</strong> <i>Basic Content Analysis</i> (2nd ed.). Newbury Park, CA: SAGE Publications.</p>
  
  <br>
  
  <h4>🎓 Further Reading</h4>
  <p>For more information on using Content Analysis and related techniques, see:</p>
  <ul>
    <li><strong>bibliometrix documentation:</strong> <a href='https://www.bibliometrix.org' target='_blank'>https://www.bibliometrix.org</a></li>
    <li><strong>contentanalysis documentation:</strong> <a href='https://massimoaria.github.io/contentanalysis-website/' target='_blank'>https://massimoaria.github.io/contentanalysis-website/</a></li>
    <li><strong>Content analysis in communication research:</strong> Riffe, D., Lacy, S., & Fico, F. (2014). <i>Analyzing Media Messages: Using Quantitative Content Analysis in Research</i> (3rd ed.). Routledge.</li>
  </ul>
  
  </div>
  </body>"

  return(list(
    biblioAI = biblioAI,
    saas = saas,
    info = info,
    publications = publications,
    filters = filters,
    authorProfile = authorProfile,
    referenceMatching = referenceMatching,
    importOrLoad = importOrLoad,
    api = api,
    mergeCollections = mergeCollections,
    mainInformation = mainInformation,
    lifeCycle = lifeCycle,
    threeFieldPlot = threeFieldPlot,
    contentAnalysis = contentAnalysis
  ))
}
