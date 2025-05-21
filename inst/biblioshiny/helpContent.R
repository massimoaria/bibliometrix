helpContent <- function(){
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
#   "<body lang=IT link='#467886' vlink='#96607D' style='tab-interval:35.4pt;
# word-wrap:break-word'>
#   
#   <div class=WordSection1>
#     
#     <p class=MsoNormal align=center style='text-align:center'><b><span lang=EN-US
#     style='font-size:16.0pt;font-family:'Times New Roman',serif;color:black;
# background:white;mso-ansi-language:EN-US'>Supported Bibliographic Databases and
#     Suggested File Formats<o:p></o:p></span></b></p>
#       
#       <p class=MsoNormal><b><span lang=EN-US style='font-size:12.0pt;font-family:
# 'Times New Roman',serif;color:black;background:white;mso-ansi-language:EN-US'><o:p>&nbsp;</o:p></span></b></p>
#       
#       <p class=MsoNormal><b><span lang=EN-US style='font-size:12.0pt;font-family:
# 'Times New Roman',serif;color:black;background:white;mso-ansi-language:EN-US'><o:p>&nbsp;</o:p></span></b></p>
#       
#       <p class=MsoNormal><b><span lang=EN-US style='font-size:12.0pt;font-family:
# 'Times New Roman',serif;color:black;background:white;mso-ansi-language:EN-US'>Biblioshiny</span></b><span
#     lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# color:black;background:white;mso-ansi-language:EN-US'> imports and analyzes
#     collection exported from:<o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpFirst style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>Web of Science </span></em><em><span
#     lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>(</span></em><span style='font-size:12.0pt;font-family:
# 'Times New Roman',serif'><a href='http://www.webofscience.com'><span
#                           lang=EN-US style='background:white;mso-ansi-language:EN-US'>www.webofscience.com</span></a></span><em><span
#                           lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>)</span></em><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'><o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpMiddle style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>Scopus</span></em><em><span
#     lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'> (</span></em><span style='font-size:12.0pt;
# font-family:'Times New Roman',serif'><a href='http://www.scopus.com'><span
#                            lang=EN-US style='background:white;mso-ansi-language:EN-US'>www.scopus.com</span></a></span><em><span
#                            lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>) </span></em><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'><o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpMiddle style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><i><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'>OpenAlex</span></i><span lang=EN-US style='font-size:12.0pt;font-family:
# 'Times New Roman',serif;color:black;background:white;mso-ansi-language:EN-US'> (</span><span
#                                                                                 style='font-size:12.0pt;font-family:'Times New Roman',serif'><a
#                                                                                 href='http://www.openalex.org'><span lang=EN-US style='background:white;
# mso-ansi-language:EN-US'>www.openalex.org</span></a></span><span lang=EN-US
#                                                                                 style='font-size:12.0pt;font-family:'Times New Roman',serif;color:black;
# background:white;mso-ansi-language:EN-US'>) <o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpMiddle style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>Dimensions </span></em><em><span
#     lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>(</span></em><span style='font-size:12.0pt;font-family:
# 'Times New Roman',serif'><a href='http://www.dimensions.ai'><span lang=EN-US
#                           style='background:white;mso-ansi-language:EN-US'>www.dimensions.ai</span></a></span><em><span
#                           lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>) </span></em><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'><o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpMiddle style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>The Lens </span></em><em><span
#     lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>(</span></em><span style='font-size:12.0pt;font-family:
# 'Times New Roman',serif'><a href='http://www.lens.org'><span lang=EN-US
#                           style='background:white;mso-ansi-language:EN-US'>www.lens.org</span></a></span><em><span
#                           lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>) </span></em><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'><o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpMiddle style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>PubMed</span></em><em><span
#     lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'> (</span></em><span style='font-size:12.0pt;
# font-family:'Times New Roman',serif'><a href='https://pubmed.ncbi.nlm.nih.gov/'><span
#                            lang=EN-US style='background:white;mso-ansi-language:EN-US'>https://pubmed.ncbi.nlm.nih.gov/</span></a></span><em><span
#                            lang=EN-US style='font-size:12.0pt;color:black;background:white;mso-ansi-language:
# EN-US;font-style:normal'>) </span></em><span lang=EN-US style='font-size:12.0pt;
# font-family:'Times New Roman',serif;color:black;background:white;mso-ansi-language:
# EN-US'><o:p></o:p></span></p>
#       
#       <p class=MsoListParagraphCxSpLast style='text-indent:-18.0pt;mso-list:l0 level1 lfo1'><![if !supportLists]><span
#     lang=EN-US style='font-size:12.0pt;font-family:Symbol;mso-fareast-font-family:
# Symbol;mso-bidi-font-family:Symbol;color:black;mso-ansi-language:EN-US'><span
#     style='mso-list:Ignore'>·<span style='font:7.0pt 'Times New Roman''>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
#     </span></span></span><![endif]><em><span lang=EN-US style='font-size:12.0pt;
# color:black;background:white;mso-ansi-language:EN-US'>Cochrane Library</span></em><span
#     lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# color:black;background:white;mso-ansi-language:EN-US'> (</span><span
#                                                         style='font-size:12.0pt;font-family:'Times New Roman',serif'><a
#                                                         href='http://www.cochranelibrary.com/'><span lang=EN-US style='background:white;
# mso-ansi-language:EN-US'>www.cochranelibrary.com/</span></a></span><span
#                                                         lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# color:black;background:white;mso-ansi-language:EN-US'>) <o:p></o:p></span></p>
#       
#       <p class=MsoNormal><span lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# color:black;background:white;mso-ansi-language:EN-US'><o:p>&nbsp;</o:p></span></p>
#       
#       <p class=MsoNormal><span lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# color:black;background:white;mso-ansi-language:EN-US'><o:p>&nbsp;</o:p></span></p>
#       
#       <p style='margin-top:6.0pt;margin-right:0cm;margin-bottom:6.0pt;margin-left:
# 0cm;background:white'><i><span lang=EN-US style='font-size:12.0pt;color:black;
# mso-ansi-language:EN-US'>Web of Science</span></i><span lang=EN-US
#     style='font-size:12.0pt;color:black;mso-ansi-language:EN-US'>, <i>Scopus</i>
#       and <i>OpenAlex</i> allow the user to export the complete set of metadata. This
#     means that it will be possible to perform all analyses implemented in biblioshiny.
#     <o:p></o:p></span></p>
#       
#       <p style='margin-top:6.0pt;margin-right:0cm;margin-bottom:6.0pt;margin-left:
# 0cm;background:white'><span lang=EN-US style='font-size:12.0pt;color:black;
# mso-ansi-language:EN-US'>Some other databases, such as Dimensions, PubMed and
#     Cochrane Library, export just a limited set of metadata types which implies
#     some limitations in the choice of the analyses to carry out.<o:p></o:p></span></p>
#       
#       <p style='margin-top:6.0pt;margin-right:0cm;margin-bottom:6.0pt;margin-left:
# 0cm;background:white'><span lang=EN-US style='font-size:12.0pt;color:black;
# mso-ansi-language:EN-US'>In the following table, for each database we repot the
#     file formats supported and the metadata contained in each. Finally, the
#     suggested file format is reported.<o:p></o:p></span></p>
#       
#       <p class=MsoNormal><span lang=EN-US style='font-size:12.0pt;font-family:'Times New Roman',serif;
# mso-ansi-language:EN-US'><o:p>&nbsp;</o:p></span></p>
#       
#       </div>
#       
#       </body>"

  
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

  
  return(list(info=info, publications=publications))
}