#' Generate a specific prompt for summarizing a scientific article with Gemini
#'
#' @param summary_type A character string specifying the type of summary.
#'   Valid options are: "short_abstract", "narrative_abstract", "imrad_summary",
#'   "thematic_bibliography", "research_questions", "background_literature",
#'   "methods_summary", "implications", "list_tables_figures".
#' @return A character string containing the prompt for the Gemini API.
#' @examples
#' # Get the prompt for a short abstract
#' prompt <- get_article_summary_prompt("short_abstract")
#' cat(prompt)
#'
#' # Get the prompt for extracting methods
#' prompt_methods <- get_article_summary_prompt("methods_summary")
#' cat(prompt_methods)

get_article_summary_prompt <- function(summary_type) {
  # Use a switch statement to map the input to the correct prompt
  prompt <- switch(
    summary_type,
    # Article summarization prompts
    "short_abstract" = {
      "Based on the provided scientific article, generate a concise, one-paragraph abstract (around 250 words) summarizing the study's primary objective, methods, key findings, and main conclusions. The summary should be suitable for a quick overview of the research."
    },
    "narrative_abstract" = {
      "From the attached scientific article, write a narrative extended abstract of no more than 500-600 words. Instead of a formal summary, tell the story of the research. Begin with the problem that motivated the study, describe the research process (methods), reveal the key discoveries (results), and conclude with the broader meaning and implications of the findings. The tone should be engaging and accessible."
    },
    "imrad_summary" = {
      "Please analyze the provided article and summarize it following the IMRaD structure. Create a distinct section for each of the following, using these exact headings:\n- **Introduction:** State the research problem, objectives, and its significance.\n- **Methods:** Describe the methodology, data collection, and analysis procedures.\n- **Results:** Present the main findings of the study without interpretation.\n- **Discussion:** Interpret the results, discuss their implications, mention limitations, and suggest future research."
    },
    "thematic_bibliography" = {
      "Analyze the bibliography/reference list of the attached scientific article. Identify the main research themes present in the cited works and group the references accordingly. For each theme, provide a brief description followed by a list of the corresponding citations from the article's bibliography. If the citations within the scientific article's text are numbered, report the author(s), title, and journal for each reference instead of just the number."
    },
    # Focus on article parts: questions, background, methods, implications, tables/figures
    "research_questions" = {
      "Extract the following specific information from the provided article. Present the answer under these exact headings:\n- **Research Questions:** List the explicit or implicit research questions the study aims to answer.\n- **Research Context:** Describe the setting, population, or specific environment in which the research was conducted.\n- **Motivation:** Explain the gap in knowledge, problem, or observation that motivated this research."
    },
    "background_literature" = {
      "Summarize the 'Background,' 'Literature Review,' or 'Theoretical Framework' section of the attached article. Identify and explain the key concepts, theories, and previous research that form the foundation for this study. Highlight the scholarly conversation the authors are contributing to."
    },
    "methods_summary" = {
      "Provide a detailed summary of the methodology used in the provided article. Structure your response with the following headings:\n- **Research Design:** Describe the overall approach (e.g., experimental, survey, case study).\n- **Data Collection:** Explain how the data was gathered, including the instruments or protocols used.\n- **Data Analysis:** Detail the statistical or qualitative techniques used to analyze the data.\n- **Tools:** List any specific software or equipment mentioned."
    },
    "implications" = {
      "Based on the 'Discussion' and 'Conclusion' sections of the article, identify and summarize the main conclusions and implications of the research findings. Please organize them into three distinct categories using these headings: 
**Conclusions**: What are the main findings or definitive statements presented by the authors? 
**Theoretical Implications**: How do the findings contribute to, challenge, or extend existing theories? 
**Practical Implications**: What are the real-world applications, recommendations for practice, or policy suggestions derived from the results?"
    },
    "list_tables_figures" = {
      "Scan the entire document and extract a complete list of all tables and figures. For each item, provide its number and its full title or caption exactly as it appears in the text (e.g., 'Table 1: Participant Demographics' or 'Figure 1: Conceptual Model') and a brief description of the content."
    },
    # Default case for invalid input
    {
      stop(paste(
        "Invalid 'summary_type'. Please use one of:",
        "short_abstract, narrative_abstract, imrad_summary,",
        "thematic_bibliography, research_questions, background_literature,",
        "methods_summary, implications, list_tables_figures"
      ))
    }
  )
  prompt <- paste0(
    prompt,
    " Do not include any additional commentary or information beyond what is requested. ",
    "Do not mention the prompt or instructions in your response. Do not mention that you are an AI model. "
  )
  # Return the selected prompt
  return(prompt)
}
