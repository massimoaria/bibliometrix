# gemini_to_html() and its LaTeX helpers live in the Biblioshiny sources, not
# in the package namespace: evaluate them from biblioAI.R.
ai_text_helpers <- function() {
  path <- system.file("biblioshiny", "biblioAI.R", package = "bibliometrix")
  wanted <- c("gemini_to_html", "latex_to_html", "latex_symbols",
              "latex_math_to_html")
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(path, encoding = "UTF-8"))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% wanted) {
      eval(expr, envir = env)
    }
  }
  env
}

# the HTML without the leading <style> block
ai_body <- function(html) sub("^<style>.*?</style>", "", html, perl = TRUE)

test_that("bold text holding italics loses its asterisks", {
  h <- ai_text_helpers()
  out <- ai_body(h$gemini_to_html("**R2 (*Random Graph / ERGM*) to T4:**"))
  expect_match(out, "<strong>R2 (<em>Random Graph / ERGM</em>) to T4:</strong>",
               fixed = TRUE)
  expect_false(grepl("*", out, fixed = TRUE))
})

test_that("nested lists and continuation lines are kept", {
  h <- ai_text_helpers()
  md <- paste(
    "1. **R2** first root:",
    "   * **T4** (n=68): a theme",
    "   * **T11**: another theme",
    "",
    "* **R1** (181 docs)  ",
    "  Anchored in Granovetter (1973).",
    sep = "\n"
  )
  out <- ai_body(h$gemini_to_html(md))
  expect_match(out, "<ol><li><strong>R2</strong> first root: <ul><li>",
               fixed = TRUE)
  expect_match(out, "<strong>R1</strong> (181 docs)<br />", fixed = TRUE)
  expect_match(out, "Anchored in Granovetter (1973).</li>", fixed = TRUE)
  # no newlines between tags: the Biblio AI box would show them as blank lines
  expect_false(grepl("\n", out, fixed = TRUE))
})

test_that("headers become HTML headers", {
  h <- ai_text_helpers()
  out <- ai_body(h$gemini_to_html("### 1. Main Roots\n\nText."))
  expect_match(out, "<h3>1. Main Roots</h3><p>Text.</p>", fixed = TRUE)
})

test_that("LaTeX math is rendered without its source", {
  h <- ai_text_helpers()
  out <- ai_body(h$gemini_to_html(
    "**R2 $\\rightarrow$ T4** ($n=68$, residual $16.06$), $p^*$/ERGM"
  ))
  expect_match(out, "<strong>R2 → T4</strong> (n=68, residual 16.06)",
               fixed = TRUE)
  expect_match(out, "p<sup>&#42;</sup>/ERGM|p<sup>\\*</sup>/ERGM")
  expect_false(grepl("[$\\\\]", out))

  expect_identical(h$latex_to_html("$\\alpha \\leq 0.05$"), "α ≤ 0.05")
  expect_identical(h$latex_to_html("$x_{ij}$"), "x<sub>ij</sub>")
  expect_identical(h$latex_to_html("$$\\frac{a}{b} \\times n$$"),
                   "a/b × n")
  expect_identical(h$latex_to_html("R2 \\rightarrow T4"), "R2 → T4")
})

test_that("dollar amounts are not taken for math", {
  h <- ai_text_helpers()
  expect_identical(h$latex_to_html("costs $5 and $10"), "costs $5 and $10")
})

test_that("plain text such as an abstract is escaped and kept", {
  h <- ai_text_helpers()
  out <- ai_body(h$gemini_to_html("Effects were small (p < 0.05) & robust."))
  expect_match(out, "<p>Effects were small (p &lt; 0.05) &amp; robust.</p>",
               fixed = TRUE)
})
