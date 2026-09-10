# Getting help with bibliometrix

Which channel to use depends on what kind of answer you need. They do not
overlap, and picking the right one is usually the difference between getting an
answer and not getting one.

## A question about how to use bibliometrix

Read the documentation, then write to **info@bibliometrix.org**.

- [bibliometrix.org](https://www.bibliometrix.org) — guides for bibliometrix and
  Biblioshiny, and the reference of every function.
- [The bibliometrix book](https://book.bibliometrix.org) — worked science
  mapping analyses, end to end. Most questions of the form *how do I ...* are
  answered there.
- The vignettes:
  [Introduction to bibliometrix](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html)
  and
  [Data importing and converting](https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html).

`info@bibliometrix.org` is the address the package prints when you load it, and
it is the right place for questions about running an analysis, choosing an
indicator, or reading a result.

Please do not open an issue for these. The tracker is read as a list of defects
to fix, so a usage question placed there is worse for both sides: it waits
behind bugs, and it makes the list of real defects harder to see.

## Something in the package is broken

Open an issue with the **Bug report** template.
[`CONTRIBUTING.md`](CONTRIBUTING.md#reporting-a-bug) says in full what makes a
report actionable. The short version: the verbatim error, the output of
`traceback()` run in the same session right after it, the output of
`sessionInfo()`, and the database and export format you imported.

Paste what the console printed rather than a description of it. A summary
written after the fact, by you or by an AI assistant, is not enough to locate a
fault, and we have open issues that cannot be diagnosed for exactly that reason.

## Something is missing

Open an issue with the **Feature request** template. Describe the analysis you
are trying to perform and why the current functions do not support it.

## You would like to fix or add it yourself

Read [`CONTRIBUTING.md`](CONTRIBUTING.md). It records the conventions of this
repository — which branch to target, how to run the suite, what a pull request
needs — most of which are not guessable from the outside.

## What we cannot do

bibliometrix is maintained by a small academic team, alongside the rest of our
work. We cannot design your study, choose your methods for you, or run your
analysis. Questions about the software we will answer; questions about your
research are for your supervisor, your co-authors and your reviewers.

## Reporting something else

Conduct that breaches the [Code of Conduct](CODE_OF_CONDUCT.md), and anything
that needs to stay private, goes to aria@unina.it.
