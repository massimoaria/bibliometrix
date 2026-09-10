# Contributing to bibliometrix

Thanks for taking the time to contribute. bibliometrix is maintained by a small
academic team, and well-prepared reports and pull requests are what make it
possible to keep up.

This file records the conventions of this repository. Most of them are not
guessable from the outside, so please read the section that applies to you
before you start.

- [Code of conduct](#code-of-conduct)
- [Reporting a bug](#reporting-a-bug)
- [Suggesting a feature](#suggesting-a-feature)
- [Sending a pull request](#sending-a-pull-request)
- [Setting up a development environment](#setting-up-a-development-environment)
- [Conventions of this repository](#conventions-of-this-repository)

## Code of conduct

Everyone taking part in this project is expected to follow the
[Code of Conduct](CODE_OF_CONDUCT.md). It is the Contributor Covenant 2.1.
Unacceptable behaviour can be reported to aria@unina.it.

## Reporting a bug

Open an issue with the **Bug report** template. The two fields that decide
whether an issue can be acted on are:

1. **The verbatim traceback.** Run `traceback()` in the same session, right
   after the error, and paste what it prints, unedited.
2. **The output of `sessionInfo()`**, so we know your R version, your platform
   and the versions of the packages actually loaded.

Please paste what the console printed rather than a description or a
reconstruction of it. A summary written after the fact, by you or by an AI
assistant, is not enough to locate a fault: we have open issues that cannot be
diagnosed because the reported call stack does not correspond to any code path
in the package, and there is nothing we can do with them until the real output
arrives.

Also tell us:

- the **database and export format** you imported (Web of Science plaintext,
  Scopus CSV, OpenAlex CSV, PubMed, Lens, Dimensions, ...), since the importers
  differ and most defects are specific to one of them;
- the **smallest collection that reproduces it**. A handful of records is far
  more useful than a full export. If the data cannot be shared, say so and
  describe its shape (number of documents, which fields are present or empty).

If the problem is in Biblioshiny, say which page and which control you used.

## Suggesting a feature

Open an issue with the **Feature request** template. Describe the analysis you
are trying to perform and why the current functions do not support it. Feature
requests are welcome but are scheduled against a small maintenance budget, so a
clear scientific motivation helps a great deal.

## Sending a pull request

**Target the `develop` branch.** `master` is the release branch: it is
protected, and it only ever receives merges from `develop`. A pull request
opened against `master` cannot be merged the normal way, and when one is merged
anyway the change has to be brought back into `develop` by hand.

So: fork the repository, branch off `develop`, and open the pull request against
`develop`.

Before you open it:

- **Reproduce the failure first, and keep the reproduction.** State in the pull
  request what the behaviour was before your change and what it is after, with
  the actual output of both. A claim that a change fixes something is not
  useful on its own; we verify every one of them, and a claim that does not
  reproduce costs more time than the fix saves.
- **Run the test suite** and make sure it is green:

  ```r
  pkgload::load_all(".")
  testthat::test_dir("tests/testthat")
  ```

  There must be no failures. The suite writes `tests/testthat/Rplots.pdf` as a
  side effect; delete it rather than committing it.
- **Add a regression test** for the defect you fixed, in the file that covers
  the function you touched.
- **Add an entry to `NEWS` and to `NEWS.md`**, under the development version
  heading, saying what was wrong and what changed.
- **Keep the change to one defect.** A pull request that fixes one thing is
  reviewed in an hour; one that fixes four unrelated things in three files
  waits.
- **Do not reformat code you are not changing.** A whitespace sweep hides the
  actual change in the diff.
- **Do not squash, and do not rebase a branch that is already pushed.** Leave
  the commits as you made them: a branch whose history shows the failing test
  first and the fix second is easier to review than one square commit, and the
  pull request is merged with a merge commit anyway, so the shape of your
  history does not affect ours. To pick up new work on `develop`, merge it in
  (`git merge origin/develop`) rather than rebasing; rewriting commits that are
  already on GitHub invalidates the review comments attached to them.
- **Write the commit message for someone reading `git log` in two years.** A
  subject line that says what changed, and a body that says why, if the subject
  is not enough. Prefixes such as `fix(module):` are neither required nor
  discouraged.

Note on continuous integration: `R-CMD-check` currently runs on pushes to
`master` and on pull requests whose base is `master`. A pull request against
`develop` therefore gets no automated check, and running the suite locally is
the only signal you will have.

## Setting up a development environment

bibliometrix requires R >= 3.5.0 and has a large set of dependencies.

```r
install.packages(c("devtools", "pkgload", "testthat"))
devtools::install_deps(dependencies = TRUE)

pkgload::load_all(".")                       # load the package from source
testthat::test_dir("tests/testthat")         # run the tests
devtools::check()                            # full R CMD check, slower
```

To work on Biblioshiny, load the package and run `biblioshiny()`. The app
sources live in `inst/biblioshiny/`.

## Conventions of this repository

- **`inst/biblioshiny/` must stay pure ASCII, and a test enforces it.** In an
  MBCS locale R ignores `encoding = "UTF-8"` and decodes the file with the
  system codepage: a non-ASCII character is corrupted, and if it falls inside a
  string the file no longer parses and Biblioshiny does not start (issue #589).
  Write such characters as `\u` escapes, or in HTML as numeric character
  references; the source stays ASCII and the value at run time is unchanged.
- **Keep `NEWS` ASCII too, but transliterate rather than escape.** `NEWS` is
  plain text, read by `news()` and shown verbatim: a `\u` escape would appear
  as the six characters you typed. So write a contributor's name as `Benzecri`,
  not as `Benzecr\u00ec`, and keep the accented spelling for `NEWS.md`, which
  is Markdown and is exempt. If a name cannot survive transliteration, spell it
  in `NEWS.md` and use the plain form in `NEWS`.
- **`R/` is UTF-8 and may hold non-ASCII characters.** `DESCRIPTION` declares
  `Encoding: UTF-8`, so `R CMD check` accepts them and several files already
  use them. Prefer ASCII in new code all the same, for the same reason as
  above, but this one is a preference and not a rule.
- **Every user-visible change gets a `NEWS` entry.** Entries here are written to
  explain the defect, not just to name it: what went wrong, in which situation,
  and what the reader will now see instead.
- **Match the style of the code around you.** The package mixes base R and
  tidyverse idioms depending on the file; follow the file you are in.
- **Comments explain why, not what.** A comment that restates the code adds
  nothing; one that records the reason a guard exists prevents it from being
  removed later.

## Licence

bibliometrix is released under GPL-3. By contributing you agree that your
contribution is licensed under the same terms.
