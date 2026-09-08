<!--
Please target the `develop` branch. `master` is the release branch and only
receives merges from `develop`. See CONTRIBUTING.md.
-->

## What this changes

<!-- One defect or one feature per pull request. -->

## Before and after

<!--
The behaviour you observed before the change and after it, with the actual
output of both. Every claim in a pull request is verified here, so a
reproduction saves a round trip.
-->

```r
# before

# after
```

## Checklist

- [ ] The pull request targets `develop`
- [ ] `testthat::test_dir("tests/testthat")` is green, with no failures
- [ ] A regression test covers the change
- [ ] `NEWS` and `NEWS.md` have an entry under the development version
- [ ] `R/`, `inst/biblioshiny/` and `NEWS` are still pure ASCII
- [ ] No unrelated reformatting in the diff
