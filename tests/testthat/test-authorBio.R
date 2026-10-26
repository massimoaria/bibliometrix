# Test per oaAuthorMatch: l'autore cercato per nome tra gli autori OpenAlex di
# un lavoro, che possono essere in un ordine diverso da Scopus (casi reali)

test_that("l'autore e' trovato per nome anche in un'altra posizione", {
  # Scopus: BREJA N;KAUR R;SAINI R;... ; OpenAlex mette Kaur per prima
  oa <- c("Rippandeep Kaur", "Ranjeeta Saini", "Harpreet Singh", "Priya R Sharma")
  expect_equal(oaAuthorMatch("KAUR R", oa, author_position = 2), 1L)
  expect_equal(oaAuthorMatch("SAINI R", oa, author_position = 3), 2L)
})

test_that("cognomi composti, accenti e trattini non impediscono il confronto", {
  # Scopus: DUARTE J;GÜNTHER-VAN DER MEIJ M ; OpenAlex ripete Duarte
  oa <- c("Joana Duarte", "Joana Duarte", "Mirjam Günther-van der Meij")
  expect_equal(oaAuthorMatch("GÜNTHER-VAN DER MEIJ M", oa, author_position = 2), 3L)
  expect_equal(oaAuthorMatch("TAYLOR-LEECH K", "Kerry Taylor-Leech"), 1L)
  expect_equal(oaAuthorMatch("PORTOLÉS L", "Laura Portolés"), 1L)
  expect_equal(oaAuthorMatch("Cenoz, J.", c("Durk Gorter", "Jasone Cenoz")), 2L)
})

test_that("un autore assente da OpenAlex da' NA, non un altro autore", {
  # Scopus: HOFER B;JESSNER U ; OpenAlex ha solo Jessner
  expect_true(is.na(oaAuthorMatch("HOFER B", "Ulrike Jessner")))
  # il cognome deve comparire come parola intera
  expect_true(is.na(oaAuthorMatch("LI W", "Wei Lin")))
})

test_that("tra autori con lo stesso cognome decidono l'iniziale e poi la posizione", {
  oa <- c("Anna Smith", "John Smith", "Jane Smith")
  expect_equal(oaAuthorMatch("SMITH A", oa, author_position = 3), 1L)
  expect_equal(oaAuthorMatch("SMITH J", oa, author_position = 3), 3L)
  expect_equal(oaAuthorMatch("SMITH J", oa, author_position = 1), 2L)
})

test_that("authorBio rifiuta un author_name non valido", {
  expect_error(authorBio(doi = "10.1/x", author_name = ""), "author_name")
  expect_error(authorBio(doi = "10.1/x", author_name = c("A B", "C D")), "author_name")
})
