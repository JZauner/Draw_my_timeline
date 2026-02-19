testthat::test_that("markdown_to_html converts emphasis and links", {
  html <- markdown_to_html("Use **bold** and [docs](https://example.com).")

  testthat::expect_s3_class(html, "html")
  html_chr <- as.character(html)

  testthat::expect_match(html_chr, "<strong>bold</strong>")
  testthat::expect_match(html_chr, "<a href='https://example.com' target='_blank'>docs</a>")
})

testthat::test_that("markdown_to_html converts paragraphs and line breaks", {
  html <- markdown_to_html("First line\\nSecond line\\n\\nThird paragraph")
  html_chr <- as.character(html)

  testthat::expect_match(html_chr, "<p>First line<br>Second line</p>")
  testthat::expect_match(html_chr, "<p>Third paragraph</p>")
})
