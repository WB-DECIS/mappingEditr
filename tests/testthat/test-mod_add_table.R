# test-generate_add_table_modal.R
test_that("generate_add_table_modal generates correct modal UI", {
  ns <- NS("test") # Create a namespace for testing

  # Generate the modal UI
  modal <- generate_add_table_modal(ns)

  # Verify the modal is a shiny.tag object
  expect_s3_class(modal, "shiny.tag")

  # Check the title of the modal
  expect_equal(modal$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children[[1]], "Add New Table")

  # Check the presence of a textInput
  expect_true(any(grepl("Enter new table name:", as.character(modal))))

  # Check the presence of radioButtons
  expect_true(any(grepl("Select table type:", as.character(modal))))

  # Verify the default radio button options
  expect_true(stringr::str_detect(as.character(modal), "Fixed"))
  expect_true(stringr::str_detect(as.character(modal), "Mapping"))

  # Verify the presence of buttons in the footer
  footer <- as.character(modal$children[[1]]$children[[1]]$children[[3]])
  expect_true(stringr::str_detect(footer, "Cancel"))
  expect_true(stringr::str_detect(footer, "Add Table"))
})

test_that("generate_add_table_modal respects custom arguments", {
  ns <- NS("test") # Create a namespace for testing

  # Custom arguments
  modal <- generate_add_table_modal(
    ns,
    title = "Custom Title",
    table_types = c("Option1", "Option2"),
    selected_type = "Option1"
  )

  # Check the custom title
  expect_equal(modal$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children[[1]], "Custom Title")

  # Check the custom radio button options
  expect_true(stringr::str_detect(as.character(modal), "Option1"))
  expect_true(stringr::str_detect(as.character(modal), "Option2"))
  expect_true(any(grepl('checked="checked".*Option1', as.character(modal))))
})

