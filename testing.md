## 1 App Startup

terminal messages:

```R
ℹ The googlesheets4 package is using a cached token for appdemo41@gmail.com.
ℹ The googledrive package is using a cached token for appdemo41@gmail.com.
Auto-refreshing stale OAuth token.
✔ The input `path` resolved to exactly 1 file.

✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ Reading from BlinkR_Combined_Class_Data.
✔ Range Sheet1.
```

## 2 Log in/ Sign up

### 2.1 Log in

```R
logging in for =  1234
✔ The input `path` resolved to exactly 1 file.
Permissions updated:
• role = reader
• type = anyone
For file:
• 1234 <id: 18kw_nQJW6gJXg2LiDNC29bChNOSn-bP3>
✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ The input `path` resolved to exactly 1 file.
✔ Reading from BlinkR Users.
✔ Range Sheet1.
```

### 2.2 Signing up (if active)

```R
signing up for =  D - 1636
✔ Writing to BlinkR Users.
✔ Appending 1 row to Sheet1.
✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ The input `path` resolved to exactly 1 file.
Created Drive file:
• 1636 <id: 18Fu8frBaQ6kXNqzMBfVSC0GZYdELIAuV>
With MIME type:
• application/vnd.google-apps.folder
Permissions updated:
• role = reader
• type = anyone
For file:
• 1636 <id: 18Fu8frBaQ6kXNqzMBfVSC0GZYdELIAuV>
✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ The input `path` resolved to exactly 1 file.
✔ Reading from BlinkR Users.
✔ Range Sheet1.
```

```R
four_digit_group_id <- function() {
compose_rules(
sv_required(message = "Group ID is required"),
sv_regex("^[0-9]{4}$", message = "Must be exactly 4 digits (e.g., 0001)")
)

}

iv <- InputValidator$new()

iv$add_rule("sign_up_group_name", compose_rules(sv_required(), sv_regex("^[0-9]+$",

message = "Digits only"

)))

iv$add_rule("sign_up_group_name", four_digit_group_id())
iv$add_rule("name", sv_required(message = "Name is required"))

iv$enable()
```

```R
observeEvent(input$sign_up_button, {


if (!iv$is_valid()) {

showNotification("Please enter a 4 digit group ID and your first name", type = "error")
return()
}

req(input$sign_up_group_name, input$name, iv$is_valid())

...
})
```

## 3 Text Area Module

- used throughout to write and save to google drive

```R
Local file:
• /var/folders/dj/6ghf0z554t9d4sc_lzcy79xw0000gn/T//RtmprZ9LCa/file154372534ea99.txt
Uploaded into Drive file:
• Hypothesis_13h_30m_43s_.txt <id: 1B0hmCrVs8UQx64CzAZ86r2E1qCjV3TxJ>
With MIME type:
• text/plain
group name  1234  saved to path BlinkR_text_results/1234 as filename Hypothesis_13h_30m_43s_.txt
```

## 4 Answer Validation

#### 4.1.1 Measurements Input

```R
Level B Inputs: 1 1 1
✔ Writing to BlinkR_Measurements.
✔ Writing to sheet 1234_2025-08-29.

###

Level A Inputs: 1 1 1
✔ Writing to BlinkR_Measurements.
✔ Writing to sheet 1234_2025-08-29.
```

- cannot submit 0 or NA values

```R
add_measurement <- function(level, inputs, submission_id) {

if (any(sapply(inputs, is.null)) || any(sapply(inputs, is.na)) || any(sapply(inputs, function(x) x == 0))) {

showNotification(i18n$t("Please enter all three measurements."), type = "error", duration = 3)

return(FALSE)

}
...
}

```

### 4.2 Analysis validation and UI output

- User answers are validated based on reactives that change depending on the underlying data. In some cases successful validation also determines the next ui output For example:

```R
#step1 asks users to run head(data) which results in a tibble
analysis_summarise_data_module_ui <- function(){
...
uiOutput(ns("step1_box"))
...
})

analysis_summarise_data_module_server <- function(){
output$step1_box <- renderUI({
...
})

observe({

req(!is.null(summarise_result_step1()),  !is.null(summarise_result_step1()$result)) #requires the reactive for step1 to be not NULL - i.e. for the code to have run

if (tibble::is_tibble(summarise_result_step1()$result)) { #requires the reactive result to be a tibble

output$summary_code_feedback_step1 <- renderUI({

tagList(

div(class = "success-box", i18n$t("🙌 Great!")),

)

})

} else {

output$summary_code_feedback_step1 <- renderUI({

div(class = "error-box", i18n$t("🤔 Not quite - try again!"))

})

}

})
...
}
```

### 4.3 Handling Code Errors

- Elaborate `TryCatch`to handle NAs and error messages in the analysis sections (where `AceEditor Module` is implemented).

```R
observeEvent(input$run_code, {

code <- input$editor

temp_env <- new.env(parent = globalenv())



if (is.list(data) && length(variable_name) == length(data)) {

for (i in seq_along(data)) {

assign(variable_name[i], data[[i]](), envir = temp_env)

}

} else {

assign(variable_name, data(), envir = temp_env)

}



forbidden_packages <- c("googledrive", "googlesheets4")

lapply(forbidden_packages, function(pkg) {

if (pkg %in% loadedNamespaces()) {

detach(paste0("package:", pkg), character.only = TRUE, unload = FALSE)

}

})



warnings_vec <- character(0)



eval_result <- tryCatch({

res <- withCallingHandlers(

withVisible(eval(parse(text = code), envir = temp_env)),

warning = function(w) {

warnings_vec <<- c(warnings_vec, conditionMessage(w))

invokeRestart("muffleWarning")

}

)

if (length(warnings_vec)) attr(res, ".__warnings__") <- warnings_vec

res

}, error = function(e) {

structure(

list(value = paste0("Error: ", conditionMessage(e))),

class = "editor_error"

)

})



lapply(forbidden_packages, function(pkg) {

if (pkg %in% installed.packages()) {

library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)

}

})



if (inherits(eval_result, "editor_error")) {

values$result <- eval_result$value

values$is_plot <- FALSE

} else {

res_value <- eval_result$value

warn <- attr(eval_result, ".__warnings__")

if (length(warn)) {

res_value <- paste(c(paste0("Warning: ", warn), "", capture.output(print(res_value))), collapse = "\n")

}

values$result <- if (eval_result$visible) res_value else res_value

values$is_plot <- inherits(res_value, "ggplot") || inherits(res_value, "recordedplot")

}



current_code(code)

}, ignoreInit = TRUE)
```

### 4.4 Persistent Storage and Userbase

- when a new user is generated -> confirmed generation of folder in google drive

```R
button for 1889 clicked;
✔ Writing to BlinkR Users.
✔ Appending 1 row to Sheet1.
✔ Reading from BlinkR Users.
✔ Range Sheet1.
✔ The input `path` resolved to exactly 1 file.
Created Drive file:
• 1889 <id: 1E7ymd84IEA6FrEndiC5ZhGHvVeDqUJWY>
With MIME type:
• application/vnd.google-apps.folder
Permissions updated:
• role = reader
• type = anyone
For file:
• 1889 <id: 1E7ymd84IEA6FrEndiC5ZhGHvVeDqUJWY>
```
