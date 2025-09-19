library(mailR)

send_error_email <- function(subject, error_message) {
  tryCatch({
    gmail_user <- Sys.getenv("GMAIL_USER")
    gmail_pass <- Sys.getenv("GMAIL_PASS")
    
    if (gmail_user == "" || gmail_pass == "") {
      cat("Email credentials not found in environment variables\n")
      return(FALSE)
    }
    
    # Update this URL with your actual GitHub username
    repo_url <- "https://github.com/aeizen834/nfl-play-seq/actions"
    
    body_text <- paste0(
      "NFL Play Sequence Analysis Error Report\n",
      "Time: ", Sys.time(), "\n",
      "Error: ", error_message, "\n\n",
      "Please check the GitHub Actions log for more details:\n",
      repo_url, "\n\n",
      "App URL: https://YOURUSERNAME.shinyapps.io/nfl-play-seq/"
    )
    
    send.mail(
      from = gmail_user,
      to = gmail_user,
      subject = paste("NFL App Error:", subject),
      body = body_text,
      smtp = list(
        host.name = "smtp.gmail.com",
        port = 587,
        user.name = gmail_user,
        passwd = gmail_pass,
        ssl = TRUE
      ),
      authenticate = TRUE,
      send = TRUE
    )
    
    cat("Error notification email sent successfully\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Failed to send error notification email:", e$message, "\n")
    return(FALSE)
  })
}