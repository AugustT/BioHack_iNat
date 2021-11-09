# Snippet for inat login test
# this can be used to test login
upload_token <- try({
  pynat$get_access_token(input$username,
                         input$password,
                         token[[3]],
                         token[[4]])
}, silent = TRUE)

vals$upload_token <- upload_token

if(length(upload_token) == 1 &
   class(upload_token) == 'character'){
  removeModal()
} else {
  showModal(loginModal(failed = TRUE))
}