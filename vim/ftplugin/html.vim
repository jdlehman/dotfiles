" protects against setting this incorrectly for other filetypes (markdown
" calls this too)
if (&ft == 'html')
  let b:ale_javascript_prettier_options = '--parser html'
endif
