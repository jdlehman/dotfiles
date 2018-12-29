let b:ale_linters = ['flow', 'flow-language-server', 'eslint']
if (&ft == 'javascript')
  let b:ale_javascript_prettier_options = '--parser flow'
endif
