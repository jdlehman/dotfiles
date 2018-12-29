let b:ale_fixers = {'json': ['prettier']}
if (&ft == 'json')
  let b:ale_javascript_prettier_options = '--parser json'
endif
