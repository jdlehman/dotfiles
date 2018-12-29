" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_fix_on_save = 1
  let g:ale_javascript_prettier_use_local_config = 1
  let g:ale_virtualtext_cursor = 'true'
  "
  let g:ale_fixers = {
        \ 'css': ['prettier'],
        \ 'go': ['gofmt', 'golint', 'go build'],
        \ 'html': ['prettier'],
        \ 'javascript': ['prettier', 'eslint', 'importjs'],
        \ 'markdown': ['prettier', 'eslint'],
        \ 'reason': 'refmt',
        \ 'typescript': ['prettier', 'tslint'],
        \ 'typescript.tsx': ['prettier', 'tslint'],
        \ 'yaml': ['prettier']
        \}
" }}}

" MAPPINGS {{{
" }}}

" autoclose error list when empty
autocmd! User ALELintPost if empty(getloclist(0)) | lclose | endif
