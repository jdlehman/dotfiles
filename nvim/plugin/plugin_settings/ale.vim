" FUNCTIONS {{{

" }}}

" SETTINGS {{{
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_fix_on_save = 1
  let g:ale_javascript_prettier_use_local_config = 1
  let g:ale_virtualtext_cursor = 'true'
  " explicit linters
  let g:ale_linters_explicit = 1
  "
  let g:ale_fixers = {
        \ 'css': ['prettier'],
        \ 'go': ['goimports'],
        \ 'html': ['prettier'],
        \ 'javascript': ['prettier', 'eslint'],
        \ 'java': ['uncrustify', 'google_java_format'],
        \ 'markdown': ['prettier', 'eslint'],
        \ 'reason': 'refmt',
        \ 'typescript': ['prettier', 'tslint', 'eslint'],
        \ 'typescript.tsx': ['prettier', 'tslint', 'eslint'],
        \ 'yaml': ['prettier']
        \}
" }}}

" MAPPINGS {{{
  nnoremap <leader>ad :ALEDetail<cr>
  nnoremap <leader>atb :ALEToggleBuffer<cr>:ALEToggleFixerBuffer<cr>
  nnoremap <leader>at :ALEToggle<cr>:ALEToggleFixer<cr>
" }}}

" autoclose error list when empty
autocmd! User ALELintPost if empty(getloclist(0)) | lclose | endif
command! ALEToggleFixer execute "let g:ale_fix_on_save = get(g:, 'ale_fix_on_save', 1) ? 0 : 1"
command! ALEToggleFixerBuffer execute "let b:ale_fix_on_save = get(b:, 'ale_fix_on_save', 1) ? 0 : 1"
