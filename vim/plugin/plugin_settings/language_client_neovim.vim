if has('nvim')
  "FUNCTIONS {{{
  " }}}

  " SETTINGS {{{
    let g:LanguageClient_autoStart = 1

    let g:LanguageClient_rootMarkers = {
        \ 'go': ['.git', 'go.mod'],
        \ }

    let g:LanguageClient_serverCommands = {
          \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
          \ 'javascript': ['flow-language-server', '--stdio'],
          \ 'typescript': ['typescript-language-server', '--stdio'],
          \ 'typescript.tsx': ['typescript-language-server', '--stdio'],
          \ 'reason': ['reason-language-server.exe'],
          \ 'ocaml': ['ocaml-language-server', '--stdio'],
          \ 'go': ['bingo', '--format-style', 'goimports'],
          \ 'ruby': ['solargraph', 'stdio'],
          \ }
  " }}}

  " MAPPINGS {{{
    nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>
    nnoremap <silent> lf :call LanguageClient_textDocument_formatting()<cr>
    nnoremap <silent> lr :call LanguageClient_textDocument_rename()<CR>
    nnoremap <silent> l# :call LanguageClient_textDocument_references()<CR>
    nnoremap <silent> lf :call LanguageClient_textDocument_documentSymbol()<CR>
    nnoremap <silent> lm :call LanguageClient_contextMenu()<CR>
    nnoremap <silent> le :call LanguageClient#explainErrorAtPoint()<CR>
  " }}}
end
