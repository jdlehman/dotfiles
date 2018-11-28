if has('nvim')
  "FUNCTIONS {{{
  " }}}

  " SETTINGS {{{
    let g:LanguageClient_autoStart = 1

    let g:LanguageClient_serverCommands = {
          \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
          \ 'javascript': ['javascript-typescript-stdio'],
          \ 'typescript': ['javascript-typescript-stdio'],
          \ 'typescript.tsx': ['javascript-typescript-stdio'],
          \ 'reason': ['reason-language-server.exe'],
          \ 'ocaml': ['ocaml-language-server', '--stdio'],
          \ 'go': ['go-langserver'],
          \ }
  " }}}

  " MAPPINGS {{{
    nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>
    nnoremap <silent> lf :call LanguageClient_textDocument_formatting()<cr>
    nnoremap <silent> lr :call LanguageClient_textDocument_rename()<CR>
  " }}}
end
