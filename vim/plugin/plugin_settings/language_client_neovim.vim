if has('neovim')
  "FUNCTIONS {{{
  " }}}

  " SETTINGS {{{
    " let g:LanguageClient_autoStart = 1
    let g:LanguageClient_serverCommands = {
          \ 'reason': ['ocaml-language-server', '--stdio'],
          \ 'ocaml': ['ocaml-language-server', '--stdio'],
          \ 'go': ['go-langserver'],
          \ }
  " }}}

  " MAPPINGS {{{
    nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>
    nnoremap <silent> gh :call LanguageClient_textDocument_hover()<cr>
    " nnoremap <silent> rr :call LanguageClient_textDocument_rename()<CR>
    " nnoremap <silent> ff :call LanguageClient_textDocument_formatting()<cr>
  " }}}
end
