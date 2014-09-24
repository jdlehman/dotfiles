" FUNCTIONS {{{
  " set theme light/dark
  function! JLSetTheme(color)
    if a:color == 'dark'
      let g:seoul256_background = 234
      colorscheme seoul256
      let g:jl_theme = 'dark'
    else
      let g:seoul256_light_background = 253
      colorscheme seoul256-light
      let g:jl_theme = 'light'
    endif
  endfunction

  " toggle theme light/dark
  function! JLToggleTheme()
    if g:jl_theme == 'dark'
      call JLSetTheme('light')
    else
      call JLSetTheme('dark')
    endif
  endfunction
" }}}

" SETTINGS {{{
  syntax enable
  colorscheme seoul256
  " default to dark colorscheme, unless already set to light
  if exists('g:jl_theme') && g:jl_theme == 'light'
    call JLSetTheme('light')
  else
    call JLSetTheme('dark')
  endif
" }}}

" MAPPINGS {{{
  " Toggle between light/dark theme
  nnoremap <leader>bg :call JLToggleTheme()<cr>
" }}}
