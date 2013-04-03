source ~/.vimrc

if has("gui_mac") || has("gui_macvim")
  macmenu &File.New\ Tab     key=<Nop>
  map <silent><D-t> :FufFile **/<CR>
endif

:macm Window.Select\ Previous\ Tab  key=<D-S-Left>
:macm Window.Select\ Next\ Tab	   key=<D-S-Right>
