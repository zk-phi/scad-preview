* scad-preview.el

Preview SCAD models in real-time in Emacs

SCAD モデルを Emacs 上でプレビュー

** Screenshot

[[file:screenshot.png]]

** Installation

Install =scad-mode= and load this script :

: (require 'scad-preview)

Then call =M-x scad-preview-mode= in a =scad-mode= buffer to open the
preview pane. You can close the pane by calling =scad-preview-mode=
again.

** Keybindings

You can rotate the preview image with following keys :

- =<right>=, =l=     :: rotate+ around z-axis
- =<left>=, =h=      :: rotate- around z-axis
- =<up>=, =k=        :: decrease distance (zoom in)
- =<down>=, =j=      :: increase distance (zoom out)
- =C-<left>=, =C-h=  :: rotate+ around y-axis
- =C-<right>=, =C-l= :: rotate- around y-axis
- =C-<up>=, =C-k=    :: rotate+ around x-axis
- =C-<down>=, =C-j=  :: rotate- around x-axis
- =M-<left>=, =M-h=  :: translate+ along x-axis
- =M-<right>=, =M-l= :: translate- along x-axis
- =M-<up>=, =M=k=    :: translate- along z-axis
- =M-<down>=, =M-j=  :: translate+ along z-axis
- =r=                :: reset view
