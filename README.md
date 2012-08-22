There is a minor mode for Emacs acting like occur but w/o creating a new window. It just hides all the text excepting lines containing matches. To use it, add the following to your .emacs file:

```elisp
(require 'loccur)
;; defines shortcut for loccur of the current word
(define-key global-map [(control o)] 'loccur-current)
;; defines shortcut for the interactive loccur command
(define-key global-map [(control meta o)] 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)
```

Now you can point the cursor to the word and press "Ctrl+o" to hide all lines except those containing this word. Moving cursor to the required line and pressing "Ctrl+o" again will shows all the text. The good thing about this mode is what you can navigate through the buffer easily. "Ctrl+Shift+o" will repeat last search. 
