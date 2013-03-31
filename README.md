Functionnalities
================


Provides is a minor mode for Emacs acting like occur but without
creating a new window. It just hides all the text except lines
containing matches for a given regex. The following functions are
provided:
 
 + `loccur` Prompts for a regex and hides all the lines not containing
 matches. If the given regex is empty, defaults to the current
 selection and, if the selection is also empty, defaults to the word
 at point.
 + `loccur-previous-match` Repeats the last `loccur` search.
 + `loccur-toggle-highlight` Disables or enables highlighting of the
 matches.
 
After `loccur` is ran, hit `RET` to move to the line where the cursor
is and display everything again.

The good thing about this mode is that you can navigate through the
buffer easily. In particular, if you bind `(loccur ""


Example
=======

If you run `loccur` on regex `[0-9]+` (finding any decimal number) on
the following buffer,

``` Lorem ipsum dolor 100 sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
ad minim veniam, quis 20090 nostrud exercitation ullamco laboris nisi
ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat 3 cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.```

we obtain

``` Lorem ipsum dolor 100 sit amet, consectetur adipisicing elit, sed
ad minim veniam, quis 20090 nostrud exercitation ullamco laboris nisi
pariatur. Excepteur sint occaecat 3 cupidatat non proident, sunt in
```

and, if we hit `RET` on the third line of this display, the first
content is displayed again and the cursor is placed on the 6th line.


Suggested settings
=================

You can add the following to your .emacs file:

```scheme
(require 'loccur)
;; defines shortcut for loccur of the current word
(define-key global-map [(control o)] 'loccur-current)
;; defines shortcut for the interactive loccur command
(define-key global-map [(control meta o)] 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)
```

Now you can point the cursor to the word and press `C-o` to hide all
lines except those containing this word. Moving cursor to the required
line and pressing `C-o` again or `RET` will shows all the text.
`C-S-o` will repeat the last search.


Quick navigation
================


You can also use `loccur` to efficiently navigate in a buffer. For
instance, the following function displays only the declaration of all
the *Python* functions in the current file; making it very easy to
jump to a particular function.

```scheme
(defun loccur/list-Python-functions()
  "Displays only the lines corresponding to a function
declaration in a Python file."
  (loccur-no-highlight "^ *def "))
```

In the same way, the following snippet provides a very useful function
for whoever uses
[beamer](http://en.wikipedia.org/wiki/Beamer_%28LaTeX%29): it lists
all the frame titles to easily jump to a particular one.


```scheme
(defun loccur/list-beamer-frames()
  "Displays only the lines corresponding to a frame title
declaration in a beamer LaTeX file."
  (loccur-no-highlight "\\frametitle"))
```

When running this command, this buffer
```
\begin{frame}
  \frametitle{First frame}

  <insert fascinating content here>
\end{frame}

\begin{frame}
  \frametitle{Then let's talk about this}

  <insert some stunning figure here>
\end{frame}

\begin{frame}
  \frametitle{Wrapping up}

  <insert witty and challenging conclusion here>
\end{frame}
```

becomes
```

  \frametitle{First frame}
  \frametitle{Then let's talk about this}
  \frametitle{Wrapping up}
```
and hitting `RET` will bring back the first buffer and place the
cursor on the line wanted.
