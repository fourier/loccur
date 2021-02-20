# Loccur

[![MELPA](https://melpa.org/packages/loccur-badge.svg)](https://melpa.org/#/loccur)

## Introduction
**Loccur** is an amazing tool to quickly navigate in a file. It is a minor mode for Emacs acting like **occur** but w/o creating a new window. It just hides all the text excepting lines containing matches.
## Installation
Add to your `.emacs` or `.emacs.d/init.el` following lines:

```scheme
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
```
                         
Press `M-x` in GNU Emacs and write `list-packages`. Find the `loccur` in the list of packages and press `i` to select this package, `x` to install the package.


## Screenshots
Better to see it once:

![loccur_gui](https://github.com/fourier/loccur/raw/screenshots/gui_emacs_with_loccur.gif "GUI Emacs with loccur")
![loccur_term](https://github.com/fourier/loccur/raw/screenshots/emacs_with_loccur.gif "Emacs in terminal with loccur")


## Usage

To use it, add the following to your .emacs file:

```scheme
(require 'loccur)
;; defines shortcut for loccur of the current word
(define-key global-map [(control o)] 'loccur-current)
;; defines shortcut for the interactive loccur command
(define-key global-map [(control meta o)] 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)
```

Now you can point the cursor to the word and press `Ctrl+o` to hide all lines except those containing this word. Moving cursor to the required line and pressing `Ctrl+o` again will shows all the text. The good thing about this mode is what you can navigate through the buffer easily. `Ctrl+Shift+o` will repeat last search.

### Available commands
Below is the list of interactive commands available for user:

* `loccur` interactively asks user for regexp to search or toggle search off (if `loccur-mode` is already enabled)
* `loccur-current` searches for the current word under the cursor
* `loccur-previous-match` repeat previous `loccur` command
* `loccur-no-highlight` is the same as `loccur` but not highlighting matches
* `loccur-toggle-highlight` toggles highlighting of matches

### Customization
* `loccur-jump-beginning-of-line` variable specifies if move the cursor to the beginning of the matching line. Default `nil`
* `loccur-highlight-matching-regexp` variable whenever `loccur` should highlight matching words. Default `t`.
* `loccur-face` face to be used while highlighting. Default points to `isearch` face.

### Tips and tricks
To combine **Loccur** and **isearch** functionality (narrowing-search) one can use the following hooks:
```lisp
(add-hook 'isearch-update-post-hook
          (lambda ()
            (let ((loccur-mode nil))
              (loccur (regexp-quote isearch-string)))))

(add-hook 'isearch-mode-end-hook
          (lambda ()
            (loccur nil)))
```


## Contributions
You can contribute to **loccur** in one of the following ways.
- Submit a bug report
- Submit a feature request
- Submit a simple pull request (with changes < 15 lines)

### Copyright issues
Since **loccur** is a part of [GNU ELPA](https://elpa.gnu.org/), it is copyrighted by the [Free Software Foundation, Inc.](http://www.fsf.org/). Therefore in order to submit nontrivial changes (with total amount of lines > 15), one needs to to grant the right to include your works in GNU Emacs to the FSF.

For this you need to complete [this](https://raw.githubusercontent.com/fourier/loccur/contributions/request-assign.txt) form, and send it to [assign@gnu.org](mailto:assign@gnu.org). The FSF will send you the assignment contract that both you and the FSF will sign.

For more information one can read [here](http://www.gnu.org/licenses/why-assign.html) to understand why it is needed.

As soon as the paperwork is done one can contribute to **loccur** with bigger pull requests.
Note what pull requests without paperwork done will not be accepted, so please notify the [maintainer](mailto:alexey.veretennikov@gmail.com) if everything is in place.

