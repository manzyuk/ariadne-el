# Ariadne plugin for Emacs

[Ariadne](https://github.com/feuerbach/ariadne) is a tool that
provides a "go-to-definition" functionality for Haskell source files.

`ariadne.el` is an Ariadne plugin for Emacs.

## Dependencies

`ariadne.el` depends on `bert.el`, BERT serialization library for
Emacs, which can be found at https://github.com/manzyuk/bert-el.

## Usage

The function `ariadne-goto-definition` queries the Ariadne server
about the location of the definition of a name at point and jumps to
that location.  Bind it to a key, for example as follows:

    (require 'ariadne)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (define-key haskell-mode-map "\C-cd" 'ariadne-goto-definition)))
