;;; nerd-icons-corfu.el --- Icons for Corfu via nerd-icons -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Luigi Sartor Piucco
;;
;; Author: Luigi Sartor Piucco <luigipiucco@gmail.com>
;; Maintainer: Luigi Sartor Piucco <luigipiucco@gmail.com>
;; Created: September 21, 2023
;; Version: 0.1.0
;; Keywords: convenience, files, icons
;; Homepage: https://github.com/LuigiPiucco/nerd-icons-corfu
;; Package-Requires: ((emacs "27.1") (nerd-icons "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Introduces a margin formatter for Corfu which adds icons. The icons are
;; configurable, but should be text icons provided by the icons fonts in
;; `nerd-icons'.
;;
;; To use, install the package and add the following to your init:
;;
;; (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;
;;; Code:

(require 'nerd-icons)

(defvar nerd-icons-corfu-mapping
  `((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
    (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
    (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
    (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
    (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
    (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
    (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
    (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
    (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
    (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
    (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
    (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
    (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
    (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
    (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
    (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
    (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
    (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
    (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
    (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
    (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
    (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
    (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
    (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
    (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
    (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
    (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
    (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
    (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
    (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
    (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
    (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
    (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
    (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
    (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
    (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))
  "Mapping of completion kinds to icons.

It should be a list of elements with the form (KIND ICON-TXT [:face FACE]).
KIND is a symbol determining what the completion is, and comes from calling the
`:company-kind' property of the completion. ICON-TXT is a string with the icon
to use, usually as a character from a `nerd-icons' symbol font. See that
package for how to get these. Note that it can be simple text if that is
preferred. FACE, if present, is applied to the icon, mainly for its color. The
special t symbol should be used for KIND to represent the default icon, and
must be present.")

;;;###autoload
(defun nerd-icons-corfu-formatter (metadata)
  "A margin formatter for Corfu, adding icons.

It receives METADATA and outputs a function that takes a candidate and returns
the icon."
  (when-let ((kindfunc (plist-get completion-extra-properties :company-kind)))
    (lambda (cand)
      (let* ((kind (funcall kindfunc cand))
             (icon-entry (assq (or kind t) nerd-icons-corfu-mapping))
             (str (cadr icon-entry))
             (props (cddr icon-entry))
             (extra-face (plist-get props :face))
             (space (propertize " " 'display '(space :width 1)))
             (str (concat " " str space)))
        (when extra-face
          (put-text-property 0 3 'face extra-face str))
        str))))

(provide 'nerd-icons-corfu)
;;; nerd-icons-corfu.el ends here
