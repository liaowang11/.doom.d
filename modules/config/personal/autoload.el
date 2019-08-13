;;; config/personal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))))

;;;###autoload
(defun doom/create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)))

;;;###autoload
(defun personal/quote-lines ()
  "Change current text block's lines to quoted lines with comma or other separator char.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

If the delimiter is any left bracket, the end delimiter is automatically the matching bracket.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-08"
  (interactive)
  (let* (
         $p1
         $p2
         ($quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         ($separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         ($beginQuote $quoteToUse)
         ($endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
            (if (eq (car $syntableValue ) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr $syntableValue))
              $quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert $beginQuote)
        (goto-char (point-max))
        (insert $endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR" )
          (replace-match
           (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))
