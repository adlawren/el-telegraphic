;; From a selected region, delete words which are not part of telegraphic speech
;;   - "leave out articles (e.g. a, an, the), conjunctions (e.g. and, or, but), prepositions (e.g. to, at, from), and auxiliaries (e.g. would, has, be)."
;;     - Source: https://study.com/academy/lesson/telegraphic-sentence-definition-examples.html#transcriptHeader
;;     - In NLTK, these seem to represented using the following tags:
;;       - Articles: DT
;;       - Conjunctions: CC, IN
;;       - Prepositions: IN, TO
;;       - Auxiliaries: MD
;;       - Sources:
;;         - https://stackoverflow.com/questions/15388831/what-are-all-possible-pos-tags-of-nltk/38264311#38264311
;;         - https://pythonprogramming.net/natural-language-toolkit-nltk-part-speech-tagging/

(setq restricted-tags '("DT" "CC" "IN" "TO" "MD"))

(defun trim-string-quotes
    (nltk-str)
  (string-remove-suffix
   "'"
   (string-remove-prefix
    "'"
    (trim-string nltk-str))))

(defun parse-word-tag-pairs
    (nltk-out)
  (mapcar
   (lambda
     (word-tag-str)
     (let
         ((word-tag-pair-str
           (split-string
            (string-remove-suffix
             ")"
             (string-remove-prefix
              "("
              word-tag-str))
            ",")))
       (cons
        (trim-string-quotes (car word-tag-pair-str))
        (trim-string-quotes (cadr word-tag-pair-str)))))
   (split-string
    (string-remove-suffix
     "]"
     (string-remove-prefix
      "["
      (trim-string nltk-out)))
    "), ")))

(defun remove-restricted-words
    (word-tag-pairs restricted-tags)
  (reduce
   (lambda
     (sentence word-tag-pair)
     (if
         (seq-contains restricted-tags (cdr word-tag-pair))
         sentence
       (let
           ((next-word (car word-tag-pair)))
         (if
             (string-empty-p sentence)
             next-word
           (format "%s %s" sentence next-word)))))
   word-tag-pairs
   :initial-value ""))

(defun convert-to-telegraphic
    ()
  (interactive)
  (replace-regexp
   (buffer-substring (region-beginning) (region-end))
   (let
       ((python-command
         (format
          "/usr/bin/env python -c \"import nltk; print(nltk.pos_tag(nltk.word_tokenize(\'%s\')))\""
          "This is a test")))
     (message
      (remove-restricted-words
       (parse-word-tag-pairs
        (if
            (eq (shell-command "which pyenv") 0)
            (shell-command-to-string
             (format "eval \"$(pyenv init -)\"; %s" python-command))
          (shell-command-to-string python-command)))
       restricted-tags)))
   nil
   (region-beginning)
   (region-end)))
