(null) @constant.builtin
(unit) @constant.builtin
(boolean) @boolean

(int) @number
(int32) @number
(int64) @number
(float) @float

(char) @string.special
(string) @string

(string) @string
(char) @character.special

(pair
  "," @punctuation.delimiter)

"[" @punctuation.bracket
"]" @punctuation.bracket
"<" @punctuation.bracket
">" @punctuation.bracket
";" @punctuation.delimiter
":" @punctuation.delimiter

(constructor
  (identifier) @constructor)

(field
  (identifier) @property)
