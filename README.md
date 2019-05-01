# Rena Scheme
Rena Scheme is a library of parsing texts. Rena Scheme makes parsing text easily.  
Rena Scheme can treat recursion of pattern, hence Rena Scheme can parse languages which described top down parsing
like arithmetic expressions and so on.  
Rena Scheme can parse class of Parsing Expression Grammar (PEG) language.  
Rena Scheme can also treat synthesized and inherited attributes.  
'Rena' is an acronym of REpetation (or REcursion) Notation API.  

## How to use

```scheme
(load "rena.scm")
```

## Expression

### Construct Expression Generation Object
```scheme
(define r (rena . options))
```

Options shown as follow are available.
```
(ignore <expression to ignore>)
(keys <key> ...)
```

An example which generates object show as follows.
```scheme
(define r (rena `(ignore . ,(r 'whitespace)) (keys "+" "-" "++")))
```

### Elements of Expression

#### Literals
String literal and character literal of Scheme are elements of expression.  
To use only one literal as expression, use then synthesized expression.

#### Attrinbute Setting Expression
Attribute setting expression is an element of expression.
```scheme
(r 'attr <attribute to set>)
```

#### Key Matching Expression
Key matching expression is an element of expression.  
If keys "+", "++", "-" are specified by option, below expression matches "+" but does not match "+" after "+".
```
(r 'key "+")
```

#### Not Key Matching Expression
Not key matching expression is an element of expression.
If keys "+", "++", "-" are specified by option, "+", "++", "-" will not match.
```
(r 'not-key)
```

#### Keyword Matching Expression
Keyword matching expression is an element of expression.
```
(r 'equals-id <keyword>)
```

The table shows how to match expression (r 'equals-id "keyword") by option.

|option|keyword|keyword1|keyword-1|keyword+|
|:-----|:------|:-------|:--------|:-------|
|no options|match|match|match|match|
|ignore: /-/|match|no match|match|no match|
|keys: ["+"]|match|no match|no match|match|
|ignore: /-/ and keys: ["+"]|match|no match|match|match|

#### Real Number
Real number expression is an element of expression and matches any real number.
```
(r 'real)
```

#### Whitespaces
Whitespaces expression is an element of expression and matches whiltespaces.
```
(r 'whitespace)
```

#### Newline
Newline expression is an element of expression and matches CR/LF/CRLF newline.
```
(r 'br)
```

#### Any character
Any-character expression is an element of expression and matches any character.
```
(r 'any)
```

#### Range of characters
Range of characters expression is an elmenet of expression and matches specified range.
```
(r 'range <character literal or (char-from char-to)>)
```

Below expression matches ASCII small letters and underscore.
```scheme
(r 'range (#\a #\z) #\_)
```

#### Complement Range of characters
Complement range of characters expression is an elmenet of expression and matches characters except specified range.
```
(r 'complement <character literal or (char-from char-to)>)
```

Below expression matches except ASCII small letters and underscore.
```scheme
(r 'complement (#\a #\z) #\_)
```

#### Procedure
Procedure which fulfilled condition shown as follow is an element of expression.  
* the function has 3 arguments
* first argument is a string to match
* second argument is last index of last match
* third argument is an attribute
* return value of the multi-value which has 3 values
  1. matched string
  2. last index of matched string
  3. result attribute

Every instance of expression is a procedure fulfilled above condition.

### Synthesized Expression

#### Sequence
Sequence expression matches if all specified expression are matched sequentially.  
Below expression matches "abc".
```
(r 'then "a" "b" "c")
```

#### Choice
Choice expression matches if one of specified expression are matched.  
Specified expression will be tried sequentially.  
Below expression matches "a", "b" or "c".
```
(r 'or "a" "b" "c")
```

#### Repetation
Repetation expression matches repetation of specified expression.  
The family of repetation expression are shown as follows.  
```
(r 'times <minimum count> <maximum count> <expression> [<action>])
(r 'at-least <minimum count> <expression> [<action>])
(r 'at-most <maximum count> <expression> [<action>])
(r 'one-or-more <expression> [<action>])
(r 'zero-or-more <expression> [<action>])
(r 'maybe <expression>)
```

(r 'at-least min expression action) is equivalent to (r 'times min false expression action).  
(r 'at-most max expression action) is equivalent to (r 'times 0 max expression action).  
(r 'one-or-more expression action) is equivalent to (r 'times 1 false expression action).  
(r 'zero-or-more expression action) is equivalent to (r 'times 0 false expression action).  
(r 'maybe expression) is equivalent to (r 'times 0 1 expression).  

The argument action must specify a function with 3 arguments and return result attribute.  
First argument of the function will pass a matched string,
second argument will pass an attribute of repetation expression ("synthesized attribtue"),
and third argument will pass an inherited attribute.  

For example, consider action of the expression.
```scheme
(define match (r 'one-or-more
                 (r 'range (#\0 #\9))
                 (lambda (match synthesized inherited) (string-append inherited ":" synthesized))))
(match "27" 0 "")
```

In first match of string "27", the arguments of function are ("2" "2" "") and results ":2".  
In second match, the arguments are ("27" "7" ":2") and results ":2:7".

Repetation expression is already greedy and does not backtrack.

#### Repetation with Delimiter
Repetation with delimiter matches repetation one or more times and delimited by delimiter expression.  
Below expression matches "1,2,3".
```
(r 'delimit (r 'range (\#0 \#9)) ",")
```

The 'delimit can pass an action as third arguments same as simple repetation.

#### Lookahead (AND predicate)
Lookahead (AND predicate) matches the specify expression but does not consume input string.
Below example matches "ab" but matched string is "a", and does not match "ad".
```
(r 'then "a" (r 'lookahead "b"))
```

#### Nogative Lookahead (NOT predicate)
Negative lookahead (NOT predicate) matches if the specify expression does not match.
Below example matches "ab" but matched string is "a", and does not match "ad".
```
(r 'then "a" (r 'lookahead-not "b"))
```

#### Condition
Condition expression matches if the predicate function returns true.  
Below example matches if the attribute is "765", but does not match if the attribute is "961".
```
(r 'attr (lambda (attr) (eqv? attr "765")))
```

#### Action
Action expression matches the specified expression.  
```
(r <expression> <action>)
```

The second argument must be a function with 3 arguments and return result attribute.  
First argument of the function will pass a matched string,
second argument will pass an attribute of repetation expression ("synthesized attribtue"),
and third argument will pass an inherited attribute.  

Below example, argument of action will be passed ("2", "2", "").
```scheme
((r (r 'range (#\0 #\9)) (lambda (match synthesized inherited) match)) "2" 0 "")
```

### Matching Expression
To apply string to match to an expression, call the expression with 3 arguments shown as follows.
1. a string to match
2. an index to begin to match
3. an initial attribute

```scheme
(define match (r 'one-or-more
                 (r 'range (#\0 #\9))
                 (lambda (match synthesized inherited) (string-append inherited ":" synthesized))))
(match "27" 0 "")
```

### Description of Recursion
The (r 'y) function is available to recurse an expression.  
The argument of (r 'y) function are functions, and return value is the return value of first function.

Below example matches balanced parenthesis.
```scheme
(define paren
  (r 'y
     (lambda (paren) (r 'then "(" (r 'maybe paren) ")"))))
};
```

## Examples

### Parsing simple arithmetic expressions
```scheme
(define expr
  (r 'y
     (lambda (term factor element)
       (r 'then
           factor
           (r 'zero-or-more
              (r 'or
                 (r 'then
                    "+"
                    (r factor (lambda (match syn inh) (+ inh syn))))))))
     (lambda (term factor element)
       (r 'then
          element
          (r 'zero-or-more
             (r 'or
                (r 'then
                   "*"
                   (r element (lambda (match syn inh) (* inh syn))))))))
     (lambda (term factor element)
       (r 'or
          (r 'real)
          (r 'then "(" term ")")))))

; outputs "1+2*3" 5 7
(expr "1+2*3" 0 0)
```

