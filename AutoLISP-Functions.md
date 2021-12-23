### Application-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (load filename [onfailure]) | Evaluates the AutoLISP expressions in a file | |
| (startapp appcmd file) | Starts an external application | |

### Arithmetic Functions Reference

| Function | Description | Info |
|----------|-------------|------|
| (+ [number number ...]) | Returns the sum of all numbers | |
| (- [number number ...]) | Subtracts the second and following numbers from the first and returns the difference | |
| (* [number number ...]) | Returns the product of all numbers | |
| (/ [number number ...]) | Divides the first number by the product of the remaining numbers and returns the quotient | |
| (~ int) | Returns the bitwise NOT (1's complement) of the argument | |
| (1+ number) | Returns the argument increased by 1 (incremented) | |
| (1- number) | Returns the argument reduced by 1 (decremented) | |
| (abs number) | Returns the absolute value of the argument | |
| (atan num1 [num2]) | Returns the arctangent of a number in radians | |
| (cos ang) | Returns the cosine of an angle expressed in radians | |
| (exp number) | Returns the constant e (a real) raised to a specified power (the natural antilog) | |
| (expt base power) | Returns a number raised to a specified power | |
| (fix number) | Returns the conversion of a real into the nearest smaller integer | |
| (float number) | Returns the conversion of a number into a real | |
| (gcd int1 int2) | Returns the greatest common denominator of two integers | |
| (log number) | Returns the natural log of a number as a real | |
| (logand [int int ...]) | Returns the result of the logical bitwise AND of a list of integers | Expected in v0.8.0 |
| (logior [int int ...]) | Returns the result of the logical bitwise inclusive OR of a list of integers | Expected in v0.8.0 |
| (lsh [int numbits]) | Returns the logical bitwise shift of an integer by a specified number of bits | Expected in v0.8.0 |
| (max [number number ...]) | Returns the largest of the numbers given | |
| (min [number number ...]) | Returns the smallest of the numbers given | |
| (minusp number) | Verifies that a number is negative | |
| (rem [num1 num2 ...]) | Divides the first number by the second, and returns the remainder | |
| (sin ang) | Returns the sine of an angle as a real expressed in radians | |
| (sqrt number) | Returns the square root of a number as a real | |
| (zerop number) | Verifies that a number evaluates to zero | |

### Conversion Functions

| Function | Description | Info |
|----------|-------------|------|
| (angtof string [mode]) | Converts a string representing an angle into a real (floating-point) value in radians | Expected in v0.8.0 |
| (angtos angle [mode [precision]]) | Converts an angular value in radians into a string | Expected in v0.8.0 |
| (ascii string) | Returns the conversion of the first character of a string into its ASCII character code (an integer) | |
| (atof string) | Returns the conversion of a string into a real | |
| (atoi string) | Returns the conversion of a string into an integer | |
| (chr integer) | Returns the conversion of an integer representing an ASCII character code into a single-character string | Expected in v0.8.0 |
| (cvunit value from to) | Converts a value from one unit of measurement to another | Expected in v0.8.0 |
| (distof string [mode]) | Converts a string that represents a real (floating-point) value into a real value | Expected in v0.8.0 |
| (itoa int) | Returns the conversion of an integer into a string | |
| (ftoa real) | Returns the conversion of a real into a string | VeLisp Extension |
| (rtos number [mode [precision]]) | Converts a number into a string | v0.8.0 |
| (trans pt from to [disp]) | Translates a point (or a displacement) from one coordinate system to another | Expected in v0.8.0 |

### Display Control Functions

| Function | Description | Info |
|----------|-------------|------|
| (prin1 [expr [file-desc]]) | Prints an expression to the command line or writes an expression to an open file | |
| (princ [expr [file-desc]]) | Prints an expression to the command line, or writes an expression to an open file | |
| (print [expr [file-desc]]) | Prints an expression to the command line, or writes an expression to an open file | |
| (prompt msg) | Displays a string on your screen's prompt area | |

### Equality and Conditional Functions

| Function | Description | Info |
|----------|-------------|------|
| (= numstr [numstr ...]) | Returns T if all arguments are numerically equal, and returns nil otherwise | |
| (/= numstr [numstr ...]) | Returns T if the arguments are not numerically equal, and nil if the arguments are numerically equal | |
| (< numstr [numstr ...]) | Returns T if each argument is numerically less than the argument to its right, and returns nil otherwise | |
| (<= numstr [numstr ...]) | Returns T if each argument is numerically less than or equal to the argument to its right, and returns nil otherwise | |
| (> numstr [numstr ...]) | Returns T if each argument is numerically greater than the argument to its right, and returns nil otherwise | |
| (>= numstr [numstr ...]) | Returns T if each argument is numerically greater than or equal to the argument to its right, and returns nil otherwise | |
| (and [expr ...]) | Returns the logical AND of a list of expressions | |
| (boole func int1 [int2 ...]) | Serves as a general bitwise Boolean function | Expected in v0.8.0 |
| (cond [(test result ...) ...]) | Serves as the primary conditional function for AutoLISP | |
| (eq expr1 expr2) | Determines whether two expressions are identical | |
| (equal expr1 expr2 [fuzz]) | Determines whether two expressions are equal | |
| (if testexpr thenexpr [elseexpr]) | Conditionally evaluates expressions | |
| (or [expr ...]) | Returns the logical OR of a list of expressions | |
| (repeat int [expr ...]) | Evaluates each expression a specified number of times, and returns the value of the last expression | |
| (while testexpr [expr ...]) | Evaluates a test expression, and if it is not nil, evaluates other expressions; repeats this process until the test expression evaluates to nil | |

### Error-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (\*error\* string) | A user-definable error-handling function | Expected in v0.7.0 |
| (\*pop-error-mode\*) | Error-handling function that ends the previous call to \*push-error-using-command\* or \*push-error-using-stack\* | Expected in v0.7.0 |
| (\*push-error-using-command\*) | Error-handling function that indicates the use of the command function within a custom \*error\* handler | Expected in v0.7.0 |
| (\*push-error-using-stack\*) | Error-handling function that indicates the use of variables from the AutoLISP stack within a custom \*error\* handler | Expected in v0.7.0 |
| (alert string) | Displays an alert dialog box with the error or warning message passed as a string | |
| (exit [code]) | Forces the current application to quit | VeLisp Extension: added optional exit code |
| (quit [code]) | Forces the current application to quit | VeLisp Extension: added optional exit code |
| (vl-catch-all-apply 'function list) | Passes a list of arguments to a specified function and traps any exceptions | Expected in v0.7.0 |
| (vl-catch-all-error-message error-obj) | Returns a string from an error object | Expected in v0.7.0 |
| (vl-catch-all-error-p arg) | Determines whether an argument is an error object returned from vl-catch-all-apply | Expected in v0.7.0 |

### File-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (chdir dirname) | Changes current working directory | VeLisp Extension |
| (close file-desc) | Closes an open file | |
| (cwd) | Returns current working directory | VeLisp Extension |
| (findfile filename) | Searches the AutoCAD library path for the specified file | Expected in vTBD |
| (findtrustedfile filename) | Searches the AutoCAD trusted file paths for the specified file | Expected in vTBD |
| (homedir) | Returns user's home directory | VeLisp Extension |
| (open filename mode) | Opens a file for access by the AutoLISP I/O functions | |
| (read-char [file-desc]) | Returns the decimal ASCII code representing the character read from the keyboard input buffer or from an open file | |
| (read-line [file-desc]) | Reads a string from the keyboard or from an open file | |
| (tmpdir) | Returns user's temp directory | VeLisp Extension |
| (filename-parse filename) | Returns the root, the name, the directory and the extension from a file name | VeLisp Extension |
| (mkdir dirname) | Creates a directory | VeLisp Extension |
| (rmdir dirname) | Removes a directory | VeLisp Extension |
| (vl-directory-files [ directory pattern directories]) | Lists all files in a given directory | |
| (vl-file-copy "source-filename" "destination-filename" [append]) | Copies or appends the contents of one file to another file | |
| (vl-file-delete "filename") | Deletes a file | |
| (vl-file-directory-p "filename") | Determines if a file name refers to a directory | |
| (vl-file-rename "old-filename" "new-filename") | Renames a file | |
| (vl-file-size "filename") | Determines the size of a file, in bytes | |
| (vl-file-systime "filename") | Returns last modification time of the specified file | |
| (vl-filename-base "filename") | Returns the name of a file, after stripping out the directory path and extension | |
| (vl-filename-directory "filename") | Returns the directory path of a file, after stripping out the name and extension | |
| (vl-filename-extension "filename") | Returns the extension from a file name, after stripping out the rest of the name | |
| (vl-filename-mktemp ["pattern" "directory" "extension"]) | Calculates a unique file name to be used for a temporary file | |
| (vl-mkdir dirname) | Creates a directory | |
| (write-char num [file-desc]) | Writes one character to the screen or to an open file | |
| (write-line string [file-desc]) | Writes a string to the screen or to an open file | |

### Function-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (apply function lst) | Passes a list of arguments to a specified function | |
| (defun sym ([arguments] [/variables ...]) expr ... ) | Defines a function | |
| (eval expr) | Returns the result of evaluating an AutoLISP expression | Expected in vTBD |
| (lambda arguments expr ...) | Defines an anonymous function | |
| (progn [expr ...]) | Evaluates each expression sequentially, and returns the value of the last expression | |
| (trace function ...) | Aids in AutoLISP debugging | Expected in vTBD |
| (untrace function ...) | Clears the trace flag for the specified functions | Expected in vTBD |

### Geometric Functions

| Function | Description | Info |
|----------|-------------|------|
| (angle pt1 pt2) | Returns an angle in radians of a line defined by two endpoints | Expected in v0.8.0 |
| (distance pt1 pt2) | Returns the 3D distance between two points | Expected in v0.8.0 |
| (inters pt1 pt2 pt3 pt4 [onseg]) | Finds the intersection of two lines | Expected in v0.8.0 |
| (polar pt ang dist) | Returns the UCS 3D point at a specified angle and distance from a point | Expected in v0.8.0 |

### List Manipulation Functions

| Function | Description | Info |
|----------|-------------|------|
| (acad_strlsort lst) | Sorts a list of strings by alphabetical order | |
| (append lst ...) | Takes any number of lists and runs them together as one list | |
| (assoc item alist) | Searches an association list for an element and returns that association list entry | |
| (caddr lst) | Returns the third element of a list | |
| (cadr lst) | Returns the second element of a list | |
| (car lst) | Returns the first element of a list | |
| (cdr lst) | Returns the specified list, except for the first element of the list | |
| (cons new-first-element lst) | The basic list constructor | |
| (enumerate lst) | Adds a zero-based counter to each item in a list | VeLisp Extension |
| (foreach name lst [expr ...]) | Evaluates expressions for all members of a list | |
| (join delim lst) | Joins a list using the delimiter | VeLisp Extension |
| (last lst) | Returns the last element in a list | |
| (length lst) | Returns an integer indicating the number of elements in a list | |
| (list [expr ...]) | Takes any number of expressions and combines them into one list | |
| (listp item) | Verifies that an item is a list | |
| (mapcar function list [list ...]) | Returns a list of the result of executing a function with the individual elements of a list or lists supplied as arguments to the function | |
| (member expr lst) | Searches a list for an occurrence of an expression and returns the remainder of the list, starting with the first occurrence of the expression | |
| (nth n lst) | Returns the nth element of a list | |
| (reverse lst) | Returns a list with its elements reversed | |
| (sort cmp lst) | Sorts the elements in a list according to a given compare function | VeLisp Extension |
| (subst newitem olditem lst) | Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item | |
| (uniq lst) | Removes duplicates from a list | VeLisp Extension |
| (usort cmp lst) | Uniquely sorts the elements in a list according to a given compare function | VeLisp Extension |
| (vl-consp list) | Determines whether or not a list is nil | |
| (vl-every predicate list [list ...]) | Checks whether the predicate is true for every element combination | |
| (vl-list* object [object ...]) | Constructs and returns a list | |
| (vl-list->string char-codes-list) | Combines the characters associated with a list of integers into a string | |
| (vl-list-length list-or-cons-object) | Calculates list length of a true list | |
| (vl-member-if predicate list) | Determines whether the predicate is true for one of the list members | |
| (vl-member-if-not predicate list) | Determines whether the predicate is nil for one of the list members | |
| (vl-position symbol list) | Returns the index of the specified list item | |
| (vl-remove element-to-remove list) | Removes elements from a list | |
| (vl-remove-if predicate list) | Returns all elements of the supplied list that fail the test function | |
| (vl-remove-if-not predicate list) | Returns all elements of the supplied list that pass the test function | |
| (vl-some predicate list [list ...]) | Checks whether the predicate is not nil for one element combination | |
| (vl-sort list comparison-function) | Sorts the elements in a list according to a given compare function | Duplicates are not removed. Use usort or uniq functions instead  |
| (vl-sort-i list comparison-function) | Sorts the elements in a list according to a given compare function, and returns the element index numbers | |
| (vl-string->list string) | Converts a string into a list of character codes | |

### Query and Command Functions

| Function | Description | Info |
|----------|-------------|------|
| (getcfg cfgname) | Retrieves application data from the AppData section of the acadXXXX.cfg file | Expected in vTBD |
| (getenv varname) | Returns the string value assigned to an environment variable | |
| (getvar varname) | Retrieves the value of an AutoCAD system variable | Expected in vTBD |
| (setcfg cfgname cfgval) | Writes application data to the AppData section of the acadXXXX.cfg file | Expected in vTBD |
| (setenv varname value) | Sets an environment variable to a specified value | |
| (setvar varname value) | Sets an AutoCAD system variable to a specified value | Expected in vTBD |
| (ver) | Returns a string that contains the current AutoLISP version number | |

### String-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (globmatch string pattern [flag]) | Performs a glob pattern match on a string | VeLisp Extension |
| (read [string]) | Returns the first list or atom obtained from a string | Expected in vTBD |
| (rematch string pattern [flag]) | Performs a regular expression pattern match on a string | VeLisp Extension |
| (split delim str) | Splits a string using the delimiter | VeLisp Extension |
| (strcase string [which]) | Returns a string where all alphabetic characters have been converted to uppercase or lowercase | |
| (strcat [string1 [string2 ...]) | Returns a string that is the concatenation of multiple strings | |
| (strlen [string ...]) | Returns an integer that is the number of characters in a string | |
| (substr string start [length]) | Returns a substring of a string | |
| (vl-prin1-to-string object) | Returns the string representation of any LISP object as if it were output by the prin1 function | |
| (vl-princ-to-string object) | Returns the string representation of any LISP object as if it were output by the princ function | |
| (vl-string->list string) | Converts a string into a list of character codes | |
| (vl-string-elt string position) | Returns the ASCII representation of the character at a specified position in a string | |
| (vl-string-left-trim character-set string) | Removes the specified characters from the beginning of a string | |
| (vl-string-mismatch str1 str2 [pos1 pos2 ignore-case-p]) | Returns the length of the longest common prefix for two strings, starting at specified positions | |
| (vl-string-position char-code str [ start-pos [from-end-p]]) | Looks for a character with the specified ASCII code in a string | |
| (vl-string-right-trim character-set string) | Removes the specified characters from the end of a string | |
| (vl-string-search pattern string [ start-pos]) | Searches for the specified pattern in a string | |
| (vl-string-subst new-str pattern string [start-pos]) | Substitutes one string for another, within a string | |
| (vl-string-translate source-set dest-set str) | Replaces characters in a string with a specified set of characters | |
| (vl-string-trim char-set str) | Removes the specified characters from the beginning and end of a string | |
| (wcmatch string pattern [flag]) | Performs a wild-card pattern match on a string | VeLisp Extension: added optional flag |

### Symbol-Handling Functions Reference

| Function | Description | Info |
|----------|-------------|------|
| (atom item) | Verifies that an item is an atom | |
| (atoms-family format [symlist]) | Returns a list of the currently defined symbols | |
| (boundp sym) | Verifies whether a value is bound to a symbol | Symbol is not automatically created if unbound |
| (not item) | Verifies that an item evaluates to nil | |
| (null item) | Verifies that an item is bound to nil | |
| (numberp item) | Verifies that an item is a real or an integer | |
| (quote expr) | Returns an expression without evaluating it | |
| (set sym expr) | Sets the value of a quoted symbol name to an expression | |
| (setq sym1 expr1 [sym2 expr2 ...]) | Sets the value of a symbol or symbols to associated expressions | |
| (type item) | Returns the type of a specified item | |
| (vl-symbol-name symbol) | Returns a string containing the name of a symbol | |
| (vl-symbol-value symbol) | Returns the current value bound to a symbol | |
| (vl-symbolp object) | Identifies whether or not a specified object is a symbol | |

### User Input Functions

| Function | Description | Info |
|----------|-------------|------|
| (getint [msg]) | Pauses for user input of an integer, and returns that integer | |
| (getkword [msg]) | Pauses for user input of a keyword, and returns that keyword | Expected in vTBD |
| (getreal [msg]) | Pauses for user input of a real number, and returns that real number | |
| (getstring [cr] [msg]) | Pauses for user input of a string, and returns that string | |
| (initget [bits] [string]) | Establishes keywords for use by the next user input function call | Expected in vTBD |

AutoLISP Core Functions from https://help.autodesk.com/ used under https://creativecommons.org/licenses/by-nc-sa/3.0/
