================================================================================
                        Concatenative Calculus Evaluator
================================================================================


An online tool for playing with concatenative calculus. Supports defining your
own base operators as well as chosing from several presets, and evaluating
expressions using those operators. In the future it is planned to add support
for defining words as reusable code snippets.

It's currently hosted on the `QWD software`_ page.

.. _QWD software: https://qwd.software/home/olus2000/concat-eval.html


Concantenative calculus overview
================================

For the purpose of this tool CC (concatenative calculus) is a pattern
substitution based evaluation scheme. Given a set of rules (operators) it will
repeatedly find the first occurence of a rule and perform a matching
substitution.


Expressions
-----------

A CC expression consists of words, some of which can correspond to operators,
and bracketed subexpressions. Any string of non-whitespace characters is a valid
word, other than brackets. Any string of whitespace-separated words with
balanced brackets is a valid expression. Expressions in brackets are called
quotations or quotes.

This tool additionally restricts the words ``(``, ``--`` and ``)`` for future
use.


Operators
---------

Substitution rules in CC consist of a pattern and a substitution. Patterns
always have the form of some number of quotes followed by the operator word.
They capture contents of the argument quotes for use in the substitution. The
substitution can be any valid expression, but it can only use words referencing
the captured arguments.

In this tool you can adjust the number of arguments of an operator using the
``+-`` buttons, set its name, and provide a substitution using numbers to
reference captured arguments. For example this pattern represents an operator
that duplicates a quotation::

    [ 1 ] dup -> [ 1 ] [ 1 ]


Words
-----

You may see that some code snippets are used often in bigger expressions. You
can name the snippets, and whenever the evaluator encounters them it will
replace them with their definition.

In standard CC words either don't exist or can only have non-recursive
definitions. Concat Eval doesn't check for recursion in definitions though, so
you are free to experiment outside the classical rules.


Numbers
-------

As in many math-level computer science settings you can define natural numbers
as functions that do something N times with their argument. In this tool it's
done by defining an expression representing zero, and a successor expression
representing a function that increments a number. If you define both then
natural numbers in expressions will be replaced with a zero expression followed
by an appropriate number of successor expressions.


Comments
--------

Just looking at CC expressions often doesn't easily convey what the expression
does, especially if it uses few words and many operators. You can put text in
parenthesis to help anyone who reads it understand its purpose. Comments can be
nested in one another and span multiple lines, but both parenthesis must be
separated from surrounding text with spaces! You can use comments in word and
number definitions and in the main expression.


The stack
---------

Another way to think about CC is in terms of operators working with a stack of
quotations. Evaluating the expression from left to right quotations are put on
the stack, and whenever an operator is encountered it's performs some operation
on the top values of the stack.


Functionality
=============

Operators can be created using the ``Add operator`` row, edited using their
interface and deleted using the trash icon. For the meaning of operator
interface see Operators_. Operators with errors will not be included in
evalutation.

Similar interface exists for word definitions.

You can toggle the replacement of numbers with zero-successor expressions using
the checkboxes. The replacement will not occur if the appropriate expression has
an error.

Help button links to this file or a halp page if I make it.

The rest of the top buttons will set the operators to one of preset bases.

After you enter the expression into the textbox at the bottom and resolve any
errors you can step through its evaluation using the buttons below the textbox.


Compilation
===========

To compile the application to runnable javascript you will need Elm_. After
installing Elm run the following command in the project root::

    elm make src/Main.elm --output main.js

This should compile the file ``main.js`` which is referenced by ``index.html``,
and ``index.html`` should now display the application when opened with a
browser.

.. _Elm: https://guide.elm-lang.org/install/elm.html


Credits
=======

This software has been created by `Aleksander "olus2000" Sabak`_ in 2023 and 
released under the `GPL v3 license`_. Thanks to the `QWD community`_ for help!

.. _Aleksander "olus2000" Sabak: https://github.com/olus2000
.. _GPL v3 license: ./LICENSE
.. _QWD community: https://qwd.software
