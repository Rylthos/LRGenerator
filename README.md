# LR Table Generator
An LR Table generator to generate the action and goto tables for an LR parser
```
usage: dune exec LRGenerator -- -i <grammar_file>
```

# Grammar file
The input file for the program follows the form
```
<start> -> <prod0>
<rule1> -> <prod1>
<rule2> -> <prod2>
...
```
Where prod is a list of productions for the rule, seperated with '|''s.
Terminals must be surrounded by double quotes (") while non terminals should be left as is.

The first rule in the grammmar file is assumed to be the starting state.
An empty string can be represented by the non terminal "EMPTY"

# Example Grammar
```
S -> E
E -> "(" E ")" | A
A -> "a"
```
The non terminals are $S, E, A$ with $(, ), a$ being the terminals. $S$ is the starting state.
Which represents the language

$$
L = \big\\{ (^n a )^n | n \in \mathbb{N}, n \geq 0 \big\\}
$$
