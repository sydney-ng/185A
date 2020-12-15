# UCLA's CS 185A - Computational Linguistics

### Professor: Ethan Poole
### Quarter: Spring 2020

Computational linguistics is a large, multifaceted and rapidly expanding field. There are many
different kinds of work that might be classified as “computational linguistics”, differing in
• goals (e.g., build a useful gadget, test a linguistic theory), and
• empirical domains (e.g., sounds, words, sentences),

## Skills Demonstrated 
• recursive generation of infinitely many expressions by a finite machine,
• interchangeability/intersubstitutability of subexpressions within larger expressions, and
• the relationship between discrete structures and probabilistic models.

# Projects 
Description of the following work: 

## Assignment 1

This project is used to exemplify my knowledge of Labmda Reductions and Lambda Calculus, which is one of the defining features of Haskell. 
The lambda calculus is often called the "assembly language" of functional programming, and variations and extensions on it form the basis of many functional compiler intermediate forms for languages like Haskell, OCaml, StandardML, etc.
In the lambda calculus all lambdas are of a single argument which may itself return another lambda. Out of convenience we often express multiple lambda expressions with their variables on one lambda symbol. This is merely a lexical convention and does not change the underlying meaning.

## Assignment 2  

This is a project that explores how Recursion is implemented in Haskell. This is explored through the `map`, `length`, and `filter` function. However, we will be implementing these functions without calling the language's built-in list functions. 
This can be used to parse parts of speech such as conjugation, negation, and disjunctions. This will be done by calculating the longest root-to-leaf sequence of nodes that are in the given sentence. 

 ## Assignment 3 
 
 This project parses sentences by breaking down the parsing of natural languages into its components. Firstly, these can be represented with machine automata. 
 When converted to Haskell, we examine sentences by writing functions that will return the number of occurences of a pattern, the depth of the pattern, and optional phrases (such as conjunctions) that can be added into a sentence (therefore changing it's semantic meaning, but not changing its grammatical structure). 

## Assignment 4 

This project focuses on automata theory and how languages can repeat certain patterns to create infinitely nested phrases. We create Finite State Automatas 
(also known as FSAs) in Haskell that will recognize requested behavior of sentences. We can now differentiate fragments from segments and sentences. 

## Assignment 5 

This project examines SLGs (also known as strictly-local grammars) to examine how language can be represented without the use of Finite State Automatons. 
These SLGs can check whether expected patterns of given fragments can be combined together to create grammatical sentences. Understanding of the difference, connection, and 
similarities of SLGs and FSAs are shown by creating conversions between the two. 

## Assignment 6 

This project expands upon how sentences can have non-repeating or non-existent parts of speech in a sentence (parts of speech that are optional). This is done by 
examining the relationship between the different words to see which are root nonterminal symbols and which are leaf terminal symbols. We are then able to use the given 
rules to construct a tree that represents how rules of a grammar can be represented visually (and visa versa). We can split sentences after `n` occurrences of a specific symbol, and see if some sentences are derivable from others (depending on the types
of terminals and nonterminals they contain. 

## Midterm 

This project combines the major concepts that lay at the foundation of the class. Given a corpus, a large body of text, we will be parsing sentences. Using the gathered data, we will dtermine the liklihood of certain sentences occurring 
and predicting what part of speech each word (even if it is non-english) has. This is done by calculating the start and end probability of a sentence to determine if there is any way that the sentence can be valid. After, we will use recusion to determine if 
any of the rules will take us from the first to last word. If so, this is a valid sentence. We can then use the generated patterns to determine the probability of what part of speech each word belongs to. The highest probable match is the one that we will return. 

