## Implementing an Evaluator

An evaluator is a kind of interpreter that executes a program.  A Pharo evaluator is an interpreter that takes as input a Pharo program and executes each one of its statements, finally returning the result of the execution.

In this chapter and the following ones, we will implement a Pharo evaluator as an AST interpreter, using the Visitor pattern we have seen before, meaning that the input of our evaluator will be AST nodes of a program to evaluate.

For presentation purposes, we will develop the evaluator in several stages, each in a different chapter.
First, in this chapter, we will show how to implement a structural evaluator, i.e., an evaluator that reads and writes the structures of objects, starting the presentation from constant values.

Later chapters will incrementally add support for other language features that deserve a chapter for themselves such as messages and blocks.

This chapter is presented in a somehow relaxed TDD (Test Driven Development) style.
For each new feature, we first define the scenario we want to cover.
Since we are developing an evaluator, each scenario will have some code to execute and an expected result.
We then define a test for the scenario and we make it pass.
Before going to the next scenario, we do some refactorings to enhance the quality of our code.

During this process, we will define and refine an AST visitor.
Note that we will write the visitor from scratch but we will reuse the nodes of the Pharo AST and their functionalities.

### Setting Up the Stage

To start writing our Pharo evaluator in TDD style, we will start by creating out test class `CInterpreterTest`.
Our class names are prefixed with `C` because we named the package of the interpreter Champollion.

```
TestCase << #CInterpreterTest
	package: 'Champollion-Tests'
```


This class will, until a change is imperative, host all our test methods.

#### Preparing the Scenarios

Our scenarios, made out of classes and methods to be interpreted, need to be written somewhere too.
We could store those scenarios in strings in our test class, that we will then need to parse and give to our interpreter as input.
We will write our scenarios as normal Pharo code, in normal Pharo classes: 
We will host our first scenarios as methods in a new class named `CHInterpretable`.

This solution is simple enough and versatile to support more complex situations in the future.

```
Object << #CInterpretable
	package: 'Champollion-Test'
```


### Evaluating Literals: Integers


Testing our evaluator requires that we test and assert some observable behavior.
In Pharo there are two main observable behaviors: either side-effects through assignments or results of methods through return statements.
We have chosen to use return statements, to introduce variables and assignments later.
To start, our first scenario is a method returning an integer, as in the code below:

```
CInterpretable >> returnInteger
	^ 5
```

Executing such a method should return an integer with value 5.

### Writing a Red Test


Our first test implements what our scenario defined above: executing our method should return 5.
This first test specifies not only part of the behavior of our interpreter but also helps us in defining the part of its API: we want our interpreter to be able to start executing from some method's AST.
Below we define a first test for it: `testReturnInteger`.

```
CInterpreterTest >> testReturnInteger
	| ast result |
	ast := (CInterpretable >> #returnInteger) parseTree.
	result := self interpreter execute: ast.
	self assert: result equals: 5
```


This first test is worth one comment: since our evaluator is an AST interpreter, it requires an AST as input.
In other words, we need to get the AST of the `returnInteger` method.
Instead of invoking the parser to get an AST from source code, we will use Pharo's reflective API to get the AST of an already existing method.

### Making the test pass: a First Literal Evaluator


Executing our first test fails first because our test does not understand `interpreter`, meaning we need to implement a method for it in our test class.
We implement it as a factory method in our test class, returning a new instance of `CInterpreter`, and we define the class `CInterpreter` as follows.

```
CInterpreterTest >> interpreter
	^ CInterpreter new
```


```
Object << #CInterpreter
	package: 'Champollion'
```


The class `CInterpreter`  is the main entry point for our evaluator, and it will implement a visitor pattern over the Pharo method ASTs.  Note that it does not inherit from the default Pharo AST Visitor.
The Pharo AST visitor already implements generic versions of the `visitXXX:` methods that will do nothing instead of failing.
Not inheriting from it allows us to make it clear when something is not yet implemented: we will get problems such as does not understand exceptions that we will be able to implement them step by step in the debugger.
We, nevertheless, follow the same API as the default AST visitor and we use the nodes' `accept:` visiting methods.

At this point, re-executing the test fails with a new error: our `CInterpreter` instance does not understand the message `execute:`.
We implement `execute:` to call the visitor main entry point, i.e., the method `visitNode:`.

```
CInterpreter >> execute: anAST
	^ self visitNode: anAST
```


```
CInterpreter >> visitNode: aNode
	^ aNode acceptVisitor: self
```


Since we evaluate a method AST, when we re-execute the test, the execution halts because of the missing `visitMethodNode:`.
A first implementation for this method simply continues the visit on the body of the method.

```
CInterpreter >> visitMethodNode: aMethodNode
	^ self visitNode: aMethodNode body
```


Execution then arrives at a missing `visitSequenceNode:`.
Indeed, the body of a method is a sequence node containing a list of temporary variable definitions and a list of statements.
Since our scenario has only a single statement with no temporary variables, a first version of `visitSequenceNode:` ignores temporary declarations and handles all the statements paying attention that the last statement value should be returned.
So we visit all the statements except the last one, and we then visit the last one and return its result.

```
CInterpreter >> visitSequenceNode: aSequenceNode
	"Visit the sequence and return the result of the last statement. 	Does not look at temporary declaration for now."
	
	aSequenceNode statements allButLast
		do: [ :each | self visitNode: each ].
	^ self visitNode: aSequenceNode statements last
```


Then the visitor visits the return node, for which we define the `visitReturnNode:` method.
This method simply visits the contents of the return node \(invoking recursively the visitor\) and returns the obtained value.
At this point, the value is not yet covered by the visitor.

```
CInterpreter >> visitReturnNode: aReturnNode
	^ self visitNode: aReturnNode value
```


Finally, the contents of the return node, the integer `5` is represented as a literal value node.
To handle this node, we define the method `visitLiteralValueNode:`.
The implementation just returns the value of the node, which is the integer we were looking for.

```
CInterpreter >> visitLiteralValueNode: aLiteralValueNode
	^ aLiteralValueNode value
```


Our first test is now green and we are ready to continue our journey.

### Evaluating Literals: Floats


For completeness, let's implement support for literal floats.
Since we already have integer constants working, let's consider next a method returning a float literal.
We can see such a scenario in the code below:

```
CHInterpretable >> returnFloat
	^ 3.14
```


Executing such a method should return `3.14`.

### Writing a Test


Testing this case is straightforward, we should test that evaluating our method should return 3.14.
We already defined that our interpreter understands the `execute:`  message, so this test can follow the implementation of our previous test.

```
CHInterpreterTest >> testReturnFloat
	| ast result |
	ast := (CHInterpretable >> #returnFloat) parseTree.
	result := self interpreter execute: ast.
	self assert: result equals: 3.14
```


Two discussions come from writing this test. First, this test is already green, because the case of floating point constants and integer constants exercise the same code, so nothing is to be done on this side.
Second, some would argue that this test is somehow repeating code from the previous scenario: we will take care of this during our refactoring step.


### Refactor: Improving the Test Infrastructure


Since we will write many tests with a similar structure during this book, it comes in handy to share some logic between them. The two tests we wrote so far show a good candidate of logic to share as repeated code we can extract.

The method `executeSelector:` extracts some common logic that will make our tests easier to read and understand: it obtains the AST of a method from its selector, evaluates it, and returns the value of the execution.

```
CHInterpreterTest >> executeSelector: aSymbol
	| ast |
	ast := (CHInterpretable >> aSymbol) parseTree.
	^ self interpreter execute: ast
```


And we can now proceed to rewrite our first two tests as follows:

```
CHInterpreterTest >> testReturnInteger
	self
		assert: (self executeSelector: #returnInteger)
		equals: 5
```


```
CHInterpreterTest >> testReturnFloat
	self
		assert: (self executeSelector: #returnFloat)
		equals: 3.14
```


We are ready to efficiently write tests for the other constants.

### Evaluating booleans

Boolean literals are the objects `false` and `true`, typically used for conditionals and control flow statements.
In the previous sections we implemented support for numbers, now we introduce support for returning boolean values as follows:

```
CHInterpretable >> returnBoolean
	^ false
```


Evaluating such a method should return `false`.
We define a test for our boolean scenario.
Note that here we do not use `deny:`, because we want to make the result explicit for the reader of the test.

```
CHInterpreterTest >> testReturnBoolean
  "Do not use deny: to make explicit that we expect the value false"

	self
	    assert: (self executeSelector: #returnBoolean)
	    equals: false
```


If everything goes ok, this test will be automatically green, without the need for implementing anything.
This is because booleans are represented in the AST with literal value nodes, which we have already implemented.


### Evaluating Literals: Arrays


Now that we support simple literals such as booleans and numbers, let's introduce literal arrays.
Literal arrays are arrays that are defined inline in methods with all their elements being other literals.
Literals refer to the fact that such objects are created during the parsing of the method code and now by sending
a message.
For this scenario, let's define two different test scenarios: an empty literal array and a literal array that has elements.

```
CHInterpretable >> returnEmptyLiteralArray
	^ #()

CHInterpretable >> returnRecursiveLiteralArray
	^ #(true 1 #('ahah'))
```


These two methods should return the respective arrays.

### Writing a Red Test

Writing tests to cover these two scenarios is again straightforward:

```
CHInterpreterTest >> testReturnEmptyLiteralArray
	self
		assert: (self executeSelector: #returnEmptyLiteralArray)
		equals: #()

CHInterpreterTest >> testReturnRecursiveLiteralArray
	self
		assert: (self executeSelector: #returnRecursiveLiteralArray)
		equals: #(true 1 #('ahah'))
```


### Making the test pass: visiting literal array nodes


We have to implement the method `visitLiteralArrayNode:` to visit literal arrays.
Literal arrays contain an array of literal nodes, representing the elements inside the literal array.
To make our tests pass, we need to evaluate a literal array node to an array where each element is the value of its corresponding literal node. Moreover, literal arrays are recursive structures: an array can contain other arrays.
In other words, we should handle the visit of literal arrays recursively.
Here we return the values returned by the interpretation of the elements.

```
CHInterpreter >> visitLiteralArrayNode: aLiteralArrayNode
	^ aLiteralArrayNode contents
			collect: [ :literalNode | self visitNode: literalNode ]
			as: Array
```


This makes our tests pass, and so far there is nothing else to refactor or clean.
Up until now, we did not consider any form of variable and we should handle them.

### Conclusion


In this chapter, we have used the Visitor pattern over AST nodes to implement a first version of a structural evaluator. 
This evaluator covers the basic literals: integers, floats, booleans and arrays.
Although we have not talked about it explicitly, we also implemented a first version of the visit of statements and return nodes.

We leave for the reader the exercise of extending this prototype with support for dynamic arrays \(e.g., `{ self expression. 1+1 }`\).