## Variables and Name Resolution


In the previous Chapter we wrote a first version of an evaluator managing literals: integers, floats, arrays and booleans.
This chapter introduces in our evaluator the capability of evaluating variables.
Variables are, using a very general definition, storage location associated with a name.
For example, a global variable in Pharo is stored in a global dictionary of key-value associations, where the association's key is the name of the variable and its value is the value of the variable respectively.
Instance variables, on their side, are storage locations inside objects.
Instance variables in Pharo have each a location in the object, specified by an index, and that index corresponds to the index of the variable's name in its class' instance variable list.

In this chapter we will study how variables are resolved.
Since basically variable nodes contain only their name, this involves a "name resolution" step: finding where the variable is located, and access it accordingly following the lexical scoping \(or static scoping\) rules.
Name resolution using lexical scoping discovers variables using the nesting hierarchy of our code, where each nesting level represents a scope. For example, classes define a scope with variables visible only by their instances; the global scope defines variables that are visible everywhere in the program, and classes are nested within the global scope.

In this chapter we will implement name resolution by modelling the program scopes.
Scope objects will act as adapters for the real storage locations of variables, giving us a polymorphic way to read and write variables. Moreover, scopes will be organized in a chain of scopes, that will model the lexical organization of the program.


### Starting up with `self` and `super`


To start working with variables, we begin with a method returning `self` as the one below.
There is now a big difference between this scenario and the previous ones about literals.
Previous scenarios did always evaluate to the same value regardless how or when they were executed.
In this case, however, evaluating this method will return a different object if the receiver of the message changes.

```
CHInterpretable >> returnSelf [
	^ self
]
```


#### Writing a Red Test


To properly test how `self` behaves we need to specify a receiver object, and then assert that the returned object is that same object with an identity check.
For this purpose we use as receiver a new object instance of `Object`, which  implement equality as identity, guaranteeing that the object will be not only equals, but also the same. We use an explicit identity check in our test to convey our intention.

```
CHInterpreterTest >> testReturnSelf [
	| receiver |
	receiver := Object new.
  "Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSelf
      withReceiver: receiver)
          == receiver
]
```


#### Introducing Support to Specify a Receiver


In this section we implement a first version of an execution context to introduce receiver objects.
This first support will get us up and running to implement initial variable support, but it will run short to properly implement message sends. This set up will be extended in later chapters to support to message sends and block temporaries.

This initial support requires that the evaluator gets access to the object that is the receiver in the current method execution.
For now we add an instance variable in our evaluator called `receiver`.

```
Object subclass: #CHInterpreter
	instanceVariableNames: 'receiver'
	classVariableNames: ''
	package: 'Champollion-Core'
```


We define an accessor so that we can easily adapt when we will introduce a better way representation later.
This instance variable is hidden behind this accessor, allowing us to change later on the implementation without breaking users.

```
CHInterpreter >> receiver [
	^ receiver
]
```



Then we replace the existing implementation of `execute:` in `CHInterpreter` by `execute:withReceiver:` which allows us to specify the receiver.

```
CHInterpreter >> execute: anAST withReceiver: anObject [
	receiver := anObject.
	^ self visitNode: anAST
]
```


And finally, since that change break all our tests, we will create a new helper method in our tests `executeSelector:withReceiver:` and then redefine the method `executeSelector:` to use this new method with a default receiver.

```
CHInterpreterTest >> executeSelector: aSymbol [
  ^ self executeSelector: aSymbol withReceiver: nil
]

CHInterpreterTest >> executeSelector: aSymbol withReceiver: aReceiver [
	| ast |
	ast := (CHInterpretable >> aSymbol) parseTree.
	^ self interpreter execute: ast withReceiver: aReceiver
]
```


Now we have refactored our code, introduced the possibility to specify a message receiver, and still keep all our previous tests passing. Our new test is still red, but we are now ready to make it pass.

#### Making the test pass: visiting self nodes


The interpretation of `self` is done in the method `visitSelfNode:`.
The Pharo parser resolves `self` and `super` nodes statically and creates special nodes for them, avoiding the need for name resolution.
Implementing it is simple, it just returns the value of the receiver stored in the interpreter.
Note that this method does not access the `receiver` variable directly, but uses instead the accessor, leaving us the possibility of redefining that method without breaking the visit.

```
CHInterpreter >> visitSelfNode: aNode [
	^ self receiver
]
```


#### Introducing `super`


Following the same logic as for `self`, we improve our evaluator to support `super`.
We start by defining a method using `super` and its companion test.

```
CHInterpretable >> returnSuper [
	^ super
]
```


```
CHInterpreterTest >> testReturnSuper [
  | receiver |
  receiver := Object new.
  "Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSuper
      withReceiver: receiver)
          == receiver
]
```


What the interpretation of `super` shows is that this variable is also the receiver of the message.
Contrary to a common and wrong belief, `super` is not the superclass or an instance of the superclass.
It simply is the receiver:

```
CHInterpreter >> visitSuperNode: aNode [
	^ self receiver
]
```



### Introducing Lexical Scopes


Variables in Pharo are of different kinds: instance variables represent variable stored in the evaluated message receiver, temporaries and arguments are variables visible only within a particular method evaluation, shared variables and global variables are variables that are in the global scope and independent of the receiver.

All these "contexts" definining variables are also named scopes, because they define the reach of variable definitions.
At any point during program execution, we can organize scopes in a hierarchy of scopes following a parent relationship.
When a method evaluates, the current scope is the method scope with all the arguments and temporary variables defined in the method. The parent of a method scope is the instance scope that defines all the variables for an instance. The parent of the instance scope is the global scope that defines all the global variables.

Notice that in this scope organization we are not explicitly talking about classes.
The instance scope takes care of that resolving all instance variables in the hierarchy of the receiver object.
A class scope could be added later to resolve class variables, and class pools.

The basic structure of our lexical scope implementation introduces the method `scopeDefining:` in our interpreter, that returns the scope defining the given name. The method `scopeDefining:` forwards the search to the current scope, obtained through `currentScope`. Finally, we will extend our interpreter with `visitVariableNode:` and `visitAssignmentNode:` to handle variable reads and writes. Reads \(and writes\) obtain the scope for the read \(written\) variable and delegate the read \(write\) to it. This means our scope objects need to define methods `scopeDefining:` and `read:` and `write:withValue:`.

```
CHInterpreter >> scopeDefining: aName [
	^ self currentScope scopeDefining: aName
]

CHInterpreter >> currentScope [
	^ ...
]

CHInterpreter >> visitVariableNode: aVariableNode [
	^ (self scopeDefining: aVariableNode name) read: aVariableNode name
]

CHInterpreter >> visitAssignmentNode: anAssignmentNode [
	| value |
	value := self visitNode: anAssignmentNode value.
	(self scopeDefining: anAssignmentNode variable name)
    write: anAssignmentNode variable name
    withValue: value.
	^ value
]
```


### Evaluating Variables: Instance Variable Reads


The first step is to support instance variable reads.
Since such variables are evaluated in the context of the receiver object, we need a receiver object that has instance variables. 
We already had such a class: `CHInterpretable`.
We then add an instance variable and a getter and setter for it to be able to control the values in that instance variable.

```
Object subclass: #CHInterpretable
  instanceVariableNames: 'x'
  classVariableNames: ''
  package: 'Champollion-Tests'

CHInterpretable >> returnX [
	^ x
]

CHInterpretable >> x: anInteger [
	x := anInteger
]
```


To test the correct evaluation of the instance variable read, we check that the getter returns the value in the instance variable, which we can previously set.

```
CHInterpreterTest >> testReturnInstanceVariableRead [
	| receiver |
	receiver := CHInterpretable new.
	receiver x: 100.
	self
		assert: (self executeSelector: #returnX withReceiver: receiver)
		equals: 100
]
```


Our test is failing so we are ready to make it work.

#### Creating an Instance Scope


To make our tests go green, we need to implement instance scopes and the `CHInterpreter>>currentScope` method.
We will model instance scopes with a new class. This class will know the receiver object, extract the list of instance variables from it, and know how to read and write from/to it.

```
Object subclass: #CHInstanceScope
	instanceVariableNames: 'receiver'
	classVariableNames: ''
	package: 'Champollion-Core'

CHInstanceScope >> receiver: anObject [
  	receiver := anObject
]

CHInstanceScope >> scopeDefining: aString [
	(self definedVariables includes: aString)
		ifTrue: [ ^ self ].
	
	^ self parentScope scopeDefining: aString
]

CHInstanceScope >> definedVariables [
	^ receiver class allInstVarNames 
]

CHInstanceScope >> read: aString [
	^ receiver instVarNamed: aString
]
```


We then can define our `currentScope` method as follows.

```
CHInterpreter >> currentScope [
  	^ CHInstanceScope new
  		receiver: self receiver;
  		yourself
]
```


All our tests should now pass!

### Refactor: Improving our Tests `setUp`


Let's simplify how receivers are managed in tests.
Instead of having to explicitly create and manage a receiver object, we add the receiver as an instance variable to the `CHInterpreterTest`.

```
TestCase subclass: #CHInterpreterTest
	instanceVariableNames: 'interpreter receiver'
	classVariableNames: ''
	package: 'Champollion-Tests'
```


We can then redefine the `setUp` method to create a new instance of `CHInterpretable` and  assign it to the variable `receiver`.

```
CHInterpreterTest >> setUp [
	super setUp.
	receiver := CHInterpretable new
]
```


And now we use this new instance variable in `executeSelector:` as the default receiver instead of `nil`.

```
CHInterpreterTest >> executeSelector: aSymbol [
  ^ self executeSelector: aSymbol withReceiver: receiver
]
```


And finally we rewrite all our test methods using an explicit receiver:

```
CHInterpreterTest >> testReturnSelf [
  "Convey our intention of checking identity by using an explicit identity check"
  self assert: (self
    executeSelector: #returnSelf
    withReceiver: receiver)
        == receiver
]

CHInterpreterTest >> testReturnSuper [
  "Convey our intention of checking identity by using an explicit identity check"
  self assert: (self
    executeSelector: #returnSelf
    withReceiver: receiver)
        == receiver
]

CHInterpreterTest >> testReturnInstanceVariableRead [
	receiver x: 100.
	self
		assert: (self executeSelector: #returnX)
		equals: 100
]
```


### Instance Variable Writes


Now that we have support for instance variable reads, we keep on going with instance variable writes.
In our scenario, a method writes a literal integer into an instance variable `x` and then returns the value of the assignment. This scenario has two observable behaviors that we will be tested separately.
First, such an assignment should be observable from the outside by reading that variable.
Second, an assignment is an expression whose value is the assigned value, thus the return value should also be the assigned value.

```
CHInterpretable >> store100IntoX [
	^ x := 100
]
```


To test this behavior, we evaluate the method above and then we validate that effectively the instance variable was mutated.
To make sure the value was modified, we set an initial value to the variable before the evaluation.
After the evaluation we should not keep that value.
The case of the return is similar to our previous tests.

```
CHInterpreterTest >> testStore100IntoX [
	receiver x: 17.
	self executeSelector: #store100IntoX.
	self assert: receiver x equals: 100
]

CHInterpreterTest >> testAssignmentReturnsAssignedValue [
	self
		assert: (self executeSelector: #store100IntoX)
		equals: 100
]
```


And finally, we implement a getter for the `x` variable.
This getter is used to extract the value in the tests.

```
CHInterpretable >> x [
	^ x
]
```


To make this tests pass, let's first see in detail our method `visitAssignmentNode`.

```
CHInterpreter >> visitAssignmentNode: anAssignmentNode [
	| value | 
	value := self visitNode: anAssignmentNode value.
	(self scopeDefining: anAssignmentNode variable name)
    write: anAssignmentNode variable name
    withValue: value.
	^ value
]
```


Evaluating an assignment node with the form `variable := expression` requires that we evaluate the `expression` of the assignment, also called the right hand side of the assignment, and then set that value to the left-side variable.
Notice that the variable we are assigning into does not need to be evaluated/visited: the evaluation of a variable is equivalent to reading its value.
And in the left part of an assignment, we do not care about the value of the variable, but about the location of the variable.
Our visit method then looks as follows: we recursively visit the right-side of the assignment \(got through the `value` accessor\), we set that value to the variable by delegating to the scope, and we finally return the stored value.
To set the value to the variable, we implement `write:withValue:` in our instance scope.

```
CHInstanceScope >> write: aString withValue: anInteger [
	receiver instVarNamed: aString put: anInteger
]
```



Now the tests should pass.


### Evaluating Variables: Global Reads


We finish this chapter with the reading of global variables, which cover two cases: proper global variables, and the access to classes.
To better control our testing environment, we decided to not use the Pharo environment by default.
Instead, the interpreter will know its global scope in an instance variable and lookup globals in it using a simple API, making it possible to use the system's global environment instead if we wanted to.
We also leave outside of the scope of this chapter the writing to global variables.
The case of global variable writing is similar to the instance variable assignment.

Our first testing scenario, similar to the previous ones, is as follows: we define a method `returnGlobal` reads and returns the global named `Global`.
When defining such method in Pharo, the IDE will ask what `Global` is, because such a variable does not exist: select to create it as a global variable.
Such a detail is only necessary to keep Pharo itself happy and it does not have any impact in our implementation.
Remember that we are writing our own Pharo implementation in the interpreter, and we will decide what to do with `Global` ourselves.

```
CHInterpretable >> returnGlobal [
	^ Global
]
```


We define a test which specifies that the interpreter' environment has a binding whose key is `#Global` and value is a new object.

```
CHInterpreterTest >> testReturnGlobal [
	| globalObject |
	globalObject := Object new.
	interpreter globalEnvironmentAt: #Global put: globalObject.
	self assert: (self executeSelector: #returnGlobal) equals: globalObject
]
```


We introduce a new global scope class `CHGlobalScope`, and an instance variable named `globalScope` in the class `CHInterpreter` initialized to a global scope.

```
Object subclass: #CHGlobalScope
	instanceVariableNames: 'globalsDictionary'
	classVariableNames: ''
	category: 'Champollion-Core'

CHGlobalScope >> initialize [
	super initialize.
	globalsDictionary := Dictionary new
]

CHGlobalScope >> globalsDictionary: anObject [
	globalsDictionary := anObject
]

CHGlobalScope >> at: aKey ifAbsent: aBlock [
  ^ globalsDictionary at: aKey ifAbsent: aBlock
]

Object subclass: #CHInterpreter
	instanceVariableNames: 'globalScope'
	classVariableNames: ''
	category: 'Champollion-Core'

CHInterpreter >> initialize [
	super initialize.
	globalScope := CHGlobalScope new
]

CHInterpreter >> globalEnvironment [
	^ globalScope
]
```


We define the method `globalEnvironmentAt:put:` to easily add new globals from our test.
```
CHInterpreter >> globalEnvironmentAt: aSymbol put: anObject [
	globalScope at: aSymbol put: anObject
]

CHGlobalScope >> at: aKey put: aValue [
	globalsDictionary at: aKey put: aValue
]
```


We decided that trying to access a global that is not defined will raise an error in the interpreter and halt the interpretation.
An alternative design would return a default value instead such as `nil`.
A more complex design would have been to throw an exception in the interpreted program.
For the moment, halting the interpreter with an error suffices for our job.

```
CHInterpreter >> visitGlobalVariableNode: aRBGlobalNode [
	^ self globalEnvironment
      at: aRBGlobalNode name
      ifAbsent: [ self error: aRBGlobalNode name, ' not found' ]
]
```


Finally, we need to chain the global scope to the instance scope, and define a `scopeDefining:` and a `read:` method in our global scope.

```
CHInterpreter >> currentScope [
	^ CHInstanceScope new
    receiver: self receiver;
		parentScope: globalScope;
		yourself
]

CHInstanceScope >> parentScope: anObject [
	parentScope := anObject
]

CHInstanceScope >> parentScope [
	^ parentScope
]

CHGlobalScope >> scopeDefining: aString [
	"I'm the root scope..."
	^ self
]

CHGlobalScope >> read: aString [
	^ globalsDictionary at: aString
]
```


### Conclusion


In this chapter we introduced support for variables and name resolution,to support instance variables and global variables. 
This first evaluator serves as basis for implementing the execution of the main operations in Pharo: messages.
The following chapters will start by decomposing message resolution into method-lookup and apply operations, introduce the execution stack, the management of temporary variables and argument, and 'super' message-sends.

We further invite the reader to explore changing the language semantics by modifying this simple evaluator: How could we implement read-only objects? Log all variable reads and writes?
