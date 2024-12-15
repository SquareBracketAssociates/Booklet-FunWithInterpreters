## Scopes and Variables
@cha:scope

Variables are, using a very general definition, storage location associated with a name.
For example, a global variable in Pharo is stored in a global dictionary of key-value associations, where the association's key is the name of the variable and its value is the value of the variable respectively.

Instance variables, on their side, are storage locations inside objects.
Instance variables in Pharo have each a location in the object, specified by an index, and that index corresponds to the index of the variable's name in its class' instance variable list.

In this chapter, we will study how variables are resolved.
Since variable nodes contain only their name, this involves a "name resolution" step: finding where the variable is located and accessing it accordingly following the lexical scoping (or static scoping) rules.
Name resolution using lexical scoping discovers variables using the nesting hierarchy of our code, where each nesting level represents a scope. For example, classes define a scope with variables visible only by their instances; the global scope defines variables that are visible everywhere in the program, and classes are nested within the global scope. Methods define another scope where their arguments and temporary variables are specific to a given method.

In this chapter, we will implement name resolution by modeling the program scopes.
Scope objects will act as adapters for the real storage locations of variables, giving us a polymorphic way to read and write variables. Moreover, scopes will be organized in a chain of scopes, that will model the lexical organization of the program.


### Evaluating Variables: Instance Variable Reads

The first step is to support instance variable reads.
Since such variables are evaluated in the context of the receiver object, we need a receiver object that has instance variables. 
We then add to `CInterpretable` an instance variable and a getter and setter for it to be able to control the values in that instance variable.

```
Object << #CInterpretable
  slots: { #x };
  package: 'Champollion-Tests'

CInterpretable >> x
	^ x
	
CInterpretable >> x: anInteger
	x := anInteger
```

We implement the method `returnInstanceVariableX` to follow the pattern we used until now. 

```
CInterpretable >> returnInstanceVariableX
	^ x
```



To test the correct evaluation of the instance variable read, we check that the getter returns the value in the instance variable, which we can previously set.

```
CHInterpreterTest >> testReturnInstanceVariableRead
	| receiver |
	receiver := CInterpretable new.
	receiver x: 100.
	self
		assert: (self executeSelector: #returnInstanceVariableX withReceiver: receiver)
		equals: 100
```


Our test is failing so we are ready to make it work.


### Introducing Lexical Scopes

Variables in Pharo are of different kinds: 
- instance variables represent variables stored in the evaluated message receiver, 
- temporaries and arguments are variables visible only within a particular method evaluation, 
- shared variables group variables that are visible to a class, its instances and its subclasses, 
- global variables are variables that are in the global scope and independent of the receiver.

All these "contexts" defining variables are also named scopes because they define the reach of variable definitions.
At any point during program execution, we can organize scopes in a hierarchy of scopes following a parent relationship.

- When a method evaluates, the current scope is the method scope with all the arguments and temporary variables defined in the method. 
- The parent of a method scope is the instance scope that defines all the variables for an instance. 
- The parent of the instance scope is the global scope that defines all the global variables.

Notice that in this scope organization, we are not explicitly talking about classes.
The instance scope takes care of that: it resolves all instance variables in the hierarchy of the receiver object.
A class scope could be added later to resolve shared variables and shared pools.

### Creating an Instance Scope

To make our tests go green, we need to model instance scopes.
We model instance scopes with a new class named 'CInstanceScope`. 
This class should know the receiver object, extracts the list of instance variables from it, and know how to read and write from/to it.
For now we focus on the reading part. 

The method `definedVariables` simply returns all the instance variables that an instance should have. 
Finally accessing the value of a variable is possible using the reflective method `instVarNamed:`. 
```
Object <<#CInstanceScope
	slots: {#receiver};
	package: 'Champollion'

CInstanceScope >> receiver: anObject
  	receiver := anObject

CInstanceScope >> definedVariables
	^ receiver class allInstVarNames 

CInstanceScope >> read: aString 
	^ receiver instVarNamed: aString
```

Now we can define the method `currentScope` in the interpreter with a first implementation. 
We will refine in the following sections. 

```
CInterpreter >> currentScope
  	^ CInstanceScope new
  		receiver: self receiver;
  		yourself
```

### Using the scope

The basic structure of our lexical scope implementation introduces the method `scopeDefining:`.
This method returns the scope defining the given name. The method `scopeDefining:` forwards the search to the current scope, obtained through `currentScope`. Since we only have one scope for now we do not cover the case where the variable is not in the variables of the receiver. 


```
CInstanceScope >> scopeDefining: aString
	(self definedVariables includes: aString)
		ifTrue: [ ^ self ].
	Error signal: 'Variable ', aString, ' not found'
```

Now we can simply define the method scopeDefining: within the interpreter. 
It just delegates to the current scope. 

```
CInterpreter >> scopeDefining: aName
	^ self currentScope scopeDefining: aName
```

We should redefine the method `visitVariableNode:` to handle variables that are different from `self` and `super`.

```
CInterpreter >> visitVariableNode: aVariableNode
	
	^ aVariableNode name = #self | (aVariableNode name = #super)
		ifTrue: [ self receiver ]
		ifFalse: [ (self scopeDefining: aVariableNode name) read: aVariableNode name ]
```

All our tests should now pass!



### Refactor: Improving our Tests `setUp`


Let's simplify how receivers are managed in tests.
Instead of having to explicitly create and manage a receiver object, we add the receiver as an instance variable to the `CInterpreterTest`.

```
TestCase << #CInterpreterTest
	slots: { #interpreter . #receiver};
	package: 'Champollion'
```


We can then redefine the `setUp` method to create a new instance of `CHInterpretable` and assign it to the variable `receiver`.

```
CInterpreterTest >> setUp 
	super setUp.
	receiver := CInterpretable new
```


And now we use this new instance variable in `executeSelector:` as the default receiver instead of `nil`.

```
CInterpreterTest >> executeSelector: aSymbol
	^ self executeSelector: aSymbol withReceiver: receiver
```

And finally we rewrite all our test methods using an explicit receiver:

```
CInterpreterTest >> testReturnSelf
	self assert: (self executeSelector: #returnSelf) == receiver

CHInterpreterTest >> testReturnSuper
	self assert: (self executeSelector: #returnSelf) == receiver

CInterpreterTest >> testReturnInstanceVariableRead
	receiver x: 100.
	self
		assert: (self executeSelector: #returnX)
		equals: 100
```



### Instance Variable Writes

We have support for instance variable reads. Let us work on instance variable writes.
In our scenario, a method writes a literal integer into an instance variable `x` and then returns the value of the assignment. This scenario has two observable behaviors that we will be tested separately.

First, such an assignment should be observable from the outside by reading that variable.
Second, an assignment is an expression whose value is the assigned value, thus the return value should also be the assigned value.

```
CInterpretable >> store100IntoInstanceVariableX
	^ x := 100
```


To test this behavior, we evaluate the method above and then we validate that effectively the instance variable was mutated.
To make sure the value was modified, we set an initial value to the variable before the evaluation.
After the evaluation, we should not keep that value.
The case of the return is similar to our previous tests.

```
CInterpreterTest >> testStore100IntoInstanceVariableX
	receiver x: 17.
	self executeSelector: #store100IntoInstanceVariableX.
	self assert: receiver x equals: 100

CInterpreterTest >> testAssignmentReturnsAssignedValue
	self
		assert: (self executeSelector: #store100IntoInstanceVariableX)
		equals: 100
```

We should extend the instance scope to support the modification of a variable. 
This is what we do using the method `instVarNamed:put:`.

```
CInstanceScope >> write: aString withValue: anInteger
	receiver instVarNamed: aString put: anInteger
```


Finally, we extend the interpreter with the method `visitAssignmentNode:`.
To make these tests pass, let's first see in detail our method `visitAssignmentNode`.

```
CHInterpreter >> visitAssignmentNode: anAssignmentNode
	| value | 
	value := self visitNode: anAssignmentNode value.
	(self scopeDefining: anAssignmentNode variable name)
		write: anAssignmentNode variable name
		withValue: value.
	^ value
```

Evaluating an assignment node with the form `variable := expression` requires that we evaluate the `expression` of the assignment, also called the right-hand side of the assignment, and then set that value to the left-side variable.

Our visit method then looks as follows: 
- We recursively visit the right side of the assignment (got through the `value` accessor). Note that the expression can be complex such as the result of multiple messages or other assignments.
- We set that value to the variable by delegating to the instance scope (message `write:withValue:`), and 
- We finally return the stored value.

Now the tests should pass.


### Evaluating Variables: Global Reads

We finish this chapter with the reading of global variables, which covers two cases: proper global variables and access to classes.
It illustrate the chain of scopes where an instance scope parent is a global scope. 

To better control our testing environment, we decided to not use the Pharo environment by default.
Instead, the interpreter will know its global scope in an instance variable and look up globals in it using a simple API, making it possible to use the system's global environment instead if we wanted to.
We also leave outside of this chapter the writing to global variables.
The case of global variable writing is similar to the instance variable assignment.



### Little setup

Our first testing scenario, similar to the previous ones, is as follows: we will define a method `returnGlobal` that reads and returns the global named `Global`.
Now the question is that from the CInterpretable class the global variable `Global` should be a Pharo global variable. 
Note that when the interpreter will encounter such a variable, it will use its own private environment. 

So to make sure that you can compile the code and execute your tests, we define the class method initialize as follows: 

```
CInterpretable class >> initialize
	self setUpGlobal
	
CInterpretable class >> setUpGlobal

	self environment at: #Global put: 33
```

Execute the method initialize doing `CInterpretable initialize`. It will be executed in the future when you load the package.

Such a detail is only necessary to keep Pharo itself happy and it does not have any impact in our implementation.
Remember that we are writing our own Pharo implementation in the interpreter, and we will decide what to do with `Global` ourselves.


### Implementing Global reads


Define the method `returnGlobal` as follows: 

```
CInterpretable >> returnGlobal
	^ Global
```

We start by enriching the class `CInterpreterTest` with an instance variable pointing to a new interpreter. 

```
CInterpreterTest >> setUp	super setUp.	interpreter := CInterpreter new. 	receiver := CInterpretable new
```

This implies that we should modify the getter to be 

```
CInterpreterTest >> interpreter	^ interpreter
```

We define a test that specifies that the interpreter's environment has a binding whose key is `#Global` and value is a new object.
You see here that we parametrize the interpreter so that it has a globalEnvironment that is different from the one its class.

```
CInterpreterTest >> testReturnGlobal
	| globalObject |
	globalObject := Object new.
	interpreter globalEnvironmentAt: #Global put: globalObject.
	self assert: (self executeSelector: #returnGlobal) equals: globalObject
```


We introduce a new global scope class `CGlobalScope`,A globalDictionary is basically a dictionary.

```
Object << #CGlobalScope
	slots: { #globalDictionary };
	package: 'Champollion'

CGlobalScope >> initialize
	super initialize.
	globalDictionary := Dictionary new

CGlobalScope >> globalDictionary: anObject 
	globalDictionary := anObject

CGlobalScope >> at: aKey ifAbsent: aBlock
  ^ globalDictionary at: aKey ifAbsent: aBlock
```

We add an instance variable named `globalScope` in the class `CInterpreter` initialized to a global scope.


```
Object << #CInterpreter
	slots: { #receiver . #globalScope };
	package: 'Champollion2

CInterpreter >> initialize
	super initialize.
	globalScope := CGlobalScope new

CInterpreter >> globalEnvironment
	^ globalScope
```


We define the method `globalEnvironmentAt:put:` to easily add new globals from our test and make sure that we can 
set the value of a global in the global scope.

```
CInterpreter >> globalEnvironmentAt: aSymbol put: anObject
	globalScope at: aSymbol put: anObject

CGlobalScope >> at: aKey put: aValue
	globalsDictionary at: aKey put: aValue
```

### Introduce scopeDefining: 

We define the methods `scopeDefining:` and a `read:` method in our global scope.

```
CGlobalScope >> scopeDefining: aString
	"I'm the root scope..."
	^ self

CGlobalScope >> read: aString
	^ globalDictionary at: aString
```


### Supporting parentScope 

We add support to represent parent scope to the class `CinstanceScope`. We add the instance variable `parentScope` to the class `CInstanceScope`.

```
Object <<#CInstanceScope
	slots: {#receiver . #parentScope};
	package: 'Champollion'

CInstanceScope >> parentScope: anObject
	parentScope := anObject

CInstanceScope >> parentScope
	^ parentScope
```

We can define the scope chain by defining that an instance scope has (for now) a global scope as parent. 
In this method we see clearly that an instance scope lives in the context of a global one and we defined previously that 
the global scope is the final scope root (see Method `CGlobalScope >> #scopeDefining: above).

```
CInterpreter >> currentScope
	^ CInstanceScope new
		receiver: self receiver;
		parentScope: globalScope;
		yourself
```


We redefine `scopeDefining:` so that if the variable is not defined in the instance variables of the instances, it looks in the parent scope. 

```
CInstanceScope >> scopeDefining: aString
	(self definedVariables includes: aString)
		ifTrue: [ ^ self ].
	
	^ self parentScope scopeDefining: aString
```

Now all the tests should be green.

### Conclusion

In this chapter, we introduced support for variables and name resolution to support instance variables and global variables. 
This first evaluator serves as a basis for implementing the execution of the main operations in Pharo: messages.

The following chapters will start by decomposing message resolution into method-lookup and apply operations, introduce the execution stack, the management of temporary variables and argument, and 'super' message-sends.

We further invite the reader to explore changing the language semantics by modifying this simple evaluator: How could we implement read-only objects? Log all variable reads and writes?
