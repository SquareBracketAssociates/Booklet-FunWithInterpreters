## Scopes and Variables
@cha:scope

Variables are, using a very general definition, storage location associated with a name.
For example, a global variable in Pharo is stored in a global dictionary of key-value associations, where the association's key is the name of the variable and its value is the value of the variable respectively.

Instance variables, on their side, are storage locations inside objects.
Instance variables in Pharo have each a location in the object, specified by an index, and that index corresponds to the index of the variable's name in its class' instance variable list.

In this chapter, we will study how variables are resolved.
Since variable nodes contain only their name, this involves a "name resolution" step: finding where the variable is located and accessing it accordingly following the lexical scoping (or static scoping) rules.
Name resolution using lexical scoping discovers variables using the nesting hierarchy of our code, where each nesting level represents a scope. For example, classes define a scope with variables visible only by their instances; the global scope defines variables that are visible everywhere in the program, and classes are nested within the global scope.

In this chapter, we will implement name resolution by modeling the program scopes.
Scope objects will act as adapters for the real storage locations of variables, giving us a polymorphic way to read and write variables. Moreover, scopes will be organized in a chain of scopes, that will model the lexical organization of the program.




### Introducing Lexical Scopes

Variables in Pharo are of different kinds: instance variables represent variables stored in the evaluated message receiver, temporaries and arguments are variables visible only within a particular method evaluation, shared variables, and global variables are variables that are in the global scope and independent of the receiver.

All these "contexts" defining variables are also named scopes because they define the reach of variable definitions.
At any point during program execution, we can organize scopes in a hierarchy of scopes following a parent relationship.

- When a method evaluates, the current scope is the method scope with all the arguments and temporary variables defined in the method. 
- The parent of a method scope is the instance scope that defines all the variables for an instance. 
- The parent of the instance scope is the global scope that defines all the global variables.

Notice that in this scope organization, we are not explicitly talking about classes.
The instance scope takes care of that: it resolves all instance variables in the hierarchy of the receiver object.
A class scope could be added later to resolve class variables and class pools.

The basic structure of our lexical scope implementation introduces the method `scopeDefining:` in our interpreter.
This method returns the scope defining the given name. The method `scopeDefining:` forwards the search to the current scope, obtained through `currentScope`. 
Finally, we will extend our interpreter with `visitAssignmentNode:` to handle variable reads and writes. Reads (and writes) obtain the scope for the read (written) variable and delegate the read (write) to it. This means our scope objects need to define methods `scopeDefining:` and `read:` and `write:withValue:`.

```
CInterpreter >> scopeDefining: aName
	^ self currentScope scopeDefining: aName

CInterpreter >> currentScope
	^ 


CHInterpreter >> visitVariableNode: aVariableNode
	^ (self scopeDefining: aVariableNode name) read: aVariableNode name

CHInterpreter >> visitAssignmentNode: anAssignmentNode
	| value |
	value := self visitNode: anAssignmentNode value.
	(self scopeDefining: anAssignmentNode variable name)
    write: anAssignmentNode variable name
    withValue: value.
	^ value
```



### Evaluating Variables: Instance Variable Reads


The first step is to support instance variable reads.
Since such variables are evaluated in the context of the receiver object, we need a receiver object that has instance variables. 
We already had such a class: `CHInterpretable`.
We then add an instance variable and a getter and setter for it to be able to control the values in that instance variable.

```
Object << #CInterpretable
  slots: { #x };
  package: 'Champollion-Tests'

CInterpretable >> returnX
	^ x

CInterpretable >> x: anInteger
	x := anInteger
```


To test the correct evaluation of the instance variable read, we check that the getter returns the value in the instance variable, which we can previously set.

```
CHInterpreterTest >> testReturnInstanceVariableRead
	| receiver |
	receiver := CInterpretable new.
	receiver x: 100.
	self
		assert: (self executeSelector: #returnX withReceiver: receiver)
		equals: 100
```


Our test is failing so we are ready to make it work.

#### Creating an Instance Scope


To make our tests go green, we need to implement instance scopes and the `CHInterpreter>>currentScope` method.
We will model instance scopes with a new class. This class will know the receiver object, extract the list of instance variables from it, and know how to read and write from/to it.

```
Object <<#CInstanceScope
	slots: {#receiver};
	package: 'Champollion-Core'

CInstanceScope >> receiver: anObject
  	receiver := anObject

CInstanceScope >> scopeDefining: aString
	(self definedVariables includes: aString)
		ifTrue: [ ^ self ].
	
	^ self parentScope scopeDefining: aString

CInstanceScope >> definedVariables
	^ receiver class allInstVarNames 


CInstanceScope >> read: aString 
	^ receiver instVarNamed: aString
```


We then can define our `currentScope` method as follows.

```
CInterpreter >> currentScope
  	^ CInstanceScope new
  		receiver: self receiver;
  		yourself
```

All our tests should now pass!



### Refactor: Improving our Tests `setUp`


Let's simplify how receivers are managed in tests.
Instead of having to explicitly create and manage a receiver object, we add the receiver as an instance variable to the `CHInterpreterTest`.

```
TestCase << #CInterpreterTest
	slots: { #interpreter . #receiver};
	package: 'Champollion-Tests'
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
CHInterpreterTest >> testReturnSelf
  "Convey our intention of checking identity by using an explicit identity check"
  self assert: (self
    executeSelector: #returnSelf
    withReceiver: receiver)
        == receiver

CHInterpreterTest >> testReturnSuper
  "Convey our intention of checking identity by using an explicit identity check"
  self assert: (self
    executeSelector: #returnSelf
    withReceiver: receiver)
        == receiver

CInterpreterTest >> testReturnInstanceVariableRead
	receiver x: 100.
	self
		assert: (self executeSelector: #returnX)
		equals: 100
```






### Instance Variable Writes


Now that we have support for instance variable reads, we keep on going with instance variable writes.
In our scenario, a method writes a literal integer into an instance variable `x` and then returns the value of the assignment. This scenario has two observable behaviors that we will be tested separately.
First, such an assignment should be observable from the outside by reading that variable.
Second, an assignment is an expression whose value is the assigned value, thus the return value should also be the assigned value.

```
CHInterpretable >> store100IntoX
	^ x := 100
```


To test this behavior, we evaluate the method above and then we validate that effectively the instance variable was mutated.
To make sure the value was modified, we set an initial value to the variable before the evaluation.
After the evaluation we should not keep that value.
The case of the return is similar to our previous tests.

```
CInterpreterTest >> testStore100IntoX
	receiver x: 17.
	self executeSelector: #store100IntoX.
	self assert: receiver x equals: 100

CInterpreterTest >> testAssignmentReturnsAssignedValue
	self
		assert: (self executeSelector: #store100IntoX)
		equals: 100
```

And finally, we implement a getter for the `x` variable.
This getter is used to extract the value in the tests.

```
CInterpretable >> x
	^ x
```


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


Evaluating an assignment node with the form `variable := expression` requires that we evaluate the `expression` of the assignment, also called the right hand side of the assignment, and then set that value to the left-side variable.
Notice that the variable we are assigning into does not need to be evaluated/visited: the evaluation of a variable is equivalent to reading its value.
And in the left part of an assignment, we do not care about the value of the variable, but about the location of the variable.
Our visit method then looks as follows: we recursively visit the right-side of the assignment \(got through the `value` accessor\), we set that value to the variable by delegating to the scope, and we finally return the stored value.
To set the value to the variable, we implement `write:withValue:` in our instance scope.

```
CInstanceScope >> write: aString withValue: anInteger
	receiver instVarNamed: aString put: anInteger
```

Now the tests should pass.


### Evaluating Variables: Global Reads


We finish this chapter with the reading of global variables, which covers two cases: proper global variables and access to classes.
To better control our testing environment, we decided to not use the Pharo environment by default.
Instead, the interpreter will know its global scope in an instance variable and look up globals in it using a simple API, making it possible to use the system's global environment instead if we wanted to.
We also leave outside of the scope of this chapter the writing to global variables.
The case of global variable writing is similar to the instance variable assignment.

Our first testing scenario, similar to the previous ones, is as follows: we define a method `returnGlobal` reads and returns the global named `Global`.
When defining such a method in Pharo, the IDE will ask what `Global` is, because such a variable does not exist: select to create it as a global variable.
Such a detail is only necessary to keep Pharo itself happy and it does not have any impact in our implementation.
Remember that we are writing our own Pharo implementation in the interpreter, and we will decide what to do with `Global` ourselves.

```
CInterpretable >> returnGlobal
	^ Global
```


We define a test which specifies that the interpreter' environment has a binding whose key is `#Global` and value is a new object.

```
CInterpreterTest >> testReturnGlobal
	| globalObject |
	globalObject := Object new.
	interpreter globalEnvironmentAt: #Global put: globalObject.
	self assert: (self executeSelector: #returnGlobal) equals: globalObject
```


We introduce a new global scope class `CGlobalScope`, and an instance variable named `globalScope` in the class `CInterpreter` initialized to a global scope.

```
Object << #CHGlobalScope
	slots: { #globalsDictionary };
	package: 'Champollion'

CGlobalScope >> initialize
	super initialize.
	globalsDictionary := Dictionary new

CGlobalScope >> globalsDictionary: anObject 
	globalsDictionary := anObject

CGlobalScope >> at: aKey ifAbsent: aBlock
  ^ globalsDictionary at: aKey ifAbsent: aBlock
```

```
Object << #CInterpreter
	slots: { #globalScope};
	package: 'Champollion-Core'

CInterpreter >> initialize
	super initialize.
	globalScope := CGlobalScope new

CInterpreter >> globalEnvironment
	^ globalScope
```


We define the method `globalEnvironmentAt:put:` to easily add new globals from our test.

```
CInterpreter >> globalEnvironmentAt: aSymbol put: anObject
	globalScope at: aSymbol put: anObject

CGlobalScope >> at: aKey put: aValue
	globalsDictionary at: aKey put: aValue
```


We decided that an access to a global that is not defined will raise an error in the interpreter and halt the interpretation.
An alternative design would return a default value instead such as `nil`.
A more complex design would have been to throw an exception in the interpreted program.
For the moment, halting the interpreter with an error suffices for our job.

```
CInterpreter >> visitGlobalVariableNode: aRBGlobalNode
	^ self globalEnvironment
      at: aRBGlobalNode name
      ifAbsent: [ self error: aRBGlobalNode name, ' not found' ]
```


Finally, we need to chain the global scope to the instance scope, define a `scopeDefining:` and a `read:` method in our global scope.

```
CInterpreter >> currentScope
	^ CInstanceScope new
		receiver: self receiver;
		parentScope: globalScope;
		yourself

CInstanceScope >> parentScope: anObject
	parentScope := anObject


CInstanceScope >> parentScope
	^ parentScope

CGlobalScope >> scopeDefining: aString
	"I'm the root scope..."
	^ self

CGlobalScope >> read: aString
	^ globalsDictionary at: aString
```


### Conclusion

In this chapter, we introduced support for variables and name resolution to support instance variables and global variables. 
This first evaluator serves as a basis for implementing the execution of the main operations in Pharo: messages.
The following chapters will start by decomposing message resolution into method-lookup and apply operations, introduce the execution stack, the management of temporary variables and argument, and 'super' message-sends.

We further invite the reader to explore changing the language semantics by modifying this simple evaluator: How could we implement read-only objects? Log all variable reads and writes?
