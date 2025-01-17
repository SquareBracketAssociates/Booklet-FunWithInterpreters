## Primitive Operations
cha:prims


Our interpreter does not handle yet any essential behavior such as basic number operations.  This prevents us from evaluating complex programs.
This chapter introduces the concept of primitive methods as done in Pharo and extends the interpreter to evaluate them. 

We will study how primitive methods work in Pharo, and how they should be properly implemented, including the evaluation of their fallback code (i.e., what happens when a primitive fails).
We will then visit some of the essential primitives we need to make work to execute complex examples.
This opens the door to the evaluation of more interesting programs.

### The need for Primitives in Pharo

We call primitive behavior, behavior that needs to be implemented in the interpreter or evaluator because it cannot be purely expressed in the programming language, Pharo in this case. 

Let's consider, for example, the operation of adding up two numbers (`+`).
We cannot express in a pure and concise way a normal method of executing an addition.

Along with arithmetics, other examples of primitive operations are object allocation and reflective object access.
Such a primitive behavior is expressed as special methods, namely **primitive methods** in Pharo, whose behavior is defined in the virtual machine.


In addition to essential behavior, primitive behavior is often used to implement performance-critical code in a much more efficient way, since primitives are implemented in the implementation language and do not suffer the interpretation overhead.

#### Primitives as messages

Differently, from languages such as Java or C, which express arithmetics as special operators that are compiled/interpreted differently, Pharo maintains the message send metaphor for primitive behavior. 
Indeed, in Pharo,  `+` is a message which triggers a look-up and a method activation. 

This separation makes redefining operators as simple as implementing a method with the selector `+` in our own class, without the need for special syntax for it. The primitive should just be tagged with an id so that the Virtual machine finds its definition and executes it. 



### Primitives in Pharo

In Pharo the design of primitives is split in three different parts: _messages_, _primitive methods_ (the Pharo method that is annotated), and the _primitive_ itself which is provided by the interpreter - in the case of our interpreter this is a method that implements the primitive behavior. In the case of a Virtual Machine, it is a C function. 


###### Primitives invoked as Messages.

The first thing to note is that in Pharo programs, primitive behavior is invoked through standard message sends.
Indeed sending the message ` 1 + 2 ` is handled as a message, but the method `+` on `Integer` is a primitive (during Pharo execution it calls primitive functions transparently from the developer).

This management of primitives as messages allows developers to define operators such as `+` as non-primitives on their own classes, by just implementing methods with the corresponding selectors and without any additional syntax. 

With some terminology abuse we could think of this as "operator redefinition", although it is no such, it is just a standard method re/definition. This operator redefinition feature is useful for example when creating internal Domain Specific Languages (DSLs), or to have polymorphism between integers and floats. 

This is why in Pharo it is possible to define a new method `+` on any class as follows:

```
MyClass >> + anArgument [
  "Redefine +"
  ...
]
```



###### Primitive annotation.

To define primitive behavior, Pharo relies on special methods called _primitive methods_:
Primitive methods are normal Pharo methods with a `primitive` annotation. 
This annotation identifies that the method is special.

For example, let us consider the method `SmallInteger>>+` method below:

```
SmallInteger >> + aNumber [
	<primitive: 1>
	^ super + aNumber
]
```


This method looks like a normal method with selector `+`, and with a normal method body doing `^super + aNumber`.
The only difference between this method and a normal one is that this method also has an annotation, or pragma, indicating that it is the primitive number 1.

The body of the method is normally not executed. In its place, the primitive 1 is executed.
The method body is only executed if the primitive failed.



###### Interpreter primitives.

Before diving into how _primitive methods_ are executed, let us introduce the third component in place: the _interpreter primitives_.
A primitive is a piece of code \(another method\) defined in the interpreter that will be executed when a primitive method is executed. 

To make a parallel between our interpreter and the Pharo virtual machine, the virtual machine is executing a C-function is executed
when a primitive method is executed.

The interpreter defines a set of supported primitives with unique ids.
In our case, for example, the primitive with id `1` implements the behavior that adds up two integers.

When a primitive method is activated, it first looks up what _primitive_ to execute based on its primitive id number, and executes it.
The primitive performs some validations if required, executes the corresponding behavior, and returns either with a success if everything went ok, or a failure if there was a problem.
On success, the method execution returns with the value computed by the primitive. 
On failure, the body of the method is executed instead. Because of this, the body of a primitive method is also named the "fall-back code".

Note that in Pharo some primitives called essential such as the object allocation cannot be executed from Pharo. 
For such primitive, implementors added a method body to describe what the primitive is doing if it could be 
written in Pharo.

### Infrastructure for Primitive Evaluation


To implement primitives in our evaluator we only need to change how methods are activated.
Indeed, as we have seen above, the method lookup nor other special nodes are required for the execution of primitives, and the AST already supports pragma nodes, from which we need to extract the method's primitive id.

We will extend the method evalution in a simple way: During the activation of a primitive method, we need to look for the primitive to execute, and check for failures.
Therefore we need to map primitive ids to primitive methods.

We implement such a mapping using a table with the form `<id, evaluator_selector>`. 
The `primitives` instance variable is initialized to a dictionary as follows: 

```
CInterpreter >> initialize
	super initialize. 
	stack := Stack new.
	primitives := Dictionary new.
	self initializePrimitiveTable.
```


Then we define the method `initializePrimitiveTable` to initialize the mapping between the primitive id and the Pharo method to be executed. 

```
CInterpreter >> initializePrimitiveTable
	primitives at: 1 put: #primitiveSmallIntegerAdd
```



Let's start by setting up our testing scenario: adding up two numbers.
We make sure to work with small enough numbers in this test, to not care about primitive failures yet.
Doing `1 + 5` the primitive should always be a success and return `6`.





```
CInterpretable >> smallintAdd
	^ 1 + 5

CInterpreterTests >> testSmallIntAddPrimitive
	self
		assert: (self executeSelector: #smallintAdd)
		equals: 6
```


### Primitives Implementation


In our first iteration, we will not care about optimizing our evaluator, for which we had already and we will have tons of opportunities.
To have a simple implementation to work on, we execute the primitive after the method's frame creation, in the `visitMethodNode:` method.
This way the primitive has a simple way to access the receiver and the arguments by reading the frame.
We leave primitive failure management for our second iteration.

Upon primitive method execution, we extract the primitive id from the pragma, get the selector of that id from the table, and use the `perform:` method on the interpreter with that selector to execute the primitive.

```
CInterpreter >> executePrimitiveMethod: anAST
	| primitiveNumber |
	primitiveNumber := anAST pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive arguments first value ]
		ifNone: [ self error: 'Not a primitive method' ].
	^ self perform: (primitives at: primitiveNumber)
```

We also need to take care of sending the receiver and arguments of the message to the primitive, so it can manipulate them.

```
CInterpreter >> visitMethodNode: aMethodNode
	aMethodNode isPrimitive ifTrue: [ 
		"Do not handle primitive failures for now"
		^ self executePrimitiveMethod: aMethodNode ].
	^ self visitNode: aMethodNode body
```


We define the primitive `primitiveSmallIntegerAdd` as follows: 

```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument |
	receiver := self receiver.
	argument := self argumentAt: 1.
	^ receiver + argument

CInterpreter >> argumentAt: anInteger
  ^ self tempAt: (self currentMethod arguments at: anInteger) name
```


### Primitive Failures and Fallback Code


Let's now consider what should happen when a primitive fails.
For example, following Pharo's specification, primitive 1 fails when the receiver or the argument are not small integers, or whenever their sum overflows and does not fit into a small integer anymore.

To produce one of such failing cases, we can implement primitive 1 in our `CInterpretable` class, which should fail because the receiver should be a small integer.
 When it fails, the fallback code should execute.

 We define two methods `failingPrimitive` and `callingFailingPrimitive` to support the test of failing primitive.
 
```
CInterpretable >> failingPrimitive
	<primitive: 1>
	^ 'failure'

CInterpretable >> callingFailingPrimitive
	^ self failingPrimitive
```


```
CInterpreterTests >> testFailingPrimitive
	self
		assert: (self executeSelector: #callingFailingPrimitive)
		equals: 'failure'
```


To add primitive failures in a clean way, we introduce them as exceptions.
We define a new subclass of `Exception` named `CHPrimitiveFail`

In the primitive `primitiveSmallIntegerAdd`, if we detect a failure condition, we raise a `CHPrimitiveFail` error.
Note that this the primitive is incomplete since we should also test that the argument and the result is small integer
as shown in the following sections.

```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument |
	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CHPrimitiveFail signal ].
	argument := self argumentAt: 1.
	^ receiver + argument
```

We then need to modify the way we evaluate methods to handle `CPrimitiveFail` exceptions and continue evaluating the body.

```
CInterpreter >>visitMethodNode: aMethodNode
	[ aMethodNode isPrimitive ifTrue: [ 
		"Do not handle primitive failures for now"
		^ self executePrimitiveMethod: aMethodNode ]]
		on: CPrimitiveFail do: [ :err | 
			"Nothing, just continue with the method body" ].
	
	^ self visitNode: aMethodNode body
```


With these changes, everything should work fine now.
  
### Typical Primitive Failure Cases

For primitives to work properly, and for Pharo to be a safe language, primitives should properly do a series of checks.
This is particularly important when the interpreter fully controls all other aspects of the language, such as the memory.
In such cases, primitives, as well as the other parts of the evaluator, have full power over our objects, potentially producing memory corruptions.

Among the basic checks that primitives should do, they should not only verify that arguments are of the primitive's expected type, as we have shown above. 

In addition, a general check is that the primitive was called with the right number of arguments. 

This check is particularly important because developers may wrongly define primitives such as we did before, where we have defined a unary method while the primitive was expecting one argument.
If we don't properly check the arguments trying to access it could cause an interpreter failure, while the proper behavior should be to just fail the primitive and let the fallback code carry on the execution.


In the following sections, we implement a series of essential primitives taking care of typical failure cases. 
With such primitives, it will be possible to execute a large range of Pharo programs.

### Essential Primitives: Arithmetic 

The basic arithmetic primitives are small integer addition, subtraction, multiplication, and division.
They all require a small integer receiver and a small integer argument, and that the result is also a small integer.
Division in addition fails in case the argument is `0`.
The following code snippet illustrates integer addition and division.
For space reasons, we do not include subtraction and multiplication, their implementation is similar to the one of addition.

```
CInterpreter >> initializePrimitiveTable
	...
	primitives at: 1 	  put: #primitiveSmallIntegerAdd.
	primitives at: 2 	  put: #primitiveSmallIntegerMinus.
	primitives at: 9 	  put: #primitiveSmallIntegerMultiply.
	primitives at: 10 	put: #primitiveSmallIntegerDivide.
	...
```


The addition primitive now checks that the receiver, argument, and result are small integers.

```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument result |
	self numberOfArguments < 1
		ifTrue: [ CHPrimitiveFail signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CHPrimitiveFail signal ].

	argument := self argumentAt: 1.
	argument class = SmallInteger
		ifFalse: [ CHPrimitiveFail signal ].

	result := receiver + argument.
	result class = SmallInteger
		ifFalse: [ CHPrimitiveFail signal ].
	^ result
```



```
CInterpreter >> primitiveSmallIntegerDivide
	| receiver argument result |
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFail signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFail signal ].
	
	argument := self argumentAt: 1.
	(argument class = SmallInteger
		and: [ argument ~= 0 ])
			ifFalse: [ CPrimitiveFail signal ].

	result := receiver / argument.
	result class = SmallInteger
		ifFalse: [ CPrimitiveFail signal ].
	^ result
```



### Essential Primitives:  Comparison


Comparison primitives span in two different sets. 
The first set contains the primitives implementing number comparisons such as less than or greater or equals than. 
The second set contains the primitives for object identity comparison: identity equals to and identity not equals to.
All number comparisons all require a small integer receiver, a small integer argument.
Identity comparisons only require that the primitive receives an argument to compare to.
The following code snippet illustrates both kind of methods with small integer less than and object idenity equality.

```
CInterpreter >> initializePrimitiveTable
	...
	primitives at: 3 	put: #primitiveSmallIntegerLessThan.
	primitives at: 4 	put: #primitiveSmallIntegerGreaterThan.
	primitives at: 5 	put: #primitiveSmallIntegerLessOrEqualsThan.
	primitives at: 6 	put: #primitiveSmallIntegerGreaterOrEqualsThan.

	primitives at: 7 	put: #primitiveSmallIntegerEqualsThan.
	primitives at: 8 	put: #primitiveSmallIntegerNotEqualsThan.

	primitives at: 110 	put: #primitiveIdentical.
	primitives at: 111 	put: #primitiveNotIdentical.
	...
```


```
CInterpreter >> primitiveSmallIntegerLessThan
	| receiver argument result |
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFail signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFail signal ].
	
	argument := self argumentAt: 1.
	argument class = SmallInteger
		ifFalse: [ CPrimitiveFail signal ].

	^ receiver < argument
```


```
CInterpreter >> primitiveIdentical
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFail signal ].

	^ self receiver == (self argumentAt: 1)
```


### Essential Primitives:  Array Manipulation


So far our interpreter is able to manipulate only objects with instance variables, but not arrays or their variants e.g., strings.
Arrays are special objects whose state is accessed with primitives, usually in methods named `at:` and `at:put:` and `size`.
Array access primitives check that the receiver is of the right kind and that the index arguments are integers within bounds of the array.
The following code snippet illustrates `Array` access primitives for general Arrays, and Strings.

```
CInterpreter >> initializePrimitiveTable
	...
	primitives at: 60 	put: #primitiveAt.
	primitives at: 61 	put: #primitiveAtPut.
	primitives at: 62 	put: #primitiveSize.
	primitives at: 63 	put: #primitiveStringAt.
	primitives at: 64 	put: #primitiveStringAtPut.
	...
```


The primitive `primitiveSize` verifies that the receiver is an object supporting the notion of size.

```
CInterpreter >> primitiveSize
	self receiver class classLayout isVariable
		ifFalse: [ CPrimitiveFail signal ].

	^ self receiver basicSize
```


The primitive `primitiveAt` verifies that the receiver is an object supporting the notion of size and in addition that the index is an integer in the range of the size of the receiver.

```
CInterpreter >> primitiveAt
	self numberOfArguments < 1
		ifTrue: [ CHPrimitiveFail signal ].

	self receiver class classLayout isVariable
		ifFalse: [ CHPrimitiveFail signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CHPrimitiveFail signal ].
		
	"Bounds check"
	self receiver size < (self argumentAt: 1)
		ifTrue: [ CHPrimitiveFail signal ].
		
	^ self receiver basicAt: (self argumentAt: 1)
```


The primitive `primitiveStringAt` verifies that the receiver is from a class whose elements are bytes.

```
CInterpreter >> primitiveStringAt
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFail signal ].
		
	self receiver class classLayout isBytes
		ifFalse: [ CPrimitiveFail signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFail signal ].
	"Bounds check"
	
	self receiver size < (self argumentAt: 1)
		ifTrue: [ CHPrimitiveFail signal ].
	
	^ self receiver at: (self argumentAt: 1)
```


### Essential Primitives:  Object Allocation


Object allocation is implemented by primitives `new` and `new:`.
The method `new` allocates a new object from a fixed-slot class.
The method `new:` allocates a new object from a variable-slot class such as `Array`, using the number of slots specified as argument. 

Both these primitives validate that the receiver are classes of the specified kinds.
In addition `new:` does check that there is an argument, it is a small integer.

```
CInterpreter >> initializePrimitiveTable
	...
	primitives at: 70 	put: #primitiveBasicNew.
	primitives at: 71 	put: #primitiveBasicNewVariable.
	...
```


```
CInterpreter >> primitiveBasicNew
	self receiver isClass
		ifFalse: [ CPrimitiveFail signal ].
	^ self receiver basicNew
```


```
CInterpreter >> primitiveBasicNewVariable
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFail signal ].

	self receiver isClass
		ifFalse: [ CPrimitiveFail signal ].
	self receiver class classLayout isVariable
		ifFalse: [ CPrimitiveFail signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFail signal ].
	
	^ self receiver basicNew: (self argumentAt: 1)
```


### Conclusion


This chapter presented primitive behavior, implementing behavior that cannot be purely expressed in the evaluated language.
Primitive behavior is accessed through _primitive methods_, which are methods marked with a `primitive:` pragma.
When a primitive method executes, it first executes the primitive behavior associated with the primitive id.
If it fails, the body of the method is executed as in non-primitive methods.

We have then discussed about primitive failures and verification  and presented a short list of essential primitives that are required to execute more interesting Pharo programs.