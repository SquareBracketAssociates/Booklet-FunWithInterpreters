## Primitive Operations
@cha:prims


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


### Pharo Primitive Study

Differently, from languages such as Java or C, which express arithmetics as special operators that are compiled/interpreted differently, Pharo maintains the message send metaphor for primitive behavior. 
Indeed, in Pharo,  `+` is a message that triggers a method look-up and a method  activation. 

This separation makes redefining operators as simple as implementing a method with the selector `+` in our own class, without the need for special syntax for it. The primitive should just be tagged with an id so that the Virtual machine finds its definition and executes it. 

In Pharo the design of primitives is split in three different parts: _messages_, _primitive methods_ (the Pharo method that is annotated), and the _primitive_ itself which is provided by the interpreter - in the case of our interpreter this is a method that implements the primitive behavior. In the case of a Virtual Machine, it is a C function. 


###Study: Primitives invoked as Messages

The first thing to note is that in Pharo programs, primitive behavior is invoked through standard message sends.
Indeed sending the message ` 1 + 2 ` is handled as a message, but the method `+` on `Integer` is a primitive (during Pharo execution it calls primitive functions transparently from the developer).

This management of primitives as messages allows developers to define operators such as `+` as non-primitives on their own classes, by just implementing methods with the corresponding selectors and without any additional syntax. 

With some terminology abuse we could think of this as "operator redefinition", although it is no such, it is just a standard method re/definition. This operator redefinition feature is useful for example when creating internal Domain Specific Languages (DSLs), or to have polymorphism between integers and floats. 

This is why in Pharo it is possible to define a new method `+` on any class as follows:

```
MyClass >> + anArgument
  "Redefine +"
  ...
```

###Study: Primitive annotation

To define primitive behavior, Pharo relies on special methods called _primitive methods_:
Primitive methods are normal Pharo methods with a `primitive` annotation. 
This annotation identifies that the method is special.

For example, let us consider the method `SmallInteger>>+` method below:

```
SmallInteger >> + aNumber
	<primitive: 1>
	^ super + aNumber
```


This method looks like a normal method with selector `+` and with a normal method body doing `^ super + aNumber`.
The only difference between this method and a normal one is that this method also has an annotation, or pragma, indicating that it is the primitive number 1.





### Study: Interpreter primitives

Before diving into how _primitive methods_ are executed, let us introduce the third component in place: the _interpreter primitives_.
A primitive is a piece of code (another method) defined in the interpreter that will be executed when a primitive method is executed. 

To make a parallel between our interpreter and the Pharo virtual machine: the virtual machine executes a C-function when a primitive method is executed.

The virtual machine interpreter defines a set of supported primitives with unique ids. We will mimic this behavior and in our interpreter, the primitive with id `1` implements the behavior that adds up two integers.


When a primitive method is activated, 
the body of the method is normally not executed. Instead the primitive 1 is executed.
The method body is only executed, if the primitive failed.

More concretely, when a primitive method is activated, 

- it first looks up what _primitive_ to execute based on its primitive id number, and executes it.
- The primitive performs some validations if required, executes the corresponding behavior, and returns either with a success if everything went ok, or a failure if there was a problem.
- On success, the method execution returns with the value computed by the primitive. 
- On failure, the body of the method is executed instead. Because of this, the body of a primitive method is also named the "fall-back code".

Note that in Pharo some primitives called essential such as the object allocation cannot be executed from Pharo. 
For such primitive, implementors added a method body to describe what the primitive is doing if it could be written in Pharo.

### Infrastructure for Primitive Evaluation

To implement primitives in our evaluator we only need to change how methods are activated.
Indeed, as we have seen above, neither a special method lookup nor dedicated nodes are required for the execution of primitives, and the AST already supports pragma nodes, from which we need to extract the method's primitive id.


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


### Primitive Table Addition

We extend the method evaluation in a simple way: During the activation of a primitive method, we need to look for the primitive to execute and check for failures.
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


We define the primitive `primitiveSmallIntegerAdd` as follows: 

```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument |
	receiver := self receiver.
	argument := self argumentAt: 1.
	^ receiver + argument
```

We introduce a way to access the value of an argument with the method `argumentAt:`.

```
CInterpreter >> argumentAt: anInteger
	^ self tempAt: (self currentMethod arguments at: anInteger) name
```



### Primitive Implementation

In the first iteration, we do not care about optimizing our evaluator (for which we had already and we will have tons of opportunities).
To have a simple implementation to work on, we execute the primitive after the method's frame creation, in the `visitMethodNode:` method.
This way the primitive has a simple way to access the receiver and the arguments by reading the frame.
We leave primitive failure management for the second iteration.

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

In addition, we specialize `visitMethodNode:` so that it executes primitives when needed.
At this stage, we do not support primitive failures.

```
CInterpreter >> visitMethodNode: aMethodNode
	aMethodNode isPrimitive ifTrue: [ 
		"Do not handle primitive failures for now"
		^ self executePrimitiveMethod: aMethodNode ].
	^ self visitNode: aMethodNode body
```

Our new test pass.


### Primitive Failure Preparation

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

We define a test to check that on failure we get the result returned by the failing primitive.

```
CInterpreterTest >> testFailingPrimitive
	self
		assert: (self executeSelector: #callingFailingPrimitive)
		equals: 'failure'
```

### Primitive Failure Implementation

To add primitive failures cleanly, we introduce them as exceptions.

We define a new subclass of `Exception` named `CPrimitiveFailed`

In the primitive `primitiveSmallIntegerAdd`, if we detect a failure condition, we raise a `CPrimitiveFailed` error.
Note that this primitive implementation is incomplete since we should also test that the argument and the result are small integers
as shown in the following sections.


```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument |
	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].
	argument := self argumentAt: 1.
	^ receiver + argument
```

We then need to modify the way we evaluate methods to handle `CPrimitiveFailed` exceptions and continue evaluating the body.

```
CInterpreter >> visitMethodNode: aMethodNode

	[ aMethodNode isPrimitive ifTrue: [ 
		^ self executePrimitiveMethod: aMethodNode ]]
		on: CPrimitiveFailed do: [ :err | 
			"Nothing, just continue with the method body" ].
	
	^ self visitNode: aMethodNode body
```


With these changes, everything should work fine now.
  
### Typical Primitive Failure Cases

For primitives to work properly, and for Pharo to be a safe language, primitives should properly do a series of checks.
This is particularly important when the interpreter fully controls all other aspects of the language, such as the memory.
In such cases, primitives, as well as the other parts of the evaluator, have full power over our objects, potentially producing memory corruption.

Among the basic checks that primitives should do, they should not only verify that arguments are of the primitive's expected type, as we have shown above. 
In addition, a general check is that the primitive was called with the right number of arguments. 

This check is particularly important because developers may wrongly define primitives.
If we don't properly check the arguments trying to access them could cause an interpreter failure, while the proper behavior should be to just fail the primitive and let the fallback code carry on the execution.

In the following sections, we implement a series of essential primitives taking care of typical failure cases. 
With such primitives, it will be possible to execute a large range of Pharo programs.

We define the method `numberOfArguments` as follows: 

```
CInterpreter >> numberOfArguments
	
	^ self currentMethod numArgs
```

We define a better implementation of the addition primitive with checks: 
We verify that the receiver, argument, and result are small integers.

```
CInterpreter >> primitiveSmallIntegerAdd
	| receiver argument result |
	self numberOfArguments = 1
		ifFalse: [ CPrimitiveFailed signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].

	argument := self argumentAt: 1.
	argument class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].

	result := receiver + argument.
	result class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].
	^ result
```


### Stepping 

The `visitMethodNode:` definition above can be confusing let us take a moment to analyse it again. 
To be clear we need to see the three pieces together: the first definition, the _interpreter_ primitive implementation and the _Pharo_ primitive implementation.

```
CInterpreter >> visitMethodNode: aMethodNode

	[ aMethodNode isPrimitive ifTrue: [ 
		^ self executePrimitiveMethod: aMethodNode ]]
		on: CPrimitiveFailed do: [ :err | 
			"Nothing, just continue with the method body" ].
	
	^ self visitNode: aMethodNode body
```

```
CInterpreter >> primitiveSmallIntegerAdd
     ...
```

```
SmallInteger >> + aNumber
	<primitive: 1>
	^ super + aNumber
```     

The argument of the `visitMethodNode:` is a Pharo method. When such a method is a _Pharo_ primitive (i.e., it has the pragma annotation)
the condition is basically doing a special interpretation: it executes the corresponding _interpreter_ method primitive that we implemented inside the 
class `CInterpreter`. When such an execution raises a `CPrimitiveFailed` exception, the interpretation continues with the interpretation of the _Pharo_ primitive fallback code. 

What we see is that there is a go and back between the _Pharo_ primitive and the definition of the corresponding primitive
in the interpreter. 
First the _Pharo_ primitive pragma is used to identify the _interpreter_ primitive, then the interpreter executes the interpreter primitive. 
This is only on failure of the such a logic (which in a runtime is implemented in C for example) that the interpreter should execute
the fallback code of _Pharo_ primitive.

At the moment since the interpreter does not support conditional and block execution, we cannot execute the primitive fallback code. 



### Conclusion

In this chapter we showed how primitive behavior is evaluated. 
In particular we showed how fallback code is executed in case the primitive fails. 
In the following chapter we will describe essential primitives. 
With such implementation we will be able to execute more realistic programs.








## Essential Primitives

In this chapter, we will focus on implementing more primitives.
We will implement more mathematical primitives but also support for comparisons, and 
array allocation. 



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
	primitives at: 10  put: #primitiveSmallIntegerDivide.
	...
```



Here is the definition of the primitive for integer division.

```
CInterpreter >> primitiveSmallIntegerDivide
	| receiver argument result |
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFailed signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].
	
	argument := self argumentAt: 1.
	(argument class = SmallInteger
		and: [ argument ~= 0 ])
			ifFalse: [ CPrimitiveFailed signal ].

	result := receiver / argument.
	result class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].
	^ result
```

We let you define integer subtraction and multiplication. 




### Essential Primitives:  Comparison

Comparison primitives span in two different sets. 
- The first set contains the primitives implementing number comparisons such as less than or greater or equals than. 
- The second set contains the primitives for object identity comparison: identity equals to and identity not equals to.

All number comparisons all require a small integer receiver, a small integer argument.
Identity comparisons only require that the primitive receives an argument to compare to.

The following definitions illustrate the two kinds of methods with small integer less than and object idenity equality.

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

The following primitives following the same patterns and are self-explanatory.  

```
CInterpreter >> primitiveSmallIntegerLessThan
	| receiver argument result |
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFailed signal ].

	receiver := self receiver.
	receiver class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].
	
	argument := self argumentAt: 1.
	argument class = SmallInteger
		ifFalse: [ CPrimitiveFailed signal ].

	^ receiver < argument
```

Following the definition of `primitiveSmallIntegerLessThan`, implement the following primitives
- `primitiveSmallIntegerGreaterThan`,
- `primitiveSmallIntegerLessOrEqualsThan`, and 
- `primitiveSmallIntegerGreaterOrEqualsThan`.



```
CInterpreter >> primitiveIdentical
	self numberOfArguments < 1
		ifTrue: [ CPrimitiveFailed signal ].

	^ self receiver == (self argumentAt: 1)
```

Define some tests to ensure that your implementation is correct.



### Essential Primitives:  Array Manipulation

So far our interpreter is able to manipulate only objects with instance variables, but not objects with variable size such as arrays or their variants e.g., strings.
Arrays are special objects whose state is accessed with primitives, usually in methods named `at:`, `at:put:`, and `size`.
Array access primitives check that the receiver is of the right kind and that the index arguments are integers within the bounds of the array.
The following definition illustrates `Array` access primitives for general Arrays, and Strings.

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


The primitive `primitiveAt` verifies that the receiver is an object supporting the notion of size and in addition that the index is an integer in the range of the size of the receiver.


```
CInterpreter >> primitiveAt

	self numberOfArguments = 1
		ifFalse: [ CPrimitiveFailed signal ].

	self receiver class classLayout isVariable
		ifFalse: [ CPrimitiveFailed signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFailed signal ].
		
	"Bounds check"
	((self argumentAt: 1) between: 1 and: self receiver size)
		ifFalse: [ CPrimitiveFailed signal ].
		
	^ self receiver basicAt: (self argumentAt: 1)
```

Here is a simple test verifying that the implementation is correct.


```
CInterpretable >> at

	^ #(11 22 33) at: 2
```

```
CInterpreterTest >> testAt
	self assert: (self executeSelector: #at) equals: 22
```	

Note that at this point, since the interpreter is not able to execute conditional and blocks, it cannot interpret 
failure of the primitive at: because it contains conditionals. 









The primitive `primitiveStringAt` verifies that the receiver is from a class whose elements are bytes.
It uses the class format information using the method `isBytes`.


```
CInterpreter >> primitiveStringAt
	self numberOfArguments = 1
		ifFalse: [ CPrimitiveFailed signal ].
		
	self receiver class classLayout isBytes
		ifFalse: [ CPrimitiveFailed signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFailed signal ].
	"Bounds check"
	
	((self argumentAt: 1) between: 1 and: self receiver size)
		ifFalse: [ CPrimitiveFailed signal ].
	
	^ self receiver at: (self argumentAt: 1)
```




The primitive `primitiveSize` verifies that the receiver is an object that has the notion of size. It uses the layout of the class to do so.

```
CInterpreter >> primitiveSize
	self receiver class classLayout isVariable
		ifFalse: [ CPrimitiveFailed signal ].

	^ self receiver basicSize
```

We define a simple test as follows:


```
CInterpretable >> atSize

	^ #(11 22 33)
```

```
CInterpreterTest >> testAtSize
	self assert: (self executeSelector: #atSize) equals: 3
```


Now we implement the primitive that supports the modification of arrays.


```
CInterpreter >> primitiveAtPut

	self numberOfArguments = 2
		ifFalse: [ CPrimitiveFailed signal ].

	self receiver class classLayout isVariable
		ifFalse: [ CPrimitiveFailed signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFailed signal ].
		
	"Bounds check"
	((self argumentAt: 1) between: 1 and: self receiver size)
		ifFalse: [ CPrimitiveFailed signal ].
		
	^ self receiver basicAt: (self argumentAt: 1) put: (self argumentAt: 2)
```

We test it as follows:

```
CInterpretable >> atPut
	| ar |
	ar := #(11 22 33).
	ar at: 2 put: 44.
	^ ar
```

```
CInterpreterTest >> testAtPut
	self assert: (self executeSelector: #atPut) equals: #(11 44 33)
```




### Essential Primitives:  Object Allocation

We will implement important primitives: the primitives for object allocation.
Object allocation is implemented by primitives `new` and `new:`.

- The method `new` allocates a new object from a fixed-slot class.
- The method `new:` allocates a new object from a variable-slot class such as `Array`, using the number of slots specified as argument. 

Both these primitives validate that the receiver are classes of the specified kinds.
In addition `new:` does check that there is an argument, it is a positive small integer.

```
CInterpreter >> initializePrimitiveTable
	...
	primitives at: 70 	put: #primitiveBasicNew.
	primitives at: 71 	put: #primitiveBasicNewVariable.
	...
```


To interpret basic new we rely on the one of Pharo because our interpreter does not its own memory management. 

```
CInterpreter >> primitiveBasicNew
	self receiver isClass
		ifFalse: [ CPrimitiveFailed signal ].
	^ self receiver basicNew
```

We test it as follows:


```
CInterpretable >> newPoint

	^ Point basicNew  
```

Here we make sure that the Pharo Point class is accessible in the global scope of the interpreter.

```
CInterpreterTest >> testPointNew
	
	| p |
	self interpreter globalEnvironmentAt: #Point put: Point.
	p := (self executeSelector: #newPoint).
	self
		assert: p class 
		equals: Point.
	 
```

For `basicNew:` we follow the same approach as before. We validate that the class is a class supporting variable number of instance variables.

```
CInterpreter >> primitiveBasicNewVariable
	self numberOfArguments = 1
		ifFalse: [ CPrimitiveFailed signal ].

	self receiver isClass
		ifFalse: [ CPrimitiveFailed signal ].
	self receiver classLayout isVariable
		ifFalse: [ CPrimitiveFailed signal ].
	
	((self argumentAt: 1) isKindOf: SmallInteger)
		ifFalse: [ CPrimitiveFailed signal ].
	
	^ self receiver basicNew: (self argumentAt: 1)
```

The following test verifies that the primitive is working. We also expose Array as a global variable of the interpreter.


```
CInterpreterTest >> testArrayNew
	
	| p |
	self interpreter globalEnvironmentAt: #Array put: Array.
	p := (self executeSelector: #newArray).
	self
		assert: p class 
		equals: Array.
	self assert: p size equals: 4
```	 

```
CInterpretable >> newArray

	^ Array basicNew: 4  
```


### Conclusion

This chapter shows the implementation of multiple primitives behavior. 
The short list of essential primitives we presented are required to execute more interesting Pharo programs.