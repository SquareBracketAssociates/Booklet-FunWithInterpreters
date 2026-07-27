## Late Binding and Method Lookup
@cha:lookup

Method lookup deserves a chapter on its own: it represents the core internal logic of late-binding. 
The method-lookup algorithm needs to support normal message-sends as well as `super` message-sends.  
In this chapter, we will implement method lookup for messages sent to an object. 
Then we will present how we handle the case of messages sent to `super`.

So far we have concentrated on method evaluation and put aside method lookup.
Our current solution fetches methods from the class of the receiver, without supporting inheritance.
In this section, we address this problem and implement a proper method lookup algorithm.

### Method Lookup Introduction

![Sending a message is a two-step process: method lookup and execution. % width=48&label=fig:ToSteps](figures/InheritanceDiagram-sendingMessage.pdf)

Sending a message is a two-step process as shown by Figure *@fig:ToSteps@*:
1. Method lookup: the method corresponding to the selector is looked up in the class of the receiver and its superclasses.
1. Method execution: the method is applied to the receiver. This means that `self` or `this` in the method is bound to the receiver.

Conceptually, sending a message can be described by the following function composition:

```
sending a message (receiver argument)
	 return apply (lookup (selector classof(receiver) receiver) receiver arguments)
```


#### Method lookup

Now the lookup process is conceptually defined as follows:
1. The lookup starts in the **class** of the **receiver**.
1. If the method is defined in that class (i.e., if the method is defined in the method dictionary), it is returned.
1. Otherwise the search continues in the superclass of the currently explored class (as shown in Figure *@fig:LookupNoError@*).
1. If no method is found and there is no superclass to explore (i.e., if we are in the class `Object`), this is an error (i.e., the method is not defined).

![Looking for a method is a two-step process: first, go to the class of receiver then follow inheritance. % width=50&label=fig:LookupNoError](figures/Ref-LookupNoError.pdf)

The method lookup walks through the inheritance graph one class at a time using the superclass relationship. Here is a possible description of the lookup algorithm that will be used for both instance and class methods.

```
lookup (selector class receiver):
   if the method is found in class
      then return it
      else if class == Object
           then Error
           else lookup (selector superclass(class) receiver)
```

Let us implement method lookup. 


### Method Lookup Test Context

To implement and test the method lookup, we should extend our scenario classes with a class hierarchy as shown in Figure *@fighierarchy@*.

We introduce two superclasses above `CInterpretable`: `CInterpretableRoot` and its subclass `CInterpretableSuperclass`.
With this setup, we can test all interesting situations, even the ones leading to infinite loops. This can happen
if our method lookup is wrongly implemented.

![A simple hierarchy for self-send lookup testing. % width=70&anchor=fighierarchy](figures/SimpleHierarchy.pdf)

```
Object << #CInterpretableRoot
	package: 'Champollion'
```


```
CInterpretableRoot << #CInterpretableSuperclass
	package: 'Champollion'
```

```
CInterpretableSuperclass << #CInterpretable
	slots: { #x . #collaborator .  #currentPeanoNumber . #evaluationOrder };
	package: 'Champollion'
```

### A First Test

Our first scenario for method lookup checks that sending a message climbs up the inheritance tree when a method is not found in the receiver's class. 

In the code below, we define a method in class `CInterpretable` that does a `self` message whose method is implemented in its superclass (`CInterpretableSuperclass`). 
Executing the first method should send the message, find the superclass method, and evaluate it.

```
CInterpretableSuperclass >> methodInSuperclass
	^ 5

CInterpretable >> sendMessageInSuperclass
	^ self methodInSuperclass

CInterpreterTest >> testLookupMessageInSuperclass
	self assert: (self executeSelector: #sendMessageInSuperclass) equals: 5
```

The test should fail with the current state of our evaluator as the evaluation of the message will not find the method in the receiver's class. 

### Refactoring the Terrain 

A first step is to refactor the method `visitMessageNode:` and extract the wrong code into the method named: 
 `lookup:fromClass:`.
We also take the opportunity to extract the management of arguments into the method `handleArgumentsOf:`.

```
CInterpreter >> handleArgumentsOf: aMessageNode 
	^ aMessageNode arguments collect: [ :each | self visitNode: each ]
```

```
CInterpreter >> lookup: aSelector fromClass: aClass
	^ aClass compiledMethodAt: aSelector
```

````	
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method args | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode.
	method := self lookup: aMessageNode selector fromClass: newReceiver class.
	^ self execute: (self astOf: method) withReceiver: newReceiver andArguments: args
```

 ### A First Lookup
 
The method `lookup:fromClass:` is now the place to implement the method lookup algorithm:
- if the current class defines the method, it returns the corresponding compiled method;
- if the current class does not define the method and we are not on the top of the hierarchy, we recursively lookup in the class' superclass;
- else when we are on top of the hierarchy, the `lookup:fromClass:` returns nil to indicate that no method was found.


```
CInterpreter >> lookup: aSelector fromClass: aClass
	"Return a compiled method or nil if none is found"
	
	"If the class defines a method for the selector, returns the method"
	(aClass includesSelector: aSelector)
		ifTrue: [ ^ aClass compiledMethodAt: aSelector ].
	"Otherwise lookup recursively in the superclass.
	If we reach the end of the hierarchy return nil"
	^ aClass superclass
			ifNil: [ nil ]
			ifNotNil: [ self lookup: aSelector fromClass: aClass superclass ]
```

The method `lookup:fromClass:` does not raise an error because this way the `visitMessageNode:` method will be able to send the `doesNotUnderstand:` message to the receiver, as we will see later in this chapter.

Our tests should pass.



### The Case of Super

Many people get confused by the semantics of `super`. The `super` pseudo variable has two different roles in the execution of an object-oriented language. 

- When the `super` variable is read, its value is the _receiver_ of the message as we saw it in the first chapter, it has the same value as `self`.

- The second role of the `super` variable is to alter the method lookup when `super` is used as the receiver of the message send. When `super` is used as the receiver of a message send, the method lookup does _not_ start from the class of the receiver, but from the class where the method is installed instead, allowing it to go up higher and higher in the hierarchy.



### Test for `super` semantics

Let us introduce a new scenario for our tests.
We define two methods named `isInSuperclass` and 
a method `doesSuperLookupFromSuperclass` as shown below (See Figure *@fighierarchySuper@*).

It is not nice since it uses `super` when it is unnecessary, but this is for a good cause. 
The handling of overridden messages will present better tests.
 
```
CInterpretableSuperclass >> isInSuperclass
	^ true
```

```
CInterpretable >> isInSuperclass
	^ false
```

```
CInterpretable >> doesSuperLookupFromSuperclass
	^ super isInSuperclass
```

![A simple hierarchy for super-send lookup testing. %width=80&anchor=fighierarchySuper](figures/HierarchyForSuper.pdf)

Once these methods are defined, we can test that the `isInSuperclass` message activates the method in the superclass, returning `true`.

```
CInterpreterTest >> testLookupSuperMessage
	self assert: (self executeSelector: #doesSuperLookupFromSuperclass)
```

This test should fail.

### Handling `super` Semantics

The `super` variable changes the method lookup described previously.
When the receiver is `super`, the lookup does not start from the class of the receiver, but from _the superclass of the class defining the method_.

This implies that we need a way to access the method that is being currently executed, and the class where it is defined.
Once we can access this method we will be able to find its class and from this class its superclass: the place where we should start lookup method when the receiver is `super`.


We can store this information in the current frame during the method's activation.
We add it for now as a fake temporary variable in the frame, with the name `___method`.
By prefixing the variable's name with `___`, we make it less probable this fake variable creates a conflict with a real variable. 
If we would have just named it e.g., `method`, any method with a normal normal temporary called `method` would be broken.


HERE: self tempAt: \#self put: anObject.?
```
CInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection
	| result |
	self pushNewMethodFrame.
	self tempAt: #___method put: anAST.
	self tempAt: #self put: anObject.
	self topFrame parentScope: (CInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself).
	self topFrame receiver: anObject.
	self manageArgumentsTemps: aCollection of: anAST.
	result := self visitNode: anAST.
	self popFrame.
	^ result
```

We also define a convenience accessor method `currentMethod`, to get the current method stored in the current frame as well as the `tempAt:` method.

In the future, if we want to change this implementation, we will have less places to change if we hide the access to the method behind an accessor.

```
CInterpreter >> tempAt: aSymbol
	^ self topFrame at: aSymbol
```

```
CInterpreter >> currentMethod
	^ self tempAt: #___method
```


Note that using the current frame to store the current method will work, even if we have several messages in sequence. 
When a message is sent a new frame is pushed with a new method, and on return the frame is popped along with its method. 
So the top frame always contains the method it executes.


Finally, we redefine the `visitMessageNode:` method to change the class where to start looking for the method e.g., in the superclass of the class defining the method for `super` receivers (`self currentMethod methodClass superclass`).

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method args lookupClass | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode.
	lookupClass := aMessageNode isSuperSend 
		ifTrue: [ self currentMethod methodClass superclass ] 
		ifFalse: [ newReceiver class ].
	method := self 
		lookup: aMessageNode selector 
		fromClass: lookupClass.	
	^ self execute: (self astOf: method) withReceiver: newReceiver andArguments: args
```

We are getting closer and closer to get the test pass.
The test still fails because it uses an old method `executeSelector:withReceiver:` that was defined as 

```
CInterpreterTest >> testLookupSuperMessage
	self assert: (self executeSelector: #doesSuperLookupFromSuperclass)
```

### Revisiting Old Logic

The method `executeSelector:withReceiver:` was quite basic and does not support lookup logic.

The test `testLookupSuperMessageNotInReceiverSuperclass` does not pass because it fails 
before being able to execute the method. 


In particular the ast did not know its methodClass. We address this to make sure that the test passes. 

```
executeSelector: aSymbol withReceiver: aReceiver
	| ast |
	ast := OCParser parseMethod: (CInterpretable >> aSymbol) sourceCode.
	ast methodClass: CInterpretable.
	^ self interpreter execute: ast withReceiver: aReceiver
```

With this last change, your tests should now all pass.

This change is not satisfactory because it hardcodes the  and we should do better.
The method `executeSelector:withReceiver:` makes the strong assumption that the executed method is defined in the class `CInterpretable` and this clearly not always the case. So it is not working for any method defined in another class.

### Introduction of `send:`

To address this limit, we introduce the following method in the interpreter: `send:receiver:lookupFromClass:arguments:`.
It first looks for the method in the class of the receive then executes the method. 

```
CInterpreter >> send: aSelector 
	receiver: newReceiver 
	lookupFromClass: lookupClass 
	arguments: arguments

	| method |
	method := self 
		lookup: aSelector 
		fromClass: lookupClass.
	^ self 
			execute: (self astOf: method) 
			withReceiver: newReceiver 
			andArguments: arguments
```

And we use it in the test method infrastructure. It is good because it removes 
the duplication of logic around getting the AST and its associated class.

```
CInterpreterTest >> executeSelector: aSymbol withReceiver: aReceive
	^ self interpreter
			send: aSymbol
			receiver: aReceiver
			lookupFromClass: aReceiver class
			arguments: #()
```

With this change most of our tests should pass. 


### The Last Failing Test

The following test is failing, and this is obvious because there is no `returnSuper` in the class Object.

```
CInterpreterTest >> testReturnSuper
	receiver := Object new.
	"Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
		executeSelector: #returnSuper
		withReceiver: receiver) == receiver
```

We update it as follows: 

```
CInterpreterTest >> testReturnSuper
	receiver := CInterpretable new.
	"Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
		executeSelector: #returnSuper
		withReceiver: receiver) == receiver
```

Now all our tests pass.
In the following chapter, we will take advantage of the method `send:receiver:lookupFromClass:arguments:`
and use it more systematically.

### Conclusion

In this chapter we extended the interpreter to implement method lookup. 
We took in particular the case of `super`. 
In the following chapter, we will make sure that we test against the wrong definition of super semantics as well as make sure that our solution handles correctly overridden methods. 
We will also cover the case where the looked up method
is not found. 

