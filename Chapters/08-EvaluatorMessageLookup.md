## Late Binding and Method Lookup
@cha:lookup

Method lookup deserves a chapter on its own: it represents the core internal logic of late-binding. 
The method-lookup algorithm needs to support normal message-sends as well as 'super' message-sends.  
In this chapter, we will implement method lookup for messages sent to an object. 
Then we will present how we handle the case of messages sent to `super`.


So far we have concentrated on method evaluation and put aside method lookup.
Our current solution fetches methods from the class of the receiver, without supporting inheritance.
In this section, we address this problem and implement a proper method lookup algorithm.

### Method Lookup Introduction

![Sending a message is a two-step process: method lookup and execution. % width=48&label=fig:ToSteps](figures/InheritanceDiagram-sendingMessage.pdf)

Sending a message is a two-step process as shown by Figure *@fig:ToSteps@*:
1. Method lookup: the method corresponding to the selector is looked up in the class of the receiver and its superclasses.
1. Method execution: the method is applied to the receiver. This means that `self` or `this` in the method will be bound to the receiver.

Conceptually, sending a message can be described by the following function composition:

```
sending a message (receiver argument)
	 return apply (lookup (selector classof(receiver) receiver) receiver arguments)
```


#### Method lookup

Now the lookup process is conceptually defined as follows:
1. The lookup starts in the **class** of the **receiver**.
1. If the method is defined in that class (i.e., if the method is defined in the method dictionary), it is returned.
1. Otherwise the search continues in the superclass of the currently explored class.
1. If no method is found and there is no superclass to explore (if we are in the class `Object`), this is an error (i.e., the method is not defined).


![Looking for a method is a two-step process: first, go to the class of receiver then follow inheritance. % width=58&label=fig:LookupNoError](figures/Ref-LookupNoError.pdf)

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


### Method Lookup Scenario

To implement and test the method lookup, we should extend our scenario classes with a class hierarchy.
We introduce two superclasses above `CInterpretable`: `CInterpretableRoot` and its subclass `CInterpretableSuperclass`.
With this setup, we can test all interesting situations, even the ones leading to infinite loops
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



Our first scenario for method lookup will check that sending a message climbs up the inheritance tree when a method is not found in the receiver's class. 
In the code below, we define a method in `CInterpretable` that does a `self` message whose method is implemented in its superclass (`CInterpretableSuperclass`). 
Executing the first method should send the message, find the superclass method, and evaluate it.

```
CInterpretableSuperclass >> methodInSuperclass
	^ 5

CInterpretable >> sendMessageInSuperclass
	^ self methodInSuperclass

CInterpreterTest >> testLookupMessageInSuperclass
	self assert: (self executeSelector: #sendMessageInSuperclass) equals: 5
```


### A First Lookup 

The test should fail with our evaluator as the evaluation of the message will not find the method in the receiver's class. 
A first step is to refactor the method `visitMessageNode:` and extract the wrong code into a `lookup:fromClass:` method.
We also take the opportunity to extract the management of arguments. 

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method args | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode arguments.
	method := self lookup: aMessageNode selector fromClass: newReceiver class.
	^ self execute: (self astOf: method) withReceiver: newReceiver andArguments: args

CInterpreter >> lookup: aSelector fromClass: aClass
	^ aClass compiledMethodAt: aSelector
  
CInterpreter >> handleArgumentsOf: aMessageNode 
	^ aMessageNode arguments collect: [ :each | self visitNode: each ]
```

The method `lookup:fromClass:` is now the place to implement the method lookup algorithm:
- if the current class defines the method returns the corresponding compiled method;
- if the current class does not define the method and we are not on the top of the hierarchy, we recursively lookup in the class' superclass;
- else when we are on top of the hierarchy and the  `lookup:fromClass:` returns nil to indicate that no method was found.


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

Many people get confused by the semantics of `super`. The `super` variable has two different roles in the execution of an object-oriented language. 
When the `super` variable is read, its value is the _receiver_ of the message as we saw it in the first chapter, it has the same value as `self`.

The second role of the `super` variable is to alter the method lookup when `super` is used as the receiver of the message send. 
When `super` is used as the receiver of a message send, the method lookup does _not_ start from the class of the receiver, but from the class where the method is installed instead, allowing it to go up higher and higher in the hierarchy.

Let us introduce a new scenario for our tests.
We define two methods named `isInSuperclass` and 
a method `doesSuperLookupFromSuperclass` as shown below (See Figure *@fighierarchySuper@*).

It is not nice since it uses `super` when it is unnecessary, but this is for a good cause. 
The handling of overridden messages will present better tests.
 
```
CInterpretableSuperclass >> isInSuperclass
	^ true

CInterpretable >> isInSuperclass
	^ false

CInterpretable >> doesSuperLookupFromSuperclass
	^ super isInSuperclass
```

![A simple hierarchy for super-send lookup testing. %width=80&anchor=fighierarchySuper](figures/HierarchyForSuper.pdf)

Once these methods are defined, we can test that the `isInSuperclass` message activates the method in the superclass, returning `true`.

```
CInterpreterTest >> testLookupSuperMessage
	self assert: (self executeSelector: #doesSuperLookupFromSuperclass)
```


The `super` variable changes the method lookup described previously.
When the receiver is `super`, the lookup does not start from the class of the receiver, but from _the superclass of the class defining the method_.
This implies that we need a way to access the method that is being currently executed, and the class where it is defined.

We can store this information in the current frame during the method's activation.
We add it for now as a fake temporary variable in the frame, with the name ___`method`.
By prefixing the variable's name with ___ we make it less probable this fake variable creates a conflict with a real variable. 
If we would have just named it e.g., `method`, any method with a normal normal temporary called `method` would be broken.

```
CInterpreter >> executeMethod: anAST withReceiver: anObject andArguments: aCollection
	| result |
	self pushNewFrame.
	self tempAt: #___method put: anAST.
	self tempAt: #self put: anObject.
	self manageArgumentsTemps: aCollection of: anAST.
	result := self visitNode: anAST body.
	self popFrame.
	^ result
```

We also define a convenience accessor method `currentMethod`, to get the current method stored in the current frame as well as the `tempAt:` method
and its companion method in the class `CMethodScope`.
In the future, if we want to change this implementation, we will have less places to change if we hide the access to the method behind an accessor.

```
CMethodScope >> at: aKey
	^ variables at: aKey

CInterpreter >> tempAt: aSymbol
	^ self topFrame at: aSymbol

CInterpreter >> currentMethod
	^ self tempAt: #___method
```


Note that using the current frame to store the current method will work, even if we have several messages in sequence. 
When a message is sent a new frame is pushed with a new method, and on return the frame is popped along with its method. 
So the top frame always contains the method it executes.


Finally, we redefine the `visitMessageNode:` method to change the class where to start looking for the method. 

```
CInterpreter >> visitMessageNode: aMessageNode

	| newReceiver method args lookupClass pragma | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode arguments.
	
	lookupClass := aMessageNode isSuperSend 
		ifTrue: [ self currentMethod methodClass superclass ] 
		ifFalse: [ newReceiver class ].
	method := self lookup: aMessageNode selector fromClass: lookupClass.	
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
```

With this last change, your tests should now all pass.


### Overridden Messages

We have made sure that sending a message to `super` starts looking at methods in the superclass of the class defining the method. 
Now we would like to make sure that the lookup works even in the presence of overridden methods.

Let's define the method `overriddenMethod` in a superclass returning a value, and in a subclass just doing a super send with the same selector.


```
CInterpretableSuperClass >> overriddenMethod
	^ 5

CInterpretable >> overriddenMethod
	^ super overriddenMethod
```


If our implementation is correct, sending the message `overriddenMethod` to our test receiver should return `5`. 
If it is not, the test should fail, or worse, loop infinitely.

Then we check that our test returns the correct value. If the test loops infinitely the test will timeout.

```
CInterpreterTest >> testLookupRedefinedMethod
	self assert: (self executeSelector: #overriddenMethod) equals: 5
```


This test should pass.


### Conclusion

In this chapter we extended the interpreter to implement method lookup. 
We took in particular the case of `super`. 
In the following chapter, we will present how to support the case where the looked up method
is not found. 

