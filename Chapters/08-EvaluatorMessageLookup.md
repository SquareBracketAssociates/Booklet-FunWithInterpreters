## Late Binding and Method Lookup


Method lookup deserves a chapter on its own: it represents the core internal logic of late-binding and it the first part of sending a message. The method-lookup algorithm needs to support normal message-sends as well as 'super' message-sends.  
We will implement method lookup for message send to an object. Then we will present message send to super and we will 
finish by looking at how to support error \(message `doesNotUnderstand:`\).


### Method Lookup Introduction


So far we have concentrated on method evaluation and put aside method lookup.
Our current solution fetches methods from the class of the receiver, without supporting inheritance.
In this section we address this problem and implement a proper method lookup algorithm.

To implement and test the method lookup, we will extend our scenario classes with a class hierarchy.
We introduce two superclasses above `CHInterpretable`: `CHInterpretableSecondSuperclass` and its subclass `CHInterpretableSuperclass`.
With this setup we will be able to test all interesting situations, even the ones leading to infinite loops
if our method lookup is wrongly implemented.

![A simple hierarchy for self-send lookup testing.](figures/SimpleHierarchy.pdf width=80&label=fighierarchy)

```
Object subclass: #CHInterpretableSecondSuperclass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Champollion-Core'
```


```
CHInterpretableSecondSuperclass subclass: #CHInterpretableSuperclass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Champollion-Core'
```


```
CHInterpretableSuperclass subclass: #CHInterpretable
	instanceVariableNames: 'x collaborator evaluationOrder current'
	classVariableNames: ''
	package: 'Champollion-Core'
```



Our first scenario for method lookup will check that sending a message climbs up the inheritance tree when a method is not found in the receiver's class class. 
In the code below, we define a method in `CHInterpretable` that does a `self` message whose method is implemented in its `CHInterpretableSuperclass` superclass. 
Executing the first method should send the message, find the superclass method, and evaluate it.

```
CHInterpretableSuperclass >> methodInSuperclass [
	^ 5
]

CHInterpretable >> sendMessageInSuperclass [
	^ self methodInSuperclass
]

CHInterpreterTest >> testLookupMessageInSuperclass [
	self assert: (self executeSelector: #sendMessageInSuperclass) equals: 5
]
```




### Implement a First Lookup


The test should fail with our evaluator as is, because the evaluation of the message send will not find the method in the receiver's class. 
A first step towards implementing the lookup is to refactor the method `visitMessageNode:` and extract the wrong code into a `lookup:fromClass:` method.

```
CHInterpreter >> visitMessageNode: aMessageNode [
	| newReceiver method args |
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	method := self lookup: aMessageNode selector fromClass: newReceiver class.
	^ self execute: method withReceiver: newReceiver andArguments: args
]

CHInterpreter >> lookup: aSelector fromClass: aClass [
  ^ (aClass compiledMethodAt: aMessageNode selector) ast.
]
```


The method `lookup:fromClass:` is now the place to implement the method lookup algorithm:
- if the current class defines the method returns the corresponding AST;
- if the current class does not define the method and we are not on the top of the hierarchy, we recursively lookup in the class' superclass;
- else when we are on top of the hierarchy and the  `lookup:fromClass:` returns nil to indicate that no method was found.


The method `lookup:fromClass:` does not raise an error because this way the `visitMessageNode:` method will be able to send the `doesNotUnderstand:` message to the receiver, as we will see later in this chapter.

```
CHInterpreter >> lookup: aSymbol fromClass: aClass [
	"Return the AST of a method or nil if none is found"

	"If the class defines a method for the selector, return the AST corresponding to the method"
	(aClass includesSelector: aSymbol)
		ifTrue: [ ^ (aClass compiledMethodAt: aSymbol) ast ].

	"Otherwise lookup recursively in the superclass.
	If we reach the end of the hierarchy return nil"
	^ aClass superclass
		ifNil: [ nil ]
		ifNotNil: [ self lookup: aSymbol fromClass: aClass superclass ]
]
```


We should call the method `lookup:fromClass:` from the `visitMessageNode:` and our test will pass.




### The Case of Super


Many people gets confused by the semantics of `super`. The `super` variable has two different roles in the execution of an object-oriented language. 
When the `super` variable is read, its value is the receiver of the message as we saw it in the first chapter, it has the same value as `self`.

The second role of the `super` variable is to alter the method lookup when `super` is used as the receiver of the message send. 
When `super` is used as the receiver of a message send, the method lookup does not start at the class of the receiver, but at the class where the method is installed instead, allowing it to go up higher and higher in the hierarchy.

We define a method `doesSuperLookupFromSuperclass` below. 
 It is not really good since it uses `super` while it is not needed. 
The handling of overridden messages will present better tests.
 
```
CHInterpretableSuperclass >> isInSuperclass [
	^ true
]

CHInterpretable >> isInSuperclass [
	^ false
]

CHInterpretable >> doesSuperLookupFromSuperclass [
	^ super isInSuperclass
]
```



![A simple hierarchy for super-send lookup testing.](figures/HierarchyForSuper.pdf width=80&label=fighierarchySuper)


Once these methods defined, we can now test that the `isInSuperclass` message activates the method in the superclass, returning `true`.

```
CHInterpreterTest >> testLookupSuperMessage [
	self assert: (self executeSelector: #doesSuperLookupFromSuperclass)
]
```


The `super` variable changes the method lookup described previously.
When the receiver is `super`, the lookup does not start from the class of the receiver, but from the superclass of the class defining the method of the current frame. 
This implies that we need a way to access the method that is being currently executed, and the class where it is defined.

We can again store this information in the current frame during the method's activation.
We add it for now as a fake temporary variable in the frame, with the name `___method`.
By prefixing the variable's name with `___` we make it less probable this fake variable creates a conflict with a real variable. 
If we would have just named it e.g., `method`, any method with a normal normal temporary called `method` would be broken.

```
CHInterpreter >> executeMethod: anAST withReceiver: anObject andArguments: aCollection [
	| result |
	self pushNewFrame.
	self tempAt: #___method put: anAST.
	self tempAt: #self put: anObject.
	anAST arguments with: aCollection do: [ :arg :value | self tempAt: arg name put: value ]. 
	result := self visitNode: anAST body.
	self popFrame.
	^ result
]
```


We also define a convenience accessor method `currentMethod`, to get the current method stored in the current frame. 
In the future, if we want to change this implementation, we will have less places to change if we hide the access to the method behind an accessor.

```
CHInterpretable >> currentMethod [
	^ self tempAt: #___method
]
```


Note that using the current frame to store the current method will work, even if we have several messages in sequence. 
When a message is sent a new frame is pushed with a new method, and upon return the frame is popped along with its method. 
So the top frame in the stack will be always contain the method it executes.
Finally, we redefine the `visitMessageNode:` method to change class where to start looking for the method. 

```
CHInterpreterTest >> visitMessageNode: aMessageNode [

	| newReceiver method args lookupClass pragma | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	
	lookupClass := aMessageNode receiver isSuper 
		ifTrue: [ self currentMethod methodClass superclass ] 
		ifFalse: [ newReceiver class ].
	method := self lookup: aMessageNode selector fromClass: lookupClass.	
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
]
```


### Overridden Messsages


We have made sure that sending a message to `super` starts looking methods in the superclass of the class defining the method. 
Now we would like to make sure that the lookup works even in presence of overridden methods.

Let's define the method `overriddenMethod` in a superclass returning a value, and in a subclass just doing a super send with the same selector. 


```
CHInterpretableSuperClass >> overriddenMethod [
	^ 5
]

CHInterpretable >> overriddenMethod [
	^ super overriddenMethod
]
```


If our implementation is correct, sending the `overriddenMethod` message to our test receiver should return `5`. 
If it is not, the test should fail, or worse, loop infinitely.

Then we check that our test returns the correct value. If the test loops infinitely the test will timeout.

```
CHInterpreterTest >> testLookupRedefinedMethod [
	self assert: (self executeSelector: #overriddenMethod) equals: 5
]
```


If our previous implementation was correct, this test should pass.


### Checking Correct Semantics


To ensure that the method lookup is correctly implemented, especially in the presence of `super` messages, we need to verify an extra condition. 
Lot of material wrongly defines that `super` messages look up methods starting from the superclass of the class of the receiver. 
This definition, illustrated in the code snippet below, is incorrect: it only works when the inheritance depth is limited to two classes, a class and its superclass. 
In other cases, this definition will create an infinite loop.

```
CHInterpreter >> visitMessageNode: aMessageNode [
	| newReceiver method args lookupClass | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].

	lookupClass := aMessageNode receiver isSuper
		ifTrue: [ newReceiver class superclass ]
		ifFalse: [ newReceiver class ].
	method :=  self lookup: aMessageNode selector fromClass: lookupClass.

	^ self executeMethod: method withReceiver: newReceiver andArguments: args
]
```


A scenario showing such a problem is shown in Figure *@fighierarchyFullWrong@*.
In this scenario, our inheritance depth is of three classes and we create two methods with the same selector.
In the highest class, the method returns a value.
In the middle class, the first method is overridden doing a super send.

![A simple situation making wrongly defined super loop endlessly: sending the message `redefinedMethod` to an instance of the class `CHInterpretable` loops forever.](figures/WrongSuperLoopsHierarchy.pdf width=100&label=fighierarchyFullWrong)

```
CHInterpretableSecondSuperClass >> redefinedMethod [
	^ 5
]
  
CHInterpretableSuperClass >> redefinedMethod [
	^ super redefinedMethod
]
```


To finish our scenario, we create an instance of the lower subclass in the hierarchy, and we send it a message with the offending selector.

```
CHInterpreterTest >> testLookupSuperMessageNotInReceiverSuperclass [
	self assert: (self executeSelector: #redefinedMethod) equals: 5
]
```


With the incorrect semantics, our test will start by activating `CHInterpretableSuperclass>>#redefinedMethod`.
When the interpreter finds the super send, it will start the lookup from the superclass of the receiver's class: `CHInterpretableSuperclass`. 
Starting the lookup from this class will again find and activate `CHInterpretableSuperclass>>#redefinedMethod`, which will lead to activating the same method over and over again...

Coming back to our previous correct definition, it works properly, and makes our test pass:

```
CHInterpreterTest >> visitMessageNode: aMessageNode [

	| newReceiver method args lookupClass pragma | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	
	lookupClass := aMessageNode receiver isSuper 
		ifTrue: [ self currentMethod methodClass superclass ] 
		ifFalse: [ newReceiver class ].
	method := self lookup: aMessageNode selector fromClass: lookupClass.	
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
]
```


### Does not understand and Message Reification


To finish this chapter we will implement support for the `doesNotUnderstand:` feature.
In Pharo, when an object receives a message for which the lookup does not find a corresponding method, it sends instead the `doesNotUnderstand:` message to that object, with the "original message" as argument. The same mechanism exists in many other languages.
This original message is not only the selector but it comprises the arguments too. 
The interpreter should take selector and arguments to create an object representation of the message. 
We say the interpreter reifies the message.

##### About Reification.

Reification is the process of making concrete something that was not. 
In the case of the interpreter of a programming language, many of the operations of the language are implicit and hidden in the interpreter execution. 
For example, the implementation of message-sends and assignments are hidden to the developer in the sense that the developer cannot manipulate assignments for example
to count the number of time an assignment has been used during program execution.
While information hiding in interpreters is important to make languages safe and sound, the language has no way to manipulate those abstractions. 
Reifications enter in the game to enable those manipulations: interpreter concepts are concretized as objects in the interpreted language, they are "lifted-up" from the interpreter level to the application.

Reifications are a powerful concept that allow us to manipulate implementation concerns from the language itself. 
In this case, the does not understand mechanism allows us to intercept the failing message-lookup algorithm and to implement in our program a strategy to handle the error. There exist in Pharo many different reifications such as classes and methods. 
In the scope of interpreters, we will see in the chapters that follow other kind of reification: context objects representing execution frames.

A word is to be said about the performance implications of reifications. Reifications add levels of indirection in the execution. In addition it allocates objects and this adds a significant overhead in the interpretation, and increases the pressure in the garbage collector.
Production interpreters try to minimize this cost to delay reifications as much as possible, and avoid them when they are not necessary.
This is what we will do with message reifications: we will create them when a method-lookup effectively fails and not before, penalizing only the execution of does not understand messages.

### Implementing `doesNotUnderstand:`


To implement the does not understand feature, let's start by setting up our testing scenario: a method sending a not understood `messageIDoNotUnderstandWithArg1:withArg:2` message.
This message should be looked-up and not found, so the interpreter should send a `doesNotUnderstand:` message to the same receiver with the message reification. 
For the message reification, we are going to follow Pharo's behaviour and expect an instance of `Message` that should have the selector and an array with all the arguments.

```
CHInterpretable >> doesNotUnderstand: aMessage [
	^ aMessage
]

CHInterpretable >> sendMessageNotUnderstood [
	^ self messageIDoNotUnderstandWithArg1: 17 withArg2: 27
]
```


```
CHInterpreterTest >> testDoesNotUnderstandReifiesMessageWithSelector [
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) selector
		equals: #messageIDoNotUnderstandWithArg1:withArg2:
]

CHInterpreterTest >> testDoesNotUnderstandReifiesMessageWithArguments [
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) arguments
		equals: #( 17 27 )
]
```


These two tests will fail in the interpreter, because the method lookup will return `nil`, which will fail during method activation. 
To fix it, we need to handle this problem and send the `doesNotUnderstand:` message, as we said before.

```
CHInterpreter >> visitMessageNode: aMessageNode [

	| newReceiver method args lookupClass |
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | 
		self visitNode: each ].

	lookupClass := aMessageNode receiver isSuperVariable
						ifTrue: [ self currentMethod methodClass superclass ]
						ifFalse: [ newReceiver class ].
	method := self lookup: aMessageNode selector fromClass: lookupClass.
	method ifNil: [ | doesNotUnderstandMethod messageReification |
		"Handle does not understand:
		 - lookup the #doesNotUnderstand: selector
		 - reify the message
		 - activate"
		doesNotUnderstandMethod := self lookup: #doesNotUnderstand: fromClass: lookupClass.
		messageReification := Message
			selector: aMessageNode selector
			arguments: args asArray.
		^ self 
			execute: doesNotUnderstandMethod 
			withReceiver: newReceiver 
			andArguments: { messageReification } ].

	^ self execute: method withReceiver: newReceiver andArguments: args
]
```


Note that reifying does not understand requires that our interpreter knows two new things about our language: what selector is used for `#doesNotUnderstand:`, and what class is used to reify `Message`. 
In this case we are implementing a Pharo evaluator that runs in the same environment as the evaluated program: they share the same memory, classes, global variables. 
Because of this we make use of the existing selector and classes in Pharo. 
In contrast, implementing an evaluator that runs on a different environment than the evaluated program \(e.g., a Pharo evaluator implemented in C\), such dependencies need to be made explicit through a clear language-interpreter interface. 
This is for this reason that the Pharo virtual machine needs to know the selector of the message to be sent in case of message not understood. 

### Refactoring Time


As a final step, we refactor our `visitMessageNode:` to avoid repeating some code.
We extract the method activation and send, separating it from the decision of the class to start the lookup.

```
CHInterpreter >> send: aSelector receiver: newReceiver lookupFromClass: lookupClass arguments: arguments [
	"Lookup a selector from a class, and activate the method.
	Handle does not undertand case and message reification on demand if lookup fails."

	| method |
	method := self lookup: aSelector fromClass: lookupClass.
	method ifNil: [ | messageReification |
		"Handle does not understand:
		 - lookup the #doesNotUnderstand: selector
		 - reify the message
		 - activate"
		messageReification := Message
			selector: aSelector
			arguments: arguments.
		^ self
			send: #doesNotUnderstand:
			newReceiver: receiver
			lookupFromClass: lookupClass
			arguments: { messageReification } ].

	^ self execute: method withReceiver: newReceiver andArguments: arguments
]
```


And then make use of it in `visitMessageNode:`.

```
CHInterpreter >> visitMessageNode: aMessageNode [

	| newReceiver args lookupClass |
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | 
				self visitNode: each ].

	lookupClass := aMessageNode receiver isSuperVariable
					ifTrue: [ self currentMethod methodClass superclass ]
					ifFalse: [ newReceiver class ].
	^ self
		send: aMessageNode selector
		receiver: newReceiver
		lookupFromClass: lookupClass
		arguments: args asArray
]
```


### Conclusion


In this chapter, we extended our interpreter to evaluate messages.
Messages are the arguably the most important part of our interpreter, as operations in object-oriented languages are expressed in terms of them. It is also the most complex part that we have implemented so far.



At the end of this chapter we have seen the method lookup algorithm to resolve what method to execute given a receiver and a selector. We have also seen the particularities of `self` and `super` sends. Finally we have shown how the `doesNotUnderstand:` feature is implemented, by handling the lookup error, and we introduced the concept of reification to concretize and lift-up the failing message from our evaluator to the language.
