## Handling Unknown Messages
@cha:dnu

In the previous chapter we presented method lookup and show the precise semantics of message sent to `super`. We did not take into the case where the method we are looking up actually exists.
In this chapter, we show how to handle this case. We extend the interpreter with support for the 
 support error and the famous `doesNotUnderstand:`.





### Correct Semantics Verification

To ensure that the method lookup is correctly implemented, especially in the presence of `super` messages, we need to verify an extra condition.  Several books wrongly define that `super` messages lookup methods starting from the superclass of the class of the receiver. This is plain wrong.

This definition, illustrated in the code snippet below, is incorrect: it only works when the inheritance depth is limited to two classes, a class, and its superclass. 
In other cases, this definition creates an infinite loop.

```
CInterpreter >> visitMessageNode: aMessageNode

	| newReceiver method args lookupClass pragma | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode arguments.
	
	lookupClass := aMessageNode isSuperSend 
		ifTrue: [ newReceiver class superclass ] 
		ifFalse: [ newReceiver class ].
	method := self lookup: aMessageNode selector fromClass: lookupClass.	
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
```

A scenario showing such a problem is shown in Figure *@fighierarchyFullWrong@*.
In this scenario, our inheritance depth is of three classes and we create two methods with the same selector.
In the highest class, the method returns a value.
In the middle class, the first method is overridden doing a super send.

![A simple situation making wrongly defined super semantics loop endlessly: sending the message `redefinedMethod` to an instance of the class `CInterpretable` loops forever. %width=60&anchor=fighierarchyFullWrong](figures/WrongSuperLoopsHierarchy.pdf)

Let us define the situation that will loop with the wrong semantics.

```
CInterpretableRoot >> redefinedMethod
	^ 5
  
CInterpretableSuperClass >> redefinedMethod
	^ super redefinedMethod
```


To finish our scenario, we create an instance of the lower subclass in the hierarchy, and we send it a message with the offending selector.

```
CInterpreterTest >> testLookupSuperMessageNotInReceiverSuperclass
	self assert: (self executeSelector: #redefinedMethod) equals: 5
```

#### Before executing our new test.

With the incorrect semantics, our test will start by activating `CInterpretableSuperclass>>#redefinedMethod`.

When the interpreter finds the super send, it will start the lookup from the superclass of the receiver's class: `CInterpretableSuperclass`. 
Starting the lookup from this class will again find and activate `CInterpretableSuperclass>>#redefinedMethod`, which will lead to activating the same method over and over again...

Coming back to our previous correct definition, it works properly, and makes our test pass:

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


### Make the Test Pass

The test `testLookupSuperMessageNotInReceiverSuperclass` does not pass because it fails 
before being able to execute the method. Indeed, the method `executeSelector:withReceiver:`
makes the strong assumption that the executed method is defined in the class `CInterpretable`
and this clearly not always the case. 

```
executeSelector: aSymbol withReceiver: aReceiver
	| ast |
	ast := OCParser parseMethod: (CInterpretable >> aSymbol) sourceCode.
	ast methodClass: CInterpretable.
	^ self interpreter execute: ast withReceiver: aReceiver
```

When we analyse the problem we see that a method lookup phase is missing.
Starting from the receiver class, we should look for the correct compiled method. Here the method `redefinedMethod` is defined in the superclas sof `Cinterpretable`.
To address this limit, we introduce the following method in the interpreter:
It first looks for the method in the class of the receiver, then executes the method. 

```
CInterpreter >> send: aSelector 
	receiver: newReceiver 
	lookupFromClass: lookupClass 
	arguments: arguments

	| method |
	method := self lookup: aSelector fromClass: lookupClass.
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

The following is failing, and this is obvious because there is no `returnSuper` in the class Object.

```
testReturnSuper
 
  receiver := Object new.
  "Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSuper
      withReceiver: receiver) == receiver
```

We update it as follows 

```
testReturnSuper
 
  receiver := CInterpretable new.
  "Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSuper
      withReceiver: receiver) == receiver
```

Now all our tests pass. 
The astute reader should think that we are not done. Indeed we can ask ourselves about the situation where the lookup does not find the method to execute. 

This is what we will see now. 

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
CInterpretable >> doesNotUnderstand: aMessage
	^ aMessage

CInterpretable >> sendMessageNotUnderstood
	^ self messageIDoNotUnderstandWithArg1: 17 withArg2: 27
```


```
CInterpreterTest >> testDoesNotUnderstandReifiesMessageWithSelector
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) selector
		equals: #messageIDoNotUnderstandWithArg1:withArg2:

CHInterpreterTest >> testDoesNotUnderstandReifiesMessageWithArguments
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) arguments
		equals: #( 17 27 )
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

In this chapter, we have shown how the `doesNotUnderstand:` feature is implemented, by handling the lookup error, and we introduced the concept of reification to concretize and lift up the failing message from our evaluator to the language.
