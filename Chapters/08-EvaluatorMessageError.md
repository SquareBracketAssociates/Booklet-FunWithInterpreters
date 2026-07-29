## Deeper into Messages: Handling Unknown Messages
@cha:dnu

In the previous chapter, we presented method lookup and showed the precise semantics of the messages sent to `super`. 
We only took into account the case where the method we are looking up actually exists.
In this chapter, we show how to handle this case. We extend the interpreter with support for the 
 support error and the famous `doesNotUnderstand:`.

We start by revisiting the current method lookup implementation. Doing so, we will be ready to handle the case of unknown messages.


### Refactoring the Terrain

Before starting to implement new functionalities, it is time to step back and take advantage that all the tests pass to improve the existing code base.

We refactor `visitMessageNode:` to use the new message `send:receiver:lookupFromClass:arguments:` as follows: 

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver args lookupClass | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode.
	lookupClass := aMessageNode isSuperSend 
		ifTrue: [ self currentMethod methodClass superclass ] 
		ifFalse: [ newReceiver class ].
	^ self
		send: aMessageNode selector
		receiver: newReceiver
		lookupFromClass: lookupClass
		arguments: args asArray
```

All the tests should pass and we are ready for the next point.


### Overridden Messages

We have made sure that sending a message to `super` starts looking at methods in the superclass of the class defining the method. 
Now we would like to make sure that the lookup works even in the presence of overridden methods.

![Testing overridden methods. %width=80&anchor=figoverriddent](figures/OverriddenMethods.pdf)

Let's define the method `overriddenMethod` in a superclass returning a value, and in a subclass just doing a super send with the same selector (as shown in Figure *@figoverriddent@*).


```
CInterpretableSuperClass >> overriddenMethod
	^ 5
```

```
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
We suggest you add more tests for example with a super message in the subclass `CInterpretable` finding a method in the root class `CInterpretableRoot`.
Such tests should also pass.



### Correct Semantics Verification

To ensure that the method lookup is correctly implemented, especially in the presence of `super` messages, we need to stress our implementation with an extra scenario. Indeed, several books wrongly define that `super` messages lookup methods starting from the superclass of the class of the receiver. This is plain wrong!

This definition, illustrated in the code snippet below, is incorrect: it only works when the inheritance depth is limited to two classes, a class, and its superclass. 
In other cases, this definition creates an infinite loop as you can experiment it with the `redefinedMethod` method definitions below.

```
CInterpreter >> visitMessageNode: aMessageNode

	| newReceiver method args lookupClass pragma | 
	newReceiver := self visitNode: aMessageNode receiver.
	args := self handleArgumentsOf: aMessageNode arguments.
	
	lookupClass := aMessageNode isSuperSend 
		ifTrue: [ newReceiver class superclass ] 
		ifFalse: [ newReceiver class ].
	^ self
			send: aMessageNode selector
			receiver: newReceiver
			lookupFromClass: lookupClass
			arguments: args asArray
```

A scenario showing such a problem is shown in Figure *@fighierarchyFullWrong@*.
In this scenario, our inheritance depth is of three classes and we create two methods with the same selector.
In the highest class, the method returns a value.
In the middle class, the first method is overridden doing a super send.

![A simple situation that breaks wrongly defined super semantics: sending the message `redefinedMethod` to an instance of the class `CInterpretable` loops forever. %width=60&anchor=fighierarchyFullWrong](figures/WrongSuperLoopsHierarchy.pdf)

Let us define the situation that will loop with the wrong semantics.

```
CInterpretableRoot >> redefinedMethod
	^ 5
```

```
CInterpretableSuperClass >> redefinedMethod
	^ super redefinedMethod
```


To finish our scenario, we create an instance of the lower subclass in the hierarchy, and we send it a message with the offending selector.

```
CInterpreterTest >> testLookupSuperMessageNotInReceiverSuperclass
	self assert: (self executeSelector: #redefinedMethod) equals: 5
```

### Stepping in Wrong Semantics

With the incorrect semantics, our test will start by activating `CInterpretableSuperclass>>#redefinedMethod`.

When the interpreter finds the super send, it will start the lookup from the superclass of the receiver's class: `CInterpretableSuperclass`. 
Starting the lookup from this class will again find and activate `CInterpretableSuperclass>>#redefinedMethod`, which will lead to activating the same method over and over again...

Coming back to our previous correct definition, it works properly, and makes our test pass!

The astute reader should think that we are not done. Indeed we can ask ourselves about the situation where the lookup does not find the method to execute. 

This is what we will see now. 


### Unknown Messages

![When a message is not found, another message is sent to the receiver supporting reflective operation. % width=65&label=fig:LookupWithError](figures/Ref-LookupWithError.pdf)


When the method is not found, the message `error` is sent as shown in Figure *@fig:LookupWithError@*. Sending a message instead of simply reporting an error using a trace or an exception is a key design decision. Indeed classes can define their own implementation of the method `error` and perform specific actions to the case of messages that are not understood.  For example, it is possible to implement proxies (objects representing other remote objects) or compile code on the fly by redefining such a message locally.


### Handling Unknown Messages

Here is a sketch of the lookup algorithm with error handling. The key point is that the lookup returns a method or nil.

```
lookup (selector class):
   if the method is found in class
      then return it
      else if class == Object
           then return nil
           else lookup (selector superclass(class))
```

And then we define sending a message as follows:
First the lookup is performed and depending on its result (i.e. the method is not found)
another message is sent else the method is executed.

```
sending a message (receiver argument)
   methodOrNil = lookup (selector classof(receiver)).
   if methodOrNil is nil
      then send the message error to the receiver
      else return apply(methodOrNil receiver arguments)
```


### Unknown Messages in Pharo

In Pharo, when an object receives a message for which the lookup does not find a corresponding method, it sends instead the `doesNotUnderstand:` message to that object, with the "original message" as an argument.
This original message is not only the selector but it comprises the arguments too. 

In fact since the original unknowm message can have difference numbers of parameters and the message `doesNotUnderstand:` only define one parameterm there is a need to collect all the information of the unknown message (selector and arguments) into a single object. This is what is done in Pharo, an instance of the class `Message` is created, fill up with the unknown message information and passed as arguments of the `doesNotUnderstand:` message.

So the interpreter should take the selector and arguments of the unknown message and create an object representation of the message. We say the interpreter reifies the message.

Before jumping in  the support for `doesNotUnderstand:` we want to discuss the notion of reification. 

### About Reification

Reification is the process of making concrete something that was not. 
In the case of the interpreter of a programming language, many of the operations of the language are implicit and hidden in the interpreter's execution. 
For example, the implementation of message-sends and assignments are hidden to the developer in the sense that the developer cannot manipulate assignments for example
to count the number of times an assignment has been used during program execution.
While information hiding in interpreters is important to make languages safe and sound, the language has no way to manipulate those abstractions. 
Reifications enter the game to enable those manipulations: interpreter concepts are concretized as objects in the interpreted language, they are "lifted up" from the interpreter level to the application.

Reifications are a powerful concept that allow us to manipulate implementation concerns from the language itself. 
In this case, the does not understand mechanism allows us to intercept the failing message-lookup algorithm and to implement in our program a strategy to handle the error. There exist in Pharo many different reifications such as classes and methods. 
In the scope of interpreters, we will see in the chapters that follow other kinds of reification: context objects representing execution frames.

A word is to be said about the performance implications of reifications. Reifications add levels of indirection to the execution. In addition, it allocates objects and this adds a significant overhead in the interpretation and increases the pressure in the garbage collector.
Production interpreters try to minimize this cost to delay reifications as much as possible, and avoid them when they are not necessary.
This is what we will do with message reifications: we will create them when a method-lookup effectively fails and not before, penalizing only the execution of does not understand messages.

### Implementing `doesNotUnderstand:`

To implement the support for unknown message feature, let's start by setting up our testing scenario: a method sending a not understood `messageIDoNotUnderstandWithArg1:withArg2:` message.

```
CInterpretable >> sendMessageNotUnderstood
	^ self messageIDoNotUnderstandWithArg1: 17 withArg2: 27
```
This message should be looked-up and not found, so the interpreter should send a `doesNotUnderstand:` message to the same receiver with the message reification. 

For the message reification, we are going to follow Pharo's behavior and expect an instance of `Message` that should have the selector and an array with all the arguments. 

The simplest implementation for the `doesNotUnderstand:` method is to simply return its argument. 
Notice that we define it on the class `CInterpretable`. Indeed every class can reimplement the way it handles message not understood by defining such a method.


```
CInterpretable >> doesNotUnderstand: aMessage
	^ aMessage
```

### Tests for DNU support

We define two tests covering that the implementation captures the message information.

```
CInterpreterTest >> testDoesNotUnderstandReifiesMessageWithSelector
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) selector
		equals: #messageIDoNotUnderstandWithArg1:withArg2:
```
```
CInterpreterTest >> testDoesNotUnderstandReifiesMessageWithArguments
	self
		assert: (self executeSelector: #sendMessageNotUnderstood) arguments
		equals: #( 17 27 )
```


These two tests will fail in the interpreter, because the method lookup will return `nil`, which will fail during method activation. 

### Support for DNU

To address it, we need to handle this problem and send the `doesNotUnderstand:` message, as we said before.
For this we modify the method ` send:receiver:lookupFromClass:arguments:` as follows: 

- it looks for the method
- if the method is not found, it creates a message, and send the message `doesNotUnderstand:` to the receiver with the message as an argument,
- else it just executes the found method. 


```
CInterpreter >> send: aSelector 
	receiver: newReceiver 
	lookupFromClass: lookupClass 
	arguments: arguments

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
				receiver: newReceiver
				lookupFromClass: lookupClass
				arguments: { messageReification } ].
	
	^ self 
			execute: (self astOf: method) 
			withReceiver: newReceiver 
			andArguments: arguments
```

All the tests should now pass. 

#### Remark.
Note that reifying does not understand requires that our interpreter knows two new things about our language: what selector is used for report the error (here `#doesNotUnderstand:`), and what class is used to reify the message. 

In our case,  we are implementing a Pharo evaluator that runs in the same environment as the evaluated program: they share the same memory, classes, global variables. 
Because of this we make use of the existing selector and classes in Pharo. 

In contrast, implementing an evaluator that runs on a different environment than the evaluated program (e.g., a Pharo evaluator implemented in C), such dependencies need to be made explicit through a clear language-interpreter interface. 
This is for this reason that the Pharo virtual machine needs to know the selector of the message to be sent in case of message not understood. 



### Conclusion

In this chapter, we have tested more thoroughly the support for `super` and shown how the `doesNotUnderstand:` feature is implemented, by handling the lookup error, and we introduced the concept of reification to concretize and lift up the failing message from our evaluator to the language.
