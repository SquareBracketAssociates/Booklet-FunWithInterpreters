## The Calling Infrastructure of Message Sends
@cha:callingInfra

In the previous chapters, we focused on structural evaluation: reading literal objects and reading and writing values from objects and globals. However, the key abstraction in object-oriented programming, and in Pharo in particular is _message-sending_. 
The work we did in the previous chapter is nevertheless important to set up the stage: we have a better taste of the visitor pattern, we started a first testing infrastructure, and eventually, message-sends need to carry out some work by using literal objects or reading and writing variables.


### Message Concerns 

Message-sends deserve a chapter on their own because they introduce many different concerns. 

On the one hand, each message-send is resolved in two steps: 
- first the method-lookup searches in the receiver's hierarchy for the method to be executed, and 
- second that method is applied on the receiver (i.e., it is evaluated with self-bound to the receiver). 

On the other hand, each method application needs to set up an execution context to store the receiver, arguments, and temporary variables for that specific method execution. These execution contexts form the _execution stack_ or _call stack_. Sending a message pushes a new context in the call stack, and returning from a method pops a context from the call stack.


These are the mechanics that we will cover in this chapter so that in the following chapter we can implement logic and support late-binding.

![ A call-stack with two frames: Executing the expression `someObject foo` and describing the execution stack before executing the first message (`self bar`) of the method `foo`. % width=80&anchor=callstack](figures/callstack.pdf)

### Introduction to Stack Management

Up until now, the way we managed the receiver so far is _overly_ simplistic.

Let us describe what should be done when executing a message: 
- each time a program sends a message to another object, we should change the receiver and 
- when a method execution ends, we should restore the previous receiver. 

Moreover, the same applies to method arguments and temporaries, as we will see later. Therefore, to introduce the notion of message-send, we need a stack: each element in the stack must capture all the execution state required to come back to it later on when a message-send will return. 

Each element in the call stack is usually named a _stack frame_, an activation record, or in Pharo's terminology a _context_.
 For the rest of this book, we will refer to them as __frames__, for shortness, and to distinguish them from the on-demand reified contexts from Pharo. 

Figure *@callstack@* presents a call stack with two message sends. The first frame in the stack (at its bottom) is for the message send `foo`. Method `foo` calls method `bar` and thus a second frame to represent such message send follows the first one in the stack. In addition, the message `foo` is sent to `self` so both frame points to the same object receiving the message. The currently executed method is the one on the top of the stack. When a method execution returns, we must restore all the state of the previous method just by _popping_ the top of the stack.





### Method Scope

Since methods define a scope with their temporary variables and arguments, we represent frames using a new kind of scope: a method scope.

For now, the method scope will store the current receiver, and later its parent scope, and a set of key-value pairs representing the variables defined in the current method execution: the arguments and temporaries (see Chapter *@cha:messageArgs@*).

We define the class `CMethodScope` with a single slot `receiver`. We omit showing the accessors
```
Object << #CMethodScope
	slots: { #receiver };
	package: 'Champollion'
```

### Using a Stack Implementation

We use the stack implementation available at github://pharo-containers/.

You can load it as follows: 

```
Metacello new
  baseline: 'ContainersStack';
  repository: 'github://pharo-containers/Containers-Stack:v1.0/src';
  load.
```

And define it in the baseline of your project as follows: 

```
BaselineOfChampollion >> baseline: spec
	<baseline>
	spec baseline: 'ContainersStack' with: [
		spec repository:
			'github://pharo-containers/Containers-Stack:v1.0/src' ].
	spec for: #pharo do: [
		spec package: 'Champollion' with: [ spec requires: #( 'ContainersStack' ) ].
		spec package: 'ChampollionTests' with: [ spec requires: #( 'Champollion' ) ] ]
```



### Stack Management

A first step to introduce stack management without breaking all our previous tests is to replace the single `receiver` instance variable with a stack that will be initialized when the evaluator is created. The top of the stack will represent the current execution, and thus we will take the current receiver at each moment from the stack top. Moreover, each time we tell our interpreter to execute something, we need to initialize our stack with a single frame.

#### Replace the receiver instance variable

```
Object << #CInterpreter
	slots: { #stack . #globalScope };
	package: 'Champollion'
```

```
CInterpreter >> initialize
	super initialize. 
	globalScope := CGlobalScope new.
	stack := CTStack new
```

With this new schema, we can rewrite the access to the receiver to just access the receiver from the top frame. 

```language=pharo
CInterpreter >> topFrame 
	^ stack top
```
```
CInterpreter >> receiver
	^ self topFrame receiver
```

#### Connect a New Frame
In addition, we define the method `pushNewMethodFrame`, which creates a new frame and pushes it on the top of the stack. 

```
CInterpreter >> pushNewMethodFrame
	| newTop |
	newTop := CMethodScope new.
	stack push: newTop.
	^ newTop
```

#### Adapted `execute:withReceiver:`

The final step is to set up a frame when the execution starts, which happens so far in the method `execute:withReceiver:`. 
We redefine the `execute:withReceiver:` to create a new frame and define the receiver as `#self` in the top frames before start the evaluation.


```
CInterpreter >> execute: anAST withReceiver: anObject
	self pushNewMethodFrame.
	self topFrame receiver: anObject.
	^ self visitNode: anAST
```


This refactor keeps all the tests green, and opens the path to introduce message sends.
As the reader may have observed, this stack can only grow.
We will take care of popping frames from the stack later when we revisit method returns.

In addition, we should make sure that the method scope is connected with the instance scope we defined earlier 
and that we show here. 

```
CInterpreter >> currentScope
	^ CInstanceScope new
		receiver: self receiver;
		parentScope: globalScope;
		yourself
```

### A First Message Send Evaluation

Let's start as usual by defining a new method showing the scenario we want to work on.
In this case, we want to start by extending our evaluator to correctly evaluate return values of message sends.

Our scenario method `sendMessageReturnX` does a self-send message and returns the value returned by this message. 
The scenario stresses two points:

- On the one hand, the receiver of both messages is the same. 
- On the other hand, the message is correctly evaluated as a return of the value of the activated method.

```language=pharo
CInterpretable >> sendMessageReturnX
	^ self returnInstanceVariableX
```

Notice that our method `sendMessageReturnX` and `returnInstanceVariableX` are defined in the same class. 
This means that in this first scenario, we can concentrate on the stack management and return value of the message sends, without caring too much about the details of the method lookup algorithm. 
For this first version, we will define a simple and incomplete yet useful method lookup algorithm.

Let us define a test named `testSelfSend`

```language=pharo
CInterpreterTest >> testSelfSend
	receiver x: 100.
	self 
		assert: (self executeSelector: #sendMessageReturnX) 
		equals: 100
```

In this test, we want to ensure that in a `self` message-send, the receiver of both the called and callee methods is the same. One way to do that is with a side-effect: if we write the instance variable `x` in one method and we access that value from the other method, we should get the same value for `x`. This will show that the object represented by `self` is the same and that we did not push another value such as `nil`.


### Define Message Node Evaluation

Conceptually evaluating a message node requires recursively evaluating the receiver node, which may be a literal node or a complex expression such as another message-send. From such an evaluation, we obtain the actual receiver object. 
Starting from the receiver, we should look up the method with the same selector as the message-send and execute the found method and return the result. 


To make this test green, we implement the method `visitMessageNode:`.

The following implementation does the following steps:
- Evaluate the message receiver, The call to `visitNode:` recursively calls the interpreter evaluation logic.
- Perform a simple method lookup: we fetch the desired method's AST from the receiver's class. 
- Finally, we can activate this method with the receiver using `execute:withReceiver:`. The activation will push a new frame to the call-stack with the given receiver, evaluate the method, and eventually return with a value.

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method ast | 
	newReceiver := self visitNode: aMessageNode receiver.
	method := newReceiver class compiledMethodAt: aMessageNode selector.
	ast := OCParser parseMethod: method sourceCode.
	^ self execute: ast withReceiver: newReceiver
```

All our tests should pass and in particular `testSelfSend`. 


### AST Access Logic Consolidation

Pharo provides a way to get an AST from a compiled method, but we do not want to use
because the AST it returns is different from the one we want for this book (the variables are resolved based on a semantical analysis). 
This is why we use `OCParser parseMethod: method sourceCode.`

To encapsulate such a decision we define the method `astOf:` and use it. 
In addition, we take the opportunity to set the class from which the method AST is originating. 

```
CInterpreter >> astOf: aCompiledMethod 
	| ast |
	ast := OCParser parseMethod: aCompiledMethod sourceCode.
	ast methodClass: aCompiledMethod methodClass.
	^ ast
```

We use `astOf:` in the previously defined method: 
```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method | 
	newReceiver := self visitNode: aMessageNode receiver.
	method := newReceiver class compiledMethodAt: aMessageNode selector.
	^ self execute: (self astOf: method) withReceiver: newReceiver
```

### Test for Handling Stack Balance

We mentioned earlier that when the execution of a method is finished and the execution returns to its caller method, its frame should be discarded from the stack. The current implementation clearly does not do it.
Indeed, we also said that our initial implementation of the stack only grows: it is clear by reading our code that we never pop frames from the stack.

To solve this issue, let us write a test showing the problem first.
The idea of this test is that upon return, the frame of the caller method should be restored and with its receiver. 
If we make that the caller and callee methods have different receiver instances, then this test can be expressed by calling some other method, ignoring its value and then returning something that depends only on the receiver. In other words, this test will fail if calling a method on some other object modifies the caller!

The method `setXAndMessage` shows a scenario that fulfills these requirements: 

- It sets an instance variable of the receiver with some value (here 1000), 
- Sends a message to an object other than `self` and
- upon its return, it accesses its instance variable again before returning it. 

```
CInterpretable >> setXAndMessage
		x := 1000.
		collaborator returnInstanceVariableX.
		^ x
```

Assuming the collaborator object does not modify `self`, then the result of evaluating this message should be that 1000 is returned.
When the interpreter will return the value of `x` it will look for the receiver in the stack frame and should return the 
one holding 1000 and not the collaborator. 


The test `testBalancingStack` executes the message `setXAndMessage` that should return 1000.

```
CInterpreterTest >> testBalancingStack
	self
		assert: (self executeSelector: #setXAndMessage)
		equals: 1000
```


We then finish our setup by extending the `CInterpretable` class to support delegating to a collaborator object.
We add a `collaborator` instance variable to the class `CInterpretable` with its companion accessors. 
This way we will be able to test that the correct object is set and passed around in the example.

```
Object << #CInterpretable
	slots: { #x . #collaborator };
	package: 'Champollion'
```


And in the `setUp` method we pass a collaborator to our initial receiver.

```
CInterpreterTest >> setUp
	super setUp.
	interpreter := CInterpreter new. 
	receiver := CInterpretable new.
	receiver collaborator: CInterpretable new
```


### Balance the Stack

Executing the test `testBalancingStack` fails because the access to the instance variable `x` returns nil, showing the limits of our current implementation.  This is because evaluating the message send `returnInstanceVariableX` creates a new frame with the collaborator as receiver, and since that frame is not popped off the stack, when the method returns, the access to the `x` instance variable accesses the one of the uninitialized collaborator instead of the caller object.

To solve this problem, we should pop the frame when the activation method finishes. 
This way the stack is balanced. 
This is what the new implementation of `execute:withReceiver:` does.

```language=pharo
CInterpreter >> execute: anAST withReceiver: anObject
	| result |
	self pushNewMethodFrame.
	self topFrame receiver: anObject.
	result := self visitNode: anAST.
	self popFrame.
	^ result

CInterpreter >> popFrame
	stack pop
```


### Extra Test for Receiver

Our previous tests ensure that messages return the correct value, activate the correct methods, and that the stack grows and shrinks. 
We, however, did not yet ensure that the receiver changes correctly on a message send.
Since we do not lose any opportunity to strengthen our trust in our implementation with a new test. 
Let's write a test for it.

The scenario, illustrated in `changeCollaboratorInstanceVariableX`, tells the collaborator to `store100IntoX`, implemented previously. 
In this scenario, we must ensure that the state of the receiver and the collaborator are indeed separate and that changing the collaborator does not affect the initial receiver's state.

```
CInterpretable >> changeCollaboratorInstanceVariableX
	collaborator store100IntoInstanceVariableX
```

Our test for this scenario is as follows:
If we give some value to the receiver and collaborator, executing our method should change the collaborator but not the initial receiver.

```
CInterpreterTest >> testInstanceVariableStoreInMethodActivationDoesNotChangeSender
	receiver x: 200.
	collaborator x: 300.

	"changeCollaboratorInstanceVariableX will replace collaborator's x but not the receiver's"
	self executeSelector: #changeCollaboratorInstanceVariableX.

	self assert: receiver x equals: 200.
	self assert: collaborator x equals: 100
```


To make our test run, we store as a convenience the collaborator object in an instance variable of the test too and modify the `setUp` method.

```
TestCase << #CInterpreterTest
	slots: { #receiver . #collaborator };
	package: 'Champollion'

CInterpreterTest >> setUp
	super setUp.
	interpreter := CInterpreter new. 
	receiver := CInterpretable new.
	collaborator := CInterpretable new.
	receiver collaborator: collaborator
```


This test passes, meaning that our implementation already covered correctly this case.
We are ready to continue our journey in message sends.



### Conclusion

In this chapter, we set the infrastructure to support message execution. 
We introduced the important notion of stack frames whose elements represent a given execution. 
We perform a rudimentary method lookup: we just look in the class of the receiver. 
The interpreter supports simple messages sent between different objects of the same class. 

