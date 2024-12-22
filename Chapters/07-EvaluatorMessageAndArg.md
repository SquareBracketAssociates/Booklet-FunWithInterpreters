## Implementing Message Sends: The Calling Infrastructure
@cha:callingInfra

In the previous chapters, we focused on structural evaluation: reading literal objects and reading and writing values from objects and globals. However, the key abstraction in object-oriented programming, and in Pharo in particular is _message-sending_. 
The work we did in the previous chapter is nevertheless important to set up the stage: we have a better taste of the visitor pattern, we started a first testing infrastructure, and eventually, message-sends need to carry out some work by using literal objects or reading and writing variables.


### Message concerns 

Message-sends deserve a chapter on their own because they introduce many different concerns. 
On the one hand, each message-send is resolved in two steps: 
- first the method-lookup searches in the receiver's hierarchy the method to be executed, and 
- second that method is applied on the receiver (i.e., it is evaluated with self-bound to the receiver). 

On the other hand, each method application needs to set up an execution context to store the receiver, arguments, and temporary variables for that specific method execution.
These execution contexts form the _execution stack_ or _call stack_.
Sending a message pushes a new context in the call stack, and returning from a method pops a context from the call stack.
This are the mechanics that we will cover in this chapter so that in the following chapter we can implement logic and support late-binding.


### Introduction to Stack Management

The way we managed the receiver so far is overly simplistic.
Indeed, each time a program sends a message to another object, we should change the receiver and when a method execution ends, we should restore the previous receiver. Moreover, the same happens with method arguments and temporaries as we will see later. Therefore to introduce the notion of message-send we need a stack: each element in the stack needs to capture all the execution state required to come back to it later on when a message-send will return. 

Each element in the call stack is usually named a _stack frame_, an activation record, or in Pharo's terminology a _context_.
 For the rest of this book we will refer to them as frames, for shortness, and to distinguish them from the reified contexts from Pharo. 

Figure *@callstack@* presents a call stack with two methods. The first method in the stack (at its bottom) is method `foo`. Method `foo` calls method `bar` and thus it follows it in the stack. In addition, the message `foo` is sent to self so both frame points to the same object receiving the message. The current method executing is the one on the top of the stack. When a method returns, we can restore all the state of the previous method just by _popping_ the top of the stack.

![A call-stack with two frames, executing the method `foo` which sends the message `self bar`. % width=80&anchor=callstack](figures/callstack.pdf)




### Putting in place the stack

We will use the stack implementation available at github://pharo-containers/.

```
Metacello new
  baseline: 'ContainersStack';
  repository: 'github://pharo-containers/Containers-Stack:v1.0/src';
  load.
```

Since methods define a scope with their temporary variables and arguments, we represent frames using a new kind of scope: a method scope.
For now, the method scope will store the current receiver, and later its parent scope, and a set of key-value pairs representing the variables defined in the current method execution: the arguments and temporaries (see Chapter *@@*).

```
Object << #CMethodScope
	slots: { #receiver };
	package: 'Champollion'

CMethodScope >> receiver: aCInterpretable
	receiver := aCInterpretable

CMethodScope >> receiver
	^ receiver
```


A first step to introduce stack management without breaking all our previous tests is to replace the single `receiver` instance variable with a stack that will be initialized when the evaluator is created. The top of the stack will represent the current execution, and thus we will take the current receiver at each moment from the stack top. Moreover, each time we tell our interpreter to execute something we need to initialize our stack with a single frame.


```caption=Replace the receiver instance variable by a stack
Object << #CInterpreter	slots: { #stack . #globalScope };	package: 'Champollion'

CInterpreter >> initialize
	super initialize. 
	globalScope := CGlobalScope new.
	stack := CTStack new.
```


With this new schema, we can now rewrite the access to the receiver to just access the value of `#self` of the top frame. 

```language=pharo
CInterpreter >> topFrame 
	^ stack top
	
CInterpreter >> receiver
	^ self topFrame receiver
```

The final step is to set up a frame when the execution starts, which happened so far in our method `execute:withReceiver:`. 
We redefine the `execute:withReceiver:` to create a new frame and define the receiver as `#self` in the top frames before start the evaluation.


```language=pharo
CInterpreter >> execute: anAST withReceiver: anObject
	self pushNewFrame.
	self topFrame receiver: anObject.
	^ self visitNode: anAST
```


The last piece in the puzzle is the method `pushNewFrame`, which creates a new frame and pushes it on the top of the stack. 

```
CInterpreter >> pushNewFrame
	| newTop |
	newTop := CMethodScope new.
	stack push: newTop.
	^ newTop
```

This refactor kept all the test green, and opened the path to introduce message-sends.
As the reader may have observed, this stack can only grow.
We will take care of popping frames from the stack later when we revisit method returns.


### Evaluating a First Message Send

Let's start as usual by defining a new method exhibiting the scenario we want to work on.
In this case, we want to start by extending our evaluator to correctly evaluate return values of message sends.

Our scenario method `sendMessageReturnX` does a self-send message and returns the value returned by this message. 
The scenario stresses two points:

- On the one hand, the receiver of both messages is the same. 
- On the other hand, the message is correctly evaluated as a return of the value of the activated method.

```language=pharo
CInterpretable >> sendMessageReturnX	^ self returnInstanceVariableX
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

In this test we want to ensure that in a `self` message-send, the receiver of both the called and callee methods is the same. One way to do that is with a side-effect: if we write the instance variable `x` in one method and we access that value from the other method, we should get the same value for `x`. This will show that the object represented by `self` is the same and that we did not push for example `nil`.


Conceptually evaluating a message node requires recursively evaluating the receiver node, which may be a literal node or a complex expression such as another message-send. From such an evaluation we obtain the actual receiver object. 
Starting from the receiver, we should look up the method with the same selector as the message-send and execute the found method and return the result. 


To make this test green, we implement the method `visitMessageNode:`.

In the first implementation proposed hereafter, we just fetch the desired method's AST from the receiver's class. 
Finally, we can activate this method with the receiver using `execute:withReceiver:` the activation will push a new frame to the call-stack with the given receiver, evaluate the method, and eventually return with a value.

```
CInterpreter >> visitMessageNode: aMessageNode	| newReceiver method ast | 	newReceiver := self visitNode: aMessageNode receiver.	method := newReceiver class compiledMethodAt: aMessageNode selector.	ast := RBParser parseMethod: method sourceCode.	^ self execute: ast withReceiver: newReceiver
```

All our tests should pass and in particular `testSelfSend`. 

### Consolidating AST access logic

Pharo provides a way to get an AST from a compiled method, but we do not want to use
because the AST it returns is different from the one we want for this book (the variables are resolved based on a semantical analysis). 
This is why we use `RBParser parseMethod: method sourceCode.`

To encapsulate such a decision we define the method `astOf:` and use it. 

```
astOf: aCompiledMethod 	^ RBParser parseMethod: aCompiledMethod sourceCode.
```

```
CInterpreter >> visitMessageNode: aMessageNode	| newReceiver method | 	newReceiver := self visitNode: aMessageNode receiver.	method := newReceiver class compiledMethodAt: aMessageNode selector.	^ self execute: (self astOf: method) withReceiver: newReceiver
```


### Balancing the Stack

We mentioned earlier that when the execution of a method is finished and the execution returns to its caller method, its frame should be also discarded from the stack. The current implementation clearly does not do it.
Indeed, we also said that our initial implementation of the stack only grows: it is clear by reading our code that we never pop frames from the stack.

To solve this issue, let us write a test showing the problem first.
The idea of this test is that upon return, the frame of the caller method should be restored and with its receiver. 
If we make that the caller and callee methods have different receiver instances, then this test can be expressed by calling some other method, ignore its value and then return something that depends only on the receiver. In other words, this test will fail if calling a method on some other object modifies the caller!

The following code snippet shows an scenario that fulfills these requirements: it sets an instance variable with some value, sends a message to an object other than `self` and upon its return it accesses its instance variable again before returning it. Assuming the collaborator object does not modify `self`, then the result of evaluating this message should be that 1000 is returned.

```
CHInterpretable >> setXAndMessage
		x := 1000.
		collaborator returnX.
		^ x
```


Our test `testBalancingStack` executes the message `setXAndMessage` that should return 1000.

```
CInterpreterTest >> testBalancingStack
	self
		assert: (self executeSelector: #setXAndMessage)
		equals: 1000
```


We then finish our setup by extending `CInterpretable` to support delegating to a collaborator object.
We add a `collaborator` instance variable to the class `CInterpretable` with its companion accessors. 
This way we will be able to test that the correct object is set and passed around in the example.

```
Object << #CInterpretable
	slots: { #x . #collaborator };
	package: 'Champollion'

CInterpretable >> collaborator
	^ collaborator

CInterpretable >> collaborator: anObject
	collaborator := anObject
```


And in the `setUp` method we pass a collaborator to our initial receiver.

```
CInterpreterTest >> setUp
	super setUp.
	receiver := CInterpretable new.
	receiver collaborator: CInterpretable new
```


#### Making the test pass

Executing this test breaks because the access to the instance variable `x` returns nil, showing the limits of our current implementation.  This is due to the fact that evaluating message send `returnX` creates a new frame with the collaborator as receiver, and since that frame is not popped from of the stack, when the method returns, the access to the `x` instance variable accesses the one of the uninitialized collaborator instead of the caller object.

To solve this problem, we should pop the frame when the activation method finishes. 
This way the stack is balanced. 
This is what the new implementation of `executeMethod:withReceiver:` does.

```language=pharo
CInterpreter >> executeMethod: anAST withReceiver: anObject
	self pushNewFrame.
	self topFrame receiver: anObject.
	result := self visitNode: anAST.
	self popFrame.
	^ result

CInterpreter >> popFrame
	stack pop
```



### Ensuring the receiver is correctly set: an Extra Test

Our previous tests did ensure that messages return the correct value, activate the correct methods, and that the stack grows and shrinks. However, we did not ensure yet that the receiver changes correctly on a message send, and since we do not lose any opportunity to strenghten our trust in our implementation with a new test, let's write a test for it.

The scenario, illustrated in `changeCollaboratorX` will ask the collaborator to `store100IntoX`, implemented previosly. In this scenario, we must ensure that the state of the receiver and the collaborator  are indeed separate and that changing the collaborator will not affect the initial receiver's state.

```
CInterpretable >> changeCollaboratorX
	collaborator store100IntoX
```


Our test for this scenario is as follows. 
If we give some value to the receiver and collaborator, executing our method should change the collaborator but not the initial receiver.

```
CInterpreterTest >> testInstanceVariableStoreInMethodActivationDoesNotChangeSender
	receiver x: 200.
	collaborator x: 300.

	"changeCollaboratorX will replace collaborator's x but not the receiver's"
	self executeSelector: #changeCollaboratorX.

	self assert: receiver x equals: 200.
	self assert: collaborator x equals: 100
```


To make our test run, we will store as a convenience the collaborator object in an instance variable of the test too.

```
TestCase << #CInterpreterTest
	slots: { #receiver . #collaborator };
	package: 'Champollion-Tests'

CInterpreterTest >> setUp
	super setUp.
	receiver := CInterpretable new.
	collaborator := CInterpretable new.
	receiver collaborator: collaborator
```


This test passes, meaning that our implementation already covered correctly this case.
We are ready to continue our journey in message-sends.







## Message Arguments and temporaries
@cha:messageArgs



### Supporting Message Arguments


So far we have worked only with unary messages. Unary messages have no arguments, so the number of programs we can express with them only is rather limited. The next step towards having a full-blown interpreter is to support message arguments, which will open us the door to support binary and keyword messages. From the evaluator point of view, as well as from the AST point of view, we will not distinguish between unary, binary and keyword messages. The parser already takes care about distinguishing them and handling their precedence. Indeed, message nodes in the AST are the same for all kind of messages, they have a selector and a collection of argument nodes. Precedence is then modelled as relationships between the AST nodes.


In addition of simply passing the arguments, from an evaluator point of view, we need to care about evaluation order too. This is particularly important because Pharo is an imperative language where messages can trigger side effects. Evaluating two messages in one order may not have the same result as evaluating them in a different order. Arguments in Pharo are evaluated eagerly after evaluating the receiver expression, but before evaluating the message, from left to right. Once all expressions are evaluated, the resulting objects are send as part of the message-send.

#### Initial Argument Support


To implement some initial support for arguments, our first scenario is to simply send a message with an argument. For our scenario we already count with one message with an argument: the `x:` setter. We can then define a method `changeCollaboratorWithArgument` which uses it.

```language=smalltalk
CHInterpretable >> changeCollaboratorWithArgument [
	collaborator x: 500
]
```


In the test, we verify that the method evaluation effectively modifies the collaborator object 
as written in `changeCollaboratorWithArgument`, and not the initial receiver object.

```language=smalltalk
CHInterpreterTest >> testArgumentAccess [

	receiver x: 200.
	collaborator x: 300.

	self executeSelector: #changeCollaboratorWithArgument.

	self assert: receiver x equals: 200.
	self assert: collaborator x equals: 500
]
```


Since we have not implemented any support for arguments yet, this test should fail.

Implementing argument support requires two main changes:
- On the caller side, we need to evaluate the arguments in the context of the caller method and then store those values in the new frame. 
- On the callee side, when an argument access is evaluated, those accesses will not re-evaluate the expressions in the caller. Instead, argument access will just read the variables pre-stored in the current frame.


Let's start with the second step, the callee side, and since all variable reads are concentrated on the scope lookup, we need to add the method scope in the scope chain, and define a `read:` method for it.

```
CHInterpreter >> execute: anAST withReceiver: anObject [
	| result |
  self pushNewFrame.

  "Set up the scope chain"
  self topFrame parentScope: (CHInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself);
	yourself.

  self topFrame receiver: anObject.
  result := self visitNode: anAST.
  self popFrame.
  ^ result

]
CHInterpreter >> currentScope [
	^ self topFrame
]

CHMethodScope >> scopeDefining: aString [
	(variables includesKey: aString)
		ifTrue: [ ^ self ].

	^ self parentScope scopeDefining: aString
]

CHMethodScope >> read: aString [
	^ variables at: aString
]
```


Then we need to update `visitMessageNode:` to compute the arguments by doing a recursive evaluation, and then use those values during the new method activation.

```
CHInterpreter >> visitMessageNode: aMessageNode [
	| newReceiver method args |
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	newReceiver := self visitNode: aMessageNode receiver.
	method := (newReceiver class compiledMethodAt: aMessageNode selector) ast.
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
]
```


To include arguments in the method activation, let's add a new `arguments` parameter to our method `execute:withReceiver:` to get `execute:withReceiver:withArguments:`. 

In addition to adding the receiver to the new frame representing the execution, we add a binding for each parameter \(called unfornately arguments in Pharo AST\) with their corresponding value in the argument collection. 
We use the message `with:do:` to iterate both the parameter list and actual arguments as pairs.

```
CHInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection [
	| result |
  self pushNewFrame.

  "Set up the scope chain"
  self topFrame parentScope: (CHInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself);
	yourself.

	self topFrame receiver: anObject.
	anAST arguments 
		with: aCollection
		do: [ :arg :value | self topFrame at: arg name put: value ]. 
	result := self visitNode: anAST.
	self popFrame.
	^ result
]
```


Instead of just removing the old `executeMethod:withReceiver:` method, we redefine it calling the new one with a default empty collection of arguments. This method was used by our tests and is part of our public API, so keeping it will avoid migrating extra code and an empty collection of arguments seems like a sensible and practical default value.
 
```
CHInterpreter >> executeMethod: anAST withReceiver: anObject [
	^ self 
		executeMethod: anAST 
		withReceiver: anObject 
		andArguments: #()
]
```


Our tests should all pass now.

### Refactoring the Terrain


Let's now refactor a bit the existing code to clean it up and expose some existing but hidden functionality. Let us extract the code that accesses `self` and the frame parameters into two other methods that make more intention revealing that we are accessing values in the current frame.

```
CHInterpreter >> tempAt: aSymbol put: anInteger [
	self topFrame at: aSymbol put: anInteger
]
```


```
CHInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection [
	| result |
	self pushNewFrame.
  
  "Set up the scope chain"
  self topFrame parentScope: (CHInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself);
	yourself.
  
	self topFrame receiver: anObject.
	anAST arguments 
		with: aCollection
		do: [ :arg :value | self tempAt: arg name put: value ].
	result := self visitNode: anAST.
	self popFrame.
	^ result
]
```











### Handling Temporaries


Temporary variables, or local variables, are variables that live within the scope of a method's **execution**.
Memory for such variables is allocated when a method is activated, and released when the method returns.
Because of this property, temporary variables are also called automatic variables in languages like C.

The usual way to implement such temporary variables is to allocate them in the method execution's frame.
This way, when the method returns, the frame is popped and all the values allocated in temporaries are discarded and can be reclaimed.
In other words, we will manage temporaries the same way as we manage arguments.

Our first scenario introducing temporaries will verify the default value of temporaries.
Indeed when temporaries are allocated in Pharo, the execution engine \(in this case our evaluator\) should make sure these variables are correctly initialized to a default value, in this case `nil`.

Notice that temporaries cannot be observed from outside the execution of a method unless we halt the evaluation of a method in the middle of the evaluation. Since our testing approach is more like a black-box approach, we need to make our scenarios visible from the outside somehow. Because of these reasons, our tests will rely on returns again, as we did before with literal objects.

```
CHInterpretable >> returnUnassignedTemp [
	| temp |
	^ temp
]
```


The companion test verifies that the value of a uninitialized temporary is `nil`.

```
CHInterpreterTest >> testUnassignedTempHasNilValue [
	self
		assert: (self executeSelector: #returnUnassignedTemp)
		equals: nil
]
```


The current subset of Pharo that we interpret does not contain blocks and their local/temporary variables \(We will implement blocks and more complex lexical scopes in a subsequent chapter\).
Therefore the temporary variable management we need to implement so far is rather simple.

To make our test pass, we modify the `execute:withReceiver:andArguments:` method to define the temporaries needed with `nil` as value.

```
CHInterpreter >> executeMethod: anAST withReceiver: anObject andArguments: aCollection [
  | result thisFrame |	
  self pushNewFrame.
  self topFrame parentScope: (CHInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself);
	yourself.
  
	self topFrame receiver: anObject.
	anAST arguments with: aCollection do: [ :arg :value | self tempAt: arg name put: value ].
	anAST temporaryNames do: [ :tempName | self tempAt: tempName name put: nil ].
	result := self visitNode: anAST body.
	self popFrame.
	^ result
]
```


The tests should be pass.

### Implementing Temporary Variable Writes


Finally we test that writes to temporary variables are working too.
We define our scenario method `writeTemporaryVariable`, which defines a temporary variable, assigns to it and returns it. 
An optimizing compiler for this code would be smart enough to do constant propagation of the literal integer and then realize that the temporary is dead code and remove it, leaving us with a method body looking like ` ^ 100 `. 
However, since the parser does not do such optimizations by itself, we are sure that the AST we get contains both the temporary definition, the assignment, and the temporary return.

```
CHInterpretable >> writeTemporaryVariable [
	| temp |
	temp := 100.
	^ temp
]
```


Its companion test checks that evaluating this method does effectively return 100, meaning that the temporary variable write succeeded, and that `temp` means the same variable in the assignment and in the access.

```
CHInterpreterTest >> testWriteTemporaryVariable [
	self
		assert: (self executeSelector: #writeTemporaryVariable)
		equals: 100
]
```



Since temporary variable name resolution is already managed by our method scopes, we just need to implement `write:withValue:` in it to make all our tests pass.

```
  CHMethodScope >> write: aString withValue: aValue [
  	variables at: aString put: aValue
]
```



### Evaluation Order


The last thing we need to make sure is that arguments are evaluated in the correct order.
The evaluation order in Pharo goes as follows: before evaluating a message, the receiver and all arguments are evaluated. The receiver is evaluated before the arguments. Arguments are evaluated in left-to-right order.

Testing the evaluation order in a black-box fashion as we were doing so far is rather challenging with our current evaluator. Indeed, our evaluator does not yet handle arithmetics, allocations nor other kind of primitive, so we are not able to easily count! A simple approach to test is to make a counter out of [Peano Axioms](https://en.wikipedia.org/wiki/Peano_axioms). The main idea is to implement numbers as nested sets, where the empty set is the zero, the set that contains the zero is one, the set that contains a one is a two, and so on. The only support we need for this is to extend our literal support for dynamic array literals. The code illustrating the idea follows.

```
CHInterpretable >> initialize [
  super initialize.
  current := { "empty" }.
]
```


```
CHInterpretable >> next [
  | result |
  "Implement a stream as an increment in terms of Peano axioms.
  See https://en.wikipedia.org/wiki/Peano_axioms"
  result := current.
  "We increment the counter"
  current := { current }.
  "We return the previous result"
  ^ result
]
```


```
CHInterpreterTests >> peanoToInt: aPeanoNumber [
	"Helper method to transform a peano number to a normal Pharo integer"
	^ aPeanoNumber
		ifEmpty: [ 0 ]
		ifNonEmpty: [ 1 + (self peanoToInt: aPeanoNumber first) ]
]
```


Using this support, we can express our evaluation order scenario and test as follows.
We will add a new instance variable to `CHInterpretable` to store its evaluation order.
Then, we are going to send a message with many arguments, evaluating for each argument `self next`.
The message receiving the arguments will receive as argument three generated peano values, that we will return as dynamic literal array. If evaluation order is right, the evaluation order of the receiver should be 0, the evaluation of the first argument should be 1, and so on.

```
Object subclass: #CHInterpretable
	instanceVariableNames: 'x collaborator evaluationOrder'
	classVariableNames: ''
	package: 'Champollion-Core'
```


```
CHInterpretable >> evaluationOrder [
  ^ evaluationOrder
]
```


```
CHInterpretable >> evaluateReceiver [
  evaluationOrder := self next.
  ^ self
]
```


```
CHInterpretable >> returnEvaluationOrder [
  ^ self evaluateReceiver
      messageArg1: self next
      arg2: self next
      arg3: self next
]
```


```
CHInterpretable >> messageArg1: arg1 arg2: arg2 arg3: arg3 [
  ^ {arg1 . arg2 . arg3}
]

CHInterpreterTests >> testEvaluationOrder [
  | argumentEvaluationOrder |
  argumentEvaluationOrder := self executeSelector: #returnEvaluationOrder.

  self assert: (self peanoToInt: receiver evaluationOrder) equals: 0.
  self
    assert: (argumentEvaluationOrder collect: 
		[ :peano | self peanoToInt: peano])
    equals: #(1 2 3)
]
```


To make this test green we need to implement previously some new support in our interpreter: writing to temporary variables and dynamic literal arrays.

```
CHInterpreter >> visitArrayNode: aRBArrayNode [
	^ aRBArrayNode statements 
		collect: [ :e | self visitNode: e ] 
		as: Array
]
```


At this point our test will fail because the evaluation order is wrong! The receiver was evaluated 4th, after all arguments. 
This is solved by changing the order of evaluation in `visitMessageNode:`.

```
CHInterpreter >> visitMessageNode: aMessageNode [
	| newReceiver method args |
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	method := (newReceiver class compiledMethodAt: aMessageNode selector) ast.
	^ self executeMethod: method withReceiver: newReceiver andArguments: args
]
```


### About Name Conflict Resolution


Inside the scope of a method, statements have access to parameters, temporaries, instance and global variables. A name conflict appears when two variables that should be visible in a method share the same name. In a conflict scenario the language developer needs to device a resolution strategy for these problems, to avoid ambiguities.

For example, consider a method `m:` that has an argument named `integer` and defines a temporary variable also named `integer`. 
How should values of that name be resolved? 
How are assignments resolved? 
A conflict resolution strategy provides a set of deterministic rules to answer these questions and let developers understand what their program do in a non-ambiguous way.

A first simple strategy to avoid conflicts is preventing them at construction time. 
That is, the language should not allow developers to define variables if they generate a name conflict. 
For example, a method should not be able to define a temporary variable with the same name as an instance variable of its class. 
Usually these validations are done once at compile time, and programs that do not follow this rules are rejected.

Another strategy to solve this problem is to allow shadowing. 
That is, we give each variable in our program a priority, and then the actual variable to read or write is looked-up using this priority system.
Typically priorities in these schemas are modelled as lexical scopes. 
Lexical scoping divides a program in a hierarchy of scopes. 
Each scope defines variables and all but the top level scope have a parent scope. 
For example, the top level scope defines global variables, the class scope defines the instance variables, the method scope defines the parameters and temporaries. 
In this way, variable visibility can be defined in terms of a scope: the variables visible in a scope are those defined in the scope or in the parents of the scope. 
Moreover, scoping also gives a conflict resolution strategy: variables defined closer to the current scope in the scope hierarchy have more priority than those defined higher in the scope hierarchy.

### About Return


As a reader you may wonder why we did not do anything for return expression and this is an interesting question. 
Up to now interpreting a return is just return the value of the interpretation of the return expression. 
In fact up until now a method execution has a single path of execution: it means that the complete method body
should be executed and that we did not introduce different condition control flow. 
This is when we will introduce block closure and conditional control flow that we will have to revisit the interpretation of return. 

### Conclusion


Supporting message sends and in particular method execution is the core of the computation in an object-oriented language and this is what this chapter covered.

Implementing messages implied modelling the call-stack and keeping it balanced on method returns.
We have seen that a call-stack is made up of frames, each frame representing the activation of a method: it stores the method, receiver, arguments, and temporaries of the method that is executing. When a message takes place, receiver and arguments are evluated in order from left to right, a new frame is created and all values are stored in the frame.

