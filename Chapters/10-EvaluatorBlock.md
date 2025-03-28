## Block Closures and Control Flow Statements


In this chapter we will extend our evaluator to manage block closures. Block closures, also named lexical closures, or just blocks in Pharo, are an important concept in most modern programming languages, including Pharo. A lexical closure is an anonymous function that captures its definition environment.

This chapter starts by explaining what blocks are and how they are evaluated.
Block evaluation, being a core part of the language definition, is a service that is requested to the evaluator/interpreter through a primitive.
We then dive into the lexical capture feature of blocks: when a block closure is created, it captures its defining context, namely its enclosing context (i.e., the visible variables that the block can see). This makes blocks able to read and write not only its own temporary variables but also all the variables accessible to its enclosing context and to maintain such a link even when passed around.
Finally, we implement non-local returns: return instructions that return to the block _definition context_ instead of the current one. 
Non-local returns are really important in Pharo since they are used to express early returns (the fact that the execution of a method can be stopped at a given point) a frequent language feature similar to `break` statements in other languages.
Without non-local return it would difficult to quit the current execution.

### Closures

Closures allow developers to abstract general algorithms from their particular details. For example, a sorting algorithm can be separated from its sorting criteria by making the sorting criteria a block closure passed as argument to it. This allows developers to have the sorting algorithm defined and tested in a single place, and being able to reuse it with multiple criterion in different contexts.

In Pharo, blocks are _lexical_ closures i.e., basically functions without a name that capture the environment in which they are defined. 
Lexical closures are at the center of the Pharo language, because Pharo leverages closures to define its _control-flow_ instructions: conditionals, iterations, and early returns. This means that implementing block closures is enough to support all kind of control flow statements in Pharo.
Moreover, Pharo libraries make usage of block closures to define library-specific control flow instructions, such as the `do:` and `select:` messages understood by collections. Pharo developers often use closures in the Domain Specific languages that they design. 
Developers are also encouraged to define their own control flow statements, to hide implementation details of their libraries from their users.

### Representing a Block Closure

When a block expression is executed `[ 1+2 ]`, the instructions inside the block definition are not executed.
Instead, a block object is created, containing those instructions.
The execution of those instructions is delayed until we send the message `value` to the block object.

This means that from the evaluator point of view, the evaluation of the closure will be different from the evaluation of its execution. Evaluating a block node will return a block object, and the method `value` will require a primitive to request the interpreter the block's execution. This means that we need a way to represent a closure object in our evaluator, and that closure should store the code it is supposed to evaluate later when receiving the `value` message.

Let us define the class `CBlock` to represent a block.
It has an instance variable `code` to hold the block's AST, instance of the `BlockNode` class.
Notice that we do not use the existing `BlockClosure` class from Pharo, since this class is tied up with the Pharo bytecode.

For the sake of simplicity, we will not reconciliate bytecode and AST implementations, meaning that we need our own AST-based block implementation.

```
Object << #CBlock
	slots: { #code };
	package: 'Champollion-Core'
```

```
CBlock >> code: aBlockNode 
	code := aBlockNode
```


```
CBlock >> code
	^ code
```





#### Block Definition


When the interpreter encounters a block node, it creates a block object for it.
We define the method `visitBlockNode:` as follows: 

```
CInterpreter >> visitBlockNode: aBlockNode 
	^ CBlock new
		code: aBlockNode;
		yourself
```

We add a simple test to verify the correct definition of block objects.


```
CInterperter >> testBlockDefinition

	| bk |
	bk := (self executeSelector: #returnBlock).
	self 
		assert: bk class 
		equals: CBlock.
		
	self 
		assert: bk code class 
		equals: RBBlockNode 
```

```
CInterpretable >> returnBlock
	^ [ 1 . 5 ]
```

### Block Execution

In Pharo, when a method does not have a return statement, it returns `self`. 
The compiler basically adds it during its compilation.

This is different for a block:  a block without return statement implicitly returns the result of its last expression. 

Let us write a testing scenario for this case: evaluating the following block should return `5` as it is its last expression. 

```
CInterpretable >> returnBlockValue
	^ [ 1 . 5 ] value
```


```
CInterpreterTest >> testBlockValueIsLastStatementValue
	self assert: (self executeSelector: #returnBlockValue) equals: 5
```


Closures are executed when they receive the message `value` or one of its variants such as value `value:`, `value:value:`...
On the reception of such messages, their bodies should be executed. Theses messages are defined in Pharo as primitives as shown in the following: 

```
BlockClosure >> value
	"Activate the receiver, creating a closure activation (MethodContext)
	whose closure is the receiver and whose caller is the sender of this
	message. Supply the copied values to the activation as its copied
	temps. Primitive. Essential."
	<primitive: 207>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	^self primitiveFailed
```


### Block Execution Implementation

We follow the design of Pharo and we add a new primitive responsible for the block body execution. 
For this we define a method value on the CBlock and tag it as a primitive. Then we declare a new primitive
in the interpreter table and finally we define a first version of the primitive corresponding to the value execution. 

We define the method `value` on the class `CBlock` as a primitive number 207. 

```
CBlock >> value
	<primitive: 207>
	"If the fallback code executes it means that block evaluation failed.
	Return nil for now in such case."
	^ nil
```


We now need to implement the new primitive in the evaluator.
A first version of it is to just visit the body of the block's code.
Remember that primitives are executed in their own frame already, so the block's body will share the frame 
created for the primitive method.

```
CInterpreter >> initializePrimitiveTable
  ...
  primitives at: 207 put: #primitiveBlockValue.
  ...

CInterpreter >> primitiveBlockValue
	^ self visitNode: self receiver code body
```


So far we implemented only a simple version of closures. 
We will extend it in the following sections. 





### Closure Temporaries

Our simplified closure implementation does not yet have support for closure temporaries.
Indeed, a closure such as the following will fail with an interpreter failure because `temp` is not defined in the frame.

```
[ | temp | temp ] value
```


To solve this we need to declare all block temporaries when activating the block, as we did previously for methods.
As a first attempt to make our test green, let's declare block temporaries once the block is activated:

```
CInterpreter >> primitiveBlockValue
	"Initialize all temporaries to nil"
	| blockCode |
	blockCode := self receiver code.
	blockCode temporaryNames do: [ :e | self tempAt: e put: nil ].
	^ self visitNode: blockCode body
```


We are now able to execute the following expression

```
[ | a b |
	a := 1.
	b := 2.
	a + b ] value
```

Let us define the following test:

```
CInterpretable >> returnBlockWithVariableValue

	^ [ | a b |
		a := 1.
		b := 2.
		a + b ] value
```

```
CInterpreterTest >> testBlockValueWithTemporariesValue
	self 
		assert: (self executeSelector: #returnBlockWithVariableValue) 
		equals: 3
```


### Removing Logic Repetition


The handling of temporaries in `primitiveBlockValue` is very similary to a sequence of messages we wrote when activating a normal method in method `execute:withReceiver:andArguments:`. In particular in the `manageArgumentsTemps:of:` method.

```
CInterpreter >> primitiveBlockValue	"Receiver is an instance of CBlock"		| blockCode |	blockCode := self receiver.	blockCode code temporaryNames do: [ :e | self tempAt: e put: nil ].	self receiver: blockCode definingContext receiver.	^ self visitNode: blockCode code body
```


```
CInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection

	...
	self manageArgumentsTemps: aCollection of: anAST.
	...
```

```
CInterpreter >>manageArgumentsTemps: aCollection of: anAST

	anAST arguments
		with: aCollection
		do: [ :arg :value | self tempAt: arg name put: value ].
	anAST temporaryNames do: [ :tempName |
		self tempAt: tempName put: nil ]
```

We solve this repetition by moving temporary initialization to the `visitSequenceNode:` method, since both method nodes and block nodes have sequence nodes inside them.

```
CInterpreter >> visitSequenceNode: aSequenceNode
	"Initialize all temporaries to nil"

	aSequenceNode temporaryNames do: [ :e | self tempAt: e put: nil ].

	"Visit all but the last statement without caring about the result"
	aSequenceNode statements allButLast 
		do: [ :each | self visitNode: each ].
	"Return the result of visiting the last statement"
	^ self visitNode: aSequenceNode statements last
```

We then rewrite `primitiveBlockValue` as follows:

```
CInterpreter >> primitiveBlockValue
	^ self visitNode: self receiver code body
```

We remove the temporary management from `manageArgumentsTemps:of:` and rename it. 
```
CInterpreter >>manageArguments: aCollection of: anAST

	anAST arguments
		with: aCollection
		do: [ :arg :value | self tempAt: arg name put: value ].
```

The resulting code is nicer and simpler. This is a clear indication that the refactoring was a good move.


### Capturing the Defining Context

Stef Here

As we stated before, a closure is not just a function, it is a function that captures the context (set of variables that it can access) at the time of its definition. Block closures capture their _defining_ context or enclosing context, i.e., the context in which they are created.
Blocks are able to read and write their own temporary variables, but also all the variables accessible to its enclosing context such as a temporary variable accessible during the block definition. In this section, we evolve our closure execution infrastructure to support closure temporaries and to provide access to the enclosing environment.

The defining execution context gives the closure access to that context's receiver, arguments, and temporaries.

Pay attention, it is a common mistake to think that the captured context is the caller context, and not the defining context.
In the example above the distinction is not done because the definition context was the caller one. 
However, as soon as we work on more complex scenarios, where blocks are sent as arguments of methods, or stored in temporary variables, this does not hold anymore.


### self Capture

A first scenario to check that our block properly captures the defining context is to evaluate `self` inside a block.
In our current design, the receiver specified in the block's frame is the block itself.
Indeed, the expression `[ ... ] value` is a message send where the block is the message receiver and `value` is the message. 
However, the `self` variable should be bound to the instance of `CHInterpretable`.

```
CInterpretable >> readSelfInBlock
	^ [ self ] value

CInterpreterTest >> testReadSelfInBlock
	self assert: (self executeSelector: #readSelfInBlock) equals: receiver
```

To make this test pass, we need to implement two different things in the evaluator.
- First we need to capture the defining context at block _definition_ time in `visitBlockNode:`. 
- Second we need to use _that_ captured context to resolve variables.

### Capture Implementation 

Capturing the defining context is as simple as storing the current `topFrame` at the moment of the method creation. 

We extend `CBlock` with a `definingContext` instance variable and corresponding accessors (omitted here after).

```
Object << #CBlock
	slots: { #code . #definingContext };
	package: 'Champollion'
```

Since a block is created when the block node is visited we extend the previous block creation to store 
the current context at this moment.
Note that this is this context that will be let block access to the temporaries and arguments it uses at the moment 
the block is created. 

```
CInterpreter >> visitBlockNode: aRBBlockNode
	^ CBlock new
		code: aRBBlockNode;
		definingContext: self topFrame;
		yourself
```

### Accessing Captured Receiver

Resolving the block variables is a trickier case, as it can be resolved in many different ways.
For now, we choose to set the correct values in the current frame upon block activation and shadow the possible ones that would be defined in the definition context.

The first variable we want to provide access to from a block is `self` which is the original receiver
of the method _at the time the block was created_. 

The following method is worth explaining

- First we grab the block itself. It is simple since the method `primitiveBlockValue` is executed during the evaluation of the message `value` sent to a block. Therefore `self receiver` returns the block currently executed.
- Second remember that `self` in a block refers to the receiver of the method at the time the block was created. So we need to set as receiver the receiver that we found in the context of the block creation. This is what `theBlock definingContext receiver` is returning.
- Finally we evaluate the block body.


```
CInterpreter >> primitiveBlockValue
	| theBlock |
	theBlock := self receiver.
	self receiver: theBlock definingContext receiver.
	^ self visitNode: theBlock code body
```

```
CInterpreter >> receiver: aValue
	^ self tempAt: #self put: aValue
```


Note that in the `primitiveBlockValue` we use the frame of message `value` execution. 
The evaluation of the block body uses this frame. When the evaluation is done such frame is simply 
popped as any other method executions (See `executeMethod:withReceiver:andArguments:`), therefore
there are no worries to be made when we change the value of receiver. 
`receiver` is not a state of the interpreter but refers to the current frame. 

Now that we can correctly resolve the receiver, instance variable reads and writes should work properly too.
We leave it as an exercise for the reader to verify their correctness.



### Looking up Temporaries in Lexical Contexts


A problem we have not solved yet involves the reads and writes of temporary variables that are not part of the current frame.
This is the case when a block tries to access a temporary of a parent lexical scope, such as another surrounding scope, or the home method.
The method `increaseEnclosingTemporary` is an example of such a situation: the block `[ temp := temp + 1 ]`  will access during its execution 
the temporary variable that was defined outside of the block. 
Note that the execution of the block could happen in another method and still be block should be able to access the temporary variable `temp`.

Our next scenario checks that blocks can correctly read and write temporaries of their enclosing contexts.
In our test, the enclosing environment creates a temporary. The block reads that value and increases it by one.
When the block executes and returns, the value of its temporary should have been updated from 0 to 1.

```
CHInterpretable >> increaseEnclosingTemporary [
	| temp |
	temp := 0.
	[ temp := temp + 1 ] value.
	^ temp
]

CHInterpreterTest >> testIncreaseEnclosingTemporary [
	self assert: (self executeSelector: #increaseEnclosingTemporary) equals: 1
]
```


!!note should add some diagrams here

This scenario is resolved by implementing a temporary variable lookup in the block's _defining_ context.
Of course, a block could be defined inside another's block context, so our lookup needs to be lookup through the complete context chain.
The lookup should stop when the current lookup context does not have a defining context i.e., it is a method and not a block.

To simplify temporary variable lookup we define first a helper method `lookupFrameDefiningTemporary:` that returns the frame in which a temporary is defined. 
This method returns a frame. It has to walk from a frame to its defining frame up to a method. 
However, so far the only object in our design knowing the defining frame is the block \(via its instance variable `definingContext`\), and we do not have any way to access a block from its frame.

One possibility is to store a block reference in its frame when it is activated, and then go from a frame to its block to its defining frame and continue the lookup. Another possibility, which we will implement, is to directly store the defining context in the frame when the block is activated.

```
CHInterpreter >> primitiveBlockValue [
	| theBlock |
	theBlock := self receiver.
	self receiver: (theBlock definingContext at: #self).
	self tempAt: #__definingContext put: theBlock definingContext.
	^ self visitNode: theBlock code body
]
```


```
CHInterpreter >> lookupFrameDefiningTemporary: aName [
	| currentLookupFrame |
	currentLookupFrame := self topFrame.
	[ currentLookupFrame includesKey: aName ]
		whileFalse: [ currentLookupFrame := currentLookupFrame at: #__definingContext ].
	^ currentLookupFrame
]
```


!!note should add some diagrams here

Now we need to redefine temporary reads and writes.
Temporary reads need to lookup the frame where the variable is defined and read the value from it.
This is what what the method `visitTemporaryNode:` does.


```
CHInterpreter >> visitTemporaryNode: aTemporaryNode [
	| definingFrame |
	definingFrame := self lookupFrameDefiningTemporary: aTemporaryNode name.
	^ definingFrame at: aTemporaryNode name
]
```


Temporary writes are similar to read. We need to lookup the frame where the variable is defined and write the value to it.

```
CHInterpreter >> visitAssignmentNode: aRBAssignmentNode [
	| rightSide |
	rightSide := self visitNode: aRBAssignmentNode value.
	aRBAssignmentNode variable variable isTempVariable
		ifTrue: [ | definingFrame |
			definingFrame := self
				lookupFrameDefiningTemporary: aRBAssignmentNode variable name.
			definingFrame at: aRBAssignmentNode variable name put: rightSide ]
		ifFalse: [ aRBAssignmentNode variable variable 
					write: rightSide 
					to: self receiver ].
	^ rightSide
]
```


### Block Non-Local Return


We have seen so far that blocks implicitly return the value of their last expression. 
For example the method `lastExpression` will return 43.
```
CHInterpretable >> lastExpression
	| tmp | 
	tmp := 1.  
	tmp := true ifTrue: [ tmp := 42. tmp := tmp + 1].
	^ tmp
```


Now this is a complete different story when a block contains an explicit return statement. 
Return statements, instead, break the execution of the defining method, namely the home method, and return from it.
For example, let's consider a method using `ifTrue:` to implement a guard which should stop the method execution if the guard fails:

```
CHInterpretable >> methodWithGuard
	true ifTrue: [ ^ nil ].
	^ self doSomethingExpensive
```


!!note put a figure here to show the stack, the blocks, their relationships.

When executing this method, the message `doSomethingExpensive` will never be executed. The execution of the method `methodWithGuard` will be stopped by the return statement in the block `[^ nil]`.

More precisely, the block is not activated by `methodWithGuard`. `methodWithGuard` executes the message `ifTrue:` which in turn activates the `[^ nil]`. Still, this block knows the context of `methodWithGuard` as its defining context.
When the block executes, the return statement should not return `nil` to the `ifTrue:` context: it should return _from_  `methodWithGuard` with the `nil` value, as if it was the return value of the method. 
Because of this, we call such return inside blocks "non-local returns", because they return from a non-local context, its home context.

The block may have been passed around, when the block executes a return statement, it will return from the method that created the block. 
We say that the execution quits the home context of the block \(the context of the method that defined it\).

To implement non-local returns, we will first start by defining a new helper method: `homeFrameOf:` that returns the home frame of a frame. 
The home frame is the frame that has a defining context. 
Note that the home frame of a normal method frame is itself.

```
CHInterpreter >> homeFrame [
	| currentLookupFrame |
	currentLookupFrame := self topFrame.
	[ currentLookupFrame includesKey: #__definingContext ]
		whileTrue: [ currentLookupFrame := currentLookupFrame at: #__definingContext ].
	^ currentLookupFrame
]
```


!!note add a diagram

A simple way to implement non-local returns in Pharo is by using exceptions: exceptions unwind automatically the call-stack, thus short-circuiting the execution of all methods automatically.

We define a new exception called `CHReturn`. It refers to the home frame and a value.
```
Error subclass: #CHReturn
	instanceVariableNames: 'value homeFrame'
	classVariableNames: ''
	package: 'Champollion-Core'
```


```
CHReturn >> homeFrame [
  ^ homeFrame
]
```


```
CHReturn >> homeFrame: aFrame [
  homeFrame := aFrame
]
```

```
CHReturn >> value [
  ^ value
]
```

```
CHReturn >> value: aValue [
  value := aValue
]
```


When we activate a method we then need to prepare ourselves to catch the exception indicating a return, and only manage it if the return is targetting the current method's context:

SD: we should explain more the `returnFrom homeFrame = thisFrame`
```
CHInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection [
	| result thisFrame |	
	thisFrame := self pushNewFrame.

	self tempAt: #__method put: anAST.
	self tempAt: #self put: anObject.
	anAST arguments with: aCollection
		do: [ :arg :value | self tempAt: arg name put: value ].

	result := [ self visitNode: anAST ]
		on: CHReturn 				"A return statement was executed"
		do: [ :return | 
			return homeFrame = thisFrame
				ifTrue: [ return value ]
				ifFalse: [ return pass ] ].

	self popFrame.
	^ result
]
```



When we visit a return we raise a return exception and we pass the context.
SD: need more explanation.

```
CHInterpreter >> visitReturnNode: aReturnNode [
  CHReturn new
		value: (self visitNode: aReturnNode value);
		homeFrame: self homeFrame;
		signal
]
```




### Conclusion


In this chapter we have extended our evaluator with block closures. Our block closure implementation required adding a kind of object to our runtime, `CHBlock`, to represent blocks containing some AST. Then we refined our evaluator to define a block evaluation primitive, and correctly set up the lexical context. Our lexical context implementation gives blocks access to the defining context's receiver and temporaries. We then shown a first implementation of non-local returns, using exceptions to unwind the stack.
