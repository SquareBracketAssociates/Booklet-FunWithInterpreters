## Better Debugging




## Block Closures

In this chapter, we extend our evaluator to manage block closures. Block closures, also named lexical closures, or just blocks in Pharo, are an important concept in most modern programming languages, including Pharo. A lexical closure is an anonymous function that captures its definition environment.

This chapter starts by explaining what blocks are and how they are evaluated.
Block evaluation, being a core part of the language definition, is a service that is requested to the evaluator/interpreter through a primitive.

We then dive into the lexical capture feature of blocks: when a block closure is created, it captures its defining context, namely its enclosing context (i.e., the visible variables that the block can see). This makes blocks able to read and write not only their own temporary variables but also all the variables accessible to its enclosing context and to maintain such a link even when passed around.
Finally, we implement non-local returns: return instructions that return to the block _definition context_ instead of the current one. 

Non-local returns are really important in Pharo since they are used to express early returns (the fact that the execution of a method can be stopped at a given point), a frequent language feature similar to `break` statements in other languages. Without non-local return, it would be difficult to quit the current execution.

### Closures: Control Flow Building Bricks

Closures allow developers to abstract general algorithms from their particular details. For example, a sorting algorithm can be separated from its sorting criteria by making the sorting criteria a block closure passed as an argument to it. This allows developers to have the sorting algorithm defined and tested in a single place, and being able to reuse it with multiple criteria in different contexts.

Lexical closures are at the center of the Pharo language, because Pharo leverages closures to define its _control-flow_ instructions: conditionals, iterations, and early returns. 

The following example of factorial illustrates this.

```
Integer >> slowFactorial
	self > 0
		ifTrue: [ ^ self * (self - 1) slowFactorial ].
	self = 0
		ifTrue: [ ^ 1 ].
	self error: 'Not valid for negative integers'
```


This means that implementing block closures is enough to support all kinds of control flow statements in Pharo.
Moreover, Pharo libraries make use of block closures to define library-specific control flow instructions, such as the `do:` and `select:` messages understood by collections. Pharo developers often use closures in the Domain Specific Languages that they design.

Developers are also encouraged to define their own control flow statements to hide implementation details of their libraries from their users.


### Block

In Pharo, blocks are _lexical_ closures i.e., basically anonymous functions without a name that capture the environment in which they are defined.


When a block _expression_ is executed `[ 1+2 ]`, the instructions inside the block definition are not executed.
Instead, a block object is created, containing those instructions.

```
[ 1 + 2 ]
>>> [ 1 + 2 ]
```

The execution of those instructions is delayed until we send the message `value` to the block object.

```
[ 1 + 2 ] value
>>> 3
```

The evaluation of a closure s different from the evaluation of its execution
From an interpreter point of view, evaluating a block node should return a block object, and the method `value` requires a primitive to request the interpreter the block's execution. This means that we need a way to represent a closure object in our evaluator, and that closure should store the code it is supposed to evaluate later when receiving the `value` message.


### Representing a Block Closure

Let us define the class `CBlock` to represent a block.
It has an instance variable `code` to hold the block's AST, instance of the `OCBlockNode` class. We omit the accessors.

Notice that we do not use the existing `BlockClosure` class from Pharo, since this class is tied up with the Pharo bytecode. For the sake of simplicity, we will not reconciliate bytecode and AST implementations, meaning that we need our own AST-based block implementation.

```
Object << #CBlock
	slots: { #code };
	package: 'Champollion-Core'
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
CInterpretable >> returnBlock
	^ [ 'a' . 5 ]
```

```
CInterperter >> testBlockDefinition

	| bk |
	bk := (self executeSelector: #returnBlock).
	self 
		assert: bk class 
		equals: CBlock.
		
	self 
		assert: bk code class 
		equals: OCBlockNode 
```



### Block Execution

In Pharo, when a method does not have a return statement, it returns `self`. 
The compiler basically adds it during its compilation.

This is different for a block:  a block without return statement implicitly returns the result of its last expression. 

Let us write a testing scenario for this case: evaluating the following block should return `5` as it is its last expression. 

```
CInterpretable >> returnBlockValue
	^ [ 'a' . 5 ] value
```


```
CInterpreterTest >> testBlockValueIsLastStatementValue
	self assert: (self executeSelector: #returnBlockValue) equals: 5
```

This test should fail because the method `value` is special for blocks.

Closures are executed when they receive the message `value` or one of its variants such as value `value:`, `value:value:`...
On the reception of such messages, their bodies should be executed. Theses messages are defined in Pharo as primitives as shown in the following method definition: 

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
For this we define a method value on the `CBlock` and tag it as a primitive. Then we declare a new primitive
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

```
CInterpreter >> primitiveBlockValue
	^ self visitNode: self receiver code body
```
Remember that primitives are executed in their own frame already, so the block's body will share the frame 
created for the primitive method.

And we declare the primitive.

```
CInterpreter >> initializePrimitiveTable
   ...
  primitives at: 207 put: #primitiveBlockValue.
   ...
```

So far we implemented only a simple version of closures. 
We will extend it in the following sections. 


### Improving Scope `printOn:`

We take a moment to improve the printing of scopes since it will help you to navigate through the scopes.

```
CInstanceScope >> printOn: aStream
	super printOn: aStream.
	receiver ifNotNil: [ 
			aStream nextPutAll: ' inst: '.
			receiver printOn: aStream ]
```

We also that the method `currentMethod` to the method scope. 

```
CMethodScope >> currentMethod
	^ variables at: #___method
```

```
CMethodScope >> printOn: aStream
	super printOn: aStream.
	receiver ifNotNil: [ 
			aStream nextPutAll: ' rec: '.
			receiver printOn: aStream.
			aStream nextPutAll: ' selector: #' , self currentMethod selector ]
```




### Closure and Variables

In Pharo, blocks are _lexical_ closures i.e., basically anonymous functions without a name that capture the environment in which they are defined.

A block can have its own temporary variables. Such variables are initialized during each block execution and are local to the block. We will see later how such variables are kept. Now the question we want to make clear is what happens when a block refers to other (non-local) variables. A block will close over the external variables it uses. It means that even if the block is executed later in an environment that does not lexically contain the variables used by a block, the block will still have access to the variables during its execution. 

In Pharo, private variables (such as self, instance variables, method temporaries, and arguments) are lexically scoped: an expression in a method can access the variables visible from that method, but the same expression put in another method or class cannot access the same variables because they are not in the scope of the expression (i.e., visible from the expression). 

SD: can we say that this is a stack frame (I mean the Pharo block refers to a context?)
At runtime, the variables that a block can access are bound (get a value associated with them) in _the context_  in which the block that contains them is _defined_, rather than the context in which the block is evaluated. It means that a block, when evaluated somewhere else, can access variables that were in its scope (visible to the block) when the block was _created_. Traditionally, the context in which a block is defined is named the _block home context_.

The block home context represents a particular point of execution (since this is a program execution that created the block in the first place), therefore, this notion of block home context is represented by an object that represents program execution: a context object in Pharo. In essence, a context (called a stack frame or activation record in other languages) represents information about the current evaluation step, such as the context from which the current one is executed, the next bytecode to be executed, and temporary variable values. A context is a Pharo execution stack element.

A block is an anonymous function created inside a context (an object that represents a point in the execution).

Let us start with a simple management of closure temporaries.




### Closure Temporaries

Our simplified closure implementation does not yet have support for closure variables (temporaries).
Indeed, a closure such as the following will fail with an interpreter failure because `temp` is not defined in the frame.

```
[ | temp | temp ] value
```

To solve this, we need to declare all block temporaries when executing the block, as we did previously for methods.
As a first attempt to make our test green, let's declare block temporaries once the block is activated:

```
CInterpreter >> primitiveBlockValue
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

The handling of temporaries in `primitiveBlockValue` is very similar to a sequence of messages we wrote when activating a normal method in method `execute:withReceiver:andArguments:`. In particular, in the `manageArgumentsTemps:of:` method.

```
CInterpreter >> primitiveBlockValue
	| blockCode |
	blockCode := self receiver.
	blockCode code temporaryNames do: [ :e | self tempAt: e put: nil ].
	^ self visitNode: blockCode code body
```

```
CInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection
	...
	self manageArgumentsTemps: aCollection of: anAST.
	...
```

```
CInterpreter >> manageArgumentsTemps: aCollection of: anAST
	anAST arguments
		with: aCollection
		do: [ :arg :value | self tempAt: arg name put: value ].
	anAST temporaryNames do: [ :tempName |
		self tempAt: tempName put: nil ]
```

We solve this repetition by moving temporary initialization to the `visitSequenceNode:` method, since both method nodes and block nodes have sequence nodes inside them.

```
CInterpreter >> visitSequenceNode: aSequenceNode
	"Visit the sequence and return the result of the last statement.
	Initialize the sequence temporaries to nil."

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

All the tests should pass and the resulting code is nicer and simpler. This is a clear indication that the refactoring was a good move.


### Capturing the Defining Context

In this section, we evolve our closure execution infrastructure to support closure temporaries and to provide access to the enclosing environment.
 
As we stated before, a closure is not just a function. It is a function that captures the context (set of variables that it can access) at the time of its definition. Block closures capture their _defining_ context or enclosing context, i.e., the context in which they are created.

Blocks are able to read and write their own temporary variables, but also all the variables accessible to their enclosing context. For example, a temporary variable is accessible during the block definition will be accessible during the block execution.

The defining execution context gives the closure access to that context's receiver, arguments, and temporaries.

Pay attention, it is a common mistake to think that the captured context is the caller context, and not the defining context.

In the previous example the distinction is not done because the definition context was the caller one. 
However, as soon as we work on more complex scenarios, where blocks are sent as arguments of methods, or stored in temporary variables, this does not hold anymore.

Let us check the following example.
A variable is looked up in the block definition context. We define two methods `setVariableAndDefineBlock` and `setVariableAndDefineBlock:`. The first one defines a variable `t` and sets it to 42, and a block `[ t ]`.
The second one defines a new variable with the same name and executes a block defined elsewhere.

```
CInterpretable >> setVariableAndDefineBlock
	
	| t |
	t := 42.
	^ self evaluateBlock: [ t ]
	
CInterpretable >> evaluateBlock: aBlock
	| t |
	t := nil.
	^ aBlock value

CInterpretable new setVariableAndDefineBlock 
>>> 42
````


Executing the `CInterpretable new setVariableAndDefineBlock` expression prints 42 in the Transcript (message `traceCr`). 

- The value of the temporary variable `t` defined in the `setVariableAndDefineBlock` method is the one used rather than the one defined inside the method `evaluateBlock:` even if the block is evaluated during the execution of this method.
- The variable `t` is  looked up in the context of the block creation (context created during the execution of the method `setVariableAndDefineBlock` and not in the context of the block evaluation (method `evaluateBlock:`).

![Non-local variables are looked up the method activation context where the block was _created_ and not where it is _evaluated_.%width=80&anchor=fig:variable](./figures/variable.pdf)

Let's look at it in detail. Figure *@fig:variable@* shows the execution of the expression `CInterpretable new setVariableAndDefineBlock`. 

- During the execution of method `setVariableAndDefineBlock`, a variable `t` is allocated in the current context and it is assigned 42. Then a block is created and this block refers to the method activation context - which holds temporary variables (Step 1). 
-  The method `evaluateBlock:` allocates its own local variable `t` with the same name than the one in the block. This is not this variable, however, that is used when the block is evaluated. While executing the method `evaluateBlock:` the block is evaluated (Step 2), during the execution of the expression `t traceCr` the non-local variable `t` is looked up in the `home context` of the block \ie the method context that _created_ the block and not the context of the currently executed method.





### Capture of `self`

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

Verify that this test fails.

To make this test pass, we need to implement two different things in the evaluator.
- First we need to capture the defining context at block _definition_ time in the method `visitBlockNode:`. 
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
Note that this is this context that will be used when a block accesses temporaries and arguments.
the block is created. 

```
CInterpreter >> visitBlockNode: aOCBlockNode
	^ CBlock new
		code: aOCBlockNode;
		definingContext: self topFrame;
		yourself
```

### Accessing the Captured Receiver

Resolving the block variables is a trickier case, as it can be resolved in many different ways.
For now, we choose to set the correct values in the current frame upon block activation and shadow the possible ones that would be defined in the definition context.

The first variable we want to provide access to from a block is `self` which is the original receiver
of the method _at the time the block was created_. 

```
CInterpreter >> primitiveBlockValue
	| theBlock |
	theBlock := self receiver.
	self receiver: theBlock definingContext receiver.
	^ self visitNode: theBlock code body
```

The previous method is worth explaining

- First we grab the block itself. It is simple since the method `primitiveBlockValue` is executed during the evaluation of the message `value` sent to a block. Therefore `self receiver` returns the block currently executed.
- Second remember that `self` in a block refers to the receiver of the method at the time the block was created. So we need to set as receiver the receiver that we found in the context of the block creation. This is what `theBlock definingContext receiver` is returning.
- Finally we evaluate the block body.

To make this works we need to define the method `receiver:`. 

```
CInterpreter >> receiver: aValue
	^ self tempAt: #self put: aValue
```


Note that in the `primitiveBlockValue` we use the frame of the message `value` execution. 
The evaluation of the block body uses this frame. When the evaluation is done such a frame is simply 
popped as any other method executions (See `executeMethod:withReceiver:andArguments:`), therefore
there are no worries to be made when we change the value of receiver. 
`receiver` is not a state of the interpreter but refers to the current frame. 

Now that we can correctly resolve the receiver, instance variable reads and writes should work properly too.
We leave it as an exercise for the reader to verify their correctness.



### Looking up Temporaries in Lexical Contexts

A problem we have not solved yet involves the reads and writes of temporary variables that are not part of the current frame.
This is the case when a block tries to access a temporary of a parent lexical scope, such as another surrounding scope, or the home method. Our next scenario should check that blocks can correctly read and write temporaries of their enclosing contexts.

The following method `readEnclosingTemporary` shows that the block `[ temp + temp ]` should be able to access the temporary variable `temp` defined in the method `readEnclosingTemporary` (See Figure *@nonlocalvariable@*). Note that such temporary could have been defined in another method and passed as an argument to another method.

```
CInterpretable >> readEnclosingTemporary
	| temp |
	temp := 1.
	^ [ temp + temp ] value.
```

Figure *@nonlocalvariable@* illustrates that 

![Reading `temp` in `[ temp + temp ].` i.e., blocks should access temporaries defined in the definition context. %anchor=nonlocalvariable ](./figures/ReadNonLocalTemp.pdf)

To validate this scenario the following test make sures that the correct behavior is implemented. 

```
CInterpreterTest >> testReadEnclosingTemporary 
	self 
		assert: (self executeSelector: #readEnclosingTemporary) 
		equals: 2
```

The following scenario is also interesting. It shows that each block has a specific defining context that different temporary variables and that the second context is nested into the first one (see Figures *@nonlocalvariable2@* and *@nonlocalvariableScreen@*).

![ Temporaries  in `[ tempMethod + tempBlock ]`. %anchor=nonlocalvariable2 ](./figures/ReadNonLocalTemp2.pdf)

```
CInterpretable >> readDoublyNestedEnclosingTemporary
	| tempMethod |
	tempMethod := 1.
	^ [ | tempBlock |
		tempBlock := 2.
		[ tempMethod + tempBlock ] value ] value
```

![Browsing frames from `[ tempMethod + tempBlock ]`. %anchor=nonlocalvariableScreen ](./figures/ReadDoublyScreenshot.png)

### Temporary Lookup Implementation

This scenario is resolved by implementing a temporary variable lookup in the block's _defining_ context.
Of course, a block could be defined inside another's block context, so our lookup needs to be lookup through the complete context chain.
The lookup should stop when the current lookup context does not have a defining context i.e., it is a method and not a block.

To simplify temporary variable lookup we define first a helper method `lookupFrameDefiningTemporary:` that returns the frame in which a temporary is defined. 

We defined the `includesVariableName:` little helper method as follows: 

```
CMethodScope >> includesVariableName: aName
	^ variables includesKey: aName
```

```
CInterpreter >> lookupFrameDefiningTemporary: aName
	| currentLookupFrame |
	currentLookupFrame := self topFrame.
	[ currentLookupFrame includesVariableName: aName ]
		whileFalse: [ currentLookupFrame := currentLookupFrame at: #__definingContext ].
	^ currentLookupFrame
```

This method returns a frame. It has to walk from a frame to its defining frame up to a method. 
However, so far the only object in our design knowing the defining frame is the block (via its instance variable `definingContext`), and we do not have any way to access a block from its frame.

One possibility is to store a block reference in its frame when it is activated, and then go from a frame to its block to its defining frame and continue the lookup. Another possibility, which we will implement, is to directly store the defining context in the frame when the block is activated.
We enhance the definition of `primitiveBlockValue` to store the context in which the block is defined. 

```
CInterpreter >> primitiveBlockValue
	| theBlock |
	theBlock := self receiver.
	self receiver: theBlock definingContext receiver.
	self tempAt: #__definingContext put: theBlock definingContext.
	^ self visitNode: theBlock code body
```


Finally we need to redefine temporary reads and writes.
Temporary reads need to lookup the frame where the variable is defined and read the value from it.
This is what what the method `visitVariableNode:` does. 
- First we only look for temporary in a defining context if such defining context exist. This makes sure that in absence of block we can still look for temporary variables. In such case we look in the scope method.
- Second, when a defining context is available we walk through the chain to find the corresponding context.

```
CInterpreter >> visitVariableNode: aVariableNode
	| name |
	name := aVariableNode name.
	(self topFrame includesVariableName: #__definingContext)
		 ifTrue: [
			| tempScope |
			tempScope := self lookupFrameDefiningTemporary: name.
			^ tempScope read: name ].	
	^ (self scopeDefining: name) read: name
```

Now that we can read variable from the defining context of a block we should make sure that writes (assignments) to such temporary variables is working too. 


### Write Temporary Support

Temporary writes are similar to read. We need to lookup the frame where the variable is defined and write the value to it.



```
CInterpretable >> increaseEnclosingTemporary
	| temp |
	temp := 0.
	[ temp := temp + 1 ] value.
	^ temp
```

The method `increaseEnclosingTemporary` is an example of such a situation: the block `[ temp := temp + 1 ]`  will access during its execution  the temporary variable that was defined outside of the block. 

Note that the execution of the block could happen in another method and still the block should be able to access the temporary variable `temp`.


In our test, the enclosing environment creates a temporary. The block reads that value and increases it by one.
When the block executes and returns, the value of its temporary should have been updated from 0 to 1.


```
CInterpreterTest >> testIncreaseEnclosingTemporary 
	self assert: (self executeSelector: #increaseEnclosingTemporary) equals: 1
```

```
CInterpreter >> visitAssignmentNode: anAssignmentNode
	| rightSide |
	rightSide := self visitNode: anAssignmentNode value.
	anAssignmentNode variable variable isTempVariable
		ifTrue: [ | definingFrame |
			definingFrame := self
				lookupFrameDefiningTemporary: anAssignmentNode variable name.
			definingFrame at: anAssignmentNode variable name put: rightSide ]
		ifFalse: [ anAssignmentNode variable variable 
					write: rightSide 
					to: self receiver ].
	^ rightSide
```








### Block Non-Local Return


We have seen so far that blocks implicitly return the value of their last expression. 
For example the method `lastExpression` will return 43.

```
CInterpretable >> lastExpression
	| tmp | 
	tmp := 1.  
	tmp := true ifTrue: [ tmp := 42. tmp := tmp + 1].
	^ tmp
```


Now this is a complete different story when a block contains an explicit return statement. 
Return statements, instead, break the execution of the defining method, namely the home method, and return from it.
For example, let's consider a method using `ifTrue:` to implement a guard which should stop the method execution if the guard fails:

```
CInterpretable >> methodWithGuard
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



## Todo 

- make sure that methodScope does not have a separate field for receiver but use the frame in previous chapter.
