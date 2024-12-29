## Message Arguments and temporaries
@cha:messageArgs

In the previous chapter, we extended the interpreter to support simple messages sent between different instances of the same class.
In this chapter, we will extend this message passing implementation to support parameters and temporaries. 
We will take advantage of the 


### Supporting Message Arguments

So far we have worked only with unary messages. Unary messages have no arguments, so the number of programs we can express with them only is limited. The next step towards having a full-blown interpreter is to support message arguments, which will open the door to support binary and keyword messages. From the evaluator's point of view, as well as from the AST point of view, we will not distinguish between unary, binary, and keyword messages. The parser already takes care of distinguishing them and handling their precedence. Indeed, message nodes in the AST are the same for all kinds of messages, they have a selector and a collection of argument nodes. 
Precedence is then modeled as relationships between the AST nodes.


In addition to simply passing arguments, the evaluator needs to care about evaluation order too. This is particularly important because Pharo is an imperative language where messages can trigger side effects. Evaluating two messages in one order may not have the same result as evaluating them in a different order. Arguments in Pharo are evaluated eagerly after evaluating the receiver expression, but before evaluating the message, from left to right. Once all expressions are evaluated, the resulting objects are sent as part of the message-send.


#### Initial Argument Setup

To implement some initial support for arguments, our first scenario is to simply send a message with an argument. We already one message with an argument: the `x:` setter. We define the method `changeCollaboratorWithArgument` which uses it.

```language=smalltalk
CInterpretable >> changeCollaboratorWithArgument
	collaborator x: 500
```


In the following test, we verify that the method evaluation effectively modifies the collaborator object 
as written in `changeCollaboratorWithArgument`, and not the initial receiver object.

```language=smalltalk
CInterpreterTest >> testArgumentAccess

	receiver x: 200.
	collaborator x: 300.

	self executeSelector: #changeCollaboratorWithArgument.

	self assert: receiver x equals: 200.
	self assert: collaborator x equals: 500
```

Since we have not implemented any support for arguments yet, this test should fail.

### Enhance Method Scope

The current method scope is limited to managing the receiver. It is not enough.
Method scopes should support variables as well as parsent scope. 

We add the `parentScope:` instance variable and its accessors as well as 

```
Object << #CMethodScope	slots: { #receiver . #parentScope . #variables };	package: 'Champollion'
```

To support variables, we add a dictionary 
to hold them and some utility methods.

```
CMethodScope >>initialize

	super initialize.
	variables := Dictionary new.
	
CMethodScope >> at: aKey
	^ variables at: aKey
	
CMethodScope >> at: aKey put: aValue
	variables at: aKey put: aValue
```

In addition, we add support for identifying the scope. 
We define the method `scopeDefining:` as follows:  It checks whether the looked up name is
a variable one.

```
CMethodScope >> scopeDefining: aString
	(variables includesKey: aString)
		ifTrue: [ ^ self ].

	^ self parentScope scopeDefining: aString
```

Note that the `scopeDefining:` delegates to its parent scope when the variable is not locally found. 

Finally we add a `read:` method using the variable implementation logic.

```
CMethodScope >> read: aString
	^ variables at: aString
```


### Support for Parameters/Arguments

Implementing argument support requires two main changes:

- On the caller side, we need to evaluate the arguments in the context of the caller method and then store those values in the new frame. 
- On the callee side, when an argument access is evaluated, those accesses will not re-evaluate the expressions in the caller. Instead, argument access will just read the variables pre-stored in the current frame.


Let's start with the second step, the callee side, and since all variable reads are concentrated on the scope lookup, we need to add the method scope in the scope chain.

#### Previous situation

Previously the method `execute:withReceiver:` was defined as follows:

```
execute: anAST withReceiver: anObject

	| result |
	self pushNewMethodFrame.
	self topFrame receiver: anObject.
	result := self visitNode: anAST.
	self popFrame.
	^ result
```

Where `pushNewMethodFrame` was pushing a method scope instance on the stack.

```
pushNewMethodFrame
	| newTop |
	newTop := CMethodScope new.
	stack push: newTop.
	^ newTop
```

#### Improved version

The new version is the following one: 

```
CInterpreter >> execute: anAST withReceiver: anObject
	| result |
	self pushNewMethodFrame.
	self topFrame parentScope: (CInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself).

	self topFrame receiver: anObject.
	result := self visitNode: anAST.
	self popFrame.
	^ result
```

After pushing to the stack a new frame representing the method execution, we should make sure that the parent scope 
of the method scope is an instance scope. This is what we were doing in the `currentScope` method.

Also notice that the instance scope and the method scope have both a receiver. 
SD: When can they be different? And why the instance one is not enough since it is in the parent scope of the method. 


We have still to make sure that `currentScope` refers to the top frame of the interpreter.
This is what the following redefinition expresses: 

```
CInterpreter >> currentScope
	^ self topFrame
```



Then we need to update `visitMessageNode:` to compute the arguments by doing a recursive evaluation, and then use those values during the new method activation.

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method args |
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	newReceiver := self visitNode: aMessageNode receiver.
	method := newReceiver class compiledMethodAt: aMessageNode selector.
	^ self executeMethod: (self astOf: method) withReceiver: newReceiver andArguments: args
```


To include arguments in the method activation, we add a new `arguments` parameter to our method `execute:withReceiver:` to get `execute:withReceiver:withArguments:`. 

In addition to adding the receiver to the new frame representing the execution, we add a binding for each parameter (called unfortunately arguments in Pharo AST) with their corresponding value in the argument collection. This binding is added to the variables of the top frame. 
The message `with:do:` iterates both the parameter list and actual arguments as pairs.

```
CInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection
	| result |
	self pushNewMethodFrame.
	self topFrame parentScope: (CInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself).

	self topFrame receiver: anObject.
	anAST arguments 
		with: aCollection
		do: [ :arg :value | self topFrame at: arg name put: value ]. 
	result := self visitNode: anAST.
	self popFrame.
	^ result
```


Instead of just removing the old `executeMethod:withReceiver:` method, we redefine it calling the new one with a default empty collection of arguments. 
This method was used by our tests and is part of our public API, so keeping it will avoid migrating extra code and an empty collection of arguments is a sensible and practical default value.
 
```
CInterpreter >> executeMethod: anAST withReceiver: anObject
	^ self 
		executeMethod: anAST 
		withReceiver: anObject 
		andArguments: #()
```


Our tests should all pass now.


### Refactoring the Terrain

Let's now refactor a bit the existing code to clean it up and expose some existing but hidden functionality. Let us extract the code that accesses `self` and the frame parameters into two other methods that make more intention revealing that we are accessing values in the current frame.

```
CInterpreter >> tempAt: aSymbol put: anInteger
	self topFrame at: aSymbol put: anInteger
```

```
CInterpreter >> execute: anAST withReceiver: anObject andArguments: aCollection
	| result |
	self pushNewMethodFrame.
	self topFrame parentScope: (CInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself).
  
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
In other words, we can manage temporaries the same way as we manage arguments.

Our first scenario introducing temporaries will verify the default value of temporaries.
Indeed when temporaries are allocated in Pharo, the execution engine (in this case our evaluator) should make sure these variables are correctly initialized to a default value, in this case `nil`.

Notice that temporaries cannot be observed from outside the execution of a method unless we halt the evaluation of a method in the middle of the evaluation. Since our testing approach is more like a black-box approach, we need to make our scenarios visible from the outside. Because of these reasons, our tests will rely on returns again, as we did before with literal objects.

We define the method `returnUnassignedTemp` that simply returns a local variable that is just allocated. 

```
CInterpretable >> returnUnassignedTemp
	| temp |
	^ temp
```


The companion test verifies that the value of an uninitialized temporary is `nil`.

```
CInterpreterTest >> testUnassignedTempHasNilValue
	self
		assert: (self executeSelector: #returnUnassignedTemp)
		equals: nil
```


The current subset of Pharo that we interpret does not contain blocks and their local/temporary variables -- We will implement blocks and more complex lexical scopes in a subsequent chapter.
Therefore the temporary variable management we need to implement is simple.

To make our test pass, we modify the `execute:withReceiver:andArguments:` method to define the temporaries needed with `nil` as value.

```
CInterpreter >> executeMethod: anAST withReceiver: anObject andArguments: aCollection
	| result  |
	self pushNewMethodFrame.
	self topFrame parentScope: (CHInstanceScope new
		receiver: anObject;
		parentScope: globalScope;
		yourself);
  
	self topFrame receiver: anObject.
	anAST arguments with: aCollection do: [ :arg :value | self tempAt: arg name put: value ].
	anAST temporaryNames do: [ :tempName | self tempAt: tempName put: nil ].
	result := self visitNode: anAST body.
	self popFrame.
	^ result
```

The tests should pass.


### Implementing Temporary Variable Writes
The next aspect we have to address is temporary writing. 

We test that writes to temporary variables are working too.
We define our scenario method `writeTemporaryVariable`, which defines a temporary variable, assigns to it and returns it. 

```
CInterpretable >> writeTemporaryVariable
	| temp |
	temp := 100.
	^ temp
```

Remark that an optimizing compiler for this code would be smart enough to do constant propagation of the literal integer and then realize that the temporary is dead code and remove it, leaving us with a method body looking like ` ^ 100 `. However, since the parser does not do such optimizations by itself, we are sure that the AST we get contains both the temporary definition, the assignment, and the temporary return.

Its companion test checks that evaluating this method does effectively return 100, meaning that the temporary variable write succeeded, and that `temp` means the same variable in the assignment and in the access.

```
CInterpreterTest >> testWriteTemporaryVariable
	self
		assert: (self executeSelector: #writeTemporaryVariable)
		equals: 100
```

If you execute the test `testWriteTemporaryVariable` it should fail. 

Since temporary variable name resolution is already managed by method scopes, we just need to implement `write:withValue:` in it to make all our tests pass.

```
CMethodScope >> write: aString withValue: aValue
	variables at: aString put: aValue
```


### Evaluation Order
HERE

The last thing we need to make sure is that arguments are evaluated in the correct order.
The evaluation order in Pharo goes as follows: before evaluating a message, the receiver and all arguments are evaluated. The receiver is evaluated before the arguments. Arguments are evaluated in left-to-right order.

Testing the evaluation order in a black-box fashion, as we have been doing so far, is rather challenging with our current evaluator. Indeed, our evaluator does not yet handle arithmetics, allocations, or other kinds of primitive, so we cannot easily count! A simple approach to test is to make a counter out of [Peano Axioms](https://en.wikipedia.org/wiki/Peano_axioms). The main idea is to implement numbers as nested sets, where the empty set _(S())_ represents the number `0`, the set _(S(S())_ that contains the empty set (`0`) represents the number `1`, the set _(S(S(S())))_ that contains the number `1` represents the number `2`, and so on. The only support we need for this is to extend our literal support for dynamic array literals. The code illustrating the idea follows.

```
CInterpretable >> initialize
	super initialize.
	current := { "empty" }
```


```
CInterpretable >> next
	| result |
	"Implement a stream as an increment in terms of Peano axioms.
	See https://en.wikipedia.org/wiki/Peano_axioms"
	result := current.
	"We increment the counter"
	current := { current }.
	"We return the previous result"
	^ result
```


```
CInterpreterTests >> peanoToInt: aPeanoNumber
	"Helper method to transform a peano number to a normal Pharo integer"
	^ aPeanoNumber
		ifEmpty: [ 0 ]
		ifNonEmpty: [ 1 + (self peanoToInt: aPeanoNumber first) ]
```


Using this support, we can express our evaluation order scenario and test as follows.
We will add a new instance variable to `CHInterpretable` to store its evaluation order.
Then, we are going to send a message with many arguments, evaluating for each argument `self next`.
The message receiving the arguments will receive as argument three generated peano values, that we will return as dynamic literal array. If evaluation order is right, the evaluation order of the receiver should be 0, the evaluation of the first argument should be 1, and so on.

```
Object << #CHInterpretable
	slots: { #x . #collaborator . #evaluationOrder};
	package: 'Champollion-Core'
```


```
CInterpretable >> evaluationOrder
	^ evaluationOrder
```


```
CInterpretable >> evaluateReceiver
	evaluationOrder := self next.
	^ self
```


```
CInterpretable >> returnEvaluationOrder
	^ self evaluateReceiver
		messageArg1: self next
		arg2: self next
		arg3: self next
```


```
CInterpretable >> messageArg1: arg1 arg2: arg2 arg3: arg3
	^ {arg1 . arg2 . arg3}

CInterpreterTests >> testEvaluationOrder
	| argumentEvaluationOrder |
	argumentEvaluationOrder := self executeSelector: #returnEvaluationOrder.

	self assert: (self peanoToInt: receiver evaluationOrder) equals: 0.
	self
		assert: (argumentEvaluationOrder collect: 
		[ :peano | self peanoToInt: peano])
	 equals: #(1 2 3)
```


To make this test green we need to implement previously some new support in our interpreter: writing to temporary variables and dynamic literal arrays.

```
CInterpreter >> visitArrayNode: aRBArrayNode
	^ aRBArrayNode statements 
		collect: [ :e | self visitNode: e ] 
		as: Array
```


At this point our test will fail because the evaluation order is wrong! The receiver was evaluated 4th, after all arguments. 
This is solved by changing the order of evaluation in `visitMessageNode:`.

```
CInterpreter >> visitMessageNode: aMessageNode
	| newReceiver method args |
	newReceiver := self visitNode: aMessageNode receiver.
	args := aMessageNode arguments collect: [ :each | self visitNode: each ].
	method := newReceiver class compiledMethodAt: aMessageNode selector.
	^ self executeMethod: (self astOf: method) withReceiver: newReceiver andArguments: args
]
```


### About Name Conflict Resolution


Inside the scope of a method, statements have access to parameters, temporaries, instances and global variables. A name conflict appears when two variables that should be visible in a method share the same name. In a conflict scenario, the language developer needs to devise a resolution strategy for these problems, to avoid ambiguities.

For example, consider a method `m:` that has an argument named `integer` and defines a temporary variable also named `integer`. 
How should the values of that name be resolved? 
How are assignments resolved? 
A conflict resolution strategy provides a set of deterministic rules to answer these questions and let developers understand what their programs do in a non-ambiguous way.

A first simple strategy to avoid conflicts is preventing them at construction time. 
That is, the language should not allow developers to define variables if they generate a name conflict. 
For example, a method should not be able to define a temporary variable with the same name as an instance variable of its class. 
Usually, these validations are done once at compile time, and programs that do not follow these rules are rejected.

Another strategy to solve this problem is to allow shadowing. 
That is, we give each variable in our program a priority, and then the actual variable to read or write is looked up using this priority system.
Typically priorities in these schemas are modelled as lexical scopes. 
Lexical scoping divides a program into a hierarchy of scopes. 
Each scope defines variables and all but the top-level scope have a parent scope. 
For example, the top-level scope defines global variables, the class scope defines the instance variables, the method scope defines the parameters and temporaries. 
In this way, variable visibility can be defined in terms of a scope: the variables visible in a scope are those defined in the scope or in the parents of the scope. 
Moreover, scoping also gives a conflict resolution strategy: variables defined closer to the current scope in the scope hierarchy have more priority than those defined higher in the scope hierarchy.

### About Return


As a reader, you may wonder why we did not do anything for return expression and this is an interesting question. 
Up to now interpreting a return is just returning the value of the interpretation of the return expression. 
In fact, up until now, a method execution has a single path of execution: it means that the complete method body
should be executed and that we did not introduce different condition control flow. 
This is when we will introduce block closure and conditional control flow and we will have to revisit the interpretation of return. 

### Conclusion


Supporting message sends and in particular method execution is the core of the computation in an object-oriented language and this is what this chapter covered.

Implementing messages implied modeling the call stack and keeping it balanced on method returns.
We have seen that a call stack is made up of frames, each frame representing the activation of a method: it stores the method, receiver, arguments, and temporaries of the method that is executing. When a message takes place, the receiver and arguments are evaluated in order from left to right, a new frame is created and all values are stored in the frame.

