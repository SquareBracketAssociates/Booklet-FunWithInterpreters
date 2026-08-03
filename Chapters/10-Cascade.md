## Support for Cascade

In this chapter, we extend our evaluator to manage cascades. 
A cascade is a way to send multiple messages to the same receiver. 
Cascades are not a fundamental language construct, however not supporting it blocks the interpretation of Pharo libraries that use it frequently.




### About Cascades

A cascade is a way to send multiple messages to the same receiver. 
Cascades are not essential language constructs, but they are super handy in the sense 
that they avoid declaring temporary variables and avoid repeating the same receiver several times. 

For example, the following code snippets are doing the same computation: configuring an HTTP client to perform a post call. The second one is expressed with a cascade. 

```
| client |
client := ZnClient new.
client url:'http://localhost:8181/books/1'.
client formAt:'author' put:'van Caekenberghe et al'.
client formAt:'title' put:'Entreprise Pharo'.
client post
```

Using a cascade avoids declaring a temporary variable, assigning value,  and repeating the receiver. 

```
ZnClient new
	url:'http://localhost:8181/books/1';
	formAt:'author' put:'van Caekenberghe et al';
	formAt:'title' put:'Entreprise Pharo';
	post
```

What we should see is 
- that a cascade is composed of different messages;
- all the messages are sent to the same receiver (here `ZnClient new`): the receiver of the first message of the cascade (here `url:`).

### Cascade Execution Implementation

Let us prepare some tests to validate the interpreter extension.
We define a simple method using a cascade. Note that we use a dynamic array (`{1. 2. 3}`) and not a literal array (`#(1  2 3)`) sine the last one is read-only by default and we want to modify the array to represent the effect of the cascade execution.

```
CInterpretable >> simpleCascade
	^ {1 . 2 . 3}
		at: 2 put: 22;
		at: 3 put: 33;
		at: 2
```

We define a test that validates that the cascade is correctly executed. 

```
CInterpreterTest >> testSimpleCascade
	| bk |
	bk := (self executeSelector: #simpleCascade).
	self 
		assert: bk 
		equals: 22
```

### Implementation

We define the following method `visitCascadeNode:` as follows.
Here are some simple explanations: 

- First, we evaluate the expression receiver of the cascade. This is this object that should receive the subsequent messages of the cascade. In our example, twice the message `at:put:` followed by the message `at:`.

- Second, we execute each of the cascade message part with the previously used receiver. 

- Finally we execute the last cascade messsage part and return its result.

```
CInterpreter >> visitCascadeNode: aCascadeNode
	| receiver last |
	receiver := self visitNode: aCascadeNode receiver.
	aCascadeNode messages allButLast 
		do: [ :msg |
			self 
				send: msg selector 
				receiver: receiver 
				lookupFromClass: receiver class 
				arguments: (self handleArgumentsOf: msg) ].
	last := aCascadeNode messages last.
	^ self 
		send: last selector 
		receiver: receiver 
		lookupFromClass: receiver class 
		arguments: (self handleArgumentsOf: last)
```


Note that we use the method `send:receiver:lookupFromClass:arguments` and not a simple recursive call to `visitNode:` sent to message node objects. We did this because the receiver of a message send _node_ in a cascade AST tree is not the value of the receiver but a node. The subsequent messages of a cascade should be sent to the _receiver object_ as we did it and not to a node.



### Conclusion


In this chapter we have extended our evaluator with cascades. 