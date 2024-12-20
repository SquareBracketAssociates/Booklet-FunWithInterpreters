## Simple variables: self and super 
@cha:self

In the previous chapter,  we wrote the first version of an evaluator managing literals: integers, floats, arrays, and booleans.
This chapter adds to our evaluator the capability of evaluating variables 
with a specific focus on self and super. 
We extend this to variables in the following chapter by introducing name resolution.





### Starting up with `self` and `super`

To start working with variables, we begin with a method returning `self` as the method `returnSelf` shows it.

```
CInterpretable >> returnSelf
	^ self
```

There is a difference between this scenario and the previous one with literals.
Previous scenarios always evaluated to the same value regardless how or when they were executed.

For `self`, however, evaluating this method will return a different object if the receiver of the message changes.  
We have to handle this case to make sure that we can properly test the interpreter. 

### Writing a Red Test

To properly test how `self` behaves we need to specify a receiver object, and then assert that the returned object is that same object with an identity check.
For this purpose we use as receiver a new object instance of `Object`, which implements equality as identity, guaranteeing that the object will be not only equals, but also the same. We use an explicit identity check in our test to convey our intention.

```
CInterpreterTest >> testReturnSelf
	| receiver |
	receiver := Object new.
 	"Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSelf
      withReceiver: receiver)
          == receiver

```


### Introducing Support to Specify a Receiver

In this section, we implement the first version of an execution context to introduce receiver objects.
This first support will get us up and running to implement initial variable support, but it will run short to properly implement message sends. 
This setup will be extended in later chapters to support message sends and block temporaries.

This initial support requires that the evaluator gets access to the object that is the receiver in the current method execution.
For now we add an instance variable in our evaluator called `receiver`.

```
Object << #CInterpreter
	slots: { #receiver};
	package: 'Champollion'
```


We define an accessor so that we can easily adapt when we will introduce a better way representation later.
This instance variable is hidden behind this accessor, allowing us to change later on the implementation without breaking users.

```
CInterpreter >> receiver 
	^ receiver
```



Then we define the method `execute:withReceiver:` which allows us to specify the receiver.

```
CInterpreter >> execute: anAST withReceiver: anObject
	receiver := anObject.
	^ self visitNode: anAST
```


And finally, we create a new helper method in our tests `executeSelector:withReceiver:` and then redefine the method `executeSelector:` to use this new method with a default receiver.

```
CInterpreterTest >> executeSelector: aSymbol withReceiver: aReceiver
	| ast |
	ast := Parse parseMethod: (CInterpretable >> aSymbol) sourceCode.
	^ self interpreter execute: ast withReceiver: aReceiver


CInterpreterTest >> executeSelector: aSymbol
  ^ self executeSelector: aSymbol withReceiver: nil
```

We can remove the method `execute: aMethodNode` from the class `CInterpreter`, since nobody calls it anymore. 
We introduced the possibility to specify a message receiver, and still keep all our previous tests passing. 
Our new test is still red, but we are now ready to make it pass.

### Making the test pass: visiting self nodes

The interpretation of `self` is done in the method `visitVariableNode:`.

The Pharo parser resolves `self` and `super` nodes statically and creates special nodes for them, avoiding the need for name resolution.
Implementing it is simple, it just returns the value of the receiver stored in the interpreter.
Note that this method does not access the `receiver` variable directly, but uses instead the accessor, leaving us the possibility of redefining that method without breaking the visit.

```
CInterpreter >> visitVariableNode: aNode
	aNode name = #self 
		ifTrue: [ ^ self receiver ]
```


Note that in Pharo, the compiler performs a pass and enriches the variable node with an instance of the `Variable` hierarchy. 
It uses then another visit method to be able to avoid such an ugly condition.
You can see the differences in the returned trees by comparing the results of  `(CInterpretable >> #returnSelf) parseTree` and `Parser parseMethod: (CInterpretable >> #returnSelf) sourceCode`.



### Introducing `super`

Following the same logic as for `self`, we improve our evaluator to support `super`.
We start by defining a method using `super` and its companion test.

```
CInterpretable >> returnSuper
	^ super
```


```
CInterpreterTest >> testReturnSuper
  | receiver |
  receiver := Object new.
  "Convey our intention of checking identity by using an explicit identity check"
	self assert: (self
      executeSelector: #returnSuper
      withReceiver: receiver)
          == receiver
```


What the interpretation of `super` shows is that this variable is also the receiver of the message.
Contrary to a common and wrong belief, `super` is not the superclass or an instance of the superclass.
It simply is the receiver:

```
CInterpreter >> visitVariableNode: aNode
	
	aNode name = #self | (aNode name = #super)
		ifTrue: [ ^ self receiver ]
```

### Conclusion

Handling `self` and `super` as variables is simple. In the following chapter we will look at other variables and propose 
a way to manage all the different variables by the introduction of scopes. 