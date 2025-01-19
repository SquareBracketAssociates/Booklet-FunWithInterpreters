## Revisiting self and super implementation
@cha:revisitingSelf

We can take advantage of the instance scope to revisit the self and super implementation. 
Indeed instead of explicitly checking for the variable name in the interpreter, we could define a binding in the instance scope for self and super.
This way the scope will treat all the variables in a similar way and free the interpreter from this task. 

With the current implementation, we still have to distinguish between instance variables and self/super. 
Therefore we introduce a little helper function `isSelfSuper:` to encapsulate this check. 

```
CInstanceScope >> isSelfSuper: aString

	^ #( 'self' 'super' ) includes: aString
```

Using `isSelfSuper:` we redefine `scopeDefining:` to make sure that this is the instance scope object that we will be used to 
look for the value of `self` and `super`.

```
CInstanceScope >> scopeDefining: aString

	(self isSelfSuper: aString) ifTrue: [ ^ self ].
	(self definedVariables includes: aString) ifTrue: [ ^ self ].
	^ self parentScope scopeDefining: aString
```

We redefine `read:` to take into account the new responsibility. 

```
CInstanceScope >> read: aString

	(self isSelfSuper: aString) ifTrue: [ ^ receiver ].
	^ receiver instVarNamed: aString
```

We could imagine forbidding assignments to `self` and `super` by adapting `write:withValue:` as follows: 

```
CInstanceScope >> write: aString withValue: anInteger

	(self isSelfSuper: aString) ifTrue: [ self error: 'We cannot modify self or super' ].
	receiver instVarNamed: aString put: anInteger
```

But this situation cannot occur since the Pharo parser prevents at the syntactical level the possibility
to use `self` and `super` as the left part of an assignment.  So redefining `write: aString withValue: anInteger`
is not necessary. This would be needed if the interpreter would support a kind of reflective API of its own implementation.

FInally we revisit  `visitVariableNode:` as follows: 

```
visitVariableNode: aVariableNode

	^ (self scopeDefining: aVariableNode name) read: aVariableNode name
```
Here you see that checking for `self` or `super` is not mandatory anymore. 
All your tests should pass. 

