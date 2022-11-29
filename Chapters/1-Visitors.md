## AST Visitors


In the previous Chapter we have seen how to create and manipulate AST nodes.
The `RBParser` class implements a parser of expressions and methods that returns AST nodes for the text given as argument.
With the AST manipulation methods we have seen before, we can already write queries on an AST.
For example, counting the number of message-sends in an AST is as simple as the following loop.

```language=smalltalk
count := 0.
aNode nodesDo: [ :n |
  n isMessage
    ifTrue: [ count := count + 1 ] ].
count
```


However, more complex manipulations do require more than an iteration and a conditional.
When a different operation is required for each kind of node in the AST, potentially with special cases depending on how nodes are composed, one object-oriented alternative is to implement it using the Visitor design pattern.

In this section we start reviewing the visitor pattern, and we then apply it for a single task: searching a string inside the tree.

### The Visitor Pattern


The Visitor pattern is one of the original design patterns from Gamma et al. , .
The main purpose of the Visitor pattern is to externalize an operation from a data structure.
For example, let's consider a file system implemented with the composite pattern, where nodes can be files or directories.
This composite forms a tree, where file nodes are leaf nodes and directory nodes are non-leaf nodes.

```language=smalltalk
Object subclass: #FileNode
  instanceVariableNames: 'size'
  package: 'VisitorExample'
```


```language=smalltalk
Object subclass: #DirectoryNode
  instanceVariableNames: 'children'
  package: 'VisitorExample'
```


Using this tree, we can take advantage of the Composite pattern and the polymorphism between both nodes to calculate the total size of a node implementing a polymorphic `size` method in each class.

```language=smalltalk
FileNode >> size [
  ^ size
]

DirectoryNode >> size [
  ^ children sum: [ :each | each size ]
]
```


Now, let's consider the users of the file system library want to extend it with their own operations.
If the users have access to the classes, they may extend them just by adding methods to them.
However, chances are users do not have access to the library classes.
One way to open the library classes is to implement the visitor **protocol**: each library class will implement a generic `acceptVisitor: aVisitor`  method that will perform a re-dispatch on the argument giving information about the receiver.
For example, when a `FileNode` receives the `acceptVisitor:` message, it will send the argument the message `visitFileNode:`, identifying itself as a file node.

```language=smalltalk
FileNode >> acceptVisitor: aVisitor [
  ^ aVisitor visitFileNode: self
]

DirectoryNode >> acceptVisitor: aVisitor [
  ^ aVisitor visitDirectoryNode: self
]
```


In this way, we can re-implement a `SizeVisitor` that calculates the total size of a node in the file system as follows.
When a size visitor visits a file, it asks the file for its size.
When it visits a directory, it must iterate the children and sum the sizes.
However, it cannot directly ask the `size` of the children, because only `FileNode` instances do understand it but directories do not. Because of this, we need to make a recursive call and re-ask the child node to accept the visitor.
Then each node will again dispath on the size visitor.


```language=smalltalk
SizeVisitor >> visitFileNode: aFileNode [
  ^ aFileNode size
]

SizeVisitor >> visitDirectoryNode: aDirectoryNode [
  ^ aDirectoryNode children sum: [ :each | each acceptVisitor: self ]
]
```


As the file system library forms trees that we can manipulate with a visitor, ASTs do so too.
RBASTs are already extensible through visitors: its nodes implement `acceptVisitor:` methods on each of the nodes.
This means we can introduce operations on the AST that were not foreseen by the original developers.
In such cases, it is up to us to implement a visitor object with the correct `visitXXX:` methods.

### Introducing AST visitors: measuring the depth of the tree


To introduce how to implement an AST visitor on RBASTs, let's implement a visitor that returns the max depth of the tree.
That is, a tree with a single node has a depth of 1.
A node with children has a depth of 1 + the maximum depth amongst all its children.
Let's call that visitor `DepthCalculatorVisitor`.

```language=smalltalk
Object subclass: #DepthCalculatorVisitor
  instanceVariableNames: ''
  classVariableNames: ''
  package: 'VisitorExample'
```


Pharo's AST nodes implement already the visitor pattern.
They have an `acceptVisitor:` method that will dispatch to the visitor with corresponding visit methods.

This means we can already use our visitor but we will have to define some methods else it will break on a visit.

#### Visiting message nodes


Let's start by calculating the depth of the expression `1+1`.
This expression is made of a message node, and two literal nodes.

```
expression := RBParser parseExpression: '1+1'.
expression acceptVisitor: DepthCalculatorVisitor new.
>>> Exception! DepthCalculatorVisitor does not understand visitMessageNode:
```


If we execute the example above, we get a debugger because `DepthCalculatorVisitor` does not understand `visitMessageNode:`.
We can then proceed to introduce that method in the debugger using the button **create** or by creating it in the browser.
We can implement the visit method as follows, by iterating the children to calculate the maximum depth amongst the children, and then adding 1 to it.

```
DepthCalculatorVisitor >> visitMessageNode: aRBMessageNode [
  ^ 1 + (aRBMessageNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```


#### Visiting literal nodes


As soon as we restart the example, it will stop again with an exception again, but this time because our visitor does not know how to visit literal nodes.
We know that literal nodes have no children, so we can implement the visit method as just returning one.

```
DepthCalculatorVisitor >> visitLiteralValueNode: aRBLiteralValueNode [
  ^ 1
]
```


#### Calculating the depth of a method


A method node contains a set of statements.
Statements are either expressions or return statements.
The example that follows parses a method with two statements whose maximum depth is 3.
The first statement, as we have seen above, has a depth of 2.
The second statement, however, has depth of three, because the receiver of the `+` message is a message itself.
The final depth of the method is then 5: 1 for the method node, 1 for the sequence node, and 3 for the statements.

```
method := RBParser parseMethod: 'method
  1+1.
  self factorial + 2'.
method acceptVisitor: DepthCalculatorVisitor new.
>>> Exception! DepthCalculatorVisitor does not understand visitMethodNode:
```


To calculate the above, we need to implement three other visiting methods: `visitMethodNode:`, `visitSequenceNode:` and `visitSelfNode:`. Since for the first two kind of nodes we have to iterate over all children in the same way, let's implement these similarly to our `visitMessageNode:`. Self nodes are variables, so they are leafs in our tree, and can be implemented as similarly to literals.

```
DepthCalculatorVisitor >> visitMethodNode: aRBMethodNode [
  ^ 1 + (aRBMethodNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```


```
DepthCalculatorVisitor >> visitSequenceNode: aRBSequenceNode [
  ^ 1 + (aRBSequenceNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```

```
DepthCalculatorVisitor >> visitSelfNode: aSelfNode [
  ^ 1
]
```


### Refactoring the implementation


This simple AST visitor does not actually require different implementation for each of its nodes.
We have seen above that we can differentiate the nodes between two kinds: leaf nodes that do not have children, and internal nodes that have children.
A first refactoring to avoid the repeated code in our solution may extract the repeated methods into a common ones: `visitNodeWithChildren:` and `visitLeafNode:`.

```
DepthCalculatorVisitor >> visitNodeWithChildren: aNode [
  ^ 1 + (aNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```

```
DepthCalculatorVisitor >> visitMessageNode: aRBMessageNode [
  ^ self visitNodeWithChildren: aRBMessageNode
]
[[[
DepthCalculatorVisitor >> visitMethodNode: aRBMethodNode [
  ^ self visitNodeWithChildren: aRBMethodNode
]
```

```
DepthCalculatorVisitor >> visitSequenceNode: aRBSequenceNode [
  ^ self visitNodeWithChildren: aRBSequenceNode
]
```


```
DepthCalculatorVisitor >> visitLeafNode: aSelfNode [
  ^ 1
]
```


```
DepthCalculatorVisitor >> visitSelfNode: aSelfNode [
  ^ self visitLeafNode: aSelfNode
]
```

```
DepthCalculatorVisitor >> visitLiteralValueNode: aRBLiteralValueNode [
  ^ self visitLeafNode: aRBLiteralValueNode
]
```


### Second refactoring


As a second step, we can refactor further by taking into account a simple intuition: leaf nodes do never have children.
This means that `aNode children` always yields an empty collection for leaf nodes, and thus the result of the following expression is alwaysa program that zero:

```language=smalltalk
(aNode children
  inject: 0
  into: [ :max :node | max max: (node acceptVisitor: self) ])
```


In other words, we can reuse the implementation of `visitNodeWithChildren:` for both nodes with and without children, to get rid of the duplicated `1+`.

Let's then rename the method `visitNodeWithChildren:` into `visitNode:` and make all visit methods delegate to it.
This will allow us also to remove the, now unused, `visitLeafNode:`.

```
DepthCalculatorVisitor >> visitNode: aNode [
  ^ 1 + (aNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```

```
DepthCalculatorVisitor >> visitMessageNode: aRBMessageNode [
  ^ self visitNode: aRBMessageNode
]
```

```
DepthCalculatorVisitor >> visitMethodNode: aRBMethodNode [
  ^ self visitNode: aRBMethodNode
]
```

```
DepthCalculatorVisitor >> visitSequenceNode: aRBSequenceNode [
  ^ self visitNode: aRBSequenceNode
]
```

```
DepthCalculatorVisitor >> visitSelfNode: aSelfNode [
  ^ self visitNode: aSelfNode
]
```

```
DepthCalculatorVisitor >> visitLiteralValueNode: aRBLiteralValueNode [
  ^ self visitNode: aRBLiteralValueNode
]
```


### Refactoring: A common Visitor superclass


If we take a look at our visitor above, we see a common structure has appeared.
We have a lot of little visit methods per kind of node where we could do specific per-node treatments.
For those nodes that do not do anything specific, with that node, we treat them as a more generic node with a more generic visit method.
Our generic visit methods could then be moved to a common superclass named `BaseASTVisitor` defining the common structure, but making a single empty hook for the `visitNode:` method.

```language=smalltalk
Object subclass: #BaseASTVisitor
  instanceVariableNames: ''
  classVariableNames: ''
  package: 'VisitorExample'
```

```
BaseASTVisitor >> visitNode: aNode [
  "Do nothing by default. I'm meant to be overridden by subclasses"
]
```

```
BaseASTVisitor >> visitMessageNode: aRBMessageNode [
  ^ self visitNode: aRBMessageNode
]
```

```
BaseASTVisitor >> visitMethodNode: aRBMethodNode [
  ^ self visitNode: aRBMethodNode
]
```

```
BaseASTVisitor >> visitSequenceNode: aRBSequenceNode [
  ^ self visitNode: aRBSequenceNode
]
```

```
BaseASTVisitor >> visitSelfNode: aSelfNode [
  ^ self visitNode: aSelfNode
]
```

```
BaseASTVisitor >> visitLiteralValueNode: aRBLiteralValueNode [
  ^ self visitNode: aRBLiteralValueNode
]
```


And our `DepthCalculatorVisitor` is then redefined as a subclass of it:

```language=smalltalk
BaseASTVisitor subclass: #DepthCalculatorVisitor
  instanceVariableNames: ''
  classVariableNames: ''
  package: 'VisitorExample'
```

```
DepthCalculatorVisitor >> visitNode: aNode [
  ^ 1 + (aNode children
      inject: 0
      into: [ :max :node | max max: (node acceptVisitor: self) ])
]
```


A more elaborate visitor could provide many more hooks.
For example, in our example above we could have differentiated `RBSelf` nodes from `RBVariableNodes`, defining the following.

```language=smalltalk
BaseASTVisitor >> visitSelfNode: aRBSelfNode [
  ^ self visitVariableNode: aRBSelfNode
]
```

```
BaseASTVisitor >> visitVariableNode: aRBVariableNode [
  ^ self visitNode: aRBVariableNode
]
```


Fortunately for us, Pharo's ASTs already provide `RBProgramNodeVisitor` a base class for our visitors, with many hooks to override in our specific subclasses.

### Searching the AST for a Token


Calculating the depth of an AST is a pretty naïve example for a visitor because we do not need special treatment per node.
It is however a nice example to introduce the concepts, learn some common patterns, and it furthermore forced us to do some refactorings and understanding a complex visitor structure.
Moreover, it was a good introduction for the `RBProgramNodeVisitor` class.

In this section we will implement a visitor that does require different treatment per node: a node search.
Our node search will look for a node in the tree that contains a token matching a string.
For the purposes of this example, we will keep it scoped to a **begins with** search, and will return all nodes it finds, in a depth-first in-order traversal.
We leave as an exercise for the reader implementing variants such as fuzzy string search, traversing the AST in different order, and being able to provide a stream-like API to get only the next matching node on demand.

Let's then start to define a new visitor class `SearchVisitor`, subclass of `RBProgramNodeVisitor`.
This class will have an instance variable to keep the token we are looking for.
Notice that we need to keep the token as part of the state of the visitor: the visitor API implemented by Pharo's ASTs do not support additional arguments to pass around some extra state. This means that this state needs to be kept in the visitor.

```language=smalltalk
RBProgramNodeVisitor subclass: #SearchVisitor
  instanceVariableNames: 'token'
  classVariableNames: ''
  package: 'VisitorExample'

SearchVisitor >> token: aToken [
  token := aToken
]
```


The main idea of our visitor is that it will return a collection with all matching nodes.
If no matching nodes are found, an empty collection is returned.

#### Searching in variables nodes


Let's then start implementing the visit methods for variable nodes.
`RBProgramNodeVisitor` will already treat special variables as variable nodes, so a single visit method is enough for all four kind of nodes.
A variable node matches the search if its name begins with the searched token.

```language=smalltalk
SearchVisitor >> visitVariableNode: aNode [
  ^ (aNode name beginsWith: token)
      ifTrue: [ { aNode } ]
      ifFalse: [ #() ]
]
```


#### Searching in message nodes


Message nodes will match a search if their selector begins with the searched token.
In addition, to follow the specification children of the message need to be iterated in depth first in-order.
This means the receiver should be iterated first, then the message node itself, finally the arguments.

```language=smalltalk
SearchVisitor >> visitMessageNode: aNode [
  ^ (aNode receiver acceptVisitor: self),
    ((aNode selector beginsWith: token)
      ifTrue: [ { aNode } ]
      ifFalse: [ #() ]),
    (aNode arguments gather: [ :each | each acceptVisitor: self ])
]
```


#### Searching in literal nodes


Literal nodes contain literal objects such as strings, but also booleans or numbers.
To search in them, we need to transform such values as string and then perform the search within that string.

```language=smalltalk
SearchVisitor >> visitLiteralNode: aNode [
  ^ (aNode value asString beginsWith: token)
      ifTrue: [ { aNode } ]
      ifFalse: [ #() ]
]
```


#### The rest of the nodes


The rest of the nodes do not contain strings to search in them.
Instead the contain children we need to search.
We can then provide a common implementation for them by simply redefining the `visitNode:` method.

```language=smalltalk
SearchVisitor >> visitNode: aNode [
  ^ aNode children gather: [ :each | each acceptVisitor: self ]
]
```


Another design would be to store the collection holding the rest inside the visitor and to avoid the temporary copies.
We let you refactor your code to implement it.

### Exercises


#### Exercises on the Visitor Pattern


1. Implement mathematical expressions as a tree, to for example model expressions like `1 + 8 / 3`, and two operations on them using the composite pattern: \(a\) calculate their final value, and \(b\) print the tree in pre-order. For example, the result of evaluating the previous expression is 3, and printing it in pre-order yields the string `'/ + 1 8 3'`.


1. Re-implement the code above using a visitor pattern.


1. Add a new kind of node to our expressions: raised to. Implement it in both the composite and visitor implementations.


1. About the difference between a composite and a visitor. What happens to each implementation if we want to add a new operation? And what happens when we want to add a new kind of node?


#### Exercises on the AST Visitors


1. Implement an AST lineariser, that returns an ordered collection of all the nodes in the AST \(similar to the pre-order exercise above\).


1. Extend your AST lineariser to handle different linearisation orders: breadth-first, depth-first pre-order, depth-first post-order.


1. Extend the Node search exercise in the chapter to have alternative search orders. E.g., bottom-up, look not only if the strings begin with them.


1. Extend the Node search exercise in the chapter to work as a stream: asking `next` repeatedly will yield the next occurrence in the tree, or `nil` if we arrived to the end of the tree. You can use the linearisations you implemented above.


### Conclusion


In this chapter we have reviewed the visitor design pattern first on a simple example, then on ASTs.
The visitor design pattern allows us to extend tree-like structures with operations without modifying the original implementation. The tree-like structure, in our case the AST, needs only to implement an accept-visit protocol. RB ASTs implement such a protocol and some handy base visitor classes.

Finally, we implemented two visitors for ASTs: a depth calculator and a node searcher.
The depth calculator is a visitor that does not require special manipulation per-node, but sees all nodes through a common view. The search visitor has a common case for most nodes, and then implements special search conditions for messages, literals and variables.

In the following chapters we will use the visitor pattern to implement AST interpreters: a program that specifies how to evaluate an AST. A normal evaluator interpreter yields the result of executing the AST.
However, we will see that abstract interpreters will evaluate an AST in an abstract way, useful for code analysis.
