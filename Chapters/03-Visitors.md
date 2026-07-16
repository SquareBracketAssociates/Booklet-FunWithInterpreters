## Reminder: the Visitor Pattern 
@cha:visitorReminder

In this chapter, we briefly review the Visitor  design pattern.
The main purpose of the Visitor pattern is to externalize an operation from a data structure.
In addition, it supports the modular definition of operations (independent from each other) and encapsulating their own data.

### A Mini Filesystem

For example, let's consider a file system where nodes can be files or directories.
The directories and files form a tree where directories can contain files or other directories: 
file nodes are leaf nodes and directory nodes are non-leaf nodes.

Such situation can be implemented with the Composite pattern as shown in Figure *@CompositeBasic@*.
Here we do not consider to have a common superclass between the `FileNode` and `DirectoryNode`.'

![Composite for file and folders. % width=80&anchor=CompositeBasic](figures/CompositeBasic.pdf) 

```language=smalltalk
Object << #FileNode
  slots: { #size };
  package: 'VisitorExample'
```


```language=smalltalk
Object << #DirectoryNode
  slots: { #children};
  package: 'VisitorExample'
```


Using this tree, we can take advantage of the Composite pattern and the polymorphism between both nodes to calculate the total size of a node implementing a polymorphic `size` method in each class.

```language=smalltalk
FileNode >> size
  ^ size

DirectoryNode >> size
  ^ children sum: [ :each | each size ]
```


Now, let's consider the users of the file system library want to extend it with their own operations.
If the users have access to the classes, they may extend them just by adding methods to them.
When this is not the case, one way to handle such situation is to implement the Visitor **protocol** based on double dispatch: each class implements a generic `acceptVisitor: aVisitor`  method that performs a re-dispatch on the argument giving information about the receiver.

For example, when a `FileNode` receives the `acceptVisitor:` message, it will send the argument the message `visitFileNode:`,  identifying itself as a file node.

```language=smalltalk
FileNode >> acceptVisitor: aVisitor
  ^ aVisitor visitFileNode: self

DirectoryNode >> acceptVisitor: aVisitor
  ^ aVisitor visitDirectoryNode: self
```

In this way, we can re-implement the size definition we showed previously as a `SizeVisitor` that calculates the total size of a node in the file system as follows.
When a size visitor visits a file, it asks the file for its size.
When it visits a directory, it must iterate the children and sum the sizes.
However, it cannot directly ask the `size` of the children, because only `FileNode` instances do understand it but directories do not. Because of this, we need to make a recursive call and re-ask the child node to accept the visitor.
Then each node will again dispacth on the size visitor.


```language=smalltalk
SizeVisitor >> visitFileNode: aFileNode
  ^ aFileNode size

SizeVisitor >> visitDirectoryNode: aDirectoryNode
  ^ aDirectoryNode children sum: [ :each | each acceptVisitor: self ]
```

### Exercises on the Visitor Pattern

1. Implement mathematical expressions as a tree: define the classes `OpPlus`, `OpMinus`, and `OpNumber`.
Then model expressions such as `1 + 8 / 3`, and two operations on them using the composite pattern: (a) calculate their final value, and (b) print the tree in pre-order. For example, the result of evaluating the previous expression is 3, and printing it in pre-order yields the string `'/ + 1 8 3'`.

1. Re-implement the code above using a Visitor pattern.

1. Add a new kind of node to our expressions: raised to by defining the class `OpRaiseTo`. Implement it in both the composite and visitor implementations.

1. About the difference between a composite and a visitor. What happens to each implementation if we want to add a new operation? And what happens when we want to add a new kind of node?

### Conclusion

In this chapter we have reviewed the Visitor design pattern first on a simple example, then on ASTs.
The Visitor design pattern allows us to extend tree-like structures with operations without modifying the original implementation. 