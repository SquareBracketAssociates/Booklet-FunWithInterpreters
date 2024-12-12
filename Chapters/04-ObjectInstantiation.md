## Representing Objects and Memory


### Class as an object


Here is the minimal information that a class should have:
- A list of instance variables to describe the values that the instances will hold,
- A method dictionary to hold methods,
- A superclass to look up inherited methods.


This minimal state is similar to that of Pharo: the Pharo `Behavior` class has a format \(compact description of instance variables\), a method dictionary, and a superclass link.

In ObjVLisp,  we have a name to identify the class. As an instance factory, the metaclass Class possesses four instance variables that describe a class:
- name, the class name,
- superclass, its superclass \(we limit to single inheritance\),
- i-v, the list of its instance variables, and
- methodDict, a method dictionary.


Since a class is an object, a class has the instance variable `class` inherited from `Object` that refers to its class as any object.

![`Point` class as an object. %width=70&anchor=fig:PointClassAsObject](figures/Ref-PointClassAsObject.pdf )

#### Example: class Point

Figure *@fig:PointClassAsObject@* shows the class `Point` as an instance with all the values taken in its instance variables.
This graphical representation is the same to the one we used for terminal objects before, except that each variable is now annotated with its meaning.
The values we show here are those declared by the programmer just before class initialization and inheritance take place.
- It is an instance of class `Class`: indeed this is a class.
- It is named `'Point'`.
- It inherits from class `Object`.
- It has two instance variables: `x` and `y`. After inheritance it will be three instance variables: `class`, `x`, and `y`.
- It has a method dictionary.



![`Class` as an object. %width=70&anchor=fig:ClassClassAsObject](figures/Ref-ClassClassAsObject.pdf )

#### Example: class Class


Figure *@fig:ClassClassAsObject@* describes the class `Class` itself. Indeed it is also an object.
- It is an instance of class `Class`: indeed this is a class.
- It is named `'Class'`.
- It inherits from class `Object`
- It has four locally defined instance variables: `name`, `superclass`, `i-v`, and  `methodDict`.
- It has a method dictionary.


![Through the prism of objects. % width=70&anchor=fig:Instanceshier](figures/Ref-InstanceGlobalPicture.pdf)

#### Everything is an object


Figure *@fig:Instanceshier@* describes a typical situation of terminal instances, class and metaclasses when viewed from an object perspective.
We see three levels of instances: terminal objects \(`mac1` and `mac2` which are instances of `Workstation`\), class objects \(`Workstation` and `Point` which are instances of `Class`\) and the metaclass \(`Class` which is instance of itself\).

![Sending a message is two-step process: method lookup and execution. % width=45&anchor=fig:ToStep](figures/InheritanceDiagram-sendingMessage.pdfs)


### Object creation

Now we are ready to understand the creation of objects. In this model there is only one way to create instances: we should send the message `new` to the class with a specification of the instance variable values as argument.

### Creation of instances of the class Point

The following examples show several point instantiations. What we see is that the model inherits from the Lisp tradition of passing arguments using keys and values, and that the order of arguments is not important.

```
Point new :x 24 :y 6
>>> aPoint (24 6)
Point new :y 6 :x 24
>>> aPoint (24 6)
```


When there is no value specified, the value of an instance variable is initialized to nil. CLOS provides the notion of default values for instance variable initialization. It can be added to ObjVlisp as an exercise and does not bring conceptual difficulties.

```testcase=true
Point new
>>> aPoint (nil nil)
```


When the same argument is passed multiple times, then the implementation takes the first occurrence.
```testcase=true
Point new :y 10 :y 15
>>> aPoint (nil 10)
```


We should not worry too much about such details: The point is that we can pass multiple arguments with a tag to identify them.

### Creation of the class Point instance of Class

Since the class `Point` is an instance of the class `Class`, to create it, we should send the message `new` to the class as follows:

```testcase=true
Class new
   :name 'Point'
   :super 'Object'
   :ivs #(x y)
>>> aClass
```


What is interesting to see here is that we use exactly the same way to create an instance of the class `Point` as the class itself.
Note that the possibility to have the same way to create objects or classes is also due to the fact that the arguments are specified using a list of pairs.

An implementation could have two different messages to create instances and classes. As soon as the same `new`, `allocate`, or `initialize` methods are involved, the essence of the object creation is similar and uniform.

#### Instance creation: Role of the metaclass


The following diagram \(Figure *@fig:metaclassrole@*\) shows that despite what one might expect, when we create a terminal instance the metaclass `Class` is involved in the process. Indeed, we send the message `new` to the class, to resolve this message, the system will look for the method in the class of the receiver \(here `Workstation`\) which is the metaclass `Class`. The method `new` is found in the metaclass and applied to the receiver, the class `Workstation`. Its effect is to create an instance of the class `Workstation`.

![Metaclass role during instance creation: Applying plain message resolution. %width=65&anchor=fig:ClassCreation](figures/Ref-InstanceCreationMetaclassRole.pdf)

The same happens when creating a class. Figure *@fig:ClassCreation@* shows the process. We send a message, now this time, to the class `Class`. The system makes no exception and to resolve the message, it looks for the method in the class of the receiver. The class of the receiver is itself, so the method `new` found in `Class` is applied to `Class` \(the receiver of the message\), and a new class is created.

![Metaclass role during class creation: Applying plain message resolution - the self instantiation link is followed.](figures/Ref-ClassCreation.pdf)

#### new = allocate and initialize

Creating an instance is the composition of two actions: a memory allocation `allocate` message and an object initialization message `initialize`.

In Pharo syntax, it means:
```
aClass new: args = (aClass allocate) initialize: args
```


We should see the following:
- The message `new` is a message sent to a class. The method `new` is a class method.
- The message `allocate` is a message sent to a class. The method `allocate` is a class method.
- The message `initialize:` will be executed on any newly created instance. If it is sent to a class, a class `initialize:` method will be involved. If it is sent to a terminal object, an instance `initialize:` method will be executed \(defined in `Object`\).



#### Object allocation: the message allocate

Allocating an object means allocating enough space to the object state but there's more: instances should be marked with their class name or id. There is an invariant in this model and in general in object-oriented programming models. Every single object must have an identifier to its class, else the system will break when trying to resolve a message.

Object allocation should return a newly created instance with:
- empty instance variables \(pointing to nil for example\);
- an identifier to its class.


In our model, the marking of an object as instance of a class is performed by setting the value of the instance variable `class` inherited from `Object`. In Pharo this information is not recorded as an instance variable but encoded in the internal object representation in the virtual machine.

The `allocate` method is defined on the metaclass `Class`. Here are some examples of allocation.

```testcase=true
Point allocate
>>> #(Point nil nil)
```

A point allocation allocates three slots: one for the class and two for x and y values.

```testcase=true
Class allocate
>>>#(Class nil nil nil nil nil)
```


The allocation for an object representing a class allocates six slots: one for class and one for each of the class instance variables: name, super, iv, keywords, and methodDict.

#### Object initialization

Object initialization is the process of passing arguments as key/value pairs and assigning the value\(s\) to the corresponding instance variable\(s\).

This is illustrated in the following snippet. An instance of class `Point` is created and the key/value pairs \(:y 6\) and \(:x 24\) are
specified. The instance is created and it receives the `initialize:` message with the key/value pairs.
The `initialize:` method is responsible for setting the corresponding variables in the receiver.

```
Point new :y 6  :x 24
>>> #(Point nil nil) initialize: (:y 6 :x 24)]
>>> #(Point 24 6)
```


When an object is initialized as a terminal instance, two actions are performed:
- First we should get the values specified during the creation, i.e., get that the y value is 6 and the x value is 24,
- Second we should assign the values to the corresponding instance variables of the created object.


#### Class initialization

During its initialization a class should perform several steps:

- First as with any initialization it should get the arguments and assign them to their corresponding instance variables. This is basically implemented by invoking the `initialize` method of `Object` via a super call, since `Object` is the superclass of `Class`.
- Second the inheritance of instance variables should be performed. Before this step the class `iv` instance variable just contains the instance variables that are locally defined. After this step the instance variable `iv` will contain all the instance variables inherited and local. In particular this is where the `class` instance variable inherited from `Object` is added to the instance variables list of the subclass of `Object`.
- Third the class should be declared as a class pool or namespace so that programmers can access it via its name.


### The Class class


Now we get a better understanding of what is the class `Class`:
- It is the initial metaclass and initial class.
- It defines the behavior of all the metaclasses.
- It defines the behavior of all the classes.


In particular, metaclasses define three messages related to instance creation.
- The `new` message, which creates an initialized instance of the class. It allocates the instance using the class message `allocate` and then initializes it by sending the message `initialize:` to this instance.
- The `allocate` message. Like message `new`, it is a class message. It allocates the structure for the newly created object.
- Finally the message `initialize:`. This message has two definitions, one on `Object` and one on `Class`.


There is a difference between the method `initialize:` executed on any instance creation and the class `initialize:` method only executed when the created instance is a class.

- The first one is a method defined on the class of the object and potentially inherited from `Object`.  This `initialize:` method just extracts the values corresponding to each instance variable from the argument list and sets them in the corresponding instance variables.


- The class `initialize:` method is executed when a new instance representing a class is executed. The message `initialize:` is sent to the newly created object but its specialization for classes will be found during method lookup and it will be executed. Usually this method invokes the default ones, because the class parameter should be extracted from the argument list and set in their corresponding instance variables. But in addition, instance variable inheritance and class declaration in the class namespace is performed.


### Accessing object instance variable values


![Instance variable offset asked to the class.](figures/offsetFromClass.pdf width=60&label=fig:offset2)

#### A first simple method.

The following test illustrates the behavior of the message `offsetFromClassOfInstanceVariable:`

```
ObjTest >> testIVOffset
   "(self  selector: #testIVOffset) run"

   self assert: ((pointClass offsetFromClassOfInstanceVariable: #x) = 2).
   self assert: ((pointClass offsetFromClassOfInstanceVariable: #lulu) = 0)
```


#### Your job.

In the protocol `'iv management'` define a method called `offsetFromClassOfInstanceVariable: aSymbol` that returns the offset of the instance variable represented by the symbol given in the parameter. It returns 0 if the variable is not defined. Look at the tests `#testIVOffset` of the class `ObjTest`.

Hints: Use the Pharo method `indexOf:`. Pay attention that such a primitive is applied to an objClass as shown in the test.

Make sure that you execute the test method: `testIVOffset`

![Instance variable offset asked to the instance itself.](figures/offsetFromObject.pdf width=60&label=fig:offset3)

#### A second simple method.


The following test illustrates the expected behavior

```
ObjTest >> testIVOffsetAndValue
   "(self  selector: #testIVOffsetAndValue) run"

   self assert: ((aPoint offsetFromObjectOfInstanceVariable: #x) = 2).
   self assert: ((aPoint valueOfInstanceVariable: #x) = 10)
```



#### Your job.

Using the previous method, define in the protocol `'iv management'`:
1. the method `offsetFromObjectOfInstanceVariable: aSymbol` that returns the offset of the instance variable. Note that this time the method is applied to an objInstance presenting an instance and not a class \(as shown in Figure *@fig:offset3@*\).
1. the method `valueOfInstanceVariable: aSymbol` that returns the value of this instance variable in the given object as shown in the test below.


Note that for the method `offsetFromObjectOfInstanceVariable:` you can check that the instance variable exists in the class of the object and else raise an error using the Pharo method `error:`.

Make sure that you execute the test method: `testIVOffsetAndValue` and it passes.

### Object allocation and initialization


The creation of an object is the composition of two elementary operations: its _allocation_ and its _initialization_.
We now define the primitives that allow us to allocate and initialize an object. Remember that:
- allocation is a class method that returns a nearly empty structure, nearly empty because the instance represented by the structure should at least know its class, and
- initialization is an instance method that given a newly allocated instance and a list of initialization arguments fill the instance.


#### Instance allocation


As shown in the class `ObjTest`, if the class `ObjPoint` has two instance variables: `ObjPoint allocateAnInstance` returns `#(#ObjPoint nil nil)`.

```
ObjTest >> testAllocate
   "(self  selector: #testAllocate) run"
   | newInstance |
   newInstance := pointClass allocateAnInstance.
   self assert: (newInstance at: 1) = #ObjPoint.
   self assert: (newInstance size) = 3.
   self assert: (newInstance at: 2) isNil.
   self assert: (newInstance at: 3) isNil.
   self assert: (newInstance objClass = pointClass)
```



#### Your job.

In the protocol `'instance allocation'` implement the primitive called `allocateAnInstance` that sent to an _objClass_ returns a new instance whose instance variable values are nil and whose objClassId represents the objClass.


Make sure that you execute the test method: `testAllocate`