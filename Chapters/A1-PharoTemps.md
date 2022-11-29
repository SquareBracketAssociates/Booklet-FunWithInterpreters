### Annexe 1 -- About Temporaries


Before implementing the interpretation of temporaries we want to show some particularities of Pharo semantics.
Temporaries cannot shadow other temporaries.

The following expression is rejected by the compiler because the second `tmp` definition is illegal. 
```language=pharo
[ 
	| tmp |
	tmp := 2. 
	[
		| tmp |
		tmp := 3 ]]
```


Similarly a temporary cannot shadow a block or a method parameter.
Therefore the following expression is not allowed.

```language=pharo
	[ :tmp |
		| tmp |
		tmp := 2 ]
```


The following method is not valid either.
```language=pharo
with: arg

	| arg |
	^ arg
```


#### Parameters are Read Only

Finally parameters are read only. 
Therefore the following method is not valid.
```language=pharo
with: arg
	arg := 42
```


As well as the following expression

```language=pharo
	[ :tmp |
		tmp := 2 ]
```
