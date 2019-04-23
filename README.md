# pizza-maxsat

## Compile

[sbt](https://www.scala-sbt.org/) needs to be installed.
```
sbt assembly
```

## Usage
```
java -jar target/scala-2.12/pizza-maxsat-assembly-0.1.jar [sat4j|openwbo] [FILE]
```

## Example

Solve an example with [OpenWBO](http://sat.inesc-id.pt/open-wbo/) (the binary `open-wbt` needs to be in the path):
```
java -jar target/scala-2.12/pizza-maxsat-assembly-0.1.jar openwbo src/test/resources/b_small.in
```
