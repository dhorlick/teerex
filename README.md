A program to solve that awful dinnertime triangle game with the pegs.

To install,

1. Install a Java JDK 1.5 or higher, if necessary.
1. Install a Scala version compatible with your JDK, if necessary.
1. Install Maven if necessary. If you have apt, you can do this with `sudo apt install maven`
1. Edit `pom.xml` and relace your JDK version for  `maven.compiler.source` and `maven.compiler.target`.
1. Replace your Scala version for `scala.version`.
1. Bring up a shell and enter `mvn compile`

To run:
```
scala -cp target/classes org.riverporpoise.tee.ImmutableField
```

or:
```
scala -cp target/classes org.riverporpoise.tee.MutableField
```