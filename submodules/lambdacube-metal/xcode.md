
## Compile metallib from .metal shaders:
```xcrun -sdk macosx metal -c MyLibrary.metal -o MyLibrary.air```
   xcrun -sdk macosx metallib MyLibrary.air   -o MyLibrary.metallib```

## Precompile Shaders on the Command Line
```xcrun -sdk macosx metal -Os MyLibrary.metal```
```xcrun -sdk macosx metal -Os MyLibrary.metal```

## Generate a Symbol File with a Single Command (For Dylib/Dynamic Linking support of a .metallib)
```xcrun -sdk macosx metal -frecord-sources=flat Shadow.metal PointLights.metal DirectionalLight.metal```

## Generate a Symbol File with Multiple Commands
You can also generate a Metal library’s symbol file with multiple commands, which may be more appropriate for your workflow than the single-command technique. 
For those scenarios, you can generate each Metal library and its companion symbol file by following these steps:

Compile each Metal source file to a Metal AIR (Apple Intermediate Representation) file.
Generate a Metal library with the source by linking its AIR files.

Separate the library’s source and save it as a companion symbol file.
First, compile each Metal source file to a Metal AIR file by passing the -c option to the compiler:

```xcrun -sdk macosx metal -c -frecord-sources Shadow.metal```
```xcrun -sdk macosx metal -c -frecord-sources PointLights.metal```
```xcrun -sdk macosx metal -c -frecord-sources DirectionalLight.metal```
The -frecord-sources option tells the Metal compiler to embed the symbols in the AIR output file for that command. However, this command doesn’t create a separate symbols file at this time, which is why the -frecord-sources option doesn’t include the =flat suffix.

Next, generate a Metal library by linking the AIR files.
```xcrun -sdk macosx metal -frecord-sources -o LightsAndShadow.metallib Shadow.air PointLights.air DirectionalLight.air```

Separate the sources from the library and create a companion symbol file by running the metal-dsymutil command.
```xcrun -sdk macosx metal-dsymutil -flat -remove-source LightsAndShadow.metallib```
