# Cake.Adaptor
A random adaptor I build to provide intellisense in ScriptCs enviroments.

To use, simply build and run Cake.Adaptor, reference the generated "Cake.Adaptor.dll" in your script,
and add "using static Cake.Adaptor.ScriptHost" to the cake file. After renaming it to
a `.csx` file, it should provide intellisense in enviroments that support C# script files,
like omnisharp-atom.

_*NOTE:*_ Be sure to strip out the `using static` before running this script file in cake.
