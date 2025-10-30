using AssemblyToWasm;
using CSharpToAssembly;

static class Program
{
    static void Main()
    {
        //var asm = Translator.Translate(File.ReadAllText("source.txt"));
        //Console.WriteLine(asm);
        Compiler.Compile(File.ReadAllText("asm.txt"));
    }
}