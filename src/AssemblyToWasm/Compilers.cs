namespace AssemblyToWasm;

static class Compiler
{
    public static void Compile(string source)
    {
        var lexer = new Lexer(source, ["export", "import"]);
        var parser = new Parser(lexer.Tokenize());
        var (funcs, importFuncs) = parser.Parse();
        foreach(var f in funcs)
        {
            Console.WriteLine(Printer.PrintFunction(f));
        }
        var html = WasmEmitter.Emit(funcs, importFuncs);
        File.WriteAllText("index.html", html);
    }
}