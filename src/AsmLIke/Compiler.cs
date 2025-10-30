namespace AsmLike;

static class Compiler
{
    public static void Compile(string source)
    {
        var lexer = new Lexer(source, ["import", "export"]);
        var parser = new Parser([.. lexer.Tokenize()]);
        var (funcs, importFuncs) = parser.Parse();
        var html = WasmEmitter.Emit(funcs, importFuncs, true);
        File.WriteAllText("index.html", html);
    }
}