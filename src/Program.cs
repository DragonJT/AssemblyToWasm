
static class Program
{
    static void Main()
    {
        var lexer = new Lexer(File.ReadAllText("asm.txt"), ["export", "import", "local"]);
        var parser = new Parser(lexer.Tokenize());
        var (funcs, importFuncs) = parser.Parse();
        var html = WasmEmitter.Emit(funcs, importFuncs);
        File.WriteAllText("index.html", html);
    }
}