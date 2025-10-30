
namespace ForthLike;

static class Compiler
{
    public static void Compile(string source)
    {
        var lexer = new Lexer(source, ["import", "export", "if", "loop", "load", "store"]);
        var parser = new Parser([.. lexer.Tokenize()]);
        var emitter = new AsmEmitter(parser.Parse());
        var asm = emitter.Emit();
        Console.WriteLine(asm);
        AsmLike.Compiler.Compile(asm);
    }
}