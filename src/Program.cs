

static class Program
{
    static void Main()
    {
        ForthLike.Compiler.Compile(File.ReadAllText("forth.txt"));
    }
}