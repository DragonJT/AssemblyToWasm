namespace AssemblyToWasm;

using System.Text;

class Parser
{
    private readonly List<Token> _toks;
    private int _i;

    public Parser(IEnumerable<Token> tokens)
    {
        _toks = [.. tokens];
    }

    bool MatchKeyword(string keyword)
    {
        var peek = Peek();
        bool match = peek.Kind == TokenKind.Keyword && peek.Lexeme == keyword;
        if (match) Advance();
        return match;
    }

    public (WasmFunction[], WasmImportFunction[]) Parse()
    {
        List<WasmFunction> functions = [];
        List<WasmImportFunction> importFunctions = [];
        while (Peek().Kind != TokenKind.EOF)
        {
            if (Peek().Kind == TokenKind.Keyword && Peek().Lexeme == "import")
            {
                importFunctions.Add(ParseImportFunction());
            }
            else
            {
                functions.Add(ParseFunction());
            }
        }
        return ([.. functions], [.. importFunctions]);
    }

    private WasmImportFunction ParseImportFunction()
    {
        ExpectKeyword("import");
        var returnType = WasmEmitter.GetValtype(Expect(TokenKind.Identifier).Lexeme);
        string name = Expect(TokenKind.Identifier).Lexeme;

        Expect(TokenKind.LParen);
        var parameters = ParseParamList().ToArray();
        Expect(TokenKind.RParen);

        Expect(TokenKind.LBrace);
        var depth = 0;
        StringBuilder code = new();
        while (true)
        {
            if (Peek().Kind == TokenKind.LBrace) depth++;
            if (Peek().Kind == TokenKind.RBrace)
            {
                depth--;
                if (depth < 0)
                    break;
            }
            code.Append(Advance().ToRawCode());
        }
        Expect(TokenKind.RBrace);
        return new WasmImportFunction(name, returnType, parameters, code.ToString());
    }

    private WasmFunction ParseFunction()
    {
        bool export = MatchKeyword("export");
        var returnType = WasmEmitter.GetValtype(Expect(TokenKind.Identifier).Lexeme);
        string name = Expect(TokenKind.Identifier).Lexeme;

        Expect(TokenKind.LParen);
        var parameters = ParseParamList().ToArray();
        Expect(TokenKind.RParen);

        Expect(TokenKind.LBrace);
        var locals = ParseLocals().ToArray();
        var bytecode = ParseBody(0).ToArray();
        Expect(TokenKind.RBrace);

        return new WasmFunction(export, name, returnType, parameters, locals, bytecode);
    }

    private List<Local> ParseLocals()
    {
        List<Local> locals = [];
        while (Match(TokenKind.Semicolon))
        {
            Valtype type = WasmEmitter.GetValtype(Expect(TokenKind.Identifier).Lexeme);
            do
            {
                var name = Expect(TokenKind.Identifier).Lexeme;
                locals.Add(new Local(type, name));
            } while (Match(TokenKind.Comma));
        }
        return locals;
    }

    private List<Parameter> ParseParamList()
    {
        var list = new List<Parameter>();
        if (Peek().Kind == TokenKind.RParen) return list;

        while (true)
        {
            var type = WasmEmitter.GetValtype(Expect(TokenKind.Identifier).Lexeme);
            var nameTok = Expect(TokenKind.Identifier);
            list.Add(new Parameter(type, nameTok.Lexeme));

            if (Match(TokenKind.Comma)) continue;
            break;
        }
        return list;
    }

    private List<WasmCode> ParseBody(int depthToLoop)
    {
        var code = new List<WasmCode>();
        while (Peek().Kind != TokenKind.RBrace)
        {
            var opParts = new List<string>();

            var first = Expect(TokenKind.Identifier);
            opParts.Add(first.Lexeme);

            while (Match(TokenKind.Dot))
            {
                var seg = Expect(TokenKind.Identifier);
                opParts.Add(seg.Lexeme);
            }

            string op = string.Join('.', opParts);

            if (op == "int.const")
            {
                code.Add(new WasmCode(Opcode.i32_const, Expect(TokenKind.Integer).Lexeme));
            }
            else if (op == "int.mul")
            {
                code.Add(new WasmCode(Opcode.i32_mul));
            }
            else if (op == "int.add")
            {
                code.Add(new WasmCode(Opcode.i32_add));
            }
            else if (op == "int.lt_s")
            {
                code.Add(new WasmCode(Opcode.i32_lt_s));
            }
            else if (op == "int.eqz")
            {
                code.Add(new WasmCode(Opcode.i32_eqz));
            }
            else if (op == "call")
            {
                code.Add(new WasmCode(Opcode.call, Expect(TokenKind.Identifier).Lexeme));
            }
            else if (op == "break")
            {
                code.Add(new WasmCode(Opcode.br, depthToLoop.ToString()));
            }
            else if (op == "local.get")
            {
                code.Add(new WasmCode(Opcode.get_local, Expect(TokenKind.Identifier).Lexeme));
            }
            else if(op == "local.set")
            {
                code.Add(new WasmCode(Opcode.set_local, Expect(TokenKind.Identifier).Lexeme));   
            }
            else if (op == "loop")
            {
                code.Add(new WasmCode(Opcode.block, "void"));
                code.Add(new WasmCode(Opcode.loop, "void"));
                Expect(TokenKind.LBrace);
                code.AddRange(ParseBody(1));
                Expect(TokenKind.RBrace);
                code.Add(new WasmCode(Opcode.br, "0"));
                code.Add(new WasmCode(Opcode.end));
                code.Add(new WasmCode(Opcode.end));
            }
            else if (op == "if")
            {
                code.Add(new WasmCode(Opcode.@if, "void"));
                Expect(TokenKind.LBrace);
                code.AddRange(ParseBody(depthToLoop + 1));
                Expect(TokenKind.RBrace);
                code.Add(new WasmCode(Opcode.end));
            }
            else
            {
                throw new Exception($"Unknown instruction: {op}");
            }
        }
        return code;
    }

    private Token Peek(int look = 0) => (_i + look) < _toks.Count ? _toks[_i + look] : _toks[^1];

    private Token Advance() => _toks[_i++];

    private bool Match(TokenKind kind)
    {
        if (Peek().Kind == kind) { _i++; return true; }
        return false;
    }

    private Token ExpectKeyword(string keyword)
    {
        var tk = Peek();
        if (tk.Kind != TokenKind.Keyword)
            throw Error($"Expected keyword {keyword}, got {tk.Kind} {tk.Lexeme}, {tk.Line}:{tk.Column}");
        _i++;
        return tk;
    }

    private Token Expect(TokenKind kind)
    {
        var tk = Peek();
        if (tk.Kind != kind)
            throw Error($"Expected {kind} but found {tk.Kind} ('{tk.Lexeme}') at {tk.Line}:{tk.Column}");
        _i++;
        return tk;
    }

    private Exception Error(string msg) => new Exception(msg);
}