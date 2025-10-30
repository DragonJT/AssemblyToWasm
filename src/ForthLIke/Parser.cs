namespace ForthLike;

using System.Text;

record Parameter(string Type, string Name);

record ImportFunction(string ReturnType, string Name, Parameter[] Parameters, string Code) : IMember;

record Function(bool Export, string ReturnType, string Name, Parameter[] Parameters, List<Token> Code) : IMember;

record Global(string Type, string Name, List<Token> Code) : IMember;

interface IMember { }

class Parser
{
    List<Token> _toks;
    int _i;

    public Parser(List<Token> tokens)
    {
        _toks = tokens;
        _i = 0;
    }

    public List<IMember> Parse()
    {
        List<IMember> members = [];
        while (Peek().Kind != TokenKind.EOF)
        {
            if (Peek().Kind == TokenKind.Keyword && Peek().Lexeme == "import")
            {
                members.Add(ParseImportFunction());
            }
            else
            {
                members.Add(ParseMember());
            }
        }
        return members;
    }

    private ImportFunction ParseImportFunction()
    {
        ExpectKeyword("import");
        var returnType = Expect(TokenKind.Identifier).Lexeme;
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
        return new ImportFunction(returnType, name, parameters, code.ToString());
    }

    private IMember ParseMember()
    {
        bool export = MatchKeyword("export");
        var type = Expect(TokenKind.Identifier).Lexeme;
        string name = Expect(TokenKind.Identifier).Lexeme;

        if (Peek().Kind == TokenKind.LBrace)
        {
            Expect(TokenKind.LBrace);
            var code = ParseBody();
            Expect(TokenKind.RBrace);

            return new Global(type, name, code);
        }
        else if (Peek().Kind == TokenKind.LParen)
        {
            Expect(TokenKind.LParen);
            var parameters = ParseParamList().ToArray();
            Expect(TokenKind.RParen);

            Expect(TokenKind.LBrace);
            var code = ParseBody();
            Expect(TokenKind.RBrace);

            return new Function(export, type, name, parameters, code);
        }
        else
        {
            throw new Exception("Expecting global or function");
        }
    }

    private List<Token> ParseBody()
    {
        var tokens = new List<Token>();
        var depth = 0;
        while (true)
        {
            if (Peek().Kind == TokenKind.LBrace) depth++;
            else if (Peek().Kind == TokenKind.RBrace)
            {
                depth--;
                if (depth < 0)
                    break;
            }
            tokens.Add(Advance());
        }
        return tokens;
    }

    private List<Parameter> ParseParamList()
    {
        var list = new List<Parameter>();
        if (Peek().Kind == TokenKind.RParen) return list;

        while (true)
        {
            var type = Expect(TokenKind.Identifier).Lexeme;
            var nameTok = Expect(TokenKind.Identifier);
            list.Add(new Parameter(type, nameTok.Lexeme));

            if (Match(TokenKind.Comma)) continue;
            break;
        }
        return list;
    }

    private Token Peek(int look = 0) => (_i + look) < _toks.Count ? _toks[_i + look] : _toks[^1];

    private Token Advance() => _toks[_i++];

    private bool Match(TokenKind kind)
    {
        if (Peek().Kind == kind) { _i++; return true; }
        return false;
    }

    private bool MatchKeyword(string keyword)
    {
        var peek = Peek();
        bool match = peek.Kind == TokenKind.Keyword && peek.Lexeme == keyword;
        if (match) Advance();
        return match;
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