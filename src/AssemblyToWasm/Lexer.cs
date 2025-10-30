namespace AssemblyToWasm;

using System.Text;

enum TokenKind
{
    EOF,
    Identifier,
    Keyword,        // if in the provided keyword set
    Integer,
    String,

    // punctuation / delimiters
    Comma,          // ,
    Semicolon,      // ;
    Colon,          // :
    Dot,            // .
    LParen,         // (
    RParen,         // )
    LBrace,         // {
    RBrace,         // }
    LBracket,       // [
    RBracket,       // ]

    // basic operators (extend later if you want)
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Equal,          // =
    Arrow,          // ->

    Unknown
}

enum TriviaKind
{
    Whitespace,    // spaces/tabs
    NewLine,       // \n or \r\n
    CommentLine,   // ; ... or // ...
    CommentBlock   // /* ... */
}

sealed record Trivia(TriviaKind Kind, string Text, int Line, int Column);

sealed record Token(TokenKind Kind, string Lexeme, int Line, int Column, IReadOnlyList<Trivia> LeadingTrivia)
{
    public string ToRawCode()
    {
        StringBuilder sb = new();
        foreach (var tr in LeadingTrivia)
            sb.Append(tr.Text);
        sb.Append(Lexeme);
        return sb.ToString();
    }

    public override string ToString()
        => $"{Kind} '{Lexeme}' @{Line}:{Column}";
}

sealed class Lexer
{
    private readonly string _src;
    private readonly string[] _keywords;

    private int _i;
    private int _line = 1;
    private int _col = 1;

    public Lexer(string source, string[] keywords)
    {
        _src = source;
        _keywords = keywords;
    }

    public IEnumerable<Token> Tokenize()
    {
        while (true)
        {
            var leading = CollectTrivia(); // gather everything until the next real token or EOF

            if (IsEOF)
            {
                yield return new Token(TokenKind.EOF, "", _line, _col, LeadingTrivia: leading);
                yield break;
            }

            int tokLine = _line, tokCol = _col;
            Token tok;

            // 2-char: ->
            if (Peek() == '-' && Peek(1) == '>')
            {
                Advance(); Advance();
                tok = new Token(TokenKind.Arrow, "->", tokLine, tokCol, LeadingTrivia: leading);
                yield return tok;
                continue;
            }

            // single-char punct & quotes
            switch (Peek())
            {
                case ',': yield return MakeSingle(TokenKind.Comma, leading); continue;
                case ';': yield return MakeSingle(TokenKind.Semicolon, leading); continue;
                case ':': yield return MakeSingle(TokenKind.Colon, leading); continue;
                case '.': yield return MakeSingle(TokenKind.Dot, leading); continue;
                case '(': yield return MakeSingle(TokenKind.LParen, leading); continue;
                case ')': yield return MakeSingle(TokenKind.RParen, leading); continue;
                case '{': yield return MakeSingle(TokenKind.LBrace, leading); continue;
                case '}': yield return MakeSingle(TokenKind.RBrace, leading); continue;
                case '[': yield return MakeSingle(TokenKind.LBracket, leading); continue;
                case ']': yield return MakeSingle(TokenKind.RBracket, leading); continue;
                case '+': yield return MakeSingle(TokenKind.Plus, leading); continue;
                case '*': yield return MakeSingle(TokenKind.Star, leading); continue;
                case '/': yield return MakeSingle(TokenKind.Slash, leading); continue;
                case '%': yield return MakeSingle(TokenKind.Percent, leading); continue;
                case '=': yield return MakeSingle(TokenKind.Equal, leading); continue;
                case '"': yield return ConsumeString(leading); continue;
            }

            // numbers: dec/hex with optional leading '-'
            if (IsNumberStart(Peek()) ||
                (Peek() == '-' && (IsDigit(Peek(1)) || (Peek(1) == '0' && (Peek(2) is 'x' or 'X')))))
            {
                yield return ConsumeInteger(leading);
                continue;
            }

            // identifiers/keywords
            if (IsIdentStart(Peek()))
            {
                yield return ConsumeIdentifier(leading);
                continue;
            }

            // unknown fallback
            var unk = Advance().ToString();
            yield return new Token(TokenKind.Unknown, unk, tokLine, tokCol, LeadingTrivia: leading);
        }
    }

    // ---- trivia ----

    private List<Trivia> CollectTrivia()
    {
        var trivia = new List<Trivia>();
        while (!IsEOF)
        {
            char c = Peek();

            // spaces/tabs
            if (c == ' ' || c == '\t')
            {
                int startLine = _line, startCol = _col;
                var sb = new StringBuilder();
                while (!IsEOF && (Peek() == ' ' || Peek() == '\t'))
                    sb.Append(Advance());
                trivia.Add(new Trivia(TriviaKind.Whitespace, sb.ToString(), startLine, startCol));
                continue;
            }

            // newlines
            if (c == '\r' || c == '\n')
            {
                int startLine = _line, startCol = _col;
                string text;
                if (c == '\r' && Peek(1) == '\n') { Advance(); Advance(); text = "\r\n"; }
                else { Advance(); text = "\n"; }
                trivia.Add(new Trivia(TriviaKind.NewLine, text, startLine, startCol));
                _line++;
                _col = 1;
                continue;
            }

            // // line comment
            if (c == '/' && Peek(1) == '/')
            {
                int startLine = _line, startCol = _col;
                Advance(); Advance();
                var sb = new StringBuilder("//");
                while (!IsEOF && Peek() != '\r' && Peek() != '\n')
                    sb.Append(Advance());
                trivia.Add(new Trivia(TriviaKind.CommentLine, sb.ToString(), startLine, startCol));
                continue;
            }

            // /* block comment */
            if (c == '/' && Peek(1) == '*')
            {
                int startLine = _line, startCol = _col;
                Advance(); Advance(); // /*
                var sb = new StringBuilder("/*");
                while (!IsEOF && !(Peek() == '*' && Peek(1) == '/'))
                {
                    if (Peek() == '\r' || Peek() == '\n')
                    {
                        // normalize lines while preserving text
                        if (Peek() == '\r' && Peek(1) == '\n') { sb.Append("\r\n"); Advance(); Advance(); _line++; _col = 1; }
                        else { sb.Append('\n'); Advance(); _line++; _col = 1; }
                    }
                    else
                    {
                        sb.Append(Advance());
                    }
                }
                if (!IsEOF) { Advance(); Advance(); sb.Append("*/"); }
                trivia.Add(new Trivia(TriviaKind.CommentBlock, sb.ToString(), startLine, startCol));
                continue;
            }

            break; // next is a real token
        }
        return trivia;
    }

    // ---- token consumers ----

    private Token MakeSingle(TokenKind kind, IReadOnlyList<Trivia> leading)
    {
        int line = _line, col = _col;
        char c = Advance();
        return new Token(kind, c.ToString(), line, col, LeadingTrivia: leading);
    }

    private Token ConsumeIdentifier(IReadOnlyList<Trivia> leading)
    {
        int line = _line, col = _col;
        var sb = new StringBuilder();
        sb.Append(Advance());
        while (!IsEOF && IsIdentPart(Peek()))
            sb.Append(Advance());
        string lex = sb.ToString();
        var kind = _keywords.Contains(lex) ? TokenKind.Keyword : TokenKind.Identifier;
        return new Token(kind, lex, line, col, LeadingTrivia: leading);
    }

    private Token ConsumeInteger(IReadOnlyList<Trivia> leading)
    {
        int line = _line, col = _col;
        int start = _i;
        var sb = new StringBuilder();

        if (Peek() == '-') sb.Append(Advance());

        if (Peek() == '0' && (Peek(1) is 'x' or 'X'))
        {
            sb.Append(Advance()); sb.Append(Advance()); // 0x
            if (!IsHexDigit(Peek()))
                return UnknownFrom(start, leading);

            while (!IsEOF && (IsHexDigit(Peek()) || Peek() == '_'))
                sb.Append(Advance());

            return new Token(TokenKind.Integer, Sub(start, _i), line, col, leading);
        }
        else
        {
            if (!IsDigit(Peek())) return UnknownFrom(start, leading);
            while (!IsEOF && (IsDigit(Peek()) || Peek() == '_'))
                sb.Append(Advance());
            return new Token(TokenKind.Integer, Sub(start, _i), line, col, leading);
        }
    }

    private Token ConsumeString(IReadOnlyList<Trivia> leading)
    {
        int line = _line, col = _col;
        int start = _i;
        Advance(); // "

        var sb = new StringBuilder();
        bool closed = false;

        while (!IsEOF)
        {
            char c = Advance();
            if (c == '"') { closed = true; break; }
            if (c == '\\')
            {
                if (IsEOF) break;
                char e = Advance();
                switch (e)
                {
                    case '"': sb.Append('\"'); break;
                    case '\\': sb.Append('\\'); break;
                    case 'n': sb.Append('\n'); break;
                    case 't': sb.Append('\t'); break;
                    case 'r': sb.Append('\r'); break;
                    case 'x':
                        char h1 = Peek(), h2 = Peek(1);
                        if (IsHexDigit(h1) && IsHexDigit(h2))
                        { Advance(); Advance(); sb.Append((char)Convert.ToInt32($"{h1}{h2}", 16)); }
                        else return UnknownFrom(start, leading);
                        break;
                    default: return UnknownFrom(start, leading);
                }
            }
            else
            {
                sb.Append(c);
            }
        }

        if (!closed) return UnknownFrom(start, leading);

        return new Token(TokenKind.String, Sub(start, _i), line, col, leading);
    }

    private Token UnknownFrom(int start, IReadOnlyList<Trivia> leading)
    {
        int line = _line, col = _col;
        string lex = Sub(start, _i);
        if (lex.Length == 0 && !IsEOF) lex = Advance().ToString();
        return new Token(TokenKind.Unknown, lex, line, col, LeadingTrivia: leading);
    }

    // ---- char helpers ----

    private static bool IsDigit(char c) => c is >= '0' and <= '9';
    private static bool IsHexDigit(char c) =>
        (c is >= '0' and <= '9') || (c is >= 'a' and <= 'f') || (c is >= 'A' and <= 'F');
    private static bool IsIdentStart(char c) =>
        (c is >= 'A' and <= 'Z') || (c is >= 'a' and <= 'z') || c == '_';
    private static bool IsIdentPart(char c) => IsIdentStart(c) || IsDigit(c);
    private bool IsNumberStart(char c) =>
        IsDigit(c) || (c == '0' && (Peek(1) is 'x' or 'X'));

    private bool IsEOF => _i >= _src.Length;
    private char Peek(int la = 0) => (_i + la) < _src.Length ? _src[_i + la] : '\0';
    private char Advance()
    {
        char c = _src[_i++];
        _col++;
        return c;
    }

    private string Sub(int start, int endExclusive) =>
        _src.Substring(start, Math.Max(0, endExclusive - start));
}
