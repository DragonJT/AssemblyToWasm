using System.Text;

namespace ForthLike;

record GlobalInfo(string Type, int MemLocation);

class AsmEmitter
{
    List<IMember> members;
    Dictionary<string, GlobalInfo> globalInfos = [];
    
    public AsmEmitter(List<IMember> members)
    {
        this.members = members;
    }

    string EmitCode(List<Token> tokens, int depth)
    {
        var indentString = new string(' ', depth * 4);
        var code = new StringBuilder();
        foreach (var t in tokens)
        {
            if (t.Kind == TokenKind.Keyword)
            {
                if (t.Lexeme == "load")
                {
                    code.AppendLine(indentString + "int.load");
                }
                else if (t.Lexeme == "store")
                {
                    code.AppendLine(indentString + "int.store");
                }
                else if (t.Lexeme == "if")
                {
                    code.AppendLine(indentString + "if");
                }
                else
                {
                    throw new Exception("Unexpected keyword " + t.Lexeme);
                }
            }
            else if (t.Kind == TokenKind.Identifier)
            {
                if (globalInfos.TryGetValue(t.Lexeme, out GlobalInfo? globalInfo))
                {
                    code.AppendLine(indentString + "int.const " + globalInfo.MemLocation);
                }
                else
                {
                    code.AppendLine(indentString + "call " + t.Lexeme);
                }
            }
            else if (t.Kind == TokenKind.Integer)
            {
                code.AppendLine(indentString + "int.const " + t.Lexeme);
            }
            else if (t.Kind == TokenKind.Plus)
            {
                code.AppendLine(indentString + "int.add");
            }
            else if (t.Kind == TokenKind.GT)
            {
                code.AppendLine(indentString + "int.gt_s");
            }
            else if (t.Kind == TokenKind.LT)
            {
                code.AppendLine(indentString + "int.lt_s");
            }
            else if (t.Kind == TokenKind.LBrace)
            {
                code.AppendLine(indentString + "{");
                depth++;
                indentString = new string(' ', depth * 4);

            }
            else if(t.Kind == TokenKind.RBrace)
            {
                depth--;
                indentString = new string(' ', depth * 4);
                code.AppendLine(indentString + "}");
            }
            else
            {
                throw new Exception($"Unexpected token {t.Kind}:{t.Lexeme}");
            }
        }
        return code.ToString();
    }

    public string Emit()
    {
        int memLocation = 0;
        var globals = members.OfType<Global>().ToArray();
        foreach (var g in globals)
        {
            globalInfos.Add(g.Name, new GlobalInfo(g.Type, memLocation));
            memLocation += 4;
        }
        StringBuilder code = new();
        foreach (var m in members)
        {
            if (m is Function function)
            {
                if (function.Export)
                {
                    code.Append("export ");
                }
                code.Append(function.ReturnType);
                code.Append(' ');
                code.Append(function.Name);
                code.Append('(');
                for (var i = 0; i < function.Parameters.Length; i++)
                {
                    code.Append(function.Parameters[i].Type + " ");
                    code.Append(function.Parameters[i].Name);
                    if (i < function.Parameters.Length - 1) code.Append(", ");
                }
                code.AppendLine(")");
                code.AppendLine("{");
                if(function.Name == "Main")
                {
                    foreach(var g in globals)
                    {
                        code.AppendLine("    int.const " + globalInfos[g.Name].MemLocation);
                        code.Append(EmitCode(g.Code, 1));
                        code.AppendLine("    int.store ");
                    }
                }
                code.Append(EmitCode(function.Code, 1));
                code.AppendLine("}");
            }
            else if (m is ImportFunction importFunction)
            {
                code.Append("import ");
                code.Append(importFunction.ReturnType);
                code.Append(' ');
                code.Append(importFunction.Name);
                code.Append('(');
                for (var i = 0; i < importFunction.Parameters.Length; i++)
                {
                    code.Append(importFunction.Parameters[i].Type + " ");
                    code.Append(importFunction.Parameters[i].Name);
                    if (i < importFunction.Parameters.Length - 1) code.Append(", ");
                }
                code.AppendLine(")");
                code.Append('{');
                code.AppendLine(importFunction.Code);
                code.AppendLine("}");
            }
            else if (m is Global global)
            {

            }
            else
            {
                throw new Exception();
            }
        }
        return code.ToString();
    }
}