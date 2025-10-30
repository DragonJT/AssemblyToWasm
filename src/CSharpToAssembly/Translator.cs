
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpToAssembly;

public static class Translator
{
    public static string Translate(string csharp)
    {
        var tree = CSharpSyntaxTree.ParseText(csharp);
        var root = tree.GetCompilationUnitRoot();

        var sb = new StringBuilder();

        foreach (var cls in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
        {
            foreach (var method in cls.Members.OfType<MethodDeclarationSyntax>())
            {
                bool isImport = HasAttr(method, "import");
                bool isExport = HasAttr(method, "export");

                // Signature line (import/export + return type + name + params)
                var ret = method.ReturnType.ToString();
                var name = method.Identifier.Text;
                var paramList = string.Join(", ",
                    method.ParameterList.Parameters
                            .Select(p => $"{NormalizeType(p.Type)} {p.Identifier.Text}"));

                // IMPORT: no body, just declare
                if (isImport)
                {
                    // Emit signature + exact original body content (between braces) verbatim
                    sb.AppendLine($"import {ret} {name}({paramList})");
                    sb.Append("{");
                    sb.Append(ExtractBodyRaw(method.Body)); // exact copy, no trimming
                    sb.AppendLine("}");
                    sb.AppendLine();
                    continue;
                }
                // EXPORT header if needed
                if (isExport)
                    sb.AppendLine("export");

                sb.AppendLine($"{NormalizeType(method.ReturnType)} {name}({paramList})");
                sb.AppendLine("{");

                // Body -> bytecode
                if (method.Body != null)
                {
                    var emitter = new Emitter(method.ParameterList.Parameters.Select(p => p.Identifier.Text));
                    foreach (var st in method.Body.Statements)
                        emitter.EmitStatement(st, sb);
                }
                else if (method.ExpressionBody != null)
                {
                    var emitter = new Emitter(method.ParameterList.Parameters.Select(p => p.Identifier.Text));
                    emitter.EmitExpression(method.ExpressionBody.Expression, sb, wantValue: false);
                }

                sb.AppendLine("}");
                sb.AppendLine();
            }
        }

        return sb.ToString().TrimEnd();
    }

    private static string ExtractBodyRaw(BlockSyntax? body)
    {
        if (body == null) return "";
        // body.ToFullString() includes braces; slice out the interior text verbatim.
        var full = body.ToFullString();
        int iOpen = full.IndexOf('{');
        int iClose = full.LastIndexOf('}');
        if (iOpen < 0 || iClose < 0 || iClose <= iOpen) return "";
        return full.Substring(iOpen + 1, iClose - iOpen - 1);
    }
        
    private static bool HasAttr(MethodDeclarationSyntax m, string name) =>
        m.AttributeLists
            .SelectMany(al => al.Attributes)
            .Any(a => string.Equals(a.Name.ToString(), name, StringComparison.OrdinalIgnoreCase)
                || string.Equals(a.Name.ToString(), $"[{name}]", StringComparison.OrdinalIgnoreCase));

    private static string NormalizeType(TypeSyntax? t)
    {
        var s = t?.ToString() ?? "void";
        return s switch
        {
            "int" => "int",
            "void" => "void",
            _ => s // extend as you add types
        };
    }

    // Emits stack-style bytecode
    private sealed class Emitter
    {
        private readonly HashSet<string> _params;

        public Emitter(IEnumerable<string> paramNames)
        {
            _params = new HashSet<string>(paramNames ?? Enumerable.Empty<string>(), StringComparer.Ordinal);
        }

        public void EmitStatement(StatementSyntax st, StringBuilder sb)
        {
            switch (st)
            {
                case ExpressionStatementSyntax es:
                    EmitExpression(es.Expression, sb, wantValue: false);
                    break;
                case LocalDeclarationStatementSyntax ld:
                    // int x = expr;
                    foreach (var v in ld.Declaration.Variables)
                    {
                        if (v.Initializer != null)
                        {
                            EmitExpression(v.Initializer.Value, sb, wantValue: true);
                            sb.AppendLine($"    // local.set {v.Identifier.Text}   ; (locals not yet modeled)");
                        }
                    }
                    break;
                case ReturnStatementSyntax rs:
                    if (rs.Expression != null)
                        EmitExpression(rs.Expression, sb, wantValue: true);
                    sb.AppendLine("    // ret            ; (ret lowering later)");
                    break;
                default:
                    sb.AppendLine($"    // unsupported: {st.Kind()}");
                    break;
            }
        }

        public void EmitExpression(ExpressionSyntax expr, StringBuilder sb, bool wantValue)
        {
            switch (expr)
            {
                case LiteralExpressionSyntax lit when lit.IsKind(SyntaxKind.NumericLiteralExpression):
                    if (wantValue)
                        sb.AppendLine($"    int.const {lit.Token.ValueText}");
                    break;

                case IdentifierNameSyntax id:
                    if (wantValue)
                    {
                        // treat params (and potential locals later) as loadable
                        sb.AppendLine($"    local.get {id.Identifier.Text}");
                    }
                    break;

                case ParenthesizedExpressionSyntax par:
                    EmitExpression(par.Expression, sb, wantValue);
                    break;

                case BinaryExpressionSyntax bin:
                    // Handle +,-,*,/ for ints
                    var op = bin.OperatorToken.Kind() switch
                    {
                        SyntaxKind.PlusToken => "int.add",
                        SyntaxKind.MinusToken => "int.sub",
                        SyntaxKind.AsteriskToken => "int.mul",
                        SyntaxKind.SlashToken => "int.div_s", // simple signed div
                        _ => null
                    };
                    if (op == null)
                    {
                        sb.AppendLine($"    // unsupported binary op: {bin.OperatorToken}");
                        break;
                    }
                    EmitExpression(bin.Left, sb, wantValue: true);
                    EmitExpression(bin.Right, sb, wantValue: true);
                    if (wantValue) sb.AppendLine($"    {op}");
                    else sb.AppendLine($"    {op}    // (discarded)");
                    break;

                case InvocationExpressionSyntax inv:
                    // Evaluate args (left-to-right), then call
                    var name = GetCallTarget(inv.Expression);
                    var args = inv.ArgumentList?.Arguments ?? default;
                    foreach (var arg in args)
                        EmitExpression(arg.Expression, sb, wantValue: true);

                    sb.AppendLine($"    call {name}");
                    break;

                case MemberAccessExpressionSyntax ma:
                    // Something like console.log(x) will be handled as part of InvocationExpressionSyntax.
                    if (wantValue)
                        sb.AppendLine($"    // member access value unused: {ma}");
                    break;

                default:
                    sb.AppendLine($"    // unsupported expr: {expr.Kind()}");
                    break;
            }
        }

        private static string GetCallTarget(ExpressionSyntax callee)
        {
            return callee switch
            {
                IdentifierNameSyntax id => id.Identifier.Text,
                MemberAccessExpressionSyntax ma => $"{ma.Expression}.{ma.Name.Identifier.Text}",
                _ => callee.ToString()
            };
        }
    }
}

