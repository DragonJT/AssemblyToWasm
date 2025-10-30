
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace CSharpToAssembly
{
    public static class Translator
    {
        // Hardcode memory address for Program.p
        private const string FieldQName = "Program.p";
        private const int    FieldAddr  = 0;

        public static string Translate(string csharp)
        {
            var tree = CSharpSyntaxTree.ParseText(csharp);
            var root = tree.GetCompilationUnitRoot();

            var sb = new StringBuilder();

            foreach (var cls in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
            foreach (var method in cls.Members.OfType<MethodDeclarationSyntax>())
            {
                bool isImport = HasAttr(method, "import");
                bool isExport = HasAttr(method, "export");

                string name = method.Identifier.Text;
                // Map Run -> Main to match your target output
                if (string.Equals(name, "Run", StringComparison.Ordinal)) name = "Main";

                string sig = BuildSignature(method, name);

                if (isImport)
                {
                    sb.AppendLine($"import {sig}");
                    sb.AppendLine("{");
                    sb.Append(ExtractBodyRaw(method.Body)); // exact copy
                    sb.AppendLine("}");
                    sb.AppendLine();
                    continue;
                }

                if (isExport) sb.Append("export ");
                sb.AppendLine(sig);
                sb.AppendLine("{");

                var emit = new Emitter(sb);
                emit.EmitMethodBody(method);

                sb.AppendLine("}");
                sb.AppendLine();
            }

            return sb.ToString().TrimEnd();
        }

        // ---------- helpers ----------

        private static bool HasAttr(MethodDeclarationSyntax m, string name) =>
            m.AttributeLists.SelectMany(al => al.Attributes)
             .Any(a => string.Equals(a.Name.ToString(), name, StringComparison.OrdinalIgnoreCase));

        private static string NormalizeType(TypeSyntax? t) =>
            (t?.ToString() ?? "void") switch { "int" => "int", "void" => "void", var s => s };

        private static string BuildSignature(MethodDeclarationSyntax m, string outName)
        {
            var ret = NormalizeType(m.ReturnType);
            var parms = string.Join(", ",
                m.ParameterList.Parameters.Select(p => $"{NormalizeType(p.Type)} {p.Identifier.Text}"));
            return $"{ret} {outName}({parms})";
        }

        private static string ExtractBodyRaw(BlockSyntax? body)
        {
            if (body == null) return "";
            var text = body.SyntaxTree.GetText();
            var start = body.OpenBraceToken.FullSpan.End;
            var end   = body.CloseBraceToken.FullSpan.Start;
            return text.ToString(TextSpan.FromBounds(start, end)); // exact interior, no extra leading blank line
        }

        // ====================================================================
        // E M I T T E R
        // ====================================================================
        private sealed class Emitter
        {
            private readonly StringBuilder _sb;

            public Emitter(StringBuilder sb) => _sb = sb;

            const int FieldAddr = 0;

            void EmitLoadP() {
                _sb.AppendLine($"    int.const {FieldAddr}");
                _sb.AppendLine("    int.load");
            }

            void EmitStoreP_FromStackValue() {
                // stack: [..., value]  -> store to p
                _sb.AppendLine($"    int.const {FieldAddr}"); // address
                _sb.AppendLine("    int.store");
            }

            void EmitSetP_Const(int v) {
                _sb.AppendLine($"    int.const {FieldAddr}"); // address
                _sb.AppendLine($"    int.const {v}");         // value
                _sb.AppendLine("    int.store");
            }

            void EmitIncP() {
                // p = p + 1, address-first pattern (dup addr for load then store)
                _sb.AppendLine($"    int.const {FieldAddr}"); // addr for store (kept)
                _sb.AppendLine($"    int.const {FieldAddr}"); // addr for load
                _sb.AppendLine("    int.load");
                _sb.AppendLine("    int.const 1");
                _sb.AppendLine("    int.add");                // value
                _sb.AppendLine("    int.store");
            }

            public void EmitMethodBody(MethodDeclarationSyntax method)
            {
                if (method.Body != null)
                {
                    foreach (var st in method.Body.Statements)
                        EmitStatement(st);
                }
                else if (method.ExpressionBody != null)
                {
                    EmitExpression(method.ExpressionBody.Expression, wantValue: false);
                }
            }

            // ---- statements ----
            private void EmitStatement(StatementSyntax st)
            {
                switch (st)
                {
                    case ExpressionStatementSyntax es:
                        EmitExpression(es.Expression, wantValue: false);
                        break;

                    case ReturnStatementSyntax rs:
                        if (rs.Expression != null) EmitExpression(rs.Expression, wantValue: true);
                        _sb.AppendLine("    // ret");
                        break;

                    case IfStatementSyntax iff:
                        EmitIf(iff);
                        break;

                    case BlockSyntax b:
                        foreach (var s in b.Statements) EmitStatement(s);
                        break;

                    case ForStatementSyntax fs:
                        EmitFor(fs);
                        break;

                    default:
                        _sb.AppendLine($"    // unsupported: {st.Kind()}");
                        break;
                }
            }

            private void EmitIf(IfStatementSyntax iff)
            {
                EmitExpression(iff.Condition, true);
                _sb.AppendLine("    if{");
                EmitStatement(iff.Statement);
                _sb.AppendLine("    }");
                if (iff.Else != null)
                {
                    _sb.AppendLine("    else{");
                    EmitStatement(iff.Else.Statement);
                    _sb.AppendLine("    }");
                }
            }

            private void EmitFor(ForStatementSyntax fs)
            {
                // init
                if (fs.Declaration != null)
                {
                    foreach (var v in fs.Declaration.Variables)
                    {
                        if (v.Initializer != null)
                        {
                            // address-first store for locals-in-memory not supported; your sample uses only 'p' as global.
                            // If you later spill locals to memory, add a symbol table here.
                            EmitExpression(v.Initializer.Value, wantValue: false);
                        }
                    }
                }
                foreach (var init in fs.Initializers) EmitExpression(init, wantValue: false);

                _sb.AppendLine("    loop{");

                if (fs.Condition != null)
                {
                    EmitExpression(fs.Condition, true);
                    _sb.AppendLine("        int.eqz");
                    _sb.AppendLine("        if{");
                    _sb.AppendLine("            break");
                    _sb.AppendLine("        }");
                }

                if (fs.Statement is BlockSyntax blk)
                    foreach (var s in blk.Statements) EmitStatement(s);
                else
                    EmitStatement(fs.Statement);

                foreach (var inc in fs.Incrementors) EmitExpression(inc, wantValue: false);

                _sb.AppendLine("    }");
            }

            // ---- expressions ----
    
            private void EmitExpression(ExpressionSyntax expr, bool wantValue)
            {
                switch (expr)
                {
                    case IdentifierNameSyntax id:
                        if (IsFieldP(id)) { if (wantValue) EmitLoadP(); }
                        else if (wantValue) _sb.AppendLine($"    local.get {id.Identifier.Text}");
                        return;

                    case PostfixUnaryExpressionSyntax post:
                        // p++ / p--
                        if (post.Operand is IdentifierNameSyntax pid && IsFieldP(pid))
                        {
                            bool isInc = post.IsKind(SyntaxKind.PostIncrementExpression);
                            bool isDec = post.IsKind(SyntaxKind.PostDecrementExpression);
                            if (wantValue) EmitLoadP(); // old value
                            _sb.AppendLine($"    int.const {FieldAddr}");
                            _sb.AppendLine($"    int.const {FieldAddr}");
                            _sb.AppendLine("    int.load");
                            _sb.AppendLine("    int.const 1");
                            _sb.AppendLine(isInc ? "    int.add" : "    int.sub");
                            _sb.AppendLine("    int.store");
                            return;
                        }
                        _sb.AppendLine($"    // unsupported postfix: {post.Kind()}");
                        return;

                    case AssignmentExpressionSyntax asn:
                        // p = <expr>
                        if (asn.Left is IdentifierNameSyntax lid && IsFieldP(lid))
                        {
                            // address first, then value, then store:
                            _sb.AppendLine($"    int.const {FieldAddr}");
                            EmitExpression(asn.Right, wantValue: true); // leaves value on top
                            _sb.AppendLine("    int.store");
                            if (wantValue) EmitLoadP(); // if used as r-value
                            return;
                        }
                        // local = <expr>
                        if (asn.Left is IdentifierNameSyntax lcl)
                        {
                            EmitExpression(asn.Right, true);
                            _sb.AppendLine($"    local.set {lcl.Identifier.Text}");
                            if (wantValue) _sb.AppendLine($"    local.get {lcl.Identifier.Text}");
                            return;
                        }
                        _sb.AppendLine("    // unsupported assignment");
                        return;

                    case LiteralExpressionSyntax lit when lit.IsKind(SyntaxKind.NumericLiteralExpression):
                        if (wantValue) _sb.AppendLine($"    int.const {lit.Token.ValueText}");
                        return;


                    case InvocationExpressionSyntax inv:
                        EmitInvocation(inv);
                        if (!wantValue)
                        {
                            // if the call returns a value but used as stmt, you may add 'drop' later
                        }
                        return;

                    case BinaryExpressionSyntax bin:
                        EmitBinary(bin, wantValue);
                        return;

                    case ParenthesizedExpressionSyntax par:
                        EmitExpression(par.Expression, wantValue);
                        return;

                    default:
                        _sb.AppendLine($"    // unsupported expr: {expr.Kind()}");
                        return;
                }
            }

            private void EmitInvocation(InvocationExpressionSyntax inv)
            {
                // args
                foreach (var a in inv.ArgumentList?.Arguments ?? default)
                    EmitExpression(a.Expression, true);

                var name = inv.Expression switch
                {
                    IdentifierNameSyntax iid => iid.Identifier.Text,
                    MemberAccessExpressionSyntax ma => $"{ma.Expression}.{ma.Name.Identifier.Text}",
                    _ => inv.Expression.ToString()
                };
                _sb.AppendLine($"    call {name}");
            }

            private void EmitBinary(BinaryExpressionSyntax bin, bool wantValue)
            {
                string? op = bin.OperatorToken.Kind() switch
                {
                    SyntaxKind.PlusToken => "int.add",
                    SyntaxKind.MinusToken => "int.sub",
                    SyntaxKind.AsteriskToken => "int.mul",
                    SyntaxKind.SlashToken => "int.div_s",
                    SyntaxKind.GreaterThanToken => "int.gt_s",
                    SyntaxKind.LessThanToken => "int.lt_s",
                    SyntaxKind.EqualsEqualsToken => "int.eq",
                    SyntaxKind.ExclamationEqualsToken => "int.ne",
                    _ => null
                };
                if (op == null)
                {
                    _sb.AppendLine($"    // unsupported binary: {bin.OperatorToken}");
                    return;
                }

                EmitExpression(bin.Left, true);
                EmitExpression(bin.Right, true);
                if (wantValue) _sb.AppendLine($"    {op}");
                else _sb.AppendLine($"    {op} // (discarded)");
            }

            private static bool IsFieldP(IdentifierNameSyntax id) =>
                string.Equals(id.Identifier.Text, "p", StringComparison.Ordinal);
        }
    }
}
